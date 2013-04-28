
// NOTE: If warnings appear, you may need to retarget this project to .NET 4.0. Show the Solution
// Pad, right-click on the project node, choose 'Options --> Build --> General' and change the target
// framework to .NET 4.0 or .NET 4.5.

module GitStats.Main

open System
open System.Diagnostics
open System.Collections.Generic
open System.Collections.Concurrent
open Microsoft.FSharp
      
let commitCollector =
    let authorStats = ConcurrentDictionary<Author, CommitStats> ()
    MailboxProcessor.Start (fun inbox ->
        let rec loop () = async {
            let! msg, (replyChan : AsyncReplyChannel<unit> option) = inbox.Receive ()
            let reply () =
                match replyChan with
                | Some ch -> ch.Reply ()
                | _ -> ()

            match msg with
            | CollectInstruction.Data x ->
                let commit = Commit.create x
                let plus = commit.files |> Seq.filter (fun x -> x.plus.IsSome) |> Seq.sumBy (fun x -> x.plus.Value)
                let minus = commit.files |> Seq.filter (fun x -> x.minus.IsSome) |> Seq.sumBy (fun x -> x.minus.Value)
                authorStats.AddOrUpdate (
                    commit.author,
                    Func<_,_>(fun _ -> { commits = 1; plus = plus; minus = minus }),
                    Func<_,_,_>(fun (_, x) -> (fun x -> { commits = x.commits + 1; plus = x.plus + plus; minus = x.minus + minus }))) // wtf?
                |> ignore
                sprintf "Collected %s" commit.hash |> info
                reply ()
                return! loop () // wait for next
            | CollectInstruction.Finished ->
                "Done collecting." |> info
                authorStats |> Seq.iter (fun x -> printfn "%s\n%A" (snd x.Key) x.Value)
                reply ()
                return () // break loop
        }
        loop ())
    
let parse data =
    match data with
    | Regex "^commit (.+)$" [ hash ] ->
        sprintf "  parsed hash %s" hash |> debug
        Commit hash
    | Regex "^Author: (.+?) <(.+)>$" [ name; email ] ->
        sprintf "  parsed author %s %s" name email |> debug
        Author (name, email)
    | Regex @"^Date:\s+(.+?) (.+?) (\d+) (\d{2}:\d{2}:\d{2}) (\d+) ([-+]\d{2})(\d{2})" [ dow; mon; day; time; year; offsetH; offsetM ] ->
        let dateStr =
            sprintf "%s %s %s %s %s %s:%s"
                dow mon day time year offsetH offsetM
        let success, date =
            DateTime.TryParseExact (
                dateStr, "ddd MMM d HH:mm:ss yyyy zzz",
                System.Globalization.CultureInfo.InvariantCulture,
                System.Globalization.DateTimeStyles.None)
        if success
        then
            sprintf "  parsed timestamp %s" <| date.ToString () |> debug
            Timestamp date
        else failwith "Could not parse date."
    | Regex "^ {4}(.+)$" [ msg ] ->
        sprintf "  parsed message %s" msg |> debug
        Message msg
    | Regex "^$" [] -> Blank
    | Regex @"^:(\d+) (\d+) ([\da-f\.]+) ([\da-f\.]+) ([AMD])\s+(.+)" [ modeB; modeA; hashB; hashA; change; path ] ->
        sprintf "  parsed file %s" path |> debug
        File { path = path.Trim (); change = Change.parse change; mode = modeB, modeA; hash = hashB, hashA }
    | Regex @"^(\d+)\s+(\d+)\s+(.+)$" [ plus; minus; path ] ->
        sprintf "  parsed stat %s" path |> debug
        let parseNum = function
            | "-" -> None
            | x -> Some <| int x
            
        let plus = parseNum plus
        let minus = parseNum minus
        let binary =
            match plus, minus with
            | None, None -> true
            | Some _, Some _ -> false
            | _ ->
                sprintf "Could not parse stat %s" data |> warn
                true
        Stat { path = path.Trim (); binary = binary; plus = plus; minus = plus }
    | _ ->
        sprintf "  did not parse %s" data |> debug
        Blank

let parser =
    MailboxProcessor.Start (fun inbox ->
        let rec loop acc = async {
            let! msg, (replyChan : AsyncReplyChannel<unit> option) = inbox.Receive ()
            let reply () =
                match replyChan with
                | Some ch -> ch.Reply ()
                | _ -> ()
            
            let postToCollector =
                List.rev
                >> CollectInstruction.Data
                >> (fun x -> (x, None))
                >> commitCollector.Post
            
            match msg with
            | Data x ->
                sprintf "Parser received %A" x |> debug
                let part = parse x
                let cont =
                    match part with
                    | Commit hash ->
                        match acc with
                        | [] -> part :: acc
                        | _ ->
                            postToCollector acc
                            part :: []
                    | Blank -> acc
                    | _ -> part :: acc
                reply ()
                return! loop cont
            | Finished ->
                "Done parsing." |> info
                postToCollector acc
                commitCollector.PostAndReply (fun ch -> CollectInstruction.Finished, Some ch)
                reply ()
                return ()
        }
        
        loop [])

let collectOutput =
    (fun (x:DataReceivedEventArgs) ->
        match x.Data with
        | null -> ()
        | _ -> parser.Post (Data x.Data, None))

let run cmd args wd =
    let args = "log --all --raw --no-color --no-merges --numstat " + args
    
    sprintf "%s %s" cmd args |> debug

    let psi = ProcessStartInfo ()
    psi.FileName <- cmd
    psi.Arguments <- args
    psi.WorkingDirectory <- wd
    psi.CreateNoWindow <- true
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false
    
    use proc = new Process ()
    proc.StartInfo <- psi
    use listener = proc.OutputDataReceived.Subscribe (collectOutput)
    proc.EnableRaisingEvents <- true
    proc.Start () |> ignore
    proc.BeginOutputReadLine ()

    Async.AwaitEvent proc.Exited
    |> Async.RunSynchronously
    |> ignore
    
    match proc.ExitCode with
    | 0 -> "Done running Git." |> info
    | x ->
        proc.StandardError.ReadToEnd () |> error
        Environment.Exit x
    
    parser.PostAndReply (fun ch -> Finished, Some ch)
    
    printfn "Everyone is done."

[<EntryPoint>]
let main args =
    let parsedArgs =
        let rec loop lst curr acc rest =
            let pushCurr () = (fst curr, snd curr |> List.rev) :: acc
            match lst with
            | [] -> pushCurr () |> Map.ofList
            | x::xs ->
                match rest with
                | true ->
                    loop xs (fst curr, x :: snd curr) acc true
                | _ ->
                    match x with
                    | Regex "^--(.+)" [ arg ] -> loop xs (arg, []) (pushCurr ()) false
                    | Regex "^--$" [] -> loop xs ("--", []) (pushCurr ()) true
                    | _ -> loop xs (fst curr, x :: snd curr) acc false
        loop (args |> List.ofArray) ("", []) [] false
    
    if parsedArgs.ContainsKey "debug"
    then
        match parsedArgs.["debug"] with
        | [] -> debugLvl := DebugLevel.Debug
        | x::xs -> debugLvl := DebugLevel.parse x
        
    sprintf "%A" parsedArgs |> debug

    let workingDir =        
        if parsedArgs.[""] <> List.empty
        then parsedArgs.[""] |> List.head
        else Environment.CurrentDirectory
    
    commitCollector.Error.Add (fun e ->
        sprintf "Commit collector failed: %A" e |> error
        Environment.Exit 1)
    
    parser.Error.Add (fun e ->
        sprintf "Parser failed: %A" e |> error
        Environment.Exit 2)
 
    let gitArgs =   
        if parsedArgs.ContainsKey "--"
        then String.Join (" ", parsedArgs.["--"])
        else ""
    run "/usr/local/bin/git" gitArgs workingDir
    0

