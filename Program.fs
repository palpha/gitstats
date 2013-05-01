
// NOTE: If warnings appear, you may need to retarget this project to .NET 4.0. Show the Solution
// Pad, right-click on the project node, choose 'Options --> Build --> General' and change the target
// framework to .NET 4.0 or .NET 4.5.

module Main

open System
open System.Diagnostics
open System.Collections.Generic
open System.Collections.Concurrent
open Microsoft.FSharp
open System.Threading

let mutable doTime = false
let mutable aliases = Map.empty
let mutable skipExts = List.empty
let mutable inclExts = List.empty

let inclExt =
    let inclSet = inclExts |> set
    let skipSet = skipExts |> set
    let cache = ConcurrentDictionary<_,_> ()
    fun ext ->
        match cache.TryGetValue ext with
        | true, x -> x
        | _ ->
            let result =
                match skipSet.Contains ext, inclSet.Count with
                | true, _ -> false
                | false, 0 -> true
                | _, x when x > 0 && inclSet.Contains ext -> true
                | _ -> false
            cache.TryAdd (ext, result) |> ignore
            result

let timeSpent = ConcurrentDictionary<_,_> ()
let time x f =
    match doTime with
    | false -> f ()
    | _ ->
        let sw = Diagnostics.Stopwatch.StartNew ()
        let result = f ()
        sw.Stop ()
        timeSpent.TryAdd (x, ConcurrentBag<_> ()) |> ignore
        timeSpent.[x].Add (sw.ElapsedMilliseconds)
        result
        
let getExt path =
    let m = Text.RegularExpressions.Regex.Match (path, @"\.([^/\.]+)$")
    if m.Success
    then m.Groups.[1].Value
    else String.Empty
            
let commitCollector =
    let activityStats = ConcurrentDictionary<_,_> ()
    let allExts = ref (set [] : Set<string>)
    MailboxProcessor.Start (fun inbox ->
        let rec loop () = async {
            let! msg, (replyChan : AsyncReplyChannel<IDictionary<_,_>> option) = inbox.Receive ()
            
            let reply =
                match replyChan with
                | Some ch -> (function
                    | Some x -> x :> IDictionary<_,_>
                    | _ -> failwith "Invalid response.") >> ch.Reply
                | _ -> (fun x -> ())

            match msg with
            | CollectInstruction.Data x ->
                time "collect" (fun () ->
                    let commit = Commit.create x
                    
                    let files =
                        commit.files
                        |> Seq.groupBy (fun x -> getExt x.path)
                        |> Seq.map (fun (k, g) ->
                            k,
                            { plus = g |> Seq.choose (fun x -> x.plus) |> Seq.sum
                              minus = g |> Seq.choose (fun x -> x.minus) |> Seq.sum
                              current = 0 })
                        |> dict
                        
                    Interlocked.Exchange (allExts, files.Keys |> set |> Set.union !allExts) |> ignore
                    
                    activityStats.AddOrUpdate (
                        commit.author,
                        Func<_,_>(fun _ ->
                            { commits = 1
                              dates = set [ commit.timestamp.Date ]
                              first = commit.timestamp.Date
                              last = commit.timestamp.Date
                              files = Dictionary<_,_> (files) }),
                        Func<_,_,_>(fun (_, x) -> (fun x ->
                            { commits = x.commits + 1
                              dates = x.dates |> Set.add commit.timestamp.Date
                              first = commit.timestamp.Date
                              last = x.last
                              files =
                                x.files.Keys
                                |> Seq.append files.Keys
                                |> Seq.map (fun k ->
                                    let get (x:IDictionary<_,_>) =
                                        match x.TryGetValue k with true, x -> x | _ -> { plus = 0; minus = 0; current = 0 }
                                    let xf = get x.files
                                    let nf = get files
                                    k, { plus = xf.plus + nf.plus; minus = xf.minus + nf.minus; current = 0 })
                                |> dict })))
                    |> ignore
                    sprintf "Collected %s" commit.hash |> info
                    reply None)
                return! loop () // wait for next
            | CollectInstruction.Finished ->
                "Done collecting." |> info
                reply <| Some activityStats
                return () // break loop
        }
        loop ())
    
let parseLog data =
    time "parse" (fun () ->
        match data with
        | Regex "^commit (.+)$" [ hash ] ->
            sprintf "  parsed hash %s" hash |> debug
            Commit hash
        | Regex "^Author: (.+?) <(.+)>$" [ name; email ] ->
            sprintf "  parsed author %s %s" name email |> debug
            let email =
                if aliases.ContainsKey email
                then
                    sprintf "    real email: %s" aliases.[email] |> debug
                    aliases.[email]
                else email
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
        | Regex "^$" [] -> Part.Blank
        | Regex @"^:(\d+) (\d+) ([\da-f\.]+) ([\da-f\.]+) ([AMD])\s+(.+?)\s*$" [ modeB; modeA; hashB; hashA; change; path ] ->
            sprintf "  parsed file %s" path |> debug
            let ext = getExt path
            match inclExt ext with
            | false -> Part.Blank
            | _ -> File { path = path.Trim (); ext = ext; change = Change.parse change; mode = modeB, modeA; hash = hashB, hashA }
        | Regex @"^(\d+)\s+(\d+)\s+(.+?)\s*$" [ plus; minus; path ] ->
            sprintf "  parsed stat %s" path |> debug
            let parseNum = function
                | "-" -> None
                | x -> Some <| int x
                
            let ext = getExt path
            match inclExt ext with
            | false -> Part.Blank
            | _ ->
                let plus = parseNum plus
                let minus = parseNum minus
                let binary =
                    match plus, minus with
                    | None, None -> true
                    | Some _, Some _ -> false
                    | _ ->
                        sprintf "Could not parse stat %s" data |> warn
                        true
                Stat { path = path.Trim (); ext = ext; binary = binary; plus = plus; minus = minus }
        | _ ->
            sprintf "  did not parse %s" data |> debug
            Part.Blank)

let logParser =
    MailboxProcessor.Start (fun inbox ->
        let rec loop acc = async {
            let! msg, (replyChan : AsyncReplyChannel<IDictionary<_,_>> option) = inbox.Receive ()

            let reply =
                match replyChan with
                | Some ch -> (function
                    | Some x -> x :> IDictionary<_,_>
                    | _ -> failwith "Invalid response.") >> ch.Reply
                | _ -> (fun x -> ())
            
            let postToCollector =
                List.rev
                >> CollectInstruction.Data
                >> (fun x -> (x, None))
                >> commitCollector.Post
            
            match msg with
            | Data x ->
                sprintf "Log parser received %A" x |> debug
                let part = parseLog x
                let cont =
                    match part with
                    | Commit hash ->
                        match acc with
                        | [] -> ()
                        | _ -> postToCollector acc
                        
                        part :: []
                    | Part.Blank -> acc
                    | _ -> part :: acc
                reply None
                return! loop cont
            | Finished ->
                "Done parsing log." |> info
                postToCollector acc
                commitCollector.PostAndReply (fun ch -> CollectInstruction.Finished, Some ch)
                |> Some |> reply
                return ()
        }
        
        loop [])
        
let lsParser =
    MailboxProcessor.Start (fun inbox ->
        let rec loop acc = async {
            let! msg, (replyChan : AsyncReplyChannel<string list> option) = inbox.Receive ()
            let reply x =
                match replyChan with
                | Some ch -> ch.Reply x
                | _ -> ()
                
            match msg with
            | Data x ->
                sprintf "Ls parser received %A" x |> debug
                let acc =
                    match x with
                    | Regex @"^(\d+|-)\s+(?:\d+|-)\s+(.+?)\s*$" [ size; path ] ->
                        sprintf "  parsed diff-tree %s" path |> debug
                        match size with
                        | "-" -> "    is binary" |> debug; acc
                        | _ -> path :: acc
                    | _ -> failwith "Could not parse ls."
                
                reply []
                return! loop acc
            | Finished ->
                "Done parsing ls, commence blaming in parallel." |> info
                reply acc
                return ()
        }
        
        loop [])
        
let createBlameParser path =
    MailboxProcessor.Start (fun inbox ->
        let buildLine parts =
            let name = ref String.Empty
            let email = ref String.Empty
            let lines = ref 0
            let chars = ref 0
            let rec loop = function
                | [] -> ()
                | x::xs ->
                    match x with
                    | Name x -> name := x
                    | Email x -> email := x
                    | Line x ->
                        Interlocked.Increment lines |> ignore
                        Interlocked.Add (chars, x.Length) |> ignore
                    loop xs
            loop parts
            
            { author = !name, !email; path = path; ext = getExt path; lines = !lines; chars = !chars } : BlameStats
    
        let rec loop parts lines = async {
            let! msg, (replyChan : AsyncReplyChannel<BlameStats list> option) = inbox.Receive ()
            let reply x = match replyChan with Some ch -> ch.Reply x | _ -> ()
            
            match msg with
            | Data x ->
                let parts, lines =
                    match x with
                    | Regex @"^author (.+?)\s*$" [ name ] ->
                        let name = Name name
                        
                        let lines =
                            match parts with
                            | [] -> lines
                            | _ -> buildLine parts :: lines
                            
                        name :: [], lines
                        
                    | Regex @"^author-mail (.+?)\s*$" [ email ] -> Email email :: parts, lines
                    | Regex @"^\t(.+?)\s*$" [ line ] -> Line line :: parts, lines
                    | _ -> parts, lines
                
                reply []
                return! loop parts lines
            | Finished ->
                sprintf "Done parsing blame for %s" path |> debug
                
                reply <| (buildLine parts) :: lines
                return ()
        }
        
        loop [] [])

let buildOutputHandler f =
    fun (x:DataReceivedEventArgs) ->
        time "eventing" (fun () ->
            match x.Data with
            | null -> ()
            | _ -> f (Data x.Data, None))

let collectLogOutput = buildOutputHandler logParser.Post
let collectLsOutput = buildOutputHandler lsParser.Post
let createBlameOutputHandler (parser:MailboxProcessor<_ * _>) = buildOutputHandler parser.Post

let run wd cmd args (outputHandler:DataReceivedEventArgs -> unit) = async {
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
    use listener = proc.OutputDataReceived.Subscribe (outputHandler)
    proc.EnableRaisingEvents <- true
    
    time "git" (fun () ->
        proc.Start () |> ignore
        proc.BeginOutputReadLine ()

        Async.AwaitEvent proc.Exited
        |> Async.RunSynchronously
        |> ignore)
    
    match proc.ExitCode with
    | 0 -> sprintf "Done running %s %s." cmd args |> info
    | x ->
        proc.StandardError.ReadToEnd () |> error
        Environment.Exit x
}

[<EntryPoint>]
let main args =
    Args.parse args
    doTime <- Args.bool "time"
    Args.singleOrFlag
        "debug"
        (fun x -> debugLvl := DebugLevel.parse x)
        (fun () -> debugLvl := DebugLevel.Debug)
        
    sprintf "Arguments: %A" Args.parsedArgs |> debug

    let workingDir = Args.singleOrEmpty "" Environment.CurrentDirectory
    let gitCmd = Args.singleOrEmpty "git" "/usr/local/bin/git"
    let run = run workingDir gitCmd
    
    Args.pairsOrEmpty "aliases" (fun x -> aliases <- x |> Map.ofList)
    Args.listOrEmpty "skipext" (fun x -> skipExts <- x)
                
    let blameOnly = Args.bool "blameonly"

    let analyzeLog = async {
        commitCollector.Error.Add (fun e ->
            sprintf "Commit collector failed: %A" e |> error
            Environment.Exit 1)
        
        logParser.Error.Add (fun e ->
            sprintf "Parser failed: %A" e |> error
            Environment.Exit 2)
            
        let gitArgs = Args.joinedList "--" " "
        let gitArgs = "log --all --raw --no-color --no-merges --numstat --ignore-space-change " + gitArgs
    
        do! run gitArgs collectLogOutput
        
        return logParser.PostAndReply (fun ch -> Finished, Some ch)
    }
    
    let analyzeBlame path = async {
        let ext = getExt path
        match inclExt ext with
        | false -> return []
        | _ ->
            sprintf "Analyzing blame for %s" path |> debug
            use parser = createBlameParser path
            let handler = createBlameOutputHandler parser
            do! run (sprintf "blame --incremental -w -- %s" path) handler
            return! parser.PostAndAsyncReply (fun ch -> Finished, Some ch)
    }
        
    let analyzeTree = async {
        lsParser.Error.Add (fun e ->
            sprintf "Ls parser failed: %A" e |> error
            Environment.Exit 3)
    
        do! run "diff-tree --numstat 4b825dc642cb6eb9a060e54bf8d69288fbee4904 HEAD" collectLsOutput
        let! result = lsParser.PostAndAsyncReply (fun ch -> Finished, Some ch)
        
        let lines =
            result
            |> List.map analyzeBlame
            |> Async.Parallel
            |> Async.RunSynchronously
            |> List.concat
            
        return lines
    }
    
    let f () =
        let activity : IDictionary<_,_> ref = ref <| dict []
        let analyzeLog = async {
            let result = analyzeLog |> Async.RunSynchronously
            activity := result
        }
        
        let lines : BlameStats list ref = ref []
        let analyzeTree = async {
            let result = analyzeTree |> Async.RunSynchronously
            lines := result
        }
    
        if not blameOnly
        then [analyzeLog; analyzeTree]
        else [analyzeTree]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
        
        Printers.printMarkdown !activity

    match doTime with
    | false -> f ()
    | _ ->
        let sw = Diagnostics.Stopwatch.StartNew ()
        
        f ()

        timeSpent
        |> Seq.filter (fun x -> x.Value.Count > 0)
        |> Seq.map (fun x -> x.Key, x.Value.Count, (x.Value |> Seq.sum))
        |> Seq.map (fun (x, c, s) -> x, c, s, (float s / float c))
        |> Seq.iter (fun (k, c, s, a) -> printfn "%s: count %i, total %i, avg %f" k c s a)

        printfn "Total time: %i" sw.ElapsedMilliseconds

    0

