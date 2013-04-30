
// NOTE: If warnings appear, you may need to retarget this project to .NET 4.0. Show the Solution
// Pad, right-click on the project node, choose 'Options --> Build --> General' and change the target
// framework to .NET 4.0 or .NET 4.5.

module GitStats.Main

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
        let isoStr (x:DateTime) = x.ToString "yyyy-MM-dd"
        let rec loop () = async {
            let! msg, (replyChan : AsyncReplyChannel<unit> option) = inbox.Receive ()
            let reply () =
                match replyChan with
                | Some ch -> ch.Reply ()
                | _ -> ()

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
                              minus = g |> Seq.choose (fun x -> x.minus) |> Seq.sum })
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
                                        match x.TryGetValue k with true, x -> x | _ -> { plus = 0; minus = 0 }
                                    let xf = get x.files
                                    let nf = get files
                                    k, { plus = xf.plus + nf.plus; minus = xf.minus + nf.minus })
                                |> dict })))
                    |> ignore
                    sprintf "Collected %s" commit.hash |> info
                    reply ())
                return! loop () // wait for next
            | CollectInstruction.Finished ->
                "Done collecting." |> info
                let topContribs = Dictionary<string, int * string * string> ()
                
                activityStats
                |> Seq.iter (fun x ->
                    let (name, email), stats = x.Key, x.Value
                    let heading = sprintf "%s <%s>" name email
                    printfn "%s\n%s" heading <| String.replicate heading.Length "="
                    
                    printfn "Commits: %i" stats.commits
                    printfn "Active days: %i" stats.dates.Count
                    printfn "Commits/active day: %f" (float stats.commits / float stats.dates.Count)
                    printfn "First commit: %s" <| isoStr stats.first
                    printfn "Last commit: %s" <| isoStr stats.last
                    
                    let daysInvolved = (stats.last - stats.first).TotalDays
                    printfn "Commits/day: %f" (float stats.commits / daysInvolved)
                    printfn ""
                    
                    stats.files
                    |> Seq.iter (fun x ->
                        let ext, stats = x.Key, x.Value

                        printfn ".%s" ext
                        printfn "%s" <| String.replicate (ext.Length + 1) "-"
                        printfn "Additions: %i" stats.plus
                        printfn "Removals: %i" stats.minus
                        printfn ""
                        
//                        match contrib with
//                        | 0 -> ()
//                        | _ ->
//                            let f () = topContribs.[ext] <- (contrib, name, email)
//                            match topContribs.TryGetValue ext with
//                            | false, _ -> f ()
//                            | _, (x, _, _) when x < contrib -> f ()
//                            | _ -> ())
                        ))
                
//                let heading = "File types, current contribution"
//                printfn "%s\n%s" heading <| String.replicate heading.Length "="
//                topContribs
//                |> Seq.iter (fun x ->
//                    let ext, (contrib, name, email) = x.Key, x.Value
//                    printfn ".%s: %s <%s>, %i lines" ext name email contrib)
                
                reply ()
                return () // break loop
        }
        loop ())
    
let parse data =
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
        | Regex "^$" [] -> Blank
        | Regex @"^:(\d+) (\d+) ([\da-f\.]+) ([\da-f\.]+) ([AMD])\s+(.+?)\s*$" [ modeB; modeA; hashB; hashA; change; path ] ->
            sprintf "  parsed file %s" path |> debug
            let ext = getExt path
            match inclExt ext with
            | false -> Blank
            | _ -> File { path = path.Trim (); ext = ext; change = Change.parse change; mode = modeB, modeA; hash = hashB, hashA }
        | Regex @"^(\d+)\s+(\d+)\s+(.+?)\s*$" [ plus; minus; path ] ->
            sprintf "  parsed stat %s" path |> debug
            let parseNum = function
                | "-" -> None
                | x -> Some <| int x
                
            let ext = getExt path
            match inclExt ext with
            | false -> Blank
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
            Blank)

let logParser =
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
                sprintf "Log parser received %A" x |> debug
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
                "Done parsing log." |> info
                postToCollector acc
                commitCollector.PostAndReply (fun ch -> CollectInstruction.Finished, Some ch)
                reply ()
                return ()
        }
        
        loop [])
        
let lsParser =
    MailboxProcessor.Start (fun inbox ->
        let rec loop acc = async {
            let! msg, (replyChan : AsyncReplyChannel<unit> option) = inbox.Receive ()
            let reply () =
                match replyChan with
                | Some ch -> ch.Reply ()
                | _ -> ()
                
            match msg with
            | Data x ->
                let cont =
                    match x with
                    | Regex @"\d blob [a-f\d]+\s+(.+?)\s*$" [ path ] ->
                        sprintf "  parsed ls %s" path |> debug
                        path :: acc
                    | _ -> failwith "Could not parse ls."
                
                reply ()
                return! loop acc
            | Finished ->
                "Done parsing ls, commence blaming in parallel." |> info
                reply ()
                return ()
        }
        
        loop [])

let buildOutputHandler f =
    fun (x:DataReceivedEventArgs) ->
        time "eventing" (fun () ->
            match x.Data with
            | null -> ()
            | _ -> f (x.Data))

let collectLogOutput = buildOutputHandler (fun x -> logParser.Post (Data x, None))
let collectLsOutput = buildOutputHandler (fun x -> lsParser.Post (Data x, None))

let run cmd args wd (outputHandler:DataReceivedEventArgs -> unit) = async {
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

    doTime <- parsedArgs.ContainsKey "time"

    if parsedArgs.ContainsKey "debug" then
        match parsedArgs.["debug"] with
        | [] -> debugLvl := DebugLevel.Debug
        | x::xs -> debugLvl := DebugLevel.parse x
        
    sprintf "Arguments: %A" parsedArgs |> debug

    let workingDir =
        if parsedArgs.[""] <> List.empty
        then parsedArgs.[""] |> List.head
        else Environment.CurrentDirectory
        
    if parsedArgs.ContainsKey "aliases" then
        aliases <-
            match parsedArgs.["aliases"] with
            | [] | [_] -> failwith "Aliases must be pairs of e-mail addresses"
            | pairs ->
                let rec loop acc = function
                    | [] | [_] -> List.rev acc
                    | x :: y :: tl ->
                        loop ((x, y) :: acc) tl
                loop [] pairs |> Map.ofList
                
    if parsedArgs.ContainsKey "skipext" then
        skipExts <- parsedArgs.["skipext"]
        
    let blameOnly = parsedArgs.ContainsKey "blame"

    let analyzeLog = async {
        commitCollector.Error.Add (fun e ->
            sprintf "Commit collector failed: %A" e |> error
            Environment.Exit 1)
        
        logParser.Error.Add (fun e ->
            sprintf "Parser failed: %A" e |> error
            Environment.Exit 2)
            
        let gitArgs =   
            if parsedArgs.ContainsKey "--"
            then String.Join (" ", parsedArgs.["--"])
            else ""
        
        let gitArgs = "log --all --raw --no-color --no-merges --numstat " + gitArgs
    
        do! run "/usr/local/bin/git" gitArgs workingDir collectLogOutput
        logParser.PostAndReply (fun ch -> Finished, Some ch)
    }
        
    let analyzeBlame = async {
        do! run "/usr/local/bin/git" "ls-tree -r HEAD" workingDir collectLsOutput
        lsParser.PostAndReply (fun ch -> Finished, Some ch)
    }
    
    let f () =
        if not blameOnly
        then [analyzeLog; analyzeBlame]
        else [analyzeBlame]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore

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

