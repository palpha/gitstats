module Main

open System
open System.Diagnostics
open System.Collections.Generic
open System.Collections.Concurrent
open Microsoft.FSharp
open System.Threading
open System.Linq
open System.Text.RegularExpressions

// some options:
let mutable doTime = false
let mutable aliases = Map.empty
let mutable skipExts = List.empty
let mutable inclExts = List.empty
let mutable ignored = List.empty

let inclPath =
    let regexes = lazy (
        ignored
        |> List.map (fun x ->
            Regex (x, RegexOptions.Compiled ||| RegexOptions.ECMAScript ||| RegexOptions.IgnoreCase))
    )

    fun path ->
        if ignored.Length > 0 then
            regexes.Force ()
            |> List.forall (fun re -> re.IsMatch (path) = false)
        else true

let inclExt =
    let inclSet = lazy (inclExts |> set)
    let skipSet = lazy (skipExts |> set)
    let cache = ConcurrentDictionary<_,_> ()
    fun ext ->
        match cache.TryGetValue ext with
        | true, x -> x
        | _ ->
            let result =
                match skipSet.Force().Contains ext, inclSet.Force().Count with
                | true, _ -> false
                | false, 0 -> true
                | _, x when x > 0 && inclSet.Force().Contains ext -> true
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
                        |> Seq.where (fun x -> not x.binary)
                        |> Seq.groupBy (fun x -> getExt x.path)
                        |> Seq.map (fun (k, g) ->
                            k,
                            { plus = g |> Seq.choose (fun x -> x.plus) |> Seq.sum
                              minus = g |> Seq.choose (fun x -> x.minus) |> Seq.sum
                              current = 0 })
                        |> Seq.where (fun (x, d) -> d.plus > 0 || d.minus > 0)
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
                        Func<_,_,_>(fun _ -> (fun x ->
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
                    info <| lazy (sprintf "Collected %s" commit.hash)
                    reply None)
                return! loop () // wait for next
            | CollectInstruction.Finished ->
                info "Done collecting."
                reply <| Some activityStats
                return () // break loop
        }
        loop ())

let parseLog data =
    time "parse" (fun () ->
        match data with
        | Regex "^commit (.+)$" [ hash ] ->
            debug <| lazy (sprintf "  parsed hash %s" hash)
            Commit hash
        | Regex "^Author: (.+?) <(.+)>$" [ name; email ] ->
            debug <| lazy (sprintf "  parsed author %s %s" name email)
            let email =
                if aliases.ContainsKey email
                then
                    debug <| lazy (sprintf "    real email: %s" aliases.[email])
                    aliases.[email]
                else email
            Author { name = name; email = email }
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
                debug <| lazy (sprintf "  parsed timestamp %s" <| date.ToString ())
                Timestamp date
            else failwith "Could not parse date."
        | Regex "^ {4}(.+)$" [ msg ] ->
            debug <| lazy (sprintf "  parsed message %s" msg)
            Message msg
        | Regex "^$" [] -> Part.Blank
        | Regex @"^:(\d+) (\d+) ([\da-f\.]+) ([\da-f\.]+) ([AMD])\s+(.+?)\s*$" [ modeB; modeA; hashB; hashA; change; path ] ->
            debug <| lazy (sprintf "  parsed file %s" path)
            let ext = getExt path
            match inclExt ext && inclPath path with
            | false -> Part.Blank
            | _ -> File { path = path.Trim (); ext = ext; change = Change.parse change; mode = modeB, modeA; hash = hashB, hashA }
        | Regex @"^(\d+|-)\s+(\d+|-)\s+(.+?)\s*$" [ plus; minus; path ] ->
            debug <| lazy (sprintf "  parsed stat %s" path)
            let parseNum = function
                | "-" -> None
                | x -> Some <| int x

            let ext = getExt path
            match inclExt ext && inclPath path with
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
            debug <| lazy (sprintf "  did not parse %s" data)
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
                debug <| lazy (sprintf "Log parser received %A" x)
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
                info "Done parsing log."
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
                debug <| lazy (sprintf "Ls parser received %A" x)
                let acc =
                    match x with
                    | Regex @"^(\d+|-)\s+(?:\d+|-)\s+(.+?)\s*$" [ size; path ] ->
                        debug <| lazy (sprintf "  parsed diff-tree %s" path)
                        match size with
                        | "-" -> debug "    is binary"; acc
                        | _ -> path :: acc
                    | "" -> acc
                    | _ -> failwith "Could not parse ls."

                reply []
                return! loop acc
            | Finished ->
                info "Done parsing ls, commence blaming in parallel."
                reply acc
                return ()
        }

        loop [])

let createBlameParser path =
    MailboxProcessor.Start (fun inbox ->
        let buildLine parts =
            let name = ref String.Empty
            let email = ref String.Empty
            let chars = ref 0
            let lines = ref 0
            let rec loop = function
                | [] -> ()
                | x::xs ->
                    match x with
                    | Name x -> name := x
                    | Email x -> email := x
                    | Line x ->
                        let len = String.length <| x.Trim ()
                        if len > 0 then
                            chars := !chars + len
                            lines := !lines + 1
                    loop xs
            loop parts

            match !chars with
            | 0 -> None
            | _ ->
                ({ author = { name = !name; email = !email }
                   lines = !lines
                   chars = !chars } : BlameLines)
                |> Some

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

                    | Regex @"^author-mail <(.+?)>\s*$" [ email ] ->
                        let email =
                            if aliases.ContainsKey email
                            then
                                debug <| lazy (sprintf "  real email: %s" aliases.[email])
                                aliases.[email]
                            else email
                    
                        Email email :: parts, lines
                    | Regex @"^\t(.+?)\s*$" [ line ] -> Line line :: parts, lines
                    | _ -> parts, lines

                reply []
                return! loop parts lines
            | Finished ->
                debug <| lazy (sprintf "Done parsing blame for %s" path)
                let result =
                    (buildLine parts) :: lines
                    |> Seq.choose id
                    |> Seq.groupBy (fun x -> x.author)
                    |> Seq.map (fun (author, xs) ->
                        { author = author
                          path = path
                          ext = getExt path
                          lines = xs |> Seq.sumBy (fun x -> x.lines)
                          chars = xs |> Seq.sumBy (fun x -> x.chars) })
                    |> Seq.toList

                reply result
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

let run cmd args (outputHandler:DataReceivedEventArgs -> unit) wd =
    debug <| lazy (sprintf "%s %s" cmd args)

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
    | 0 -> info <| lazy (sprintf "Done running %s %s." cmd args)
    | x ->
        proc.StandardError.ReadToEnd () |> error
        Environment.Exit x

let runBuf cmd args wd =
    debug <| lazy (sprintf "%s %s" cmd args)

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

    time "git buf" (fun () ->
        proc.Start () |> ignore
        let output = proc.StandardOutput.ReadToEnd ()
        proc.WaitForExit ()

        match proc.ExitCode with
        | 0 ->
            info <| lazy (sprintf "Done running %s %s." cmd args)
            output
        | x ->
            proc.StandardError.ReadToEnd () |> error
            Environment.Exit x; String.Empty)

[<EntryPoint>]
let main args =
    let spec =
        [ListOrDefault ("", None, [ Environment.CurrentDirectory ]), "ArgPaths"
         SingleOrDefault ("git", Some "g", "/usr/local/bin/git"), "ArgGit"
         ListOrEmpty ("include-ext", Some "i"), "ArgInclude"
         ListOrEmpty ("exclude-ext", Some "x"), "ArgExclude"
         PairsOrEmpty ("aliases", None), "ArgAliases"
         ListOrEmpty ("ignore", None), "ArgIgnore"
         SingleOrDefault ("parallel", None, "-1"), "ArgParallel"
         SingleOrDefault ("loglevel", Some "ll", "warn"), "ArgLogLevel"
         Flag ("log-only", None), "ArgLogOnly"
         Flag ("blame-only", None), "ArgBlameOnly"
         Flag ("time", None), "ArgTime"
         ListOrEmpty ("--", None), "ArgGitArgs"]

    let args = Args.parse spec args

    debugLvl := DebugLevel.parse <| unbox args.["loglevel"]
    warn <| lazy (sprintf "Arguments: %A" args)

    doTime <- unbox args.["time"]
    aliases <-
        match unbox args.["aliases"] with
        | [] -> Map.empty
        | x -> Map.ofList x
    skipExts <- unbox args.["exclude-ext"]
    inclExts <- unbox args.["include-ext"]
    ignored <- unbox args.["ignore"]

    let workingDirs : string list = unbox args.[""] |> Set.ofList |> Seq.toList
    let gitCmd : string = unbox args.["git"]
    let parallelism = int <| unbox<string> args.["parallel"]
    let logOnly : bool = unbox args.["log-only"]
    let blameOnly : bool = unbox args.["blame-only"]
    let gitArgs = String.Join<string> (" ", unbox args.["--"])

    let analyzeLog () =
        commitCollector.Error.Add (fun e ->
            sprintf "Commit collector failed: %A" e |> error
            Environment.Exit 1)

        logParser.Error.Add (fun e ->
            sprintf "Parser failed: %A" e |> error
            Environment.Exit 2)

        let gitArgs = "log --raw --no-color --no-merges --numstat --ignore-space-change " + gitArgs

        workingDirs |> List.iter (run gitCmd gitArgs collectLogOutput)

        logParser.PostAndReply (fun ch -> Finished, Some ch)

    let analyzeBlame wd path =
        let toParser (p:MailboxProcessor<_ * _>) (x:string) =
            x.Split ([|"\n".[0]|]) |> Array.iter (fun x -> p.Post (Data x, None))
        let ext = getExt path
        match inclExt ext && inclPath path with
        | false -> []
        | _ ->
            debug <| lazy (sprintf "Analyzing blame for %s" path)
            let output = runBuf gitCmd (sprintf "blame --porcelain -w -- %s" ("\"" + path + "\"")) wd
            let parser = createBlameParser path
            output |> toParser parser
            parser.PostAndReply (fun ch -> Finished, Some ch)

    let analyzeTree wd =
        let parse x =
            match x with
            | Regex @"^(\d+|-)\s+(?:\d+|-)\s+(.+?)\s*$" [ size; path ] ->
                debug <| lazy (sprintf "  parsed diff-tree %s" path)
                match size with
                | "-" -> debug "    is binary"; None
                | _ ->
                    match IO.Directory.Exists <| IO.Path.Combine (wd, path) with
                    | false -> Some path
                    | _ -> debug "    is directory"; None
            | "" -> None
            | _ -> failwith "Could not parse ls."


        let output = runBuf gitCmd "diff-tree --numstat 4b825dc642cb6eb9a060e54bf8d69288fbee4904 HEAD" wd

        let lines =
            let i = ref 0
            let parallelize =
                match parallelism with
                | -1 -> Environment.ProcessorCount / 2
                | x -> x

            time "blame" (fun () ->
                let paths =
                    output.Split ([|"\n".[0]|])
                    |> Seq.choose parse

                paths
                    .AsParallel()
                    .WithDegreeOfParallelism(parallelize)
                    .WithExecutionMode(ParallelExecutionMode.ForceParallelism)
                    .Select(analyzeBlame wd)
                |> Seq.concat
                |> Seq.toList)

        lines

    let f () =
        let activity : IDictionary<_,_> ref = ref <| dict []
        let analyzeLog () = activity := analyzeLog ()

        let lines : BlameStats list ref = ref []
        let analyzeTree () =
            let rec loop lst acc =
                match lst with
                | [] -> acc
                | x::xs ->
                    loop xs <| analyzeTree x @ acc

            lines := loop workingDirs []

        match logOnly, blameOnly with
        | true, false -> [ analyzeLog ]
        | false, true -> [ analyzeTree ]
        | _ -> [ analyzeLog; analyzeTree ]
        |> Seq.iter (fun x -> x ())
        |> ignore

        time "print" (fun () -> Printers.printMarkdown !activity !lines)

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

