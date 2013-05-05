module Printers

open System
open System.Collections.Generic

let isoStr (x:DateTime) = x.ToString "yyyy-MM-dd"

let printMarkdown (logStats:IDictionary<_,_>) (blameStats:BlameStats list) =
    let h1, h2 =
        let h u x = printfn "%s\n%s" x <| String.replicate x.Length u
        h "=", h "-"
    
    let contribByExt, topContribByExt, topContribByAuthor =
        let byExt =
            blameStats
            |> Seq.groupBy (fun x -> x.ext)
            |> Seq.map (fun (ext, xs) ->
                ext,
                xs
                |> Seq.groupBy (fun x -> x.author)
                |> Seq.map (fun (author, xs) ->
                    author,
                    { author = author
                      path = null
                      ext = ext
                      lines = xs |> Seq.sumBy (fun x -> x.lines)
                      chars = xs |> Seq.sumBy (fun x -> x.chars) })
                |> dict)
            |> dict

        let top =
            byExt
            |> Seq.map (fun x -> x.Key, x.Value |> Seq.maxBy (fun x -> x.Value.chars))
            |> dict
                        
        let byAuthor =
            blameStats
            |> Seq.groupBy (fun x -> x.author)
            |> Seq.map (fun (author, xs) ->
                { author = author
                  path = null
                  ext = null
                  lines = xs |> Seq.sumBy (fun x -> x.lines)
                  chars = xs |> Seq.sumBy (fun x -> x.chars) })
            |> Seq.sortBy (fun x -> x.chars)
            |> Seq.toList
            |> List.rev
            
        byExt, top, byAuthor

    logStats
    |> Seq.iter (fun x ->
        let author, stats = x.Key, x.Value
        let name, email = author
        sprintf "%s <%s>" name email |> h1
        
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

            sprintf ".%s" ext |> h2
            printfn "Additions: %i" stats.plus
            printfn "Removals: %i" stats.minus
            if contribByExt.ContainsKey ext && contribByExt.[ext].ContainsKey author then
                printfn "Current lines: %i" contribByExt.[ext].[author].lines
                printfn "Current chars: %i"  contribByExt.[ext].[author].chars
                
            printfn ""))

    if topContribByExt.Count > 0 then
        "Current top contributors" |> h1
        printfn ""
        topContribByExt
        |> Seq.iter (fun x ->
            sprintf ".%s" x.Key |> h2
            let name, email = x.Value.Key
            printfn "%s <%s>: %i lines, %i chars" name email x.Value.Value.lines x.Value.Value.chars
            printfn "")
            
        h2 "Overall"
        topContribByAuthor
        |> Seq.iter (fun x ->
            let name, email = x.author
            printfn "%s <%s>: %i lines, %i chars" name email x.lines x.chars)