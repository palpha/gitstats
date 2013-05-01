module Printers

open System
open System.Collections.Generic

let isoStr (x:DateTime) = x.ToString "yyyy-MM-dd"

let printMarkdown (stats:IDictionary<_,_>) =
    stats
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
            printfn ""))