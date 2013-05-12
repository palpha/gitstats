module Resources

// homegrown resource thing, because I couldn't be bothered
// to figure out how to work with .resx files in Xamarin Studio.

open System

let get =
    let strings =
        [ "ArgPaths", "Zero or more paths to include in statistics aggregation. Default: current directory"
          "ArgGit", "Path to git. Default: {0}"
          "ArgGitArgs", "Arguments that will be passed to git."
          "ArgInclude", "File extensions to include."
          "ArgExclude", "File extensions to exclude."
          "ArgAliases", "Space-separated e-mail address pairs: <alias> <real>."
          "ArgIgnore", "Space-separated RegExp patterns describing paths to ignore."
          "ArgParallel", "Degree of parallelization. Default: -1 (base on number of CPUs)"
          "ArgLogLevel", "Log level (debug, info, warn, error). Default: warn"
          "ArgLogOnly", "Only run log analysis, not blame."
          "ArgBlameOnly", "Only run blame analysis, not log."
          "ArgTime", "Time operations." ]
        |> Map.ofList
    (fun x ->
        match Map.containsKey x strings with
        | true -> strings.[x]
        | _ -> String.Empty)