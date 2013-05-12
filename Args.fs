module Args

// sort of hacky argument parser, but it works for me :)

open System

let parse spec args =
    let parsed =
        let rec loop (lst : string list) curr acc rest =
            let pushCurr () =
                let t, n, l = curr
                (t + "-" + n, l |> List.rev) :: acc
            match lst with
            | [] -> pushCurr () |> Map.ofList
            | x::xs ->
                match rest with
                | true ->
                    let t, n, l = curr
                    loop xs (t, n, x :: l) acc true
                | _ ->
                    match x with
                    | Regex "^--(.+)" [ arg ] -> loop xs ("l", arg, []) (pushCurr ()) false
                    | Regex "^--$" [] -> loop xs ("l", "--", []) (pushCurr ()) true
                    | Regex "^-([^-]+)" [ arg ] -> loop xs ("s", arg, []) (pushCurr ()) false
                    | _ ->
                        let t, n, l = curr
                        loop xs (t, n, x :: l) acc false
        loop (args |> List.ofArray) ("l", "", []) [] false
        
    let pop l (s : string option) =
        match parsed.ContainsKey ("l-" + l), s.IsSome && parsed.ContainsKey ("s-" + s.Value) with
        | true, _ -> Some parsed.["l-" + l]
        | _, true -> Some parsed.["s-" + s.Value]
        | _ -> None

    match pop "help" (Some "h") with
    | Some _ ->
        let optionize l s =
            match l, s with
            | "--", _ -> "-- "
            | "", _ -> ""
            | l, Some s -> sprintf "--%s|-%s " l s
            | l, _ -> sprintf "--%s " l
            
        let usage = "usage: GitStats"
        let args =
            spec
            |> Seq.map (fun (arg, _) ->
                match arg with
                | Flag (l, s) -> sprintf "[%s]" <| (optionize l s).Trim()
                | SingleOrDefault (l, s, _) -> sprintf "[%s<val>]" <| optionize l s
                | ListOrEmpty (l, s)
                | ListOrDefault (l, s, _) -> sprintf "[%s<val> ...]" <| optionize l s
                | PairsOrEmpty (l, s) -> sprintf "[%s<item> <item> ...]" <| optionize l s)
            |> Seq.mapi (fun i x ->
                if (i + 1) % 4 = 0
                then [x; "\n" + (String.replicate usage.Length " ")]
                else [x])
            |> Seq.concat
            |> (fun x -> String.Join (" ", x))
        
        printfn "%s %s" usage args
        printfn "Options:"
        
        let options =
            spec
            |> List.map (fun (arg, key) ->
                match arg with
                | Flag (l, s)
                | ListOrEmpty (l, s)
                | PairsOrEmpty (l, s) -> l, s, key, None
                | SingleOrDefault (l, s, d) -> l, s, key, Some d
                | ListOrDefault (l, s, d) -> l, s, key, Some <| String.Join (" ", d))
        
        let longestS = (options |> List.map (fun (_, s, _, _) -> match s with Some s -> s.Length | _ -> 0) |> List.max) + 1
        let longestL = (options |> List.map (fun (l, _, _, _) -> l.Length) |> List.max) + 2

        options
        |> List.iter (fun (l, s, key, d) ->
            let helpText =
                let r = Resources.get key
                if r.Contains ("{0}")
                then match d with | Some x -> String.Format (r, x) | _ -> r
                else r
            match s with
            | Some s ->
                printfn
                    "  -%s,%s--%s%s%s"
                    s
                    (String.replicate (longestS - s.Length) " ")
                    l
                    (String.replicate (longestL - l.Length) " ")
                    helpText
            | _ when l <> "" && l <> "--" ->
                printfn
                    "  %s--%s%s%s"
                    (String.replicate (longestS + 2) " ")
                    l
                    (String.replicate (longestL - l.Length) " ")
                    helpText
            | _ when l = "" -> printfn "  Unqualified first option: %s" helpText
            | _ ->
                printfn
                    "  %s--%s%s"
                    (String.replicate (longestS + 2) " ")
                    (String.replicate (longestL) " ")
                    helpText)
        System.Environment.Exit 0
    | _ -> ()

    let rec loop lst acc =
        match lst with
        | [] -> acc |> Map.ofList
        | x::xs ->
            let value =
                match x with
                | Flag (l, s) ->
                    match pop l s with
                    | Some x -> (l, box true)
                    | _ -> (l, box false)
                | SingleOrDefault (l, s, d) ->
                    match pop l s with
                    | Some x when x.Length > 0 -> (l, box (x |> List.head))
                    | _ -> (l, box d)
                | ListOrEmpty (l, s) ->
                    match pop l s with
                    | Some x -> (l, box x)
                    | _ -> (l, box ([] : string list))
                | ListOrDefault (l, s, d) ->
                    match pop l s with
                    | Some x when x.Length > 0 -> (l, box x)
                    | _ -> (l, box d)
                | PairsOrEmpty (l, s) ->
                    match pop l s with
                    | Some x ->
                        let pairs = x |> Seq.pairwise |> Seq.toList
                        (l, box pairs)
                    | _ -> (l, box ([] : (string * string) list))
            loop xs <| value :: acc
    loop (spec |> Seq.map fst |> Seq.toList) []
