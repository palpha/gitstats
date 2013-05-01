module Args

let mutable parsedArgs = Map.empty

let parse args =
    parsedArgs <-
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

let bool x = parsedArgs.ContainsKey x
let singleOrBool x =
    if 