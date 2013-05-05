module Args

open System

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
let singleOrFlag x ifs iff =
    if parsedArgs.ContainsKey x then
        match parsedArgs.[x] with
        | [] -> iff ()
        | x::_ -> ifs x

let singleOrEmpty x defaultValue =
    if parsedArgs.ContainsKey x then
        if parsedArgs.[x] <> List.empty
        then parsedArgs.[x] |> List.head
        else defaultValue
    else defaultValue

let pairsOrEmpty x ifp =
    if parsedArgs.ContainsKey x then
        match parsedArgs.[x] with
        | [] | [_] -> failwithf "%s must be a list of pairs." x
        | pairs ->
            let rec loop acc = function
                | [] | [_] -> List.rev acc
                | x :: y :: tl ->
                    loop ((x, y) :: acc) tl
            loop [] pairs |> ifp

let listOrEmpty x ifl =
    if parsedArgs.ContainsKey x
    then ifl parsedArgs.[x]

let joinedList x s =
    if parsedArgs.ContainsKey x
    then String.Join (s, parsedArgs.[x])
    else String.Empty