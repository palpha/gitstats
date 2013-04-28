[<AutoOpen>]
module GitStats.Structures

open System
open System.Diagnostics
open System.Collections.Generic
open Microsoft.FSharp

type DebugLevel =
    | Debug
    | Info
    | Warn
    static member parse x =
        let cmp x y = System.StringComparer.OrdinalIgnoreCase.Equals (x, y)
        if cmp x "info"
        then Info
        else if cmp x "warn"
        then Warn
        else Debug

let debugLvl = ref Warn
let debug, info, warn, error =
    let synced = System.Console.Out |> System.IO.TextWriter.Synchronized
        
    let debug l x =
        match l, !debugLvl with
        | Warn, _
        | Info, Info
        | Info, Debug
        | Debug, Debug ->
            let time, ms =
                let now = DateTime.Now
                now.ToString ("yyyy-MM-dd HH:mm:ss"),
                now.Millisecond.ToString ("000")
            sprintf "[%A] %s.%s - %s" l time ms x
            |> synced.WriteLine
            
        | _ -> ()
    debug Debug,
    debug Info,
    debug Warn,
    debug Warn

type Change =
    | Added
    | Modified
    | Deleted
    static member parse = function
        | "A" -> Added
        | "M" -> Modified
        | "D" -> Deleted
        | x -> failwithf "Unknown change %s" x

type Hash = string
type Author = string * string

type FileChange =
    { path : string
      change : Change
      mode : string * string
      hash : Hash * Hash }
      
type FileStat =
    { path : string
      plus : int option
      minus : int option
      binary : bool }
      
type FileData =
    { path : string
      change : Change
      mode : string * string
      hash : Hash * Hash
      binary : bool
      plus : int option
      minus : int option }
    
type Part =
    | Commit of Hash
    | Author of Author
    | Timestamp of DateTime
    | Message of string
    | File of FileChange
    | Stat of FileStat
    | Blank
    
type CollectInstruction =
    | Finished
    | Data of Part list

type ParseInstruction =
    | Finished
    | Data of string

let (|Regex|_|) regex str =
   let m = (System.Text.RegularExpressions.Regex (regex)).Match (str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

type Commit =
    { hash : Hash
      author : string * string
      timestamp : DateTime
      message : string
      files : FileData list }
    static member create data =
        let msgBuilder = Text.StringBuilder ()
        let files = Dictionary<string, FileData> ()
        let hash = ref ""
        let author = ref ("", "")
        let timestamp = ref DateTime.MinValue
        
        let rec loop lst =
            match lst with
            | [] -> ()
            | x::xs ->
                match x with
                | Commit x -> hash := x
                | Author (name, email) -> author := (name, email)
                | Timestamp x -> timestamp := x
                | Message x -> msgBuilder.AppendLine x |> ignore
                | File x ->
                    files.[x.path] <-
                        { path = x.path
                          change = x.change
                          mode = x.mode
                          hash = x.hash
                          binary = false
                          plus = None
                          minus = None }
                | Stat x ->
                    if files.ContainsKey x.path
                    then
                        files.[x.path] <-
                            { files.[x.path] with
                                binary = x.binary
                                plus = x.plus
                                minus = x.minus }
                | _ -> ()
                loop xs
        try
            loop data
            { hash = !hash
              author = !author
              timestamp = !timestamp
              message = msgBuilder.ToString ()
              files = files |> Seq.map (fun x -> x.Value) |> List.ofSeq }
        with ex ->
            sprintf "Unable to build commit %s" !hash |> error
            reraise ()

type CommitStats =
    { commits : int
      plus : int
      minus : int }

type ActivityStats =
    { first : DateTime
      last : DateTime
      activeDays : int }