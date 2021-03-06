[<AutoOpen>]
module Structures

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
let (debug : obj -> unit),
    (info : obj -> unit),
    (warn : obj -> unit),
    (error : obj -> unit) =
        let synced = System.Console.Out |> System.IO.TextWriter.Synchronized
        let debug l (f:obj) =
            match l, !debugLvl with
            | Warn, _
            | Info, Info
            | Info, Debug
            | Debug, Debug ->
                let time, ms =
                    let now = DateTime.Now
                    now.ToString ("yyyy-MM-dd HH:mm:ss"),
                    now.Millisecond.ToString ("000")
                let str =
                    match f with
                    | :? string as x -> x
                    | :? (unit -> string) as f -> f ()
                    | :? Lazy<string> as x -> x.Force ()
                    | _ -> failwith "Invalid logger argument."

                sprintf "[%A] [%i] - %s.%s - %s" l Threading.Thread.CurrentThread.ManagedThreadId time ms str
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

[<CustomEquality; CustomComparison>]
type Author =
    { name : string; email : string }

    override x.Equals(y) =
        match y with
        | :? Author as y -> (x.email = y.email)
        | _ -> false

    override x.GetHashCode() = hash x.email
    interface System.IComparable with
      member x.CompareTo y =
          match y with
          | :? Author as y -> compare x.email y.email
          | _ -> invalidArg "y" "cannot compare values of different types"

type FileChange =
    { path : string
      ext : string
      change : Change
      mode : string * string
      hash : Hash * Hash }

type FileStat =
    { path : string
      ext : string
      plus : int option
      minus : int option
      binary : bool }

type FileData =
    { path : string
      ext : string
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

type BlamePart =
    | Name of string
    | Email of string
    | Line of string

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
      author : Author
      timestamp : DateTime
      message : string
      files : FileData list }
    static member create data =
        let msgBuilder = Text.StringBuilder ()
        let files = Dictionary<string, FileData> ()
        let hash = ref ""
        let author = ref { name = ""; email = "" }
        let timestamp = ref DateTime.MinValue

        let rec loop lst =
            match lst with
            | [] -> ()
            | x::xs ->
                match x with
                | Commit x -> hash := x
                | Author x -> author := x
                | Timestamp x -> timestamp := x
                | Message x -> msgBuilder.AppendLine x |> ignore
                | File x ->
                    files.[x.path] <-
                        { path = x.path
                          ext = x.ext
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

type FileStats =
    { plus : int
      minus : int
      current : int }

type ActivityStats =
    { commits : int
      first : DateTime
      last : DateTime
      dates : Set<DateTime>
      files : IDictionary<string, FileStats> }

type BlameLines =
    { author : Author
      lines : int
      chars : int }

type BlameStats =
    { author : Author
      path : string
      ext : string
      lines : int
      chars : int }

type Long = string
type Short = string option
type Arg =
    | Flag of Long * Short
    | SingleOrDefault of Long * Short * string
    | ListOrEmpty of Long * Short
    | ListOrDefault of Long * Short * string list
    | PairsOrEmpty of Long * Short