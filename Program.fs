
// NOTE: If warnings appear, you may need to retarget this project to .NET 4.0. Show the Solution
// Pad, right-click on the project node, choose 'Options --> Build --> General' and change the target
// framework to .NET 4.0 or .NET 4.5.

module GitStats.Main

open System
open System.Diagnostics

let runGit args =
    let psi = ProcessStartInfo ()
    psi.FileName <- "git"
    psi.Arguments <- "log"
    psi.WorkingDirectory <- ""
    let proc = Process ()
    

[<EntryPoint>]
let main args = 
    Console.WriteLine("Hello world!")
    0

