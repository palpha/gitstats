
// NOTE: If warnings appear, you may need to retarget this project to .NET 4.0. Show the Solution
// Pad, right-click on the project node, choose 'Options --> Build --> General' and change the target
// framework to .NET 4.0 or .NET 4.5.

module GitStats.Main

open System
open System.Diagnostics

let collectOutput =
    (fun (x:DataReceivedEventArgs) ->
        printfn "%A" x.Data)

let run cmd args wd =
    let psi = ProcessStartInfo ()
    psi.FileName <- cmd
    psi.Arguments <- "log --all --raw"
    psi.WorkingDirectory <- wd
    psi.CreateNoWindow <- true
    psi.RedirectStandardOutput <- true
    psi.UseShellExecute <- false
    
    use proc = new Process ()
    proc.StartInfo <- psi
    use listener = proc.OutputDataReceived.Subscribe (collectOutput)
    proc.EnableRaisingEvents <- true
    proc.Start () |> ignore
    proc.BeginOutputReadLine ()
    
    Async.AwaitEvent proc.Exited
    |> Async.RunSynchronously
    |> ignore
    
    Console.ReadLine () |> ignore

[<EntryPoint>]
let main args = 
    run "/usr/local/bin/git" "" Environment.CurrentDirectory
    0

