module IO

open System
open System.Threading.Tasks
open System.Collections.Concurrent
open System.Collections.Generic

type InputFunc = unit -> int
type OutputFunc = int -> unit

let rec readFromConsole () =
    Console.Write("Input: ")
    let input = Console.ReadLine() |> Int32.TryParse
    match input with
    | true, num -> num
    | _ ->
        Console.WriteLine "Bad input."
        readFromConsole ()


let printToConsole =
    printfn "Output: %d"


let createIoPipe () =
    let queue = ConcurrentQueue<int>()
    let rec input () =
        match queue.TryDequeue() with
        | true, value -> value
        | _ ->
            Task.Delay(100) |> Async.AwaitTask |> Async.RunSynchronously
            input ()
    let output x =
        printfn "writing %d" x // todo: remove
        queue.Enqueue x
    output, input


let composeInputs input1 input2 =
    let mutable firstCall = true
    let composed () =
        printfn "waiting for input..." // todo: remove
        firstCall <- false
        let x = match firstCall with
                | true -> input1 ()
                | false -> input2 ()
        printfn "received input" // todo: remove
        x

    composed


let outputCapture () =
    let queue = List<int>()
    let output = queue.Add
    queue, output