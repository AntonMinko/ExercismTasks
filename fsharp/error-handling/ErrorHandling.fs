module ErrorHandling
open System

// Custom class that implements IDisposable
type Resource() = 
    let mutable disposed = false

    member this.Disposed() = disposed

    interface System.IDisposable with
        member this.Dispose() =
            disposed <- true

let handleErrorByThrowingException() = raise(System.Exception("Exception"))

let handleErrorByReturningOption (input: string) =
    match System.Int32.TryParse input with
    | true, n -> Some(n)
    | _ -> None

let handleErrorByReturningResult (input: string) =
    try
        input |> int |> Ok
    with
    | _ -> Error "Could not convert input to integer"

let bind switchFunction twoTrackInput =
    match twoTrackInput with
    | Ok x -> switchFunction x
    | Error e -> Error e

let cleanupDisposablesWhenThrowingException (resource: Resource) =
    use r = resource
    failwith "Always failing"