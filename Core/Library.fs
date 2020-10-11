[<AutoOpen>]
module Prelude

let inline (^) f x = f x
let inline (@@) f x = f x

let inline (<!) f a () = f a
let inline (>>=) ma mf = async.Bind(ma, mf)
let inline (>>-) ma f = async.Bind(ma, f >> async.Return)
let inline (>>-!) t f = (Async.AwaitTask t) >>- f

let inline always a _ = a
let inline flip f a b = f b a
let inline curry f a b = f (a, b)
let inline uncurry f (a, b) = f a b

let inline (!>) (x: ^a): ^b =
    ((^a or ^b): (static member op_Implicit: ^a -> ^b) x)

let [<System.Obsolete>] TODO() = raise ^ System.NotImplementedException()

let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

module Result =
    let inline unwrap r = match r with | Ok x -> x | Error e -> failwith e
    let inline wrap f = try f () |> Ok with e -> Error e

type Microsoft.FSharp.Control.AsyncBuilder with
    member __.Bind (t : System.Threading.Tasks.Task<'T>, f:'T -> Async<'R>) : Async<'R> =
        async.Bind(Async.AwaitTask t, f)
    member __.ReturnFrom (t : System.Threading.Tasks.Task<'T>) : Async<'T> =
        async.ReturnFrom(Async.AwaitTask t)
