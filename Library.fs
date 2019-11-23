[<AutoOpen>]
module Prelude

let inline (^) f x = f x

let inline (<!) f a () = f a
let inline (>>=) ma mf = async.Bind(ma, mf)
let inline (>>-) ma f = async.Bind(ma, f >> async.Return)
let inline (>>-!) t f = (Async.AwaitTask t) >>- f

let inline always a _ = a
let inline flip f a b = f b a
let inline curry f a b = f (a, b)
let inline uncurry f (a, b) = f a b
