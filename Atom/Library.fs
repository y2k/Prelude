module Atom

type IAtom<'T when 'T : not struct>(value : 'T) =
    let refCell = ref value
    
    let rec swap' f = 
        let currentValue = !refCell
        let result = System.Threading.Interlocked.CompareExchange<'T>(refCell, f currentValue, currentValue)
        if obj.ReferenceEquals(result, currentValue) then result
        else System.Threading.Thread.SpinWait 20; swap' f

    member __.Value with get() = !refCell
    member __.swap (f : 'T -> 'T) = swap' f
    member this.update f = this.swap f |> ignore
    member this.dispatch f = this.swap (f >> fst) |> (f >> snd)

let atom value = new IAtom<_>(value)
