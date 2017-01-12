// Bare en legeplads til at afprøve småting i.

let ( .+ ) x y = ((fst x + fst y),(snd x + snd y))

printfn "%A" ((2,2) .+ (2,2))