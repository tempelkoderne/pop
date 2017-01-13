// Bare en legeplads til at afprøve småting i.

let ( .+ ) x y = ((fst x + fst y),(snd x + snd y))

printfn "%A" ((2,2) .+ (2,2))

let vLen ((x : float),(y : float)) = sqrt ((x**2.0) + (y**2.0))

printfn "%A" (vLen (5.0,4.0))