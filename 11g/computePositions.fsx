(*Compute next planetary positions*) 
let CNPP (data : (float * float) []) =
    // Dependencies. Operators and "global" values.
    let GMs = 2.959122082322128e-4 // AU^3 / dag^2
    let ( .+ ) x y = ((fst x + fst y),(snd x + snd y))
    let ( .* ) x y = (x*(fst y),x*(snd y))
    let r0 = data.[0]
    let v0 = data.[1]
    let a0 = data.[2]

    let r1 = (r0 .+ v0)
    let r0Len = sqrt (((fst r0)**2.0) + ((snd r0)**2.0))
    let a1 = -(GMs/(r0Len**3.0)) .* r0
    let v1 = (v0 .+ a0)
    let positions = [|r1;v1;a1|]
    positions

// Test med begyndelsesdata fra Merkur: r0, v0 og a0 + efterfølgende dage.
// printfn "CNPP"
// printfn "- 01 jan: %A" ([|(0.2804392026,0.1643945659); (-0.02091908706,0.0247536421); (-0.002415813724,-0.001416159527)|])
// printfn "- 02 jan: %A" (CNPP [|(0.2804392026,0.1643945659); (-0.02091908706,0.0247536421); (-0.002415813724,-0.001416159527)|])
// printfn "- 03 jan: %A" (CNPP [|(0.2595201155, 0.189148208); (-0.02333490078, 0.02333748257); (-0.002415813723, -0.001416159526)|])
// printfn "- 04 jan: %A" (CNPP [|(0.2361852147, 0.2124856906); (-0.0257507145, 0.02192132304); (-0.002318839501, -0.001690059113)|])



(*Compute all planetary positions*) 
let CAPP (days : int) (data : (float * float) []) =
    // Dependencies. Operators and "global" values.
    let GMs = 2.959122082322128e-4 // AU^3 / dag^2
    let ( .+ ) x y = ((fst x + fst y),(snd x + snd y))
    let ( .* ) x y = (x*(fst y),x*(snd y))

    let r0 = data.[0]
    let v0 = data.[1]
    let a0 = data.[2]

    // Recursive function. Creates an array containing tuples of x,y-coordinates.
    let rec compute (days : int) (r0 : float * float) (v0 : float * float) (a0 : float * float) =
        if days >= 1 then
            // compute r, concatentate result to array and recursive call with new values
            let r1 = (r0 .+ v0)
            let r0Len = sqrt (((fst r0)**2.0) + ((snd r0)**2.0))
            let a1 = -(GMs/(r0Len**3.0)) .* r0
            let v1 = (v0 .+ a0)

            Array.concat [[|r0|] ; (compute (days - 1) r1 v1 a1)]
        else
            // days < 1, return empty array.
            [||]
    let positions = compute days r0 v0 a0
    positions

// Test med begyndelsesdata fra Merkur: r0, v0 og a0.
// printfn "CAPP, Mercury - 1st of jan + 90 days"
// printfn "%A" (CAPP 90 [|(0.2804392026,0.1643945659); (-0.02091908706,0.0247536421); (-0.002415813724,-0.001416159527)|])

printfn "CAPP, Earth - 1st to 26th of jan"
printfn "%A" (CAPP 26 [|(-0.1666759033, 0.969084551); (-0.01720978776, -0.003125375992); (5.187516652e-05, -0.0003016118194)|])