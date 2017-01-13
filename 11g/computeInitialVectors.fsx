(*Compute Initial Vectors*)
// 1st element: longtitude, 2nd element: latitude, 3rd element: radius
let CIV (day0 : float []) (day1: float []) =
    let GMs = 2.959122082322128e-4 // AU^3 / dag^2
    let ( .* ) x y = (x*(fst y),x*(snd y))

    let degToRad (deg : float) = (deg) * (System.Math.PI)/180.0 // Omregner fra degrees to radians.
    let longtitude0 = (degToRad day0.[0])
    let latitude0 = (degToRad (day0.[1] + 90.0))
    let longtitude1 = (degToRad day1.[0])
    let latitude1 = (degToRad (day1.[1] + 90.0))
    let x0 = (day0.[2]) * System.Math.Sin(latitude0) * System.Math.Cos(longtitude0)
    let y0 = (day0.[2]) * System.Math.Sin(latitude0) * System.Math.Sin(longtitude0)
    let x1 = (day1.[2]) * System.Math.Sin(latitude1) * System.Math.Cos(longtitude1)
    let y1 = (day1.[2]) * System.Math.Sin(latitude1) * System.Math.Sin(longtitude1)

    let r0 = (x0,y0) // Begyndelsesposition, r0.
    let r0Len = sqrt (((fst r0)**2.0) + ((snd r0)**2.0))
    let a0 = -(GMs/(r0Len**3.0)) .* r0  // Accelerationsvektor.
    let v0 = ((x1-x0),(y1-y0))

    let vectors = [|r0;v0;a0|]
    vectors
    
    // printfn "a0: %A\nr0: %A\nv0: %A" a0 r0 v0
    // let vLength = sqrt ((vX**2.0) + (vY**2.0))  // Længde af hastighedsvektor.
    // let speed = ((vLength / 86400.0) / 1000.0) * au



// day0.[0] = long1, lat1, rad1 ... long2, lat2, rad2
// For dato: 2457388.500000000
// printfn "a0 is the initial acceleration vector.\nv0 is the initial speed vector.\nr0 is the initial position vector."
// printfn ""

printfn "Based on  01 to 02 jan 2016 : 2457388.500000000 to 2457389.500000000"
printfn ""
printfn "Mercury: r0, v0, a0"
printfn "%A" (CIV [|30.3790; -2.1662; 0.325304334680|] [|36.0860; -1.4902; 0.321243721300|])

printfn "Venus: r0, v0, a0"
printfn "%A" (CIV [|184.6680; 3.2280; 0.720361799843|] [|186.2854; 3.1971; 0.720471891014|])

printfn "Earth: r0, v0, a0"
printfn "%A" (CIV [|99.7590; -0.0020; 0.983313645229|] [|100.7782; -0.0020; 0.983306196628|])

printfn "Mars: r0, v0, a0"
printfn "%A" (CIV [|174.1074; 1.5217; 1.657734803227|] [|174.5484; 1.5135; 1.657327090727|])

printfn "Jupiter: r0, v0, a0"
printfn "%A" (CIV [|162.9198; 1.1555; 5.415869377566|] [|162.9964; 1.1563; 5.416060985675|])

printfn "Saturn: r0, v0, a0"
printfn "%A" (CIV [|248.2076; 1.7709; 10.010709670611|] [|248.2379; 1.7700; 10.010839369394|])

printfn "Uranus: r0, v0, a0"
printfn "%A" (CIV [|19.1521; -0.6310; 19.975233976928|] [|19.1629;  -0.6309; 19.975143209926|])

printfn "Neptune: r0, v0, a0" 
printfn "%A" (CIV [|338.9131;  -0.8074; 29.959661175883|] [|338.9191; -0.8075; 29.959637509324|])

printfn "Pluto: r0, v0, a0"
printfn "%A" (CIV [|284.9817; 1.6359; 33.013492974393|] [|284.9870; 1.6343; 33.014108060669|])