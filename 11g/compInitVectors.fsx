let au = 149597870700.0 // astronomisk enhed, au, i meter
let G = 6.67384e-11 // Gravitationsmasse, solen
let GMs = 2.959122082322128e-4 // AU^3 / dag^2

// 1st element: longtitude, 2nd element: latitude, 3rd element: radius
let compInitVectors (day0 : float []) (day1: float []) =
    let degToRad (deg : float) = (deg) * (System.Math.PI)/180.0
    let longtitude0 = (degToRad day0.[0])
    let latitude0 = (degToRad (day0.[1] + 90.0))
    let longtitude1 = (degToRad day1.[0])
    let latitude1 = (degToRad (day1.[1] + 90.0))
    let x0 = (day0.[2]) * System.Math.Sin(latitude0) * System.Math.Cos(longtitude0)
    let y0 = (day0.[2]) * System.Math.Sin(latitude0) * System.Math.Sin(longtitude0)
    let x1 = (day1.[2]) * System.Math.Sin(latitude1) * System.Math.Cos(longtitude1)
    let y1 = (day1.[2]) * System.Math.Sin(latitude1) * System.Math.Sin(longtitude1)
    printfn "- r0: < %A,   %A >" x0 y0

    let r0Len = sqrt ((x0**2.0) + (y0**2.0))    // Bruges til beregning af aX og aY. Længde af initiel positionsvektor ift. solen r.
    let aX = -(GMs/(r0Len**3.0))*x0             // Accelerationsvektor x-koordinat.
    let aY = -(GMs/(r0Len**3.0))*y0             // Accelerationsvektor y-koordinat.
    printfn "- a0: < %A AU^3/day^2,   %A AU^3/day^2 >" aX aY // Jf. tegning. Initiel accelerationsvektor. Bestemt via opgivet formel for ai og planetens position 01jan.

    let vX = (x1-x0)                            // Hastighedsvektor x-koordinat.
    let vY = (y1-y0)                            // Hastighedsvektor y-koordinat.
    let vLength = sqrt ((vX**2.0) + (vY**2.0))  // Længde af hastighedsvektor.
    printfn "- v0: < %A AU,   %A AU >" (vX) (vY)                    // Jf. tegning. Initiel hastighedsvektor. Position1 <x1,y1> minus Position0 <x0,y0> giver en retningsvektor.
    printfn "- v0-length: %A AU" (vLength)                                        // Længden af initiel hastighedsvektor.

    let speed = ((vLength / 86400.0) / 1000.0) * au
    printfn "- Planetary speed: %A km/s" speed                                // Planetens beregnede hastighed. Kun til sammenligning med officielle målinger.
    printfn ""
    printfn "%A" [|(x0,y0);(aX,aY);(vX,vY)|]
    sprintf ""  // For at gøre Fsharp glad, indtil vi ved, hvad funktionen skal returnere.

// day0.[0] = long1, lat1, rad1 ... long2, lat2, rad2
// For dato: 2457388.500000000
// printfn "a0 is the initial acceleration vector.\nv0 is the initial speed vector.\nr0 is the initial position vector."
// printfn ""

// printfn "Mercury 01 to 02 jan 2016:"
// (compInitVectors [|30.3790; -2.1662; 0.325304334680|] [|36.0860; -1.4902; 0.321243721300|])

// printfn "Venus:"
// (compInitVectors [|184.6680; 3.2280; 0.720361799843|] [|186.2854; 3.1971; 0.720471891014|])

// printfn "Earth:"
// (compInitVectors [|99.7590; -0.0020; 0.983313645229|] [|100.7782; -0.0020; 0.983306196628|])

// printfn "Mars:"
// (compInitVectors [|174.1074; 1.5217; 1.657734803227|] [|174.5484; 1.5135; 1.657327090727|])

// printfn "Jupiter:"
// (compInitVectors [|162.9198; 1.1555; 5.415869377566|] [|162.9964; 1.1563; 5.416060985675|])

// printfn "Saturn:"
// (compInitVectors [|248.2076; 1.7709; 10.010709670611|] [|248.2379; 1.7700; 10.010839369394|])

// printfn "Uranus:"
// (compInitVectors [|19.1521; -0.6310; 19.975233976928|] [|19.1629;  -0.6309; 19.975143209926|])

// printfn "Neptune:" 
// (compInitVectors [|338.9131;  -0.8074; 29.959661175883|] [|338.9191; -0.8075; 29.959637509324|])

// printfn "Pluto:"
// (compInitVectors [|284.9817; 1.6359; 33.013492974393|] [|284.9870; 1.6343; 33.014108060669|])

// Omregner planetens position fra sfæriske koordinater til kartesianske.
// let sphericToCartesian (day0 : float []) =
//     let degToRad (deg : float) = (deg) * (System.Math.PI)/180.0
//     let longtitude0 = (degToRad day0.[0])
//     let latitude0 = (degToRad (day0.[1] + 90.0))
//     let x0 = (day0.[2]) * System.Math.Sin(latitude0) * System.Math.Cos(longtitude0)
//     let y0 = (day0.[2]) * System.Math.Sin(latitude0) * System.Math.Sin(longtitude0)
//     printfn "- Positional vector, r: < %A,   %A >" x0 y0
//     sprintf ""
// printfn "Mercury 01 jan 2016:"
// (sphericToCartesian [|30.3790; -2.1662; 0.325304334680|])
// printfn "Mercury 02 jan 2016:"
// (sphericToCartesian [|36.0860; -1.4902; 0.321243721300|])
// printfn "Mercury 03 jan 2016:"
// (sphericToCartesian [|41.9275; -0.7825; 0.317624647948|])
// printfn "Mercury 04 jan 2016:"
// (sphericToCartesian [|47.8924; -0.0514; 0.314495672985|])


// Udkast til rekursiv positionsfremskriver.
// Compute planetary positions
let cPP (days : int) (r0 : float * float) (v0 : float * float) (a0 : float * float) =
    // Vi bør måske lave en vektor-class/type med mulighed for addition og gange. Her anvendes custom operators.
    let ( .+ ) x y = ((fst x + fst y),(snd x + snd y))
    let ( .* ) x y = (x*(fst y),x*(snd y))

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

// Test med begyndelsesdata fra Merkur.
printfn "%A" (cPP 4 (0.2804392026,0.1643945659) (-0.02091908706,0.0247536421) (-0.002415813724,-0.001416159527))