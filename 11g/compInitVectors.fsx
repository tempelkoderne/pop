let au = 149597870700.0 // astronomisk enhed, au, i meter
let G = 6.67384e-11 // Gravitationsmasse, solen
let GMs = 2.959122082322128e-4 // AU^3 / dag^2

// 1st element: longtitude, 2nd element: latitude, 3rd element: radius
let compInitVectors (day0 : float []) (day1: float []) =
    let longToRad1 = (day0.[0]) * (System.Math.PI)/180.0
    let latToRad1 = ((day0.[1]) + 90.0) * (System.Math.PI)/180.0
    let longToRad2 = (day1.[0]) * (System.Math.PI)/180.0
    let latToRad2 = ((day1.[1]) + 90.0) * (System.Math.PI)/180.0
    let x0 = (day0.[2]) * System.Math.Sin(latToRad1) * System.Math.Cos(longToRad1)
    let y0 = (day0.[2]) * System.Math.Sin(latToRad1) * System.Math.Sin(longToRad1)
    let x1 = (day1.[2]) * System.Math.Sin(latToRad2) * System.Math.Cos(longToRad2)
    let y1 = (day1.[2]) * System.Math.Sin(latToRad2) * System.Math.Sin(longToRad2)

    let r0Len = sqrt ((x0**2.0) + (y0**2.0))    // Bruges til beregning af aX og aY. Længde af initiel positionsvektor ift. solen r.
    let aX = -(GMs/(r0Len**3.0))*x0             // Accelerationsvektor x-koordinat.
    let aY = -(GMs/(r0Len**3.0))*y0             // Accelerationsvektor y-koordinat.
    printfn "- Initial acceleration vector,.a0: < %A AU^3/day^2,   %A AU^3/day^2 >" aX aY // Jf. tegning. Initiel accelerationsvektor. Bestemt via opgivet formel for ai og planetens position 01jan.

    let vX = (x1-x0) * au                       // Hastighedsvektor x-koordinat.
    let vY = (y1-y0) * au                       // Hastighedsvektor y-koordinat.
    let vLength = sqrt ((vX**2.0) + (vY**2.0))  // Længde af hastighedsvektor.
    printfn "- Initial speed vector,........v0: < %A AU,   %A AU >" (vX/au) (vY/au)                    // Jf. tegning. Initiel hastighedsvektor. Position1 <x1,y1> minus Position0 <x0,y0> giver en retningsvektor.
    printfn "- Length of v0: %A AU" (vLength/au)                                        // Længden af initiel hastighedsvektor.

    let speed = (vLength / 86400.0) / 1000.0
    printfn "- Planetary speed: %A km/s" speed                                // Planetens beregnede hastighed. Kun til sammenligning med officielle målinger.
    printfn ""
    sprintf ""  // For at gøre Fsharp glad, indtil vi ved, hvad funktionen skal returnere.

// day0.[0] = long1, lat1, rad1 ... long2, lat2, rad2
printfn "Mercury:"
(compInitVal [|30.3790; -2.1662; 0.325304334680|] [|36.0860; -1.4902; 0.321243721300|])

printfn "Venus:"
(compInitVal [|184.6680; 3.2280; 0.720361799843|] [|186.2854; 3.1971; 0.720471891014|])

printfn "Earth:"
(compInitVal [|99.7590; -0.0020; 0.983313645229|] [|100.7782; -0.0020; 0.983306196628|])

printfn "Mars:"
(compInitVal [|174.1074; 1.5217; 1.657734803227|] [|174.5484; 1.5135; 1.657327090727|])

printfn "Jupiter:"
(compInitVal [|162.9198; 1.1555; 5.415869377566|] [|162.9964; 1.1563; 5.416060985675|])

printfn "Saturn:"
(compInitVal [|248.2076; 1.7709; 10.010709670611|] [|248.2379; 1.7700; 10.010839369394|])

printfn "Uranus:"
(compInitVal [|19.1521; -0.6310; 19.975233976928|] [|19.1629;  -0.6309; 19.975143209926|])

printfn "Neptune:" 
(compInitVal [|338.9131;  -0.8074; 29.959661175883|] [|338.9191; -0.8075; 29.959637509324|])

printfn "Pluto:"
(compInitVal [|284.9817; 1.6359; 33.013492974393|] [|284.9870; 1.6343; 33.014108060669|])