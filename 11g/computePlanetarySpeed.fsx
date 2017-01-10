// 1st element: longtitude, 2nd element: latitude, 3rd element: radius

let compPlanetarySpeed (day1 : float []) (day2: float []) =
    let au = 149597870700.0
    let longToRad1 = (day1.[0]) * (System.Math.PI)/180.0
    let latToRad1 = ((day1.[1]) + 90.0) * (System.Math.PI)/180.0
    let longToRad2 = (day2.[0]) * (System.Math.PI)/180.0
    let latToRad2 = ((day2.[1]) + 90.0) * (System.Math.PI)/180.0
    let x1 = (day1.[2]) * System.Math.Sin(latToRad1) * System.Math.Cos(longToRad1)
    let y1 = (day1.[2]) * System.Math.Sin(latToRad1) * System.Math.Sin(longToRad1)
    let x2 = (day2.[2]) * System.Math.Sin(latToRad2) * System.Math.Cos(longToRad2)
    let y2 = (day2.[2]) * System.Math.Sin(latToRad2) * System.Math.Sin(longToRad2)
    let x = (x2-x1)*au
    let y = (y2-y1)*au
    let distance = sqrt ((x**2.0) + (y**2.0))
    let speed = (distance / 86400.0) / 1000.0
    speed




// day1.[0], lat1, rad1 ... long2, lat2, rad2
printfn "Earth: %A km/s" (compPlanetarySpeed [|99.7590; -0.0020; 0.983313645229|] [|100.7782; -0.0020; 0.983306196628|])

printfn "Jupiter: %A km/s" (compPlanetarySpeed [|162.9198; 1.1555; 5.415869377566|] [|162.9964; 1.1563; 5.416060985675|])

printfn "Mercury: %A km/s" (compPlanetarySpeed [|30.3790; -2.1662; 0.325304334680|] [|36.0860; -1.4902; 0.321243721300|])

printfn "Neptune: %A km/s" (compPlanetarySpeed [|338.9131;  -0.8074; 29.959661175883|] [|338.9191; -0.8075; 29.959637509324|])

printfn "Pluto: %A km/s" (compPlanetarySpeed [|284.9817; 1.6359; 33.013492974393|] [|284.9870; 1.6343; 33.014108060669|])

printfn "Saturn: %A km/s" (compPlanetarySpeed [|248.2076; 1.7709; 10.010709670611|] [|248.2379; 1.7700; 10.010839369394|])