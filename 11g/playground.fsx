// TODO:
// 1) produce brief report
// 2) unit tests
// (OPTIONAL) consider making offset a function of window size



// Bare en legeplads til at afprøve småting i.

type vector = float * float
let ( .- ) (x:vector) (y:vector) = ((fst x - fst y), (snd x - snd y))

module Map =
    let keys (m: Map<'Key, 'T>) =
        Map.fold (fun keys key _ -> key::keys) [] m

let mutable simDataMap = Map.empty
simDataMap <- simDataMap.Add ("Mercury", [|(0.2, 0.1); (0.2, 0.1)|])
simDataMap <- simDataMap.Add ("Earth", [|(0.9, 0.3); (0.10, 0.2)|])

let mutable obsDataMap = Map.empty
obsDataMap <- obsDataMap.Add ("Mercury", [|(0.3,0.2); (0.4,0.3)|])
obsDataMap <- obsDataMap.Add ("Earth", [|(0.14,0.12); (0.15,0.1)|])

let mutable errorMap = Map.empty

// printfn "%A" (simData.["Mercury"])
// printfn "%A" (Map.keys simData)
// printfn "%A" (Map.find "Mercury" simData)
// printfn "%A" (Map.tryFind "Mercury" obsDataMap)

// Hjælpefunktion.
let compDistError (name : string) =
    let compLenVec = Array.map2 (fun x y -> x .- y) simDataMap.[name] obsDataMap.[name]
    let compDist = Array.map (fun x -> sqrt (((fst x)**2.0) + ((snd x)**2.0))) compLenVec
    errorMap <- errorMap.Add (name, compDist)

// errorMap <- errorDistance.Add ( osv...

compDistError "Mercury"
compDistError "Earth"

printfn "%A" errorMap