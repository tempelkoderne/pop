// validate : code -> code -> answer
// som tager den skjulte opgave og et gæt og returnerer antallet af hvid og sort svarstifter.

type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int // sort * hvid
type board = (code * answer) list
type player = Human | Computer


// toHistogram omdanner en code til et histogram, en tuple der indeholder antallet af forekomster af hver farve.
// let toHistogram (c : code) =
//     let mutable r = 0
//     let mutable g = 0
//     let mutable y = 0
//     let mutable p = 0
//     let mutable w = 0
//     let mutable b = 0
//     for i = 0 to 3 do 
//         match c.[i] with
//         | Red -> r <- r + 1
//         | Green -> g <- g + 1
//         | Yellow -> y <- y + 1
//         | Purple -> p <- p + 1
//         | White -> w <- w + 1
//         | Black -> b <- b + 1
//     (r,g,y,p,w,b)

// compareHistograms returnerer summen af intersections ml. to tupler à længden 6.
// let compareHistograms (r1,g1,y1,p1,w1,b1) (r2,g2,y2,p2,w2,b2) = // Secret tuple         guess tuple
//     let r =
//         if r2 <= r1 then r2
//         elif r2 > r1 then r1
//         else 0
//     let g =
//         if g2 <= g1 then g2
//         elif g2 > g1 then g1
//         else 0
//     let y =
//         if y2 <= y1 then y2
//         elif y2 > y1 then y1
//         else 0
//     let p =
//         if p2 <= p1 then p2
//         elif p2 > p1 then p1
//         else 0
//     let w =
//         if w2 <= w1 then w2
//         elif w2 > w1 then w1
//         else 0
//     let b =
//         if b2 <= b1 then b2
//         elif b2 > b1 then b1
//         else 0
//     (r+g+y+p+w+b)

// let validate (secretCode : code) (guessCode : code) =
//     let mutable blacks = 0
//     let mutable whites = 0
//     for i = 0 to 3 do
//         // Optæller antallet af sorte stifter for hver rigtig farve i rigtig position.
//         if secretCode.[i] = guessCode.[i] then
//             blacks <- blacks + 1
//     let secretHistogram = (toHistogram secretCode)
//     // Bestemmer et histogram over farverne i secretCode
//     let guessHistogram = (toHistogram guessCode)
//     // Bestemmer et histogram over farverne i guessCode
//     let sumHistograms = (compareHistograms secretHistogram guessHistogram)
//     // Beregner antallet af "intersections" ml. de to histogrammer. Så hvis kode = 2 rød og gæt = 1x rød er antallet = 1. 
//     whites <- whites + sumHistograms
//     if whites > 0 then
//         whites <- whites - blacks
//     let result : answer = (blacks,whites)
//     result

// printfn "%A" (validate sCode gCode)



// meget mere simpel validate, uden fucking ulæselig jon-sporring-histogramkode
// bonus: rekursiv blacks-optælling (indeksering er langsomt i lister)

let validate (gCode : code) (sCode : code) =
    // count blacks
    let blacks (gCode : code) (sCode : code) =
        let rec hits l1 l2 =
            match (l1, l2) with
            | ([],_) -> 0
            | (_,[]) -> 0
            | (gh::gt, sh::st) -> if gh = sh then 1 + (hits gt st)
                                  else 0 + (hits gt st)
        let blacks = hits gCode sCode
        blacks

    // count whites
    let whites (gCode : code) (sCode : code) =
        let rec notBlacks l1 l2 =
            match (l1, l2) with
            | ([],_) -> []
            | (_,[]) -> []
            | (gh::gt,sh::st) -> if gh = sh then notBlacks gt st
                                 else gh::(notBlacks gt st)
        let sortedNotBlacks = (List.sortBy (fun elem -> elem) (notBlacks gCode sCode),
                               List.sortBy (fun elem -> elem) (notBlacks sCode gCode))
        let rec intersect (lists : codeColor list * codeColor list) =
            match lists with
            | ([],_) -> 0
            | (_,[]) -> 0
            | (h1::t1, h2::t2) when h1=h2 -> 1 + (intersect (t1,t2))
            | (h1::t1, h2::t2) when h1<h2 -> (intersect (t1,(h2::t2)))
            | (h1::t1, h2::t2) when h1>h2 -> (intersect ((h1::t1), t2))
            | _ -> 666
        intersect sortedNotBlacks

    // return answer
    ((blacks gCode sCode), (whites gCode sCode))

// En secret code til test.
let sCode = [White; Red; Black; Black]
// En guess code til test.
let gCode = [Black; White; Red; Black]


printfn "%A" (validate [Black;Black;White;White] [White;White;Black;Black])

