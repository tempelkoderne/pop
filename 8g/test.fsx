// guess : player -> board -> code
// som tager en spillertype, et bræt bestående af et spils tidligere gæt og svar og returnerer et nyt gæt enten ved input fra brugeren eller ved at programmet beregner et gæt.

type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int // sort * hvid
type board = (code * answer) list
type player = Human | Computer


// Dependency for guess.
let generateCode () =
    let colors = [Red; Green; Yellow; Purple; White; Black]
    let rand = System.Random()
    let code = [colors.[rand.Next(0,5)];
                colors.[rand.Next(0,5)];
                colors.[rand.Next(0,5)];
                colors.[rand.Next(0,5)]]
    code

// Dependency for guess.
let endGame () =
    printfn "GAME OVER"
    [Red;Red;Red;Red] // placeholder



// Håndterer og retter human input. Spørger ved tvivlstilfælde.
let enterCode () =
    let rec inputCode() =
        printfn "Pick your colors!"
        printfn "([R]ed; [G]reen; [Y]ellow; [P]urple; [W]hite; [B]lack)"
        let input = System.Console.ReadLine().ToLower()
        let sep = [|" "; "; "; ", "; ";"; ","; "-"|]
        let splinput = input.Split (sep, System.StringSplitOptions.None)
        if splinput.Length <> 4 then
            printfn "You need to type 4 colors!"
            inputCode ()
        else
            formatCode (Array.toList splinput)
    and formatCode input =
        match input with
        | [] -> []
        | h::t when h.[0] = 'r' -> Red::(formatCode t)
        | h::t when h.[0] = 'g' -> Green::(formatCode t)
        | h::t when h.[0] = 'y' -> Yellow::(formatCode t)
        | h::t when h.[0] = 'p' -> Purple::(formatCode t)
        | h::t when h.[0] = 'w' -> White::(formatCode t)
        | h::t when h.[0] = 'b' -> Black::(formatCode t)
        | h::t -> printfn "What did you mean by \"%s\"?" h;
                  formatCode (System.Console.ReadLine().ToLower()::t)
    inputCode()


 
// guess : Tager en playertype og board for det nuværende spil og kalder en passende funktion, der genererer en kode.
let guess (playerType : player) (currentBoard : board) =
    if currentBoard.Length < 20 && playerType = Human then
        enterCode  ()
    elif currentBoard.Length < 20 && playerType = Computer then
        generateCode ()
    else
        endGame ()

let printBoard (aBoard : board) =
    let mutable stringBoard = (sprintf "%-6s%-10s%-10s%-10s%-10s %-5s\n----------------------------------------------------\n" "Turn" "Col1" "Col2" "Col3" "Col4" "B, W")
    for i = 0 to aBoard.Length - 1 do   // Løber igennem hvert element, altså tuple, (code * answer) i et board.
        stringBoard <- stringBoard + (sprintf "%-6d" (i+1))
        for j = 0 to 3 do               // Løber gennem det første element, en code, i hver tuple i board og skriver hvert element i code til stringBoard.
            stringBoard <- stringBoard + (sprintf "%-10s" (sprintf "%A" (fst (aBoard.[i])).[j]))
        stringBoard <- stringBoard + (sprintf "%-6s" (sprintf "%A" (snd (aBoard.[i])))) +  "\n" // Løber gennem det andet element, et answer, i hver tuple i board og skriver det til stringBoard
    stringBoard

// makeCode tager en spillertype og kalder en passende funktion, som returnerer en code.
let makeCode (user : player) =
    if user = Human then
        enterCode ()
    else
        generateCode ()


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





// let player1 = Human
let secretCode = [Red;Red;Red;Red]
// let guessCode = (makeCode player1)
// let masterBoard = []
// printfn "%s" (printBoard (masterBoard @ [(guessCode),(validate secretCode guessCode)]))

let gameFlow () =
    let player1 = Human
    let player2 = Computer
    let secretCode = [Red;Red;Red;Red]
    let masterBoard = []
    let mutable counter = 0
    let rec guessFlow (b : board) (pT : player) (sC : code) =
    while counter < 20 do
        printfn "%s" (printBoard masterBoard)
        let guessCode = (makeCode player1)
        let addToBoard =
            masterBoard @ [(guessCode),(validate secretCode guessCode)]
        printfn "%A" addToBoard
        counter <- counter + 1
        
gameFlow ()
            

// let mutable life = 8
// let mutable miniboard = []


// let guesser =
//    if whoCodeBreaker = Human then
//        while life > 0 do
//            printfn "Life: %A" life
//            let mutable guesscode = makeCode(whoCodeBreaker)
//            let mutable guessval = (validate Secretcode guesscode)
//            miniboard <- guesscode 
//            printfn "%A %A" guesscode guessval
//            life <- life - 1
//            if (validate Secretcode guesscode) = (4,0) then
//                life <- life - 30
//            else
//                ()
//    else 
//        let mutable guesscode1 = makeCode(Computer)
//        miniboard <- guesscode1
//        printfn "%A" miniboard
//    if life < -1 then
//        printfn "Game over! You won!"
//    else
//        printfn "Game over! You Lost!"