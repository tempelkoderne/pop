
type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = (code * answer) list
type player = Human | Computer

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let rec toColour (str : string) =
    if str.Length > 0 then 
        match (str.[0]) with    
        | 'r' -> Red
        | 'g' -> Green
        | 'y' -> Yellow
        | 'p' -> Purple
        | 'w' -> White
        | 'b' -> Black
        | _ -> printf "Invalid input. Try again:"; toColour((System.Console.ReadLine ()).ToLower())
    else 
        printf "Invalid input. Try again:"; toColour((System.Console.ReadLine ()).ToLower())

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let enterCode () =
    printfn "Pick your colours!"
    printfn "(red / green / yellow / purple / white / black)"
    printf "1st colour: "
    let col1 =  toColour((System.Console.ReadLine ()).ToLower())    // Til type (Til lower case (Kalder brugerinput))
    printf "2nd colour: "
    let col2 =  toColour((System.Console.ReadLine ()).ToLower())
    printf "3rd colour: "
    let col3 =  toColour((System.Console.ReadLine ()).ToLower())
    printf "4th colour: "
    let col4 =  toColour((System.Console.ReadLine ()).ToLower())
    let colours = [col1] @ [col2] @ [col3] @ [col4]
    printfn "Your pick: %A" colours
    colours

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let generateCode () =
    let colors = [Red; Green; Yellow; Purple; White; Black]
    let rand = System.Random()
    let code = [colors.[rand.Next(0,5)];
                colors.[rand.Next(0,5)];
                colors.[rand.Next(0,5)];
                colors.[rand.Next(0,5)]]
    code

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let makeCode (user : player) =
    if user = Human then
        enterCode ()
    else
        printfn "The computer is generating code."
        generateCode ()

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


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

    // return guess with blacks & whites
    (((blacks gCode sCode), (whites gCode sCode)))


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


// Omdanner et board til en string, som kan printes.
let printBoard (aBoard : board) =
    let mutable stringBoard = (sprintf "%-6s%-10s%-10s%-10s%-10s %-5s\n----------------------------------------------------\n" "Turn" "Col1" "Col2" "Col3" "Col4" "B, W")
    for i = 0 to aBoard.Length - 1 do   // Løber igennem hvert element, altså tuple, (code * answer) i et board.
        stringBoard <- stringBoard + (sprintf "%-6d" (i+1))
        for j = 0 to 3 do               // Løber gennem det første element, en code, i hver tuple i board og skriver hvert element i code til stringBoard.
            stringBoard <- stringBoard + (sprintf "%-10s" (sprintf "%A" (fst (aBoard.[i])).[j]))
        stringBoard <- stringBoard + (sprintf "%-6s" (sprintf "%A" (snd (aBoard.[i])))) +  "\n" // Løber gennem det andet element, et answer, i hver tuple i board og skriver det til stringBoard
    stringBoard


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let humanGuess() =
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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let intro = "__          __  __                            _          __  __           _              __  __ _           _ 
 \ \        / / | |                          | |        |  \/  |         | |            |  \/  (_)         | |
  \ \  /\  / /__| | ___ ___  _ __ ___   ___  | |_ ___   | \  / | __ _ ___| |_ ___ _ __  | \  / |_ _ __   __| |
   \ \/  \/ / _ \ |/ __/ _ \| '_ ` _ \ / _ \ | __/ _ \  | |\/| |/ _` / __| __/ _ \ '__| | |\/| | | '_ \ / _` |
    \  /\  /  __/ | (_| (_) | | | | | |  __/ | || (_) | | |  | | (_| \__ \ ||  __/ |    | |  | | | | | | (_| |
     \/  \/ \___|_|\___\___/|_| |_| |_|\___| \__\____/  |_|  |_|\__,_|___/\__\___|_|    |_|  |_|_|_| |_|\__,_|
                                                                                                              
                                                                                                              "

(*Intro skærm kan altid ændres*)


let pressto = "Press any key to continue:"


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let mutable playmore = 0

while playmore = 0 do
        printfn "%s" intro //printer intro skærmen ud
        printfn "%A" pressto //printer "press any key to continue ud"
        System.Console.ReadKey() |> ignore //Cleaner skærmen når man trykker på tastaturet
        System.Console.Clear()
        let whoCodeMaker = //Vælger hvem der laver koden
            printf "Who should be the code maker? (Computer (C)/ Human (H)) \n" 
            let valg = ((System.Console.ReadLine ()).ToLower())
            if valg.Length > 0 then
                if valg.[0] = 'c' then
                    printfn "Code Maker: Computer"
                    Computer
                else
                    printfn "Code Maker: Human"
                    Human
            else
                    printfn "Code Maker: Human"
                    Human
        let Secretcode = makeCode(whoCodeMaker) //Sætter den hemmelige kode
        let whoCodeBreaker =
            printf "Who should be the code breaker? (Computer (C)/ Human (H)) \n"  //Vælger hvem kodeløseren skal være
            let valg = ((System.Console.ReadLine ()).ToLower())
            if valg.Length > 0 then
                if valg.[0] = 'c' then
                    printfn "Code Breaker: Computer"
                    Computer
                else
                    printfn "Code Breaker: Human"
                    Human
            else
                    printfn "Code Breaker: Human"
                    Human
        printfn "%A" pressto
        System.Console.ReadKey() |> ignore
        System.Console.Clear() //Cleaner skærmen og gør klar til kodeløseren
        let mutable life = 8 //Sætter livet 
        let mutable masterboard = [] //Sætter masterboardet op
        if whoCodeBreaker = Human then
            while life > 0 do
                System.Console.Clear()
                printfn "Life: %A" life
                printfn "%s" (printBoard masterboard)
                let mutable guesscode = humanGuess()
                let mutable valiguess = (validate Secretcode guesscode)
                masterboard <- masterboard @ [(guesscode), (valiguess)] 
                life <- life - 1
                if (validate Secretcode guesscode) = (4,0) then //Hvis løsningen gættes. 
                    life <- life - 30
                else
                    ()
        else 
            life <- 0
            while life >= 0 do
                System.Console.Clear()
                printfn "Turns: %A" life
                printfn "%s" (printBoard masterboard)
                let mutable guesscode1 = makeCode(Computer)
                let mutable valiguess = (validate Secretcode guesscode1)
                masterboard <- masterboard @ [(guesscode1), (valiguess)] 
                life <- life + 1
                if (validate Secretcode guesscode1) = (4,0) then
                    life <- life - 999999
                else
                    ()
        if life < -1 then
            printfn "The secret code was: %A" Secretcode 
            printfn "Game over! You won!"
        else
            printfn "The secret code was: %A" Secretcode
            printfn "Game over! You Lost!"
        printfn "Do you want to play again? (Y/N)"
        let genvalg = ((System.Console.ReadLine ()).ToLower())
        if genvalg = "Y" || genvalg = "y" then
            System.Console.Clear()
            playmore <- 0
        else
            playmore <- 1


        