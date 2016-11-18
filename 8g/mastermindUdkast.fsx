
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Typer.

type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = (code * answer) list
type player = Human | Computer


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// enterCode
let enterCode() =
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
        printfn "computer laver kode"
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



////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Intro og tutorial.

printfn "__          __  __                            _          __  __           _              __  __ _           _ 
 \ \        / / | |                          | |        |  \/  |         | |            |  \/  (_)         | |
  \ \  /\  / /__| | ___ ___  _ __ ___   ___  | |_ ___   | \  / | __ _ ___| |_ ___ _ __  | \  / |_ _ __   __| |
   \ \/  \/ / _ \ |/ __/ _ \| '_ ` _ \ / _ \ | __/ _ \  | |\/| |/ _` / __| __/ _ \ '__| | |\/| | | '_ \ / _` |
    \  /\  /  __/ | (_| (_) | | | | | |  __/ | || (_) | | |  | | (_| \__ \ ||  __/ |    | |  | | | | | | (_| |
     \/  \/ \___|_|\___\___/|_| |_| |_|\___| \__\____/  |_|  |_|\__,_|___/\__\___|_|    |_|  |_|_|_| |_|\__,_|
                                                                                                              
                                                                                                              "

printfn "Press any key to continue."
System.Console.ReadKey() |> ignore
System.Console.Clear();;

printfn "Immerse yourself  through the power of your keyboard!  In this game you"
printfn "will be  required  to enter one  or several characters.  Your choice is" 
printfn "confirmed by pressing Enter. Let's try it out!"
printfn ""
printfn "Enter [M]astermind."
printfn "(You can either type 'mastermind' or 'm' and press [Enter] to confirm)."

// Tutorial.

let rec tutorial () =
    let input = ((System.Console.ReadLine ()).ToLower())
    if input.Length > 0 then
        if input.[0] = 'm' then
            System.Console.Clear()
        else
            printfn "You must type the character 'm' and press [Enter] to continue."
            tutorial ()
    else
        printfn "You must type the character 'm' and press [Enter] to continue."
        tutorial ()

tutorial ()

(*Vi er gået væk fra indeksering da det kan crashe programmet hvis man bare trykker enter. *)
(* Vi er gået tilbage til indeksering fordi det er coolt, og fordi vi har fixet det med for loops. *)


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// gameFlow.
let mutable playmore = 0

while playmore = 0 do
        printfn "Press any key to continue."      
        System.Console.ReadKey() |> ignore //Cleaner skærmen når man trykker på tastaturet
        System.Console.Clear()
        let codeMaker = //Vælger hvem der laver koden
            printf "Who should be the code maker? ([C]omputer / [H]uman) \n" 
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
        let secretCode = makeCode(codeMaker) //Sætter den hemmelige kode
        let codeBreaker =
            printf "Who should be the code breaker? ([C]omputer / [H]uman) \n"  //Vælger hvem kodeløseren skal være
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
        printfn "Press any key to continue."
        System.Console.ReadKey() |> ignore
        System.Console.Clear() //Cleaner skærmen og gør klar til kodeløseren
        let mutable life = 8 //Sætter livet 
        let mutable masterboard = [] //Sætter masterboardet op
        if codeBreaker = Human then
            while life > 0 do
                System.Console.Clear()
                printfn "Life: %A" life
                printfn "%s" (printBoard masterboard)
                let mutable guesscode = enterCode()
                let mutable valiguess = (validate secretCode guesscode)
                masterboard <- masterboard @ [(guesscode), (valiguess)] 
                life <- life - 1
                if (validate secretCode guesscode) = (4,0) then //Hvis løsningen gættes. 
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
                let mutable valiguess = (validate secretCode guesscode1)
                masterboard <- masterboard @ [(guesscode1), (valiguess)] 
                life <- life + 1
                if (validate secretCode guesscode1) = (4,0) then
                    life <- life - 999999
                else
                    ()
        if life < -1 then
            printfn "The secret code was: %A" secretCode 
            printfn "Game over! You won!"
        else
            printfn "The secret code was: %A" secretCode
            printfn "Game over! You Lost!"
        printfn "Do you want to play again? ([Y]es / [N]o)"
        let playAgain = ((System.Console.ReadLine ()).ToLower())
        if playAgain.[0] = 'y' then
            System.Console.Clear()
            playmore <- 0
        else
            playmore <- 1