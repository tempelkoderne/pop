(* Aflevering 8g - Mastermind *)
// Tempelkoderne
//      Anders Geil
//      Peter Lim
//      Tobias Stannius

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(* Types *)
///<summary>
/// Various types used throughout the program.
///</summary>


type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = (code * answer) list
type player = Human | Computer

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(* enterCode *)
///<summary>
/// Prompts the user to enter several characters when called. The input is matched with the allowed codeColor types and appended to a list.
///</summary>
///<returns>
/// A code (codeColor list) containing four elements.
///</returns>



///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(* generateCode *)
///<summary>
/// Generates a random code when called.
///</summary>
///<returns>
/// A code containing four elements.
///</returns>


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(* makeCode *)
///<summary>
/// Calls the appropriate function for creating or generating a code, depending on the player type.
///</summary>
///<params name="user">
/// A player type.
///</params>
///<returns>
/// Unit.
///</returns>

let makeCode (user : player) =
    if user = Human then
        let rec inputCode () =
            printfn "Pick your colors!"
            printfn "([R]ed; [G]reen; [Y]ellow; [P]urple; [W]hite; [B]lack)"
            let input = System.Console.ReadLine().ToLower()
            let sep = [|" "; "; "; ", "; ";"; ","; "-"|]
            let splinput = input.Split (sep, System.StringSplitOptions.None)
            if splinput.Length <> 4 || (Array.exists ((=) "") splinput) then
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
            | h::t -> printfn "Your color must match one of the options above. \nWhat did you mean by \"%s\"?" h; colFixer t
        and colFixer (t:string list) =
            match System.Console.ReadLine().ToLower()::t with
            | h::t when h.Length > 0 -> formatCode (h::t)
            | h::t when h.Length < 1 -> colFixer t
            | _ -> [Black]
        inputCode()
    else
        let colors = [Red; Green; Yellow; Purple; White; Black]
        let rand = System.Random()
        let code = [colors.[rand.Next(0,5)];
                    colors.[rand.Next(0,5)];
                    colors.[rand.Next(0,5)];
                    colors.[rand.Next(0,5)]]
        code

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(* validate *)
///<summary>
/// Validates the guess of the player against the secret code.
/// Increments the number of blacks if the same codeColor is at the same index in both guess and code.
/// Increments the number of whites if the same codeColor exists in guess and code, but at different indexes.
///</summary>
///<params name="guess'">
/// A code of four elements.
///</params>
///<params name="code'">
/// A code of four elements.
///</params>
///<returns>
/// A tuple of integers. Blacks first, whites second.
///</returns>

let validate (guess : code) (code : code) =
    // count blacks
    let blacks (guess : code) (code : code) =
        let rec hits l1 l2 =
            match (l1, l2) with
            | ([],_) -> 0
            | (_,[]) -> 0
            | (gh::gt, sh::st) -> if gh = sh then 1 + (hits gt st)
                                  else 0 + (hits gt st)
        let blacks = hits guess code
        blacks

    // count whites
    let whites (guess : code) (code : code) =
        let rec notBlacks l1 l2 =
            match (l1, l2) with
            | ([],_) -> []
            | (_,[]) -> []
            | (gh::gt,sh::st) -> if gh = sh then notBlacks gt st
                                 else gh::(notBlacks gt st)
        let sortedNotBlacks = (List.sortBy (id) (notBlacks guess code),
                               List.sortBy (id) (notBlacks code guess))
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
    (((blacks guess code), (whites guess code)))


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

let generatePermutations () =
    let cols = [Red; Green; Yellow; Purple; Black; White]
    let mutable perms : code list = []
    for i in cols do
        for j in cols do
            for k in cols do
                for l in cols do
                    perms <- [i;j;k;l]::perms
    Set.ofList perms

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let mutable codePermutations : code Set = Set.ofList [[Red;Red;Red;Red];[Red;Red;Red;Green];[Red;Red;Green;Green];[Red;Red;Green;Yellow];[Red;Green;Green;Green];[Green;Green;Green;Green];[Black;Black;Black;Green];[Red;Black;Black;Black];[Yellow;Purple;Green;Black]]

// codePermutations indeholder en række codes til test.
let botGuess (currentBoard : board) =
    // Remove permutations via sidste guess og answer, hvis currentBoard.Length >=1, dvs. der er gættet før.
    if currentBoard.Length >= 1 then
        let prevGuess = (fst currentBoard.[(currentBoard.Length - 1)])  // prevGuess giver det seneste guess i masterboard.
        let prevAns = (snd currentBoard.[(currentBoard.Length - 1)])    // prevAns giver det seneste validate til det seneste guess i masterboard.
        let remainingPerms = Set.filter (fun permsElement -> (validate prevGuess permsElement) = prevAns) codePermutations      // Filtrerer de elementer fra, som ikke giver prevAnswer, når de validates ift. prevGuess (som secret code).
        codePermutations <- remainingPerms
        // Finder det mindste element blandt de tilbageværende permutationer.
        if (Set.count remainingPerms) >= 1 then
            Set.minElement remainingPerms
        else
            printfn "No permutations left."
            [Red;Red;Red;Red]
    // Standardgæt, hvis currentBoard.Length < 1, dvs. der ikke er gættet før.
    else
        [Red;Red;Green;Green]

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let guess (playerType : player) (currentBoard : board) =
    if currentBoard.Length < 20 then
        if playerType = Human then
            enterCode ()
        else
            botGuess (currentBoard)
    else
        [Black;Black;Black;Black]


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Intro og tutorial.

printfn "__          __  __                            _          __  __           _              __  __ _           _ 
 \ \        / / | |                          | |        |  \/  |         | |            |  \/  (_)         | |
  \ \  /\  / /__| | ___ ___  _ __ ___   ___  | |_ ___   | \  / | __ _ ___| |_ ___ _ __  | \  / |_ _ __   __| |
   \ \/  \/ / _ \ |/ __/ _ \| '_ ` _ \ / _ \ | __/ _ \  | |\/| |/ _` / __| __/ _ \ '__| | |\/| | | '_ \ / _` |
    \  /\  /  __/ | (_| (_) | | | | | |  __/ | || (_) | | |  | | (_| \__ \ ||  __/ |    | |  | | | | | | (_| |
     \/  \/ \___|_|\___\___/|_| |_| |_|\___| \__\____/  |_|  |_|\__,_|___/\__\___|_|    |_|  |_|_|_| |_|\__,_|
                                                                                                              
                                                                                                              "

printf "%70s" "Press any key to continue."
System.Console.ReadKey() |> ignore
System.Console.Clear();;

printfn "Immerse yourself  through the power of your keyboard!  During this game"
printfn "you will be  asked to enter  one or several characters.  Your choice is" 
printfn "confirmed by pressing Enter. Let's try it out!"
printfn ""
printfn "Enter [M]astermind."
printfn "(You can either type 'mastermind' or 'm' and press [Enter] to confirm)."

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
// gameFlow. Initialiserer spillet, sætter variable og kontrollerer flow.

let gameFlow () =
    let mutable playmore = 1
    while playmore = 1 do
            System.Console.Clear()
            
            // Sætter hvem der laver koden afhængigt af input.
            let codeMaker =
                printf "Choose a codemaker ([C]omputer / [H]uman):\n" 
                let valg = ((System.Console.ReadLine ()).ToLower())
                if valg.Length > 0 then
                    if valg.[0] = 'c' then
                        printfn "Codemaker: Computer"
                        printfn "Code generated!\n"
                        Computer
                    else
                        printfn "Codemaker: Human"
                        Human
                else
                        printfn "Codemaker: Human"
                        Human
            
            // Sætter den hemmelige kode afhængigt af hvem der er codeMaker. TOS: Ville foretrække at have denne efter codemaker og codeBreaker.
            let secretCode = (makeCode codeMaker)

            // Sætter hvem der gætter koden afhængigt af input.
            let codeBreaker =
                printf "Choose a codebreaker ([C]omputer / [H]uman):\n"  //Vælger hvem kodeløseren skal være
                let valg = ((System.Console.ReadLine ()).ToLower())
                if valg.Length > 0 then
                    if valg.[0] = 'c' then
                        printfn "Codebreaker: Computer"
                        Computer
                    else
                        printfn "Codebreaker: Human"
                        Human
                else
                        printfn "Codebreaker: Human"
                        Human
            
            // Initialiserer gætte-fasen.
            System.Console.Clear()
            printfn "Press any key to start the game!"
            System.Console.ReadKey() |> ignore
            System.Console.Clear()
            let mutable life = 8 // Sætter livet. Bør måske ændres, så vi udnytter, at guess tager masterBoard som argument.  
            let mutable masterBoard = [] //Sætter masterBoardet op
            
            // Reset permutations.
            codePermutations <- (generatePermutations ())

            // snip til fordel for brug af guess - til gengæld kan computeren ikke køre uendeligt nu, til den gætter rigtigt.
            while life > 0 do
                System.Console.Clear()
                printfn "Life: %A\n" life
                printfn "%s" (printBoard masterBoard)
                let mutable guessCode = (guess codeBreaker masterBoard) // Bemærk: vi udnytter ikke, at guess tager masterBoard! Kan måske omdannes til at styre hvornår spillet slutter?
                let mutable validateGuess = (validate secretCode guessCode)
                masterBoard <- masterBoard @ [(guessCode), (validateGuess)] 
                life <- life - 1
                if (validate secretCode guessCode) = (4,0) then //Hvis løsningen gættes. 
                    life <- life - 30
                else
                    ()
            if life < -1 then
                printfn "The secret code was: %A" secretCode 
                printfn "Game over! %A won!" codeBreaker
            else
                printfn "The secret code was: %A" secretCode
                printfn "Game over! %A won!" codeMaker
            
            // Genstart funktion.
            printfn "Do you want to play again? ([Y]es / [N]o)"
            let restart = ((System.Console.ReadLine ()).ToLower())
            if restart.Length > 0 then                
                if restart.[0] = 'y' then
                    System.Console.Clear()
                    playmore <- 1
                else
                    System.Console.Clear()
                    printfn "Goodbye."
                    playmore <- 0
            else
                System.Console.Clear()
                printfn "Goodbye."

gameFlow ()
