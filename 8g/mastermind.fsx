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
(* makeCode *)
///<summary>
/// Depending on the player type supplied to the function:
/// - Human: Prompts the user to enter several characters. The input is then
///   matched with the allowed codeColor types and appended to a list.
/// - Computer: Generates a random code.
///</summary>
///<params name="user">
/// A player type.
///</params>
///<returns>
/// A code (codeColor list) containing four elements.
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
/// Increments the number of whites if the same codeColor exists in guess and code.
/// Subtracts the number of blacks from whites.
///</summary>
///<params name="guess">
/// A code of four elements.
///</params>
///<params name="code">
/// A code of four elements.
///</params>
///<returns>
/// An answer (int * int) - a tuple of integers. Blacks first, whites second.
///</returns>

let validate (guess : code) (code : code) =
    // Count blacks.
    let blacks (guess : code) (code : code) =
        let rec hits l1 l2 =
            match (l1, l2) with
            | ([],_) -> 0
            | (_,[]) -> 0
            | (gh::gt, sh::st) -> if gh = sh then 1 + (hits gt st)
                                  else 0 + (hits gt st)
        let blacks = hits guess code
        blacks

    // Count whites.
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

    // Return guess with blacks & whites.
    (((blacks guess code), (whites guess code)))

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(* printBoard *)
///<summary>
/// Takes a board and creates an equivalent string with column titles.
///</summary>
///<params name="board">
/// A board / (code * answer) list.
///</params>
///<returns>
/// A string.
///</returns>

let printBoard (board : board) =
    let mutable stringBoard = (sprintf "%-6s%-10s%-10s%-10s%-10s %-5s\n----------------------------------------------------\n" "Turn" "Col1" "Col2" "Col3" "Col4" "B, W")
    for i = 0 to board.Length - 1 do   // Løber igennem hvert element, altså tuple, (code * answer) i et board.
        stringBoard <- stringBoard + (sprintf "%-6d" (i+1))
        for j = 0 to 3 do               // Løber gennem det første element, en code, i hver tuple i board og skriver hvert element i code til stringBoard.
            stringBoard <- stringBoard + (sprintf "%-10s" (sprintf "%A" (fst (board.[i])).[j]))
        stringBoard <- stringBoard + (sprintf "%-6s" (sprintf "%A" (snd (board.[i])))) +  "\n" // Løber gennem det andet element, et answer, i hver tuple i board og skriver det til stringBoard
    stringBoard

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// codePermutations is overwritten with the return value of generatePermutations and 
// used by botGuess to narrow down the possible permutations of the secret code.

let mutable codePermutations : code Set = Set.ofList []

(* botGuess *)
///<summary>
/// Removes the elements from the global mutable codePermutations that do not
/// return an answer equal to the previous answer (E.G. (1,1), when validated
/// against the previous guess.
///</summary>
///<params name="currentBoard">
/// A board / (code * answer) list.
///</params>
///<returns>
/// A code.
///</returns>

let botGuess (currentBoard : board) =
    // Remove permutations via the previous guess and answer, if currentBoard.Length >=1 (one guess has been made).
    if currentBoard.Length >= 1 then
        let prevGuess = (fst currentBoard.[(currentBoard.Length - 1)])  // Equals the previous guess in masterboard.
        let prevAns = (snd currentBoard.[(currentBoard.Length - 1)])    // Equals the previous answer in masterboard.
        let remainingPerms = Set.filter (fun permsElement -> (validate prevGuess permsElement) = prevAns) codePermutations      // Filters the elements that returns an answer equal to prevAnswer, when validated with prevGuess (as code).
        codePermutations <- remainingPerms
        // Finds the smallest element among the remaining permutations.
        if (Set.count remainingPerms) >= 1 then
            Set.minElement remainingPerms
        else
            printfn "No permutations left."
            [Red;Red;Red;Red]
    // The standard guess, if currentBoard.Length < 1, (no guesses have been made).
    else
        [Red;Red;Green;Green]

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(* guess *)
///<summary>
/// Calls the appropriate function with appropriate arguments, depending on the supplied playertype.
///</summary>
///<params name="player">
/// A player.
///</params>
///<params name="board">
/// A board / (code * answer) list.
///</params>
///<returns>
/// A code.
///</returns>
let guess (player : player) (board : board) =
    if board.Length < 20 then
        if player = Human then
            makeCode (player)
        else
            botGuess (board)
    else
        [Black;Black;Black;Black]

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(*Introduction art and tutorial*)

printfn "__          __  __                            _          __  __           _              __  __ _           _ 
 \ \        / / | |                          | |        |  \/  |         | |            |  \/  (_)         | |
  \ \  /\  / /__| | ___ ___  _ __ ___   ___  | |_ ___   | \  / | __ _ ___| |_ ___ _ __  | \  / |_ _ __   __| |
   \ \/  \/ / _ \ |/ __/ _ \| '_ ` _ \ / _ \ | __/ _ \  | |\/| |/ _` / __| __/ _ \ '__| | |\/| | | '_ \ / _` |
    \  /\  /  __/ | (_| (_) | | | | | |  __/ | || (_) | | |  | | (_| \__ \ ||  __/ |    | |  | | | | | | (_| |
     \/  \/ \___|_|\___\___/|_| |_| |_|\___| \__\____/  |_|  |_|\__,_|___/\__\___|_|    |_|  |_|_|_| |_|\__,_|
                                                                                                              
                                                                                                              "

printf "%70s" "Press any key to continue."
// Prevents the program from continuing until the user interacts with the console through their keyboard.
System.Console.ReadKey() |> ignore
System.Console.Clear();;

printfn "Immerse yourself  through the power of your keyboard!  During this game"
printfn "you will be  asked to enter  one or several characters.  Your choice is" 
printfn "confirmed by pressing Enter. Let's try it out!"
printfn ""
printfn "Enter [M]astermind."
printfn "(You can either type 'mastermind' or 'm' and press [Enter] to confirm)."

(* tutorial *)
///<summary>
/// A tutorial on how to interact with the program.
///</summary>
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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(* gameFlow *)
///<summary>
/// Initializes and controls the flow of the game by setting dependencies
/// and variables (E.G. codeMaker, codeBreaker, codePermutations, etc.).
/// See comments for details.
///</summary>

let gameFlow () =
    // The playmore value ensures that the game can end and/or restart.
    let mutable playmore = 1
    while playmore = 1 do
            System.Console.Clear()
            
            // Sets the player type of the code maker. The variable is used by various functions.
            let codeMaker =
                printf "Choose a codemaker ([C]omputer / [H]uman):\n" 
                let valg = ((System.Console.ReadLine ()).ToLower())
                if valg.Length > 0 then
                    if valg.[0] = 'c' then
                        printfn "Codemaker: Computer\n"
                        Computer
                    else
                        printfn "Codemaker: Human\n"
                        Human
                else
                        printfn "Codemaker: Human\n"
                        Human

            // Sets the player type of the code breaker. The variable is used by various functions.
            let codeBreaker =
                printf "Choose a codebreaker ([C]omputer / [H]uman):\n"
                let valg = ((System.Console.ReadLine ()).ToLower())
                if valg.Length > 0 then
                    if valg.[0] = 'c' then
                        printfn "Codebreaker: Computer\n"
                        Computer
                    else
                        printfn "Codebreaker: Human\n"
                        Human
                else
                        printfn "Codebreaker: Human\n"
                        Human
            
            // Sets the secret code via makeCode and the type of the codeMaker.
            printfn "Codemaker:"
            let secretCode = (makeCode codeMaker)
            System.Console.Clear()

            // Prevents the program from continuing until the user interacts with the console through their keyboard.
            printfn "Press any key to start the game!"
            System.Console.ReadKey() |> ignore
            System.Console.Clear()

            // Sets the amount of life, thus settling the ancient dispute over the meaning and value of life.
            let mutable life = 12
            
            // Sets up an empty board.
            let mutable masterBoard = []
            
            (* generatePermutations *)
            ///<summary>
            /// Generates a set containing all permutations of a code of four codeColors. Repetitions allowed.
            ///</summary>
            ///<returns>
            /// A set of codes.
            ///</returns>

            let generatePermutations () =
                let cols = [Red; Green; Yellow; Purple; Black; White]
                let mutable perms : code list = []
                for i in cols do
                    for j in cols do
                        for k in cols do
                            for l in cols do
                                perms <- [i;j;k;l]::perms
                Set.ofList perms

            // Assigns codePermutations a set of all possible code permutations.
            codePermutations <- (generatePermutations ())

            // While the codebreaker still has turns left, the board will be printed and the codebreaker prompted to make a guess.
            while life > 0 do
                System.Console.Clear()
                printfn "Life: %A\n" life
                printfn "%s" (printBoard masterBoard)
                let mutable guessCode = (guess codeBreaker masterBoard)
                let mutable validateGuess = (validate secretCode guessCode)
                masterBoard <- masterBoard @ [(guessCode), (validateGuess)] 
                life <- life - 1
                // If the correct code is guessed, ends the game by subtracting 30 from life, thereby ending the while-loop above.
                if (validate secretCode guessCode) = (4,0) then 
                    life <- life - 30
                else
                    ()
            if life < -1 then
                printfn "The secret code was: %A" secretCode 
                printfn "Game over! %A won!" codeBreaker
            else
                printfn "The secret code was: %A" secretCode
                printfn "Game over! %A won!" codeMaker
            
            // Prompts the player to restart or end the game by setting the playmore value.
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