let gameFlow =
    gameInit ()     // Kører introen. Press key to start.
    player1 = (who ())    // Bestemmer om codemaker er Human / Computer
    player2 = (who ())    // Bestemmer om codebreaker er Human / Computer
    secretCode = (codeMaker player1)    // Sætter en secretCode afhængigt af om player1 er Human / Computer
    masterBoard = []    // Et tomt board, som der skrives til.
    while (validate secretCode (guess player2 masterBoard) <> (4,0) do  // Så længe der ikke valideres til 4 sorte, så kører loopet.
        masterBoard @ ((guess player2 masterBoard), (validate secretCode (guess player2 masterBoard)))  // Skal appende til masterboard: en tuple med (seneste gæt * valideringen
