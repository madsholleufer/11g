module Game
open Players
open Chess
open Pieces

(*
 *  Dette er klassen der tillader at to spillere kan 
 *  spille et spil skak. Den tager to argumenter af 
 *  typen Player, som er base class for Human og 
 *  Computer. Spillerne kan således være en af de to 
 *  typer.
 *)
type Game(playerOne : Player, playerTwo : Player) =
    //spiller 1s tur
    member this.playerOne = playerOne
    //spiller 2s tur
    member this.playerTwo = playerTwo
    (*
     *  Metode, der læser spiller 1s træk og tjekker om
     *  det er "quit", og hvis det er, afsluttes spillet,
     *  ellers udføres trækket. Dernæst udføres spiller 2s
     *  træk, og hvis dette ikke er en computer, udføres 
     *  samme tjek som for spiller 1. Hvis spiller 2 er 
     *  en computer, udføres dens træk og turen går 
     *  tilbage til spiller 1.
     *)
    member this.run(board : Board, pieces : chessPiece array) =
        let mutable gameplay = true
        let mutable codestring1 = "" //codestring for spiller 1
        let mutable codestring2 = "" //codestring for spiller 2
        // Printer brættet
        board.printBoard board pieces
        while (gameplay) do
            // Spiller 1s tur
            // Henter spillerens input og gemmer i codestring variablen
            codestring1 <- this.playerOne.nextMove(board)
            // Vil spilleren stoppe med at spille?
            if codestring1 = "quit" then
                printfn "GAME OVER. \nSpiller 1 har forladt spillet!"
                gameplay <- false
            else 
                // Konverterer codestring til en Position
                // Caster char tal til integers (der passer til brættet). 
                // Har brugt en ASCII tabel til at finde den tilsvarende talværdi
                let firstArg = (int) codestring1.[0] - 97
                let secondArg = (int) codestring1.[1] - 48
                let thirdArg = (int) codestring1.[3] - 97
                let fourthArg = (int) codestring1.[4] - 48
                //Putter brikken der skal flyttes i en tuppel
                let source = (firstArg, secondArg)
                //Putter destinatonsfeltet (targetSquare) i en tuppel
                let target = (thirdArg, fourthArg)
                board.move source target
                printfn "Spiller 1 har flyttet sin brik fra %A til %A.\n" source target
                // Printer brættet med den flyttede brik
                board.printBoard board pieces

            // Spiller 2s tur
            // Hvis spilleren er Human og den første spiller ikke har skrevet quit
            if (this.playerTwo :? Human && codestring1 <> "quit") then
                codestring2 <- this.playerTwo.nextMove(board)
                // Vil spilleren stoppe med at spille?
                if codestring2 = "quit" then
                    printfn "GAME OVER. \nSpiller 2 har forladt spillet!"
                    gameplay <- false
                else 
                    // Konverterer codestring til en Position
                    // Caster char tal til integers (der passer til brættet). 
                    // Har brugt en ASCII tabel til at finde den tilsvarende talværdi
                    let firstArg = (int) codestring2.[0] - 97
                    let secondArg = (int) codestring2.[1] - 48
                    let thirdArg = (int) codestring2.[3] - 97
                    let fourthArg = (int) codestring2.[4] - 48
                    //Putter brikken der skal flyttes i en tuppel
                    let source2 = (firstArg, secondArg)
                    //Putter destinatonsfeltet (targetSquare) i en tuppel
                    let target2 = (thirdArg, fourthArg)
                    board.move source2 target2
                    printfn "Spiller 2 har flyttet sin brik fra %A til %A.\n" source2 target2
                    // Printer brættet med den flyttede brik
                    board.printBoard board pieces
                    
            //Hvis spilleren er Computer
            elif (this.playerTwo :? Computer && codestring1 <> "quit") then
                codestring2 <- this.playerTwo.nextMove(board)
                // Konverterer codestring til en Position
                // Caster char tal til integers (der passer til brættet). 
                // Har brugt en ASCII tabel til at finde den tilsvarende talværdi
                let firstArg = (int) codestring2.[0] - 48
                let secondArg = (int) codestring2.[1] - 48
                let thirdArg = (int) codestring2.[3] - 48
                let fourthArg = (int) codestring2.[4] - 48
                //Putter brikken der skal flyttes i en tuppel
                let source2 = (firstArg, secondArg)
                //Putter destinatonsfeltet (targetSquare) i en tuppel
                let target2 = (thirdArg, fourthArg)
                board.move source2 target2
                printfn "Computeren har flyttet sin brik...\n"
                // Printer brættet med den flyttede brik
                board.printBoard board pieces