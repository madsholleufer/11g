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
    member this.run(board : Board) =
        let mutable gameplay = true
        let mutable codestring = ""
        while (gameplay) do
            // Spiller 1s tur
            // Henter spillerens input og gemmer i codestring variablen
            codestring <- this.playerOne.nextMove(board)
            // Vil spilleren stoppe med at spille?
            if codestring = "quit" then
                printfn "player 1 quit!!"
                gameplay <- false
            else 
                // Konverterer codestring til en Position
                // Caster char tal til integers (der passer til brættet). 
                // Har brugt en ASCII tabel til at finde den tilsvarende talværdi
                let firstArg = (int) codestring.[0] - 97
                let secondArg = (int) codestring.[1] - 48
                let thirdArg = (int) codestring.[3] - 97
                let fourthArg = (int) codestring.[4] - 48
                //Putter brikken der skal flyttes i en tuppel
                let source = (firstArg, secondArg)
                //Putter destinatonsfeltet (targetSquare) i en tuppel
                let target = (thirdArg, fourthArg)
                board.move source target
                printfn "succes player 1!!"

            // Spiller 2s tur
            // Hvis spilleren er Human
            if (this.playerTwo :? Human && codestring <> "quit") then
                codestring <- this.playerTwo.nextMove(board)
                // Vil spilleren stoppe med at spille?
                if codestring = "quit" then
                    printfn "player 2 quit!!"
                    gameplay <- false
                else 
                    // Konverterer codestring til en Position
                    // Caster char tal til integers (der passer til brættet). 
                    // Har brugt en ASCII tabel til at finde den tilsvarende talværdi
                    let firstArg = (int) codestring.[0] - 97
                    let secondArg = (int) codestring.[1] - 48
                    let thirdArg = (int) codestring.[3] - 97
                    let fourthArg = (int) codestring.[4] - 48
                    //Putter brikken der skal flyttes i en tuppel
                    let source1 = (firstArg, secondArg)
                    //Putter destinatonsfeltet (targetSquare) i en tuppel
                    let target1 = (thirdArg, fourthArg)
                    board.move source1 target1
                    printfn "succes player 2 human!!"
                    
            //Hvis spilleren er Computer
            elif (this.playerTwo :? Computer && codestring <> "quit") then
                codestring <- this.playerTwo.nextMove(board)
                // Konverterer codestring til en Position
                // Caster char tal til integers (der passer til brættet). 
                // Har brugt en ASCII tabel til at finde den tilsvarende talværdi
                let firstArg = (int) codestring.[0] - 48
                let secondArg = (int) codestring.[1] - 48
                let thirdArg = (int) codestring.[3] - 48
                let fourthArg = (int) codestring.[4] - 48
                //Putter brikken der skal flyttes i en tuppel
                let source = (firstArg, secondArg)
                //Putter destinatonsfeltet (targetSquare) i en tuppel
                let target = (thirdArg, fourthArg)
                board.move source target
                printfn "succes player 2 computer!!"