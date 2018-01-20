module Players
open Chess
open Pieces

(*
 * Definerer en funktion, der tager en liste af skakbrikker
 * som input og pakker hver briks position ud af dens 
 * option type. På denne måde får vi udtrukket brikkernes 
 * positioner.
 *)
let optionHelp (lst : chessPiece list) : Position list =
   let pieces = List.map (fun (x : chessPiece) -> x.position) lst
   let positions = (pieces |> List.choose id) // vælger elementer ud som "har et id", dvs er "Some p"
   positions

(*
 *  Det her er base klassen som begge nedarver fra.
 *  Den deklarerer en abstrakt funktion kaldet nextMove
 *  som andre klasser der nedarver fra denne klasse, er
 *  nødt til at implementere.
 *)
[<AbstractClass>]
type Player(color : Color) =
    member this.color = color // White, Black
    member this.getColor() = color
    // Hvis man har farven sort er man spiller 1 og så starter man,
    // ellers er man spiller 2 og har farven hvid.
    member this.playerNumber : int = if this.color = Black then 1 else 2 //bruges til at vise hvis tur det er
    abstract member nextMove : Board -> string

(*
 *  Dette er en af de nedarvede klasser, der spørger
 *  spilleren om hvilken brik de vil rykke og hvorhen. 
 *  Gyldigt input er enten "quit" eller et gyldigt træk,
 *  ellers spørges igen. Til sidst retuneres enten trækket 
 *  eller quit.
 *)
type Human(color : Color) =
    inherit Player(color)
    override this.nextMove(board : Board) =
        // boolean flag der medvirker at den bliver ved med at spørge indtil der vælges en gyldig codestring
        let mutable invalidMove = true
        let mutable codestring = ""

        (*
         * Tjekker om spilleren har brikker på brættet. 
         * Hvis ja, så fortsætter vi, ellers har spilleren
         * tabt og spillet afsluttes.
         *)
        // Henter spillerens brikker
        let availablePieces = board.piecesOnBoard() |> List.filter (fun x -> (this.color = x.color))
        if availablePieces.IsEmpty then
            printfn "GAME OVER. YOU WIN!" //spilleren taber.
            codestring <- "quit"
            invalidMove <- false
        //spilleren har brikker, så vi prompter for gyldigt input
        while (invalidMove) do
            printfn "\nSpiller %A du har farven: %A. \nHvor vil du rykke fra/til?" (this.playerNumber) (this.getColor())
            codestring <- System.Console.ReadLine() //gemmer input
            if codestring = "quit" then
                invalidMove <- false //exit game/gyldigt input
            //Tjekker om input har korrekt længde og indeholder et mellemrum det rigtige sted
            elif ((codestring.Length) = 5) && ( (int) codestring.[2] = 32) then 
                // Caster bogstaver til tal der passer til brættet. 
                // Har brugt en ASCII tabel til at finde den tilsvarende talværdi
                let firstArg = (int) codestring.[0] - 97 // char a = 97 decimal
                let secondArg = (int) codestring.[1] - 48 // char 0 = 48 decimal
                let thirdArg = (int) codestring.[3] - 97
                let fourthArg = (int) codestring.[4] - 48
                //Putter brikken der skal flyttes i en tuppel
                let pieceToMove = (firstArg, secondArg)
                //Putter destinatonsfeltet (targetSquare) i en tuppel
                let targetSquare = (thirdArg, fourthArg)

                //Tjekker om targetSquare er et gyldigt træk.   
                // Finder brikken, der skal flyttes. Vi bruger tryFind in case den indtastede brik ikke eksisterer.
                let optionChosenPiece = List.tryFind (fun (x : chessPiece) -> Some pieceToMove = x.position) availablePieces
                if optionChosenPiece.IsSome then
                    let chosenPiece = Option.get optionChosenPiece
                    // Finder brikkens mulige træk.
                    // Vi kan nemt udtrække listen med positioner, men vi skal også finde positionerne på 
                    // de brikker, der gives i chessPiece listen. Disse skal udpakkes fra deres option type.
                    let positions = fst(chosenPiece.availableMoves(board)) //udtrækker liste af positioner
                    let chessPieces = snd(chosenPiece.availableMoves(board)) //udtrækker liste af chessPieces
                    let chessPiecePos = optionHelp(chessPieces) //udpakker chessPieces fra option type vha hjælpefunktion
                    let allPositions = List.append positions chessPiecePos //kombinerer positionerne i én liste
                    
                    //Indeholder disse (allPositions) targetSquare?
                    let targetSquareValid = List.contains targetSquare allPositions

                    // Hvis pieceToMove matcher med en af brikkernes position OG hvis 
                    // targetSquare er et gyldigt træk for den givne brik, så er det 
                    // en gyldig codestring

                    // Brikkernes positioner pakkes ud fra option type
                    let pos = optionHelp(availablePieces)
                    // Udfører tjekket
                    if (List.contains pieceToMove pos) && (targetSquareValid) then
                        invalidMove <- false
        codestring
(*
 *  Dette er en af de nedarvede klasser, der laver en
 *  codestring tilfældigt. Den vælger en vilkårlig brik og
 *  vælger dernæst et vilkårligt træk fra dens liste af
 *  mulige træk.
 *)
type Computer(color : Color) = 
    inherit Player(color)
    override this.nextMove (board : Board) =
        //Laver codestring som en string
        let mutable codestring = ""
        //Henter alle egne brikker på brættet
        let piecesList = board.piecesOnBoard() |> List.filter (fun x -> (this.color = x.color))
        //
        if piecesList.IsEmpty then
            printfn "GAME OVER. YOU WIN!" //spilleren vinder hvis computeren ikke har flere brikker tilbage
            codestring <- "quit"
        else
            // Vælger en vilkårlig brik
            // Genererer et tilfældigt tal
            let gen = System.Random()
            let x = gen.Next(0, piecesList.Length) // indtil sidste indeks i listen
            let chosenPiece = piecesList.[x]
            //bruger hjælpefunktionen til at pakke positionen ud fra option type. 
            //funktionen returnerer en liste, så vi bruger indeksering til at udtrække positionen fra listen
            let chosenCoors = optionHelp([chosenPiece]).[0]

            // Finder brikkens mulige træk.
            // Vi kan nemt udtrække listen med positioner, men vi skal også finde positionerne på 
            // de brikker, der gives i chessPiece listen. Disse skal udpakkes fra deres option type.
            let positions = fst(chosenPiece.availableMoves(board)) //udtrækker positioner
            let chessPieces = snd(chosenPiece.availableMoves(board)) //udtrækker chessPieces
            let chessPiecePos = optionHelp(chessPieces) //udpakker option type
            let allPositions = List.append positions chessPiecePos //sætter positionerne og positionerne på chessPieces sammen i én liste
            
            if allPositions.IsEmpty then
                codestring <- "quit" //spillet slutter hvis computeren ikke har flere ledige træk
            else
                //vælger vilkårligt en position
                let randomIndex = gen.Next(0, allPositions.Length) // indtil sidste indeks i arrayet
                let randomPos = allPositions.[randomIndex]
                // Caster positionerne til strings
                let a = (string) (fst(chosenCoors))
                let b = (string) (snd(chosenCoors))
                let c = (string) (fst(randomPos))
                let d = (string) (snd(randomPos))
                // tilføjer alle positioner til codestring
                codestring <-String.concat "" [a; b; " "; c; d]

        codestring
