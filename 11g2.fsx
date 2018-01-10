open Chess
open Pieces
open System

(*
 *  Det her er base klassen som begge nedarver fra.
 *  Den deklarerer en abstrakt funktion kaldet nextMove
 *  som andre klasser der nedarver fra denne klasse, er
 *  nødt til at implementere.
 *)
[<AbstractClass>]
type Player() = 
    abstract member nextMove : unit -> string

(*
 *  Det her er en af de nedarvede klasser, der spørger
 *  spilleren om hvilken brik de vil rykke og hvorhen. 
 *  Dette er enten "quit" eller et gyldigt træk, ellers
 *  spørges igen. Til sidst retuneres enten trækket 
 *  eller quit.
 *)
type Human() =
    inherit Player()
    override this.nextMove() =
        // boolean flag der medvirker at den bliver ved med at spørge
        let mutable invalidMove = true
        let mutable playerMove = ""
        while (invalidMove) do
            printfn "Hvor vil du rykke fra/til?"
            playerMove <- System.Console.ReadLine() //gemmer input
            if playerMove = "quit" then
                invalidMove <- false //exit game/der sker ikke noget
            elif (playerMove.Length) = 5 then //input har korrekt længde
                let pieceToMove = playerMove.[0..1] //udtrækker brikken, der skal flyttes
                //check om der er en brik på dette felt
                // hvis ikke, så spørg igen om en codestring
                // hvilken brik er det?
                let targetSquare = playerMove.[3..4] //udtrækker feltet, der skal flyttes til
                // check om det er et gyldigt træk for den givne brik
                // hvis ikke, så spørg igen om en codestring
                // ellers returner det og ændr invalidMove.
                printfn "success! %A %A " pieceToMove targetSquare
                invalidMove <- false
        playerMove

type Computer() = 
    inherit Player()
    override this.nextMove () =
        "not-yet-implemented"

let spiller1 = new Human()
spiller1.nextMove()
