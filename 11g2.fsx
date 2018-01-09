module Players
open Chess
open Pieces
open System

[<AbstractClass>]
type Player() = 
    abstract member nextMove : string

type Human() =
    inherit Player()
    override this.nextMove = 
        // prøv evt while (true)
        let mutable validMove = true
        let mutable playerMove = ""
        while (validMove) do
            printfn "Hvor vil du rykke fra/til?"
            playerMove <- System.Console.ReadLine()
            if (playerMove.Length) = 5 then
                (*for i = 0 to playerMove.Length - 1 do
                    playerMove.[i]*)
                printfn "success!"
                validMove <- false
        playerMove

(*type Computer = 
    inherit Player
*)

let spiller1 = new Human()
spiller1.nextMove