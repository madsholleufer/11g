open Chess
open Pieces
open Players
open Game

// Create a game
let board = Chess.Board () // Create a board
// Pieces are kept in an array for easy testing
let pieces = [|
  king (White) :> chessPiece;
  rook (White) :> chessPiece;
  king (Black) :> chessPiece |]
// Place pieces on the board
board.[0,0] <- Some pieces.[0]
board.[3,1] <- Some pieces.[1]
board.[4,1] <- Some pieces.[2]

// Spillet
// Spørger brugeren hvem der skal spille spillet.
let mutable gameinit = true
let mutable s1 = ""
let mutable s2 = ""
while (gameinit) do
  printfn "Spiller 1: Human eller Computer?"
  s1 <- System.Console.ReadLine()
  printfn "Spiller 2: Human eller Computer?"
  s2 <- System.Console.ReadLine()
  if (s1 = "Human" || s1 = "Computer") && (s2 = "Computer" || s2 = "Human") then
    gameinit <- false
  else 
    printfn "Ugyldigt input. Prøv igen.\n"

(* 
 * Opretter spillerne. Spiller 1 er altid sort.
 * Hvis der ikke skrives "Human" for en spiller,
 * oprettes de som en computer.
 * Pga. scope issues har vi først oprettet 
 * spillerne som mutable Players, som overskrives
 * til den givne spillertype inden i vores 
 * if-else statements. Dette kunne godt optimeres...
*)
let mutable spiller1 = new Player(Black)
let mutable spiller2 = new Player(White)
if s1 = "Human" then
  spiller1 <- new Human(Black)
  printfn "Spiller 1 er Human :-)"
else 
  spiller1 <- new Computer(Black)
  printfn "Spiller 1 er Computer :-)"

if s2 = "Human" then
  spiller2 <- new Human(White)
  printfn "Spiller 2 er Human :-)"
else 
  spiller2 <- new Computer(White)
  printfn "Spiller 2 er Computer :-)"

// initialiserer spillet
let spil = new Game(spiller1, spiller2)
spil.run(board, pieces)