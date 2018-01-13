module Pieces
open Chess
/// A king is a chessPiece which moves 1 square in any direction
type king(col : Color) =
  inherit chessPiece(col)
  override this.nameOfType = "king"
  // king has runs of 1 in 8 directions: (N, NE, E, SE, S, SW, W, NW)
  override this.candiateRelativeMoves =
      [[(-1,0)];[(-1,1)];[(0,1)];[(1,1)];
      [(1,0)];[(1,-1)];[(0,-1)];[(-1,-1)]]
  // Ændrer i implementationen af availableMoves
  override this.availableMoves (board : Board) : (Position list * chessPiece list) = 
    // Gemmer de mulige moves for kongen
    let moves : (Position list * chessPiece list) = board.getVacantNNeighbours this
    // Henter alle brikker på brættet
    let piecesList = board.piecesOnBoard()
    // Filtrerer modstanderbrikkerne ud i en liste
    let opponentPiecesList = piecesList |> List.filter (fun (x : chessPiece) -> (this.color <> x.color))
    // Nu finder vi alle modstanderbrikkernes mulige moves, og disse vil vi
    // tilføje til et array af modstandermoves
    let mutable opponentMovesList = [] // tom liste til modstander moves
    for i = 0 to opponentPiecesList.Length - 1 do
      //finder modstander moves
      let tuppel = board.getVacantNNeighbours (opponentPiecesList.[i])
      // Udtrækker positionen på brikker der gemmes som navnet på brikken
      // på pladsen snd i tuplen.
      let tuppelnew = snd(tuppel) |> List.map (fun (x : chessPiece) -> match x.position with Some x -> x)
      // ovenstående position er en Position option, så vi konverterer til Position i den anonyme funktion
      //tilføjer modstander moves til listen
      opponentMovesList <- List.append opponentMovesList (List.append (fst(tuppel)) tuppelnew)

    // Nu tjekker vi kongens moves op imod modstandermoves og fjerner dem, der overlapper
    let availableMovesFinal = List.filter (fun (x : Position) -> not (List.contains x opponentMovesList)) (fst(moves))
    (availableMovesFinal,(snd(moves)))

/// A rook is a chessPiece which moves horisontally and vertically
type rook(col : Color) =
  inherit chessPiece(col)
  // rook can move horisontally and vertically
  // Make a list of relative coordinate lists. We consider the
  // current position and try all combinations of relative moves
  // (1,0); (2,0) ... (7,0); (-1,0); (-2,0); ...; (0,-7).
  // Some will be out of board, but will be assumed removed as
  // illegal moves.
  // A list of functions for relative moves
  let indToRel = [
    fun elm -> (elm,0); // South by elm
    fun elm -> (-elm,0); // North by elm
    fun elm -> (0,elm); // West by elm
    fun elm -> (0,-elm) // East by elm
    ]
  // For each function in indToRel, we calculate List.map f [1..7].
  // swap converts (List.map fct indices) to (List.map indices fct).
  let swap f a b = f b a
  override this.candiateRelativeMoves =
    List.map (swap List.map [1..7]) indToRel (*//§\label{chessPieceSwapApp}§*)
  override this.nameOfType = "rook"

(*
 * Definerer en metode, der pakker en option type ud.
 * Metoden bruges mest til at udtrække koordinaterne 
 * på positionen af en skakbrik.
 * Eksempel: Some (1,3) udpakkes til (1,3).
 *)
 let optionHelp (lst : chessPiece list) : Position list =
    let pieces = List.map (fun (x : chessPiece) -> x.position) lst
    let positions = (pieces |> List.choose id) // vælger elementer ud som "har et id", dvs er "Some p"
    positions