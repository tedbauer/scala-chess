import scala.collection.immutable.HashMap

package object game {

  sealed trait Player
  case object Black extends Player
  case object White extends Player

  sealed trait PieceType
  case object Queen extends PieceType
  case object King extends PieceType
  case object Rook extends PieceType
  case object Bishop extends PieceType
  case object Knight extends PieceType
  case object Pawn extends PieceType

  case class Piece(pieceType: PieceType, owner: Player)

  // col: 1-8 incl
  // row: a-h incl
  case class Location(col: Int, row: Char)

  type Board = Map[Location, Piece]

  type Prison = List[Piece]

  case class GameState(
      board: Board,
      currentTurn: Player,
      whitePrison: Prison,
      blackPrison: Prison
  )

  sealed trait MoveResult
  case class ValidMove(s: GameState) extends MoveResult
  case object InvalidMove extends MoveResult

  val initialBoard: Board = HashMap(
    Location(2, 'a') -> Piece(Pawn, White),
    Location(2, 'b') -> Piece(Pawn, White),
    Location(2, 'c') -> Piece(Pawn, White),
    Location(2, 'd') -> Piece(Pawn, White),
    Location(2, 'e') -> Piece(Pawn, White),
    Location(2, 'f') -> Piece(Pawn, White),
    Location(2, 'g') -> Piece(Pawn, White),
    Location(2, 'h') -> Piece(Pawn, White),
    Location(1, 'a') -> Piece(Rook, White),
    Location(1, 'b') -> Piece(Bishop, White),
    Location(1, 'c') -> Piece(Knight, White),
    Location(1, 'd') -> Piece(Queen, White),
    Location(1, 'e') -> Piece(King, White),
    Location(1, 'f') -> Piece(Knight, White),
    Location(1, 'g') -> Piece(Bishop, White),
    Location(1, 'h') -> Piece(Rook, White),
    Location(7, 'a') -> Piece(Pawn, Black),
    Location(7, 'b') -> Piece(Pawn, Black),
    Location(7, 'c') -> Piece(Pawn, Black),
    Location(7, 'd') -> Piece(Pawn, Black),
    Location(7, 'e') -> Piece(Pawn, Black),
    Location(7, 'f') -> Piece(Pawn, Black),
    Location(7, 'g') -> Piece(Pawn, Black),
    Location(7, 'h') -> Piece(Pawn, Black),
    Location(8, 'a') -> Piece(Rook, Black),
    Location(8, 'b') -> Piece(Bishop, Black),
    Location(8, 'c') -> Piece(Knight, Black),
    Location(8, 'd') -> Piece(Queen, Black),
    Location(8, 'e') -> Piece(King, Black),
    Location(8, 'f') -> Piece(Knight, Black),
    Location(8, 'g') -> Piece(Bishop, Black),
    Location(8, 'h') -> Piece(Rook, Black)
  )

  val initialGame: GameState = GameState(initialBoard, White, List(), List())

  private def moveIsLegal(
      piece: Piece,
      turn: Player,
      src: Location,
      dst: Location
  ): Boolean = {
    piece.owner == turn && (piece.pieceType match {
      case Queen =>
        ((dst.col - src.col.toInt).abs == (dst.row - src.row.toInt).abs) ||
          (dst.col == src.col && dst.row != src.row) ||
          (dst.row == src.row && dst.col != src.col)
      case King =>
        val colChange = (dst.col - src.col.toInt).abs
        val rowChange = (dst.row - src.row.toInt).abs
        colChange <= 1 && rowChange <= 1 && (colChange != 0 || rowChange != 0)
      case Rook =>
        (dst.col == src.col && dst.row != src.row) ||
          (dst.row == src.row && dst.col != src.col)
      case Bishop =>
        (dst.col - src.col.toInt).abs == (dst.row - src.row.toInt).abs
      case Pawn =>
        dst.row == src.row && (
          (turn == White && dst.col == src.col + 1) ||
            (turn == Black && dst.col == src.col - 1)
        )
      case Knight =>
        val colChange = (dst.col - src.col.toInt).abs
        val rowChange = (dst.row - src.row.toInt).abs
        (colChange == 2 && rowChange == 1) || (rowChange == 2 && colChange == 1)
    })
  }

  private def flipPlayer(player: Player): Player = {
    player match {
      case White => Black
      case Black => White
    }
  }

  def move(state: GameState, src: Location, dst: Location): MoveResult = {

    // 1. check out the src. No piece? invalid move; piece?...
    //. check out the dst. no person? if it's valid, cool
    //     there is a person? then..
    //       if there's a friend, then invalidmove
    // .     if there's an enemy, then if it's valid, cool, and take the dst piece

    //

    state.board.get(src) map { srcPiece =>
      state.board.get(dst) map { dstPiece =>
        if (dstPiece.owner == state.currentTurn) {
          InvalidMove
        } else {
          if (moveIsLegal(piece, state.currentTurn, src, dst)) {
            ValidMove(
              GameState(
                (state.board - dst).updated(dst, piece)
              )
            )
          }
        }
      }

      if (moveIsLegal(piece, state.currentTurn, src, dst)) {
        ValidMove(
          GameState(
            state.board.updated(dst, piece) - src,
            flipPlayer(state.currentTurn),
            state.whitePrison,
            state.blackPrison
          )
        )
      } else {
        InvalidMove
      }
    } getOrElse InvalidMove
  }
}
