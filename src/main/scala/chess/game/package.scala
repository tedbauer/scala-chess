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

  case class Move(src: Location, dst: Location)

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
      state: GameState,
      move: Move
  ): Boolean = {
    state.board.get(move.src) map { piece =>
      piece.owner == state.currentTurn && (piece.pieceType match {
        case Queen =>
          val colChange = (move.dst.col - move.src.col.toInt).abs
          val rowChange = (move.dst.row - move.src.row.toInt).abs
          colChange == rowChange ||
          (move.dst.col == move.src.col && move.dst.row != move.src.row) ||
          (move.dst.row == move.src.row && move.dst.col != move.src.col)
        case King =>
          val colChange = (move.dst.col - move.src.col.toInt).abs
          val rowChange = (move.dst.row - move.src.row.toInt).abs
          colChange <= 1 && rowChange <= 1 && (colChange != 0 || rowChange != 0)
        case Rook =>
          (move.dst.col == move.src.col && move.dst.row != move.src.row) ||
            (move.dst.row == move.src.row && move.dst.col != move.src.col)
        case Bishop =>
          val colChange = (move.dst.col - move.src.col.toInt).abs
          val rowChange = (move.dst.row - move.src.row.toInt).abs
          colChange == rowChange
        case Pawn =>
          val colChange = move.dst.col - move.src.col.toInt
          val rowChange = (move.dst.row - move.src.row.toInt).abs
          val canAttack = (state.currentTurn, state.board.get(move.dst)) match {
            case (White, Some(Piece(_, Black))) =>
              colChange == 1 && rowChange == 1
            case (Black, Some(Piece(_, White))) =>
              colChange == -1 && rowChange == 1
            case _ => false
          }
          canAttack || move.dst.row == move.src.row && (
            (state.currentTurn == White && move.dst.col == move.src.col + 1) ||
              (state.currentTurn == Black && move.dst.col == move.src.col - 1)
          )
        case Knight =>
          val colChange = (move.dst.col - move.src.col.toInt).abs
          val rowChange = (move.dst.row - move.src.row.toInt).abs
          (colChange == 2 && rowChange == 1) || (rowChange == 2 && colChange == 1)
      })
    } getOrElse false
  }

  private def flipPlayer(player: Player): Player = {
    player match {
      case White => Black
      case Black => White
    }
  }

  def makeMove(state: GameState, move: Move): MoveResult = {
    state.board.get(move.src) map { srcPiece =>
      state.board.get(move.dst) map { dstPiece =>
        if (dstPiece.owner == state.currentTurn) {
          InvalidMove
        } else {
          if (moveIsLegal(state, move)) {
            ValidMove(
              GameState(
                (state.board - move.dst).updated(move.dst, srcPiece) - move.src,
                flipPlayer(state.currentTurn),
                if (state.currentTurn == White) dstPiece :: state.whitePrison
                else state.whitePrison,
                if (state.currentTurn == Black) dstPiece :: state.blackPrison
                else state.blackPrison
              )
            )
          } else {
            InvalidMove
          }
        }
      } getOrElse {
        if (moveIsLegal(state, move)) {
          ValidMove(
            GameState(
              state.board.updated(move.dst, srcPiece) - move.src,
              flipPlayer(state.currentTurn),
              state.whitePrison,
              state.blackPrison
            )
          )
        } else { InvalidMove }
      }
    } getOrElse InvalidMove
  }
}
