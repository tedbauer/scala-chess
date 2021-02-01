package chess.web

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html

import game._

object Web {

  type Ctx2D = dom.CanvasRenderingContext2D

  val CanvasLength = 500
  val Padding = .2
  val BoardCoords = ((Padding / 2) * CanvasLength, (Padding / 2) * CanvasLength)
  val BoardLength = (1 - Padding) * CanvasLength
  val GridLength = 8
  val TileSize = BoardLength.toFloat / GridLength

  sealed trait Command
  case object DoNothing extends Command
  case class SelectTile(tile: (Int, Int)) extends Command
  case class SubmitMove(tile1: (Int, Int), tile2: (Int, Int)) extends Command

  sealed trait ViewState
  case object NothingSelected extends ViewState
  case class TileSelected(tile: (Int, Int)) extends ViewState

  def main(args: Array[String]): Unit = {
    document.addEventListener(
      "DOMContentLoaded",
      { (e: dom.Event) =>
        val canvas = document
          .createElement("canvas")
          .asInstanceOf[html.Canvas]
        canvas.width = CanvasLength
        canvas.height = CanvasLength
        val canvasCtx = canvas
          .getContext("2d")
          .asInstanceOf[Ctx2D]
        document.body.appendChild(canvas)

        var gameState: GameState = initialGame
        var viewState: ViewState = NothingSelected
        drawGame(canvasCtx, gameState, viewState)

        canvas.onclick = { (e: dom.MouseEvent) =>
          val selectedTile = findClickedTile(e)

          val command = getCommand(e, viewState)
          gameState = updateGameState(gameState, command)
          viewState = updateViewState(viewState, command)

          drawGame(canvasCtx, gameState, viewState)
        }

      }
    )
  }

  def getCommand(e: dom.MouseEvent, viewState: ViewState): Command = {
    viewState match {
      case NothingSelected        => SelectTile(findClickedTile(e))
      case TileSelected(prevTile) => SubmitMove(prevTile, findClickedTile(e))
    }
  }

  def updateViewState(viewState: ViewState, command: Command): ViewState = {
    (viewState, command) match {
      case (NothingSelected, SelectTile(tile)) => TileSelected(tile)
      case (TileSelected(_), SelectTile(_))    => NothingSelected

      case (TileSelected(_), SubmitMove(_, _)) => NothingSelected // Impossible
      case (NothingSelected, SubmitMove(_, _)) => NothingSelected // Impossible

      case (s, DoNothing) => s
    }
  }

  def submitMove(
      tile1: (Int, Int),
      tile2: (Int, Int),
      gameState: GameState
  ): GameState = {
    val src = Location(tile1._1, (tile1._2 + 97).toChar)
    val dst = Location(tile2._1, (tile2._2 + 97).toChar)

    move(gameState, src, dst) match {
      case InvalidMove             => gameState
      case ValidMove(newGameState) => newGameState
    }
  }

  def updateGameState(gameState: GameState, command: Command): GameState = {
    val (board, _, _, _) = gameState

    command match {
      case DoNothing          => gameState
      case SelectTile(_)      => gameState
      case SubmitMove(t1, t2) => submitMove(t1, t2, gameState)
    }
  }

  def pieceToString(p: Piece): String = {
    p match {
      case Queen  => "Queen"
      case King   => "King"
      case Rook   => "Rook"
      case Bishop => "Bishop"
      case Knight => "Knight"
      case Pawn   => "Pawn"
    }
  }

  def findClickedTile(e: dom.MouseEvent): (Int, Int) = {
    ((e.clientX / TileSize).toInt, (e.clientY / TileSize).toInt)
  }

  def drawGame(
      ctx: Ctx2D,
      gameState: GameState,
      viewState: ViewState
  ) = {

    ctx.clearRect(0, 0, CanvasLength, CanvasLength)

    ctx.strokeStyle = "black"
    ctx.lineWidth = 2
    ctx.beginPath()

    ctx.rect(0, 0, CanvasLength, CanvasLength)

    val (board, _, _, _) = gameState

    var i = 0
    var j = 0
    for (i <- 0 until GridLength) {
      ctx.fillText(i.toString(), i * 40 + 15, 32)
      for (j <- 0 until GridLength) {
        ctx.rect(
          BoardCoords._1 + i * TileSize,
          BoardCoords._2 + j * TileSize,
          TileSize,
          TileSize
        )

        viewState match {
          case NothingSelected => ()
          case TileSelected(tile) =>
            if ((i, j) == tile) {
              ctx.fillRect(i * TileSize, j * TileSize, TileSize, TileSize)
            }
        }

        val piece = board.get(Location(i - 1, (j + 97).toChar))
        piece match {
          case Some(p) =>
            ctx.fillText(pieceToString(p._1), i * 40 + 20, j * 40 + 20)
          case None => ()
        }
      }
    }

    ctx.stroke()
  }
}
