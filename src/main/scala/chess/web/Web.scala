package chess.web

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html

import game._

object Web {

  type Ctx2D = dom.CanvasRenderingContext2D

  type ViewState = (Int, Int)

  val CanvasLength = 500
  val BoardCoords = (.1 * CanvasLength, .1 * CanvasLength)
  val BoardLength = .8 * CanvasLength
  val GridLength = 8
  val TileSize = BoardLength.toFloat / GridLength

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

        val g = initialGame
        drawGame(canvasCtx, g, None)

        canvas.onclick = { (e: dom.MouseEvent) =>
          val selectedTile = findClickedTile(e)
          // See if game state needs to be updated.
          // Redraw game if necessary.

          drawGame(canvasCtx, g, Some(selectedTile))
        }

      }
    )
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
      viewState: Option[ViewState]
  ) = {
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

        if (viewState.isDefined) {
          if ((i, j) == viewState.get) {
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
