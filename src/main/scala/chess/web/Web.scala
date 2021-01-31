package chess.web

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html

import game._

object Web {
  def main(args: Array[String]): Unit = {
    document.addEventListener(
      "DOMContentLoaded",
      { (e: dom.Event) =>
        setupUI()
      }
    )
  }

  def appendPar(targetNode: dom.Node, text: String): Unit = {
    val parNode = document.createElement("p")
    parNode.textContent = text
    targetNode.appendChild(parNode)
  }

  def addClickedMessage(): Unit = {
    appendPar(document.body, "Hellloo")
  }

  def setupUI(): Unit = {
    val button = document.createElement("button")
    button.textContent = "Click meee"
    button.addEventListener(
      "click",
      { (e: dom.MouseEvent) =>
        addClickedMessage()
      }
    )
    document.body.appendChild(button)

    val title = document.createElement("h1")
    title.textContent = "Scala Chess"
    document.body.appendChild(title)

    type Ctx2D =
      dom.CanvasRenderingContext2D
    val c = document.createElement("canvas").asInstanceOf[html.Canvas]
    val ctx = c
      .getContext("2d")
      .asInstanceOf[Ctx2D]

    document.body.appendChild(c)
    val w = 400
    c.width = w
    c.height = w

    ctx.strokeStyle = "black"
    ctx.lineWidth = 2
    ctx.beginPath()

    ctx.rect(10, 10, w - 10, w - 10)

    val g = initialBoard

    var i = 0
    var j = 0
    for (i <- 1 to 8) {
      ctx.fillText(i.toString(), i * 40 + 15, 32)
      for (j <- 1 to 8) {
        ctx.rect(i * 40, j * 40, 40, 40)
        val piece = g.get(Location(i - 1, (j + 97).toChar))
        piece match {
          case Some(p) => ctx.fillText("P", i * 40 + 20, j * 40 + 20)
          case None    => ()
        }
      }
    }

    ctx.stroke()
  }
}
