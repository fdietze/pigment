package pigment

import scala.scalajs.js
import js.annotation._
import org.scalajs.dom._

object CanvasHelpers {
  def arcStroke(ctx: CanvasRenderingContext2D, x: Double, y: Double, r: Double, color: String, start: Double = 0.0, end: Double = 1.0) {
    ctx.beginPath()
    ctx.strokeStyle = color
    ctx.arc(x, y, r,
      Math.PI * (1.5 + 2 * start),
      Math.PI * (1.5 + 2 * end))
    ctx.stroke()
  }
  def arcFill(ctx: CanvasRenderingContext2D, x: Double, y: Double, r: Double, color: String, start: Double = 0.0, end: Double = 1.0) {
    ctx.beginPath()
    ctx.fillStyle = color
    ctx.arc(x, y, r,
      Math.PI * (1.5 + 2 * start),
      Math.PI * (1.5 + 2 * end))
    ctx.fill()
  }

  def percentCirle(ctx: CanvasRenderingContext2D, x: Double, y: Double, r: Double, width: Double, a: Double, refa: Option[Double] = None, fillColor: Option[String] = None) {
    def stroke(color: String, start: Double = 0.0, end: Double = 1.0) { arcStroke(ctx, x, y, r, color, start, end) }
    def fill(color: String, start: Double = 0.0, end: Double = 1.0) { arcFill(ctx, x, y, r, color, start, end) }
    fillColor foreach (fill(_))

    ctx.lineWidth = width

    refa match {
      case Some(ref) =>
        val (first, second) = (if (ref <= a) (ref, a) else (a, ref))
        stroke("white", 0, first)
        stroke(if (ref <= a) "rgb(170, 170, 170)" else "rgb(85, 85, 85)", first, second)
        stroke("black", second, 1)

      case None =>
        stroke("white", 0, a)
        stroke("black", a, 1)
    }
  }
}
