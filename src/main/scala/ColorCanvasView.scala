package pigment

import scala.scalajs.js
import js.annotation._
import org.scalajs.dom._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import diode._
import diode.react._

import CanvasHelpers._

trait ColorCanvasView {
  val width = 200.0
  val height = 200.0

  val colorRadius = 10.0
  val colorBorder = 2.0

  type Draggable
  case class DragState(
    dragging: Option[Draggable] = None,
    startX: Double = 0,
    startY: Double = 0,
    offsetX: Double = 0,
    offsetY: Double = 0
  )

  case class Props(proxy: ModelProxy[RootModel]) {
    def palette = proxy.value.palette
  }
  type State

  def colorX(color: Color, s: State): Double
  def colorY(color: Color, s: State): Double
  def colorAt(x: Double, y: Double, state: State): LAB

  def hitDraggable(x: Double, y: Double, p: Props, s: State): Option[Draggable] = None

  def mouseOffset(e: ReactMouseEventH): (Double, Double) = {
    val rect = e.target.getBoundingClientRect
    val mx = e.clientX - rect.left
    val my = e.clientY - rect.top
    (mx, my)
  }

  def drawBackgroundOnCanvas(bgCanvas: raw.HTMLCanvasElement, state: State) {
    val ctx = bgCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    val imageData = ctx.createImageData(bgCanvas.width, bgCanvas.height)

    val data = imageData.data.asInstanceOf[js.Array[Int]]
    val start = System.currentTimeMillis
    for (x <- 0 until bgCanvas.width; y <- 0 until bgCanvas.height) {
      val i = (y * bgCanvas.width + x) * 4
      val color = colorAt(x, y, state).toRGB
      data(i + 0) = color.r
      data(i + 1) = color.g
      data(i + 2) = color.b
      data(i + 3) = 255
    }
    val duration = System.currentTimeMillis - start
    println(s"${duration}ms")
    ctx.putImageData(imageData, 0, 0)
  }

  def drawColor(ctx: CanvasRenderingContext2D, color: Color, a: Double, refa: Double, s: State) {
    percentCirle(
      ctx,
      x = colorX(color, s),
      y = colorY(color, s),
      r = colorRadius,
      width = colorBorder,
      a = a,
      refa = Some(refa),
      fillColor = Some(color.toCSS)
    )
  }

  val bgCanvasRef = Ref[raw.HTMLCanvasElement]("canvas-bg")
  val fgCanvasRef = Ref[raw.HTMLCanvasElement]("canvas-fg")

  trait BgFgBackend {
    def $: BackendScope[Props, State]

    lazy val bgCanvas = bgCanvasRef($).get
    lazy val fgCanvas = fgCanvasRef($).get

    def drawBackground(s: State): Callback = Callback {
      drawBackgroundOnCanvas(bgCanvas, s)
    }
    def drawForeground(p: Props, s: State): Callback

    def draw(p: Props, s: State) = drawBackground(s) >> drawForeground(p, s)

    def handleMouseWheel(p: Props)(e: ReactWheelEventH): Callback
    def handleMouseDown(p: Props)(e: ReactMouseEventH): Callback
    def handleMouseMove(p: Props)(e: ReactMouseEventH): Callback
    def handleMouseUp(p: Props)(e: ReactMouseEventH): Callback

    val widthAttr = "width".reactAttr
    val heightAttr = "height".reactAttr
    def render(p: Props) = {
      <.div(
        <.div(
          ^.width := s"${width}px",
          ^.height := s"${height}px",
          ^.position := "relative",
          <.canvas(
            ^.ref := "canvas-bg",
            widthAttr := width,
            heightAttr := height,
            ^.position := "absolute"
          ),
          <.canvas(
            ^.ref := "canvas-fg",
            widthAttr := width,
            heightAttr := height,
            ^.position := "absolute",
            ^.cursor := "default",
            ^.onWheel ==> handleMouseWheel(p),
            ^.onMouseDown ==> handleMouseDown(p),
            ^.onMouseMove ==> handleMouseMove(p),
            ^.onMouseUp ==> handleMouseUp(p)
          )
        )
      )
    }
  }
}
