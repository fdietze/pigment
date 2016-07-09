package pigment

import scala.scalajs.js
import js.annotation._
import org.scalajs.dom._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import diode._
import diode.react._

import scala.util.Try

import CanvasHelpers._

trait CanvasView {
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

  def colorX(color: Color): Double
  def colorY(color: Color): Double
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

  def drawColor(ctx: CanvasRenderingContext2D, color: Color, a: Double, refa: Double) {
    percentCirle(
      ctx,
      x = colorX(color),
      y = colorY(color),
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

object LuminanceView extends CanvasView {
  case class State(
    chroma: Double = 100,
    dragState: DragState = DragState()
  )

  type Draggable = (Color, Int)

  def colorX(color: Color) = (if (color.isGray) color.hueHint else color.hue) / (Math.PI * 2) * width
  def colorY(color: Color) = ((100 - color.l) / 100) * height
  def colorAt(x: Double, y: Double, state: State) = {
    val hue = (x / width) * (Math.PI * 2)
    val a = Math.cos(hue) * state.chroma
    val b = Math.sin(hue) * state.chroma
    val l = 100 - (y / height * 100)
    LAB(l, a, b, hueHint = hue)
  }

  override def hitDraggable(x: Double, y: Double, p: Props, s: State): Option[(Color, Int)] = {
    import p._
    import s._
    palette.zipWithIndex.find {
      case (col, i) =>
        val cx = colorX(col)
        val cy = colorY(col)
        val r = colorRadius + colorBorder / 2.0
        (x - cx) * (x - cx) + (y - cy) * (y - cy) <= r * r
    }
  }

  class Backend(val $: BackendScope[Props, State]) extends BgFgBackend {

    def drawForeground(p: Props, s: State) = Callback {
      import p._
      import s._

      val ctx = fgCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

      ctx.asInstanceOf[js.Dynamic].resetTransform() //TODO: add to scalajs.dom library
      ctx.clearRect(0, 0, fgCanvas.width, fgCanvas.height)

      val colors = dragState.dragging match {
        case Some((col, i)) => palette.updated(i, col)
        case None => palette
      }
      for (color <- colors) {
        drawColor(ctx, color, color.chroma / 128.0, s.chroma / 128.0)
      }
    }

    def handleMouseWheel(p: Props)(e: ReactWheelEventH) = $.state.flatMap { s =>
      import p._
      import s._

      val (mx, my) = mouseOffset(e)
      // TODO: cross browser deltaY
      // val deltaY = mouseDeltaY(e)
      // val deltaY = if (e.nativeEvent.detail < 0 || e.nativeEvent.asInstanceOf[js.Dynamic].wheelDelta.asInstanceOf[Int] > 0) -1 else 1
      val deltaY = e.deltaY
      // console.log(deltaY, e.deltaY)

      e.preventDefault()

      hitDraggable(mx, my, p, s) match {
        case Some((col, i)) =>
          val col = palette(i)
          val newCol = col.withChroma((col.chroma - deltaY / 10).max(0).min(128))
          val newState = s.copy(chroma = newCol.chroma)
          proxy.dispatch(UpdateColor(i, newCol)) >> $.setState(newState) >> drawBackground(newState)

        case None =>
          Callback.empty
      }
    }

    //TODO: native drag event
    def handleMouseDown(p: Props)(e: ReactMouseEventH) = $.state.flatMap { s =>
      import p._
      import s._

      val (mx, my) = mouseOffset(e)

      hitDraggable(mx, my, p, s) match {
        case Some(target @ (col, i)) =>
          val newState = s.copy(
            chroma = col.chroma,
            DragState(
              dragging = Some(target),
              startX = mx,
              startY = my,
              offsetX = colorX(col) - mx,
              offsetY = colorY(col) - my
            )
          )
          $.setState(newState) >> drawBackground(newState)
        case None =>
          Callback.empty
      }
    }

    def handleMouseMove(p: Props)(e: ReactMouseEventH) = $.state.flatMap { s =>
      import p._
      import s._
      val (mx, my) = mouseOffset(e)

      import dragState.{offsetX, offsetY}
      dragState.dragging match {
        case Some((col, i)) =>
          val newCol = col.copy(lab = colorAt(mx + offsetX, my + offsetY, s))
          val newState = s.copy(dragState = dragState.copy(dragging = Some((newCol, i))))
          $.setState(newState) >>
            proxy.dispatch(UpdateColor(i, newCol)) >>
            drawForeground(p, s)

        case None =>
          Callback.empty
      }
    }

    def handleMouseUp(p: Props)(e: ReactMouseEventH) = $.state.flatMap { s =>
      import p._
      import s._

      $.modState(_.copy(
        dragState = dragState.copy(dragging = None)
      ))
    }

  }

  private val component = ReactComponentB[Props]("LuminanceView")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(c => c.backend.draw(c.props, c.state))
    .componentDidUpdate(c => c.$.backend.draw(c.currentProps, c.currentState))
    .shouldComponentUpdate(c => { c.$.backend.drawForeground(c.currentProps, c.currentState).runNow(); false })
    .build

  def apply(proxy: ModelProxy[RootModel]) = component(Props(proxy))
}
