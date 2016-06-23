package pigment

import scala.scalajs.js
import js.annotation._
import org.scalajs.dom._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import diode._
import diode.react._

import scala.util.Try

object LuminanceView {

  val width = 200.0
  val height = 200.0
  val colorRadius = 10.0
  val colorBorder = 2.0

  case class Props(proxy: ModelProxy[RootModel]) {
    def palette = proxy.value.palette
  }

  case class State(
    val chroma: Double = 100,
    val draggingPalette: Option[(LAB, Int)] = None,
    val dragStartX: Double = 0,
    val dragStartY: Double = 0,
    val dragOffsetX: Double = 0,
    val dragOffsetY: Double = 0
  ) {
  }

  def colorX(color: LAB) = color.hue / (Math.PI * 2) * width
  def colorY(color: LAB) = ((100 - color.l) / 100) * height
  def color(x: Double, y: Double, chroma: Double) = {
    val hue = (x / width) * (Math.PI * 2)
    val a = Math.cos(hue) * chroma
    val b = Math.sin(hue) * chroma
    val l = 100 - (y / height * 100)
    LAB(l, a, b)
  }

  def insideColor(p: Props, s: State, index: Int, x: Double, y: Double): Boolean = {
    import p._
    import s._

    val c = palette(index)
    val cx = colorX(c)
    val cy = colorY(c)
    val r = colorRadius + colorBorder / 2.0
    (x - cx) * (x - cx) + (y - cy) * (y - cy) <= r * r
  }

  val bgCanvasRef = Ref[raw.HTMLCanvasElement]("canvas-bg")
  val fgCanvasRef = Ref[raw.HTMLCanvasElement]("canvas-fg")

  class Backend($: BackendScope[Props, State]) {
    lazy val bgCanvas = bgCanvasRef($).get
    lazy val fgCanvas = fgCanvasRef($).get

    def draw(p: Props, s: State) = drawBackground(s) >> drawForeground(p, s)

    def drawBackground(s: State) = Callback {
      import s._
      import CanvasHelpers._

      val ctx = bgCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
      val imageData = ctx.createImageData(bgCanvas.width, bgCanvas.height)

      val data = imageData.data.asInstanceOf[js.Array[Int]]
      val start = System.currentTimeMillis
      for (x <- 0 until bgCanvas.width; y <- 0 until bgCanvas.height) {
        val hue = (x / width) * (Math.PI * 2)
        val a = Math.cos(hue) * chroma
        val b = Math.sin(hue) * chroma
        val l = 100 - (y / height * 100)
        val i = (y * bgCanvas.width + x) * 4
        val color = ColorConversion.labToRGB(l, a.toInt, b.toInt)
        data(i + 0) = color(0).toInt.max(0).min(255)
        data(i + 1) = color(1).toInt.max(0).min(255)
        data(i + 2) = color(2).toInt.max(0).min(255)
        data(i + 3) = 255
      }
      val duration = System.currentTimeMillis - start
      println(s"${duration}ms")
      ctx.putImageData(imageData, 0, 0)
    }

    def drawForeground(p: Props, s: State) = Callback {
      import p._
      import s._
      import CanvasHelpers._

      val ctx = fgCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

      ctx.asInstanceOf[js.Dynamic].resetTransform() //TODO: add to scalajs.dom library
      ctx.clearRect(0, 0, fgCanvas.width, fgCanvas.height)

      def paletteCircle(ctx: CanvasRenderingContext2D, color: LAB) {
        percentCirle(
          ctx,
          x = colorX(color),
          y = colorY(color),
          r = colorRadius,
          width = colorBorder,
          a = color.chroma / 128.0,
          refa = Some(chroma / 128.0),
          fillColor = Some(color.toCSS)
        )
      }

      val colors = draggingPalette match {
        case Some((col, i)) => palette.updated(i, col)
        case None => palette
      }
      for (color <- colors) {
        paletteCircle(ctx, color)
      }
    }

    //TODO: native drag event
    def handleMouseDown(p: Props)(e: ReactMouseEvent) = $.state.flatMap { s =>
      import p._
      import s._

      val mx = e.clientX - e.nativeEvent.srcElement.getBoundingClientRect.left
      val my = e.clientY - e.nativeEvent.srcElement.getBoundingClientRect.top

      (0 until palette.size).reverse.find(i => insideColor(p, s, i, mx, my)) match {
        case Some(i) =>
          val col = palette(i)
          val newState = s.copy(
            chroma = col.chroma,
            dragStartX = mx,
            dragStartY = my,
            dragOffsetX = colorX(col) - mx,
            dragOffsetY = colorY(col) - my,
            draggingPalette = Some((col, i))
          )
          $.setState(newState) >> drawBackground(newState)
        case _ =>
          Callback.empty
      }
    }

    def handleMouseMove(p: Props)(e: ReactMouseEvent) = $.state.flatMap { s =>
      import p._
      import s._
      val mx = e.clientX - e.nativeEvent.srcElement.getBoundingClientRect.left
      val my = e.clientY - e.nativeEvent.srcElement.getBoundingClientRect.top

      draggingPalette match {
        case Some((col, i)) =>
          val newCol = color(mx + dragOffsetX, my + dragOffsetY, col.chroma)
          val newState = s.copy(draggingPalette = Some((newCol, i)))
          $.setState(newState) >>
            proxy.dispatch(UpdateColor(i, newCol)) >>
            drawForeground(p, s)

        case _ =>
          Callback.empty
      }
    }

    def handleMouseUp(p: Props)(e: ReactMouseEvent) = $.state.flatMap { s =>
      import p._
      import s._
      val mx = e.clientX - e.nativeEvent.srcElement.getBoundingClientRect.left
      val my = e.clientY - e.nativeEvent.srcElement.getBoundingClientRect.top

      (draggingPalette match {
        case Some((col, i)) =>
          proxy.dispatch(UpdateColor(i, col))
        case _ =>
          Callback.empty
      }) >> $.modState(_.copy(
        draggingPalette = None
      ))
    }

    def handleMouseWheel(p: Props)(e: ReactWheelEvent) = $.state.flatMap { s =>
      import p._
      import s._

      val mx = e.clientX - e.nativeEvent.srcElement.getBoundingClientRect.left
      val my = e.clientY - e.nativeEvent.srcElement.getBoundingClientRect.top
      val deltaY = e.deltaY

      e.preventDefault()

      (0 until palette.size).reverse.find(i => insideColor(p, s, i, mx, my)) match {
        case Some(i) =>
          val col = palette(i)
          val newCol = col.withChroma((col.chroma - deltaY / 10.0).max(0).min(128))
          val newState = s.copy(chroma = newCol.chroma)
          proxy.dispatch(UpdateColor(i, newCol)) >> $.setState(newState) >> drawBackground(newState)
        case None =>
          Callback.empty
      }
    }

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

  private val component = ReactComponentB[Props]("LuminanceView")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(c => c.backend.draw(c.props, c.state))
    .componentDidUpdate(c => c.$.backend.draw(c.currentProps, c.currentState))
    .shouldComponentUpdate(c => { c.$.backend.drawForeground(c.currentProps, c.currentState).runNow(); false })
    .build

  def apply(proxy: ModelProxy[RootModel]) = component(Props(proxy))
}
