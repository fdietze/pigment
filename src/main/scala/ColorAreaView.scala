package pigment

import scala.scalajs.js
import js.annotation._
import org.scalajs.dom._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import diode._
import diode.react._

import scala.util.Try

object ColorAreaView {

  val width = 200.0
  val height = 200.0
  val colorRadius = 10.0
  val colorBorder = 2.0
  val chromaCircleRadius = width * 0.4
  val chromaCircleBorder = 2.0

  case class Props(proxy: ModelProxy[RootModel]) {
    def palette = proxy.value.palette
  }

  case class State(
    chroma: Double = 100,
    luminance: Double = 70,
    val draggingLuminance: Boolean = false,
    val draggingPalette: Option[(LAB, Int)] = None,
    val dragStartX: Double = 0,
    val dragStartY: Double = 0,
    val dragOffsetX: Double = 0,
    val dragOffsetY: Double = 0
  ) {
    def zoom = chromaCircleRadius / chroma
  }

  def insideColor(p: Props, s: State, index: Int, x: Double, y: Double): Boolean = {
    import p._
    import s._

    val c = palette(index)
    val cx = c.a * zoom + width / 2
    val cy = c.b * zoom + height / 2
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
      println("drawbg")
      import s._
      import CanvasHelpers._

      val ctx = bgCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
      val imageData = ctx.createImageData(bgCanvas.width, bgCanvas.height)

      val data = imageData.data.asInstanceOf[js.Array[Int]]
      val start = System.currentTimeMillis
      for (x <- 0 until bgCanvas.width; y <- 0 until bgCanvas.height) {
        val a = (x - width / 2) / zoom
        val b = (y - height / 2) / zoom
        val i = (y * bgCanvas.width + x) * 4
        val color = ColorConversion.labToRGB(luminance, a.toInt, b.toInt)
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
      println("drawfg")
      import p._
      import s._
      import CanvasHelpers._

      val ctx = fgCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

      ctx.asInstanceOf[js.Dynamic].resetTransform() //TODO: add to scalajs.dom library
      ctx.clearRect(0, 0, fgCanvas.width, fgCanvas.height)

      ctx.translate(width / 2, height / 2)

      def paletteCircle(ctx: CanvasRenderingContext2D, color: LAB, zoom: Double = zoom) {
        percentCirle(ctx, color.a, color.b,
          r = colorRadius / zoom,
          width = colorBorder / zoom,
          a = color.luminance / 100.0,
          refa = Some(luminance / 100.0),
          fillColor = Some(color.toCSS))
      }

      // circle on chroma:
      percentCirle(ctx, 0, 0, chromaCircleRadius, width = chromaCircleBorder, luminance / 100.0)

      val colors = draggingPalette match {
        case Some((col, i)) => palette.updated(i, col)
        case None => palette
      }
      if (chroma > 0) {
        ctx.scale(zoom, zoom)
        for (color <- colors) {
          paletteCircle(ctx, color)
        }
      } else { // chroma is zero => infinite zoom, only draw gray colors
        for (color <- colors if color.isGray) {
          paletteCircle(ctx, color, zoom = 1.0)
        }
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
          $.modState(_.copy(
            dragStartX = mx,
            dragStartY = my,
            dragOffsetX = (zoom) * col.a - mx,
            dragOffsetY = (zoom) * col.b - my,
            draggingPalette = Some((col, i))
          ))
        case None =>
          $.modState(_.copy(draggingLuminance = true))
      }
    }

    def handleMouseMove(p: Props)(e: ReactMouseEvent) = $.state.flatMap { s =>
      //TODO: moving color when zoomlevel = 0
      import p._
      import s._
      val mx = e.clientX - e.nativeEvent.srcElement.getBoundingClientRect.left
      val my = e.clientY - e.nativeEvent.srcElement.getBoundingClientRect.top

      e.stopPropagation()

      draggingPalette match {
        case Some((col, i)) =>
          val newCol = col.copy(
            a = (mx + dragOffsetX) / zoom,
            b = (my + dragOffsetY) / zoom
          )
          val newState = s.copy(draggingPalette = Some((newCol, i)))
          $.setState(newState) >>
            proxy.dispatch(UpdateColor(i, col)) >>
            drawForeground(p, s)

        case None if draggingLuminance =>
          val luminance = ((Math.PI * 2.5 + Math.atan2(my - height / 2.0, mx - width / 2.0)) / (Math.PI * 2) * 100.0) % 100.0
          val newState = s.copy(luminance = luminance)
          $.setState(newState) >> draw(p, newState)

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
          if (dragStartX == mx && dragStartY == my) {
            val newState = s.copy(luminance = col.luminance, chroma = col.chroma)
            println("boom")
            $.setState(newState) >> drawBackground(newState) //TODO: drawForeground is not triggered / triggered with old state
          } else
            proxy.dispatch(UpdateColor(i, col))

        case None if draggingLuminance =>
          val luminance = ((Math.PI * 2.5 + Math.atan2(my - height / 2.0, mx - width / 2.0)) / (Math.PI * 2) * 100.0) % 100.0
          val newState = s.copy(luminance = luminance)
          $.setState(newState) >> drawBackground(newState)

        case _ =>
          Callback.empty
      }) >> $.modState(_.copy(
        draggingPalette = None,
        draggingLuminance = false
      ))
    }

    def handleMouseWheel(p: Props)(e: ReactWheelEvent) = $.state.flatMap { s =>
      import p._
      import s._

      val mx = e.clientX - e.nativeEvent.srcElement.getBoundingClientRect.left
      val my = e.clientY - e.nativeEvent.srcElement.getBoundingClientRect.top
      val deltaY = e.deltaY

      e.stopPropagation()

      (0 until palette.size).reverse.find(i => insideColor(p, s, i, mx, my)) match {
        case Some(i) =>
          val col = palette(i)
          proxy.dispatch(UpdateColor(i, col.copy(l = (col.l - deltaY / 10.0).max(0).min(100))))
        case None =>
          val newState = s.copy(chroma = (chroma + deltaY / 10.0).max(0).min(128))
          $.setState(newState) >> drawBackground(newState)
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

  private val component = ReactComponentB[Props]("ColorAreaView")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(c => c.backend.draw(c.props, c.state))
    .componentDidUpdate(c => c.$.backend.draw(c.currentProps, c.currentState))
    .shouldComponentUpdate(c => { c.$.backend.drawForeground(c.currentProps, c.currentState).runNow(); false })
    .build

  def apply(proxy: ModelProxy[RootModel]) = component(Props(proxy))
}
