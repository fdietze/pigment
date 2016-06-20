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

  case class State(chroma: Double = 100, luminance: Double = 70) {
    def zoom = chromaCircleRadius / chroma
  }

  def insideColor(p: Props, s: State, index: Int, x: Double, y: Double): Boolean = {
    val c = p.palette(index)
    val cx = c.a * s.zoom + width / 2
    val cy = c.b * s.zoom + height / 2
    val r = colorRadius + colorBorder / 2.0
    (x - cx) * (x - cx) + (y - cy) * (y - cy) <= r * r
  }

  class Backend($: BackendScope[Props, State]) {

    val canvasBgRef = Ref[raw.HTMLCanvasElement]("canvas-bg")
    val canvasFgRef = Ref[raw.HTMLCanvasElement]("canvas-fg")

    var draggingLuminance = false
    var draggingPalette: Option[(LAB, Int)] = None
    var dragStartX: Double = 0
    var dragStartY: Double = 0
    var dragOffsetX: Double = 0
    var dragOffsetY: Double = 0

    def initCanvas(p: Props, s: State) = Callback {
      for (canvas <- canvasFgRef($)) {
        // val canvas = document.getElementById("color-area-canvas-fg").asInstanceOf[html.Canvas]

        canvas.onmousewheel = (e: WheelEvent) => {
          val mx = e.clientX - e.srcElement.getBoundingClientRect.left
          val my = e.clientY - e.srcElement.getBoundingClientRect.top

          (0 until p.palette.size).reverse.find(i => insideColor(p, s, i, mx, my)) match {
            case Some(i) =>
              val col = p.palette(i)
              p.proxy.dispatch(UpdateColor(i, col.copy(l = (col.l - e.deltaY / 10.0).max(0).min(100)))).runNow()
            case None =>
              $.modState(_.copy(chroma = (s.chroma + e.deltaY / 10.0).max(0).min(128))).runNow()
          }
          e.stopImmediatePropagation()
          false
        }

        //TODO: native drag event
        canvas.onmousedown = (e: MouseEvent) => {
          val mx = e.clientX - e.srcElement.getBoundingClientRect.left
          val my = e.clientY - e.srcElement.getBoundingClientRect.top

          (0 until p.palette.size).reverse.find(i => insideColor(p, s, i, mx, my)) match {
            case Some(i) =>
              val col = p.palette(i)
              dragStartX = mx
              dragStartY = my
              dragOffsetX = (s.zoom) * col.a - mx
              dragOffsetY = (s.zoom) * col.b - my
              draggingPalette = Some((col, i))
            case None =>
              draggingLuminance = true
          }
          e.stopImmediatePropagation()
        }

        canvas.onmousemove = (e: MouseEvent) => {
          val mx = e.clientX - e.srcElement.getBoundingClientRect().left
          val my = e.clientY - e.srcElement.getBoundingClientRect().top

          draggingPalette match {
            case Some((col, i)) =>
              val newCol = col.copy(
                a = (mx + dragOffsetX) / s.zoom,
                b = (my + dragOffsetY) / s.zoom
              )
              draggingPalette = Some((newCol, i))
              p.proxy.dispatch(UpdateColor(i, col)).runNow()
              drawForeground(p, s)

            case None if draggingLuminance =>
              val luminance = ((Math.PI * 2.5 + Math.atan2(my - height / 2.0, mx - width / 2.0)) / (Math.PI * 2) * 100.0) % 100.0
              $.modState(_.copy(luminance = luminance)).runNow()

            case _ =>
          }
          e.stopImmediatePropagation()
        }

        canvas.onmouseup = (e: MouseEvent) => {
          val mx = e.clientX - e.srcElement.getBoundingClientRect().left
          val my = e.clientY - e.srcElement.getBoundingClientRect().top

          draggingPalette match {
            case Some((col, i)) =>
              if (dragStartX == mx && dragStartY == my) {
                $.modState(_.copy(luminance = col.luminance, chroma = col.chroma)).runNow()
              } else
                p.proxy.dispatch(UpdateColor(i, col)).runNow()

            case None if draggingLuminance =>
              val luminance = ((Math.PI * 2.5 + Math.atan2(my - height / 2.0, mx - width / 2.0)) / (Math.PI * 2) * 100.0) % 100.0
              $.modState(_.copy(luminance = luminance)).runNow()

            case _ =>
          }
          draggingPalette = None
          draggingLuminance = false
          e.stopImmediatePropagation()
        }

        drawBackground(p, s)
        drawForeground(p, s)
      }
    }

    def drawBackground(p: Props, s: State) {
      import CanvasHelpers._
      for (canvas <- canvasBgRef($)) {

        val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
        val imageData = ctx.createImageData(canvas.width, canvas.height)

        val data = imageData.data.asInstanceOf[js.Array[Int]]
        val start = System.currentTimeMillis
        for (x <- 0 until canvas.width; y <- 0 until canvas.height) {
          val a = (x - width / 2) / s.zoom
          val b = (y - height / 2) / s.zoom
          val i = (y * canvas.width + x) * 4
          val color = ColorConversion.labToRGB(s.luminance, a.toInt, b.toInt)
          data(i + 0) = color(0).toInt.max(0).min(255)
          data(i + 1) = color(1).toInt.max(0).min(255)
          data(i + 2) = color(2).toInt.max(0).min(255)
          data(i + 3) = 255
        }
        val duration = System.currentTimeMillis - start
        println(s"${duration}ms")
        ctx.putImageData(imageData, 0, 0)
      }
    }

    def drawForeground(p: Props, s: State) {
      import CanvasHelpers._
      for (canvas <- canvasFgRef($)) {

        val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

        ctx.asInstanceOf[js.Dynamic].resetTransform() //TODO: add to scalajs.dom library
        ctx.clearRect(0, 0, canvas.width, canvas.height)

        ctx.translate(width / 2, height / 2)

        def paletteCircle(ctx: CanvasRenderingContext2D, color: LAB, zoom: Double = s.zoom) {
          percentCirle(ctx, color.a, color.b,
            r = colorRadius / zoom,
            width = colorBorder / zoom,
            a = color.luminance / 100.0,
            refa = Some(s.luminance / 100.0),
            fillColor = Some(color.toCSS))
        }

        // circle on s.chroma:
        percentCirle(ctx, 0, 0, chromaCircleRadius, width = chromaCircleBorder, s.luminance / 100.0)

        val colors = draggingPalette match {
          case Some((col, i)) => p.palette.updated(i, col)
          case None => p.palette
        }
        if (s.chroma > 0) {
          ctx.scale(s.zoom, s.zoom)
          for (color <- colors) {
            paletteCircle(ctx, color)
          }
        } else { // chroma is zero => infinite zoom, only draw gray colors
          for (color <- colors if color.isGray) {
            paletteCircle(ctx, color, zoom = 1.0)
          }
        }
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
            ^.cursor := "default"
          )
        )
      // <.br,
      // <.input(^.`type` := "range", ^.value := s.chroma, ^.min := 0, ^.max := 128, ^.step := 1, ^.onChange ==> setChroma(p)),
      // <.input(^.value := s.chroma, ^.onChange ==> setChroma(p), ^.size := 3),
      // <.br,
      // <.input(^.`type` := "range", ^.width := width, ^.value := p.luminance, ^.min := 0, ^.max := 100, ^.step := 1, ^.onChange ==> setLuminance(p))
      )
    }
  }

  private val component = ReactComponentB[Props]("ColorAreaView")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(c => c.backend.initCanvas(c.props, c.state))
    .componentDidUpdate(c => c.$.backend.initCanvas(c.currentProps, c.currentState))
    .build

  def apply(proxy: ModelProxy[RootModel]) = component(Props(proxy))
}
