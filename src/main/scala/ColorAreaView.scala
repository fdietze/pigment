package rainbow

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
    def luminance = proxy.value.colorArea.luminance
    def chroma = proxy.value.colorArea.chroma
    def palette = proxy.value.palette
    def zoom = chromaCircleRadius / chroma

    def insideColor(index: Int, x: Double, y: Double): Boolean = {
      val c = palette(index)
      val cx = c.a * zoom + width / 2
      val cy = c.b * zoom + height / 2
      // val mx = e.clientX - e.srcElement.getBoundingClientRect.left
      // val my = e.clientY - e.srcElement.getBoundingClientRect.top
      (x - cx) * (x - cx) + (y - cy) * (y - cy) <= (colorRadius + colorBorder / 2.0) * (colorRadius + colorBorder / 2.0)
    }
  }

  class Backend($: BackendScope[Props, Unit]) {
    var draggingPalette: Option[(LAB, Int)] = None
    var dragOffsetX: Double = 0
    var dragOffsetY: Double = 0

    def initCanvas(p: Props) = Callback {
      val canvas = document.getElementById("color-area-canvas-fg").asInstanceOf[html.Canvas]

      canvas.onmousewheel = (e: WheelEvent) => {
        val mx = e.clientX - e.srcElement.getBoundingClientRect.left
        val my = e.clientY - e.srcElement.getBoundingClientRect.top

        (0 until p.palette.size).reverse.find(i => p.insideColor(i, mx, my)) match {
          case Some(i) =>
            val col = p.palette(i)
            p.proxy.dispatch(UpdateColor(i, col.copy(l = (col.l - e.deltaY / 10.0).max(0).min(100)))).runNow()
          case None =>
            p.proxy.dispatch(UpdateChroma((p.chroma + e.deltaY / 10.0).max(0).min(128))).runNow()
        }
        console.log(e)
        e.stopImmediatePropagation()
        false
      }

      //TODO: native drag event
      canvas.onmousedown = (e: MouseEvent) => {
        val mx = e.clientX - e.srcElement.getBoundingClientRect.left
        val my = e.clientY - e.srcElement.getBoundingClientRect.top

        (0 until p.palette.size).reverse.find(i => p.insideColor(i, mx, my)).foreach { i =>
          val col = p.palette(i)
          draggingPalette = Some((col, i))
          dragOffsetX = (p.zoom) * col.a - mx
          dragOffsetY = (p.zoom) * col.b - my
        }
        e.stopImmediatePropagation()
      }

      canvas.onmousemove = (e: MouseEvent) => {
        draggingPalette match {
          case Some((col, i)) =>
            val mx = e.clientX - e.srcElement.getBoundingClientRect().left
            val my = e.clientY - e.srcElement.getBoundingClientRect().top
            val newCol = col.copy(
              a = (mx + dragOffsetX) / p.zoom,
              b = (my + dragOffsetY) / p.zoom
            )
            draggingPalette = Some((newCol, i))
            p.proxy.dispatch(UpdateColor(i, col)).runNow()
            drawForeground(p)

          case None =>
        }
        e.stopImmediatePropagation()
      }

      canvas.onmouseup = (e: MouseEvent) => {
        draggingPalette match {
          case Some((col, i)) =>
            draggingPalette = None
            p.proxy.dispatch(UpdateColor(i, col)).runNow()
          case None =>
        }
        e.stopImmediatePropagation()
      }

      drawBackground(p)
      drawForeground(p)
    }

    def drawBackground(p: Props) {
      import CanvasHelpers._
      val canvas = document.getElementById("color-area-canvas-bg").asInstanceOf[html.Canvas]

      val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
      val imageData = ctx.createImageData(canvas.width, canvas.height)

      val data = imageData.data.asInstanceOf[js.Array[Int]]
      val start = System.currentTimeMillis
      for (x <- 0 until canvas.width; y <- 0 until canvas.height) {
        val a = (x - width / 2) / p.zoom
        val b = (y - height / 2) / p.zoom
        val i = (y * canvas.width + x) * 4
        val color = ColorConversion.labToRGB(p.luminance, a.toInt, b.toInt)
        data(i + 0) = color(0).toInt.max(0).min(255)
        data(i + 1) = color(1).toInt.max(0).min(255)
        data(i + 2) = color(2).toInt.max(0).min(255)
        data(i + 3) = 255
      }
      val duration = System.currentTimeMillis - start
      println(s"${duration}ms")
      ctx.putImageData(imageData, 0, 0)
    }

    def drawForeground(p: Props) {
      import CanvasHelpers._
      val canvas = document.getElementById("color-area-canvas-fg").asInstanceOf[html.Canvas]

      val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

      ctx.asInstanceOf[js.Dynamic].resetTransform() //TODO: add to scalajs.dom library
      ctx.clearRect(0, 0, canvas.width, canvas.height)

      ctx.translate(width / 2, height / 2)

      def paletteCircle(ctx: CanvasRenderingContext2D, color: LAB, zoom: Double = p.zoom) {
        percentCirle(ctx, color.a, color.b,
          r = colorRadius / zoom,
          width = colorBorder / zoom,
          a = color.luminance / 100.0,
          refa = Some(p.luminance / 100.0),
          fillColor = Some(color.toCSS))
      }

      // circle on p.chroma:
      percentCirle(ctx, 0, 0, chromaCircleRadius, width = chromaCircleBorder, p.luminance / 100.0)

      val colors = draggingPalette match {
        case Some((col, i)) => p.palette.updated(i, col)
        case None => p.palette
      }
      if (p.chroma > 0) {
        ctx.scale(p.zoom, p.zoom)
        for (color <- colors) {
          paletteCircle(ctx, color)
        }
      } else { // chroma is zero => infinite zoom, only draw gray colors
        for (color <- colors if color.isGray) {
          paletteCircle(ctx, color, zoom = 1.0)
        }
      }
    }

    def setLuminance(p: Props)(e: ReactEventI) = {
      p.proxy.dispatch(UpdateLuminance(Try(e.target.value.toDouble).getOrElse(0.0)))
    }

    def setChroma(p: Props)(e: ReactEventI) = {
      p.proxy.dispatch(UpdateChroma(Try(e.target.value.toDouble).getOrElse(0.0)))
    }

    val widthAttr = "width".reactAttr
    val heightAttr = "height".reactAttr
    def render(p: Props) = {
      <.div(
        <.input(^.`type` := "range", ^.value := p.chroma, ^.min := 0, ^.max := 128, ^.step := 1, ^.onChange ==> setChroma(p)),
        <.input(^.value := p.chroma, ^.onChange ==> setChroma(p), ^.size := 3),
        <.br,
        <.input(^.`type` := "range", ^.value := p.luminance, ^.min := 0, ^.max := 100, ^.step := 1, ^.onChange ==> setLuminance(p)),
        <.input(^.value := p.luminance, ^.onChange ==> setLuminance(p), ^.size := 3),
        <.br,
        <.div(
          ^.width := width,
          ^.height := height,
          ^.position := "relative",
          <.canvas(
            ^.id := "color-area-canvas-bg",
            widthAttr := width,
            heightAttr := height,
            ^.position := "absolute"
          ),
          <.canvas(
            ^.id := "color-area-canvas-fg",
            widthAttr := width,
            heightAttr := height,
            ^.position := "absolute",
            ^.cursor := "default"
          )
        )
      )
    }
  }

  private val component = ReactComponentB[Props]("ColorAreaView")
    .renderBackend[Backend]
    .componentDidMount(c => c.backend.initCanvas(c.props))
    .componentDidUpdate(c => c.$.backend.initCanvas(c.currentProps))
    .build

  def apply(proxy: ModelProxy[RootModel]) = component(Props(proxy))
}
