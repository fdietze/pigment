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

  case class Props(proxy: ModelProxy[RootModel]) {
    def luminance = proxy.value.colorArea.luminance
    def chroma = proxy.value.colorArea.chroma
    def palette = proxy.value.palette
  }

  val colorRadius = 10.0
  val colorBorder = 2.0

  class Backend($: BackendScope[Props, Unit]) {
    var draggingPalette: Option[(LAB, Int)] = None
    var dragOffsetX: Double = 0
    var dragOffsetY: Double = 0

    def initCanvas(p: Props) = Callback {
      val canvas = document.getElementById("color-area-canvas").asInstanceOf[html.Canvas]
      val width = canvas.width.toDouble
      val height = canvas.height.toDouble

      canvas.addEventListener("mousedown", (event: MouseEvent) => {
        // println("down: " + event.clientX);
        def inside(index: Int): Boolean = {
          val c = p.palette(index)
          val cx = c.a * 100.0 / p.chroma + width / 2
          val cy = c.b * 100.0 / p.chroma + height / 2
          val mx = event.clientX - event.srcElement.getBoundingClientRect.left
          val my = event.clientY - event.srcElement.getBoundingClientRect.top
          (mx - cx) * (mx - cx) + (my - cy) * (my - cy) <= (colorRadius + colorBorder / 2.0) * (colorRadius + colorBorder / 2.0)
        }
        (0 until p.palette.size).reverse.find(inside).foreach { i =>
          println(s"down $i")
          val col = p.palette(i)
          draggingPalette = Some((col, i))
          val mx = event.clientX - event.srcElement.getBoundingClientRect.left
          val my = event.clientY - event.srcElement.getBoundingClientRect.top
          dragOffsetX = (100.0 / p.chroma) * col.a - mx
          dragOffsetY = (100.0 / p.chroma) * col.b - my
        }
        event.stopPropagation()
      })
      canvas.addEventListener("mousemove", (event: MouseEvent) => {
        draggingPalette match {
          case Some((col, i)) =>
            val mx = event.clientX - event.srcElement.getBoundingClientRect().left
            val my = event.clientY - event.srcElement.getBoundingClientRect().top
            val newCol = col.copy(
              a = (p.chroma / 100.0) * (mx + dragOffsetX),
              b = (p.chroma / 100.0) * (my + dragOffsetY)
            )
            // println(s"drag $i d:($dx,$dy) $col -> $newCol")
            draggingPalette = Some((newCol, i))
            draw(p)

          case None =>
        }
        event.stopPropagation()
      })
      canvas.addEventListener("mouseup", (event: MouseEvent) => {
        println(s"up")
        draggingPalette match {
          case Some((col, i)) =>
            draggingPalette = None
            p.proxy.dispatch(UpdateColor(i, col)).runNow()
          case None =>
        }
        event.stopPropagation()
      })

      draw(p)
    }

    def draw(p: Props) {
      val canvas = document.getElementById("color-area-canvas").asInstanceOf[html.Canvas]
      val width = canvas.width.toDouble
      val height = canvas.height.toDouble

      val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
      val imageData = ctx.createImageData(canvas.width, canvas.height)

      val data = imageData.data.asInstanceOf[js.Array[Int]]
      val start = System.currentTimeMillis
      for (x <- 0 until canvas.width; y <- 0 until canvas.height) {
        val a = (p.chroma / 100.0) * (x - width / 2)
        val b = (p.chroma / 100.0) * (y - height / 2)
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

      ctx.asInstanceOf[js.Dynamic].resetTransform() //TODO: add to scalajs.dom library
      ctx.translate(width / 2, height / 2)
      if (p.chroma > 0) {
        ctx.scale(100.0 / p.chroma, 100.0 / p.chroma)

        percentCirle(ctx, 0, 0, r = p.chroma, width = 2.0 * p.chroma / 100.0, p.luminance / 100.0)

        for ((color, index) <- p.palette.zipWithIndex if !draggingPalette.isDefined || index != draggingPalette.get._2) {
          percentCirle(ctx, color.a, color.b, r = p.chroma / colorRadius, width = colorBorder * p.chroma / 100.0, color.l / 100.0, refa = Some(p.luminance / 100.0), fillColor = Some(color.toCSS))
        }
        for ((color, index) <- draggingPalette) {
          percentCirle(ctx, color.a, color.b, r = p.chroma / colorRadius, width = colorBorder * p.chroma / 100.0, color.l / 100.0, refa = Some(p.luminance / 100.0), fillColor = Some(color.toCSS))
        }
      } else { // chroma is zero => infinite zoom
        percentCirle(ctx, 0, 0, r = 100.0, width = 2.0, p.luminance / 100.0)
        for (color <- p.palette if color.a == 0.0 && color.b == 0.0) {
          percentCirle(ctx, 0, 0, r = colorRadius, width = colorBorder, color.l / 100.0, refa = Some(p.luminance / 100.0), fillColor = Some(color.toCSS))
        }
      }
    }

    def setLuminance(p: Props)(e: ReactEventI) = {
      p.proxy.dispatch(UpdateLuminance(Try(e.target.value.toDouble).getOrElse(0.0)))
    }

    def setChroma(p: Props)(e: ReactEventI) = {
      p.proxy.dispatch(UpdateChroma(Try(e.target.value.toDouble).getOrElse(0.0)))
    }

    val width = "width".reactAttr
    val height = "height".reactAttr
    def render(p: Props) = {
      <.div(
        <.input(^.`type` := "range", ^.value := p.luminance, ^.min := 0, ^.max := 100, ^.step := 1, ^.onChange ==> setLuminance(p)),
        <.input(^.value := p.luminance, ^.onChange ==> setLuminance(p), ^.size := 3),
        <.br,
        <.input(^.`type` := "range", ^.value := p.chroma, ^.min := 0, ^.max := 128, ^.step := 1, ^.onChange ==> setChroma(p)), //TODO: change zoom
        <.input(^.value := p.chroma, ^.onChange ==> setChroma(p), ^.size := 3),
        <.br,
        <.canvas(
          ^.id := "color-area-canvas",
          width := 256,
          height := 256
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
