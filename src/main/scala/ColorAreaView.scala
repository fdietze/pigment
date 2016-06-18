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
  case class Props(proxy: ModelProxy[ColorArea])
  class Backend($: BackendScope[Props, Unit]) {
    def draw(p: Props) = Callback {
      val start = System.currentTimeMillis
      val canvas = document.getElementById("color-area-canvas").asInstanceOf[html.Canvas]
      val ctx = canvas.getContext("2d")
      val imageData = ctx.createImageData(canvas.width, canvas.height)

      val width = canvas.width.toDouble
      val height = canvas.height.toDouble
      val data = imageData.data.asInstanceOf[js.Array[Int]]
      // for (i <- 3 until (canvas.width * canvas.height) * 4)
      //   data(i) = 255
      for (x <- 0 until canvas.width; y <- 0 until canvas.height) {
        val a = ((x / width) * 2 - 1) * 128
        val b = ((y / height) * 2 - 1) * 128
        val i = (y * canvas.width + x) * 4
        // val color = Lab(p.proxy.value.luminance, a.toInt, b.toInt).rgb()
        val color = ColorConversion.labToRGB(p.proxy.value.luminance, a.toInt, b.toInt)
        data(i + 0) = color(0).toInt.max(0).min(255)
        data(i + 1) = color(1).toInt.max(0).min(255)
        data(i + 2) = color(2).toInt.max(0).min(255)
        data(i + 3) = 255
      }
      ctx.putImageData(imageData, 0, 0)

      val duration = System.currentTimeMillis - start
      println(s"${duration}ms")

      // ctx.globalCompositeOperation = "destination-in"
      ctx.strokeStyle = "rgba(255, 255, 255, 0.4)"
      ctx.lineWidth = width / 256 * 2
      ctx.arc(width / 2, height / 2, width / 256 * 100, 0, Math.PI * 2)
      ctx.stroke()
    }
    def setLuminance(p: Props)(e: ReactEventI) = {
      p.proxy.dispatch(UpdateLuminance(Try(e.target.value.toInt).getOrElse(0)))
    }

    val width = "width".reactAttr
    val height = "height".reactAttr
    def render(p: Props) = {
      <.div(
        <.input(^.`type` := "range", ^.value := p.proxy.value.luminance, ^.min := 0, ^.max := 100, ^.onChange ==> setLuminance(p)),
        <.input(^.value := p.proxy.value.luminance, ^.onChange ==> setLuminance(p), ^.size := 3),
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
    .componentDidMount(c => c.backend.draw(c.props))
    // .componentDidUpdate(c => Callback { println("redraw...") })
    .componentDidUpdate(c => c.$.backend.draw(c.currentProps))
    .build

  def apply(proxy: ModelProxy[ColorArea]) = component(Props(proxy))
}
