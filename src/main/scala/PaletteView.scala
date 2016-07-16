package pigment

import scala.scalajs.js
import js.annotation._
import org.scalajs.dom._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import diode._
import diode.react._

import scala.util.Try

import Math._

object PaletteView {

  case class Props(proxy: ModelProxy[RootModel]) {
    def palette = proxy.value.palette
    def groups = proxy.value.groups
  }

  class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props) = {
      <.table(
        <.thead(<.tr(<.th())),
        <.tbody(
          p.groups.map {
            case (groupId, group) =>
              val avgLuminance = group.map(_._1.luminance).sum / group.size
              val avgChroma = group.map(_._1.chroma).sum / group.size
              val hues = group.map(_._1.hue)
              val huesOver = hues :+ (hues.min + 2 * PI)
              val freeHue = if (huesOver.size == 0)
                50
              else if (huesOver.size < 2)
                huesOver.head + PI
              else
                huesOver.sorted.sliding(2).maxBy { case Seq(a, b) => b - a }.sum / 2

              <.tr(
                group.map {
                  case (col, i) =>
                    val rgb = col.lab.toRGB
                    <.td(
                      ^.textAlign := "center",
                      <.div(^.width := "50px", ^.height := "50px",
                        ^.backgroundColor := col.toCSS),
                      <.pre(
                        ^.fontFamily := "monospace",
                        ^.fontSize := "8px",
                        ^.margin := "0px",
                        "%3d,%3d,%3d" format (col.l.toInt, col.a.toInt, col.b.toInt)
                      ),
                      <.pre(
                        ^.fontFamily := "monospace",
                        ^.fontSize := "8px",
                        ^.margin := "0px",
                        s"#${rgb.toHEX}"
                      ),
                      <.pre(
                        ^.fontFamily := "monospace",
                        ^.fontSize := "8px",
                        ^.margin := "0px",
                        "%3d,%3d,%3d" format (rgb.r, rgb.g, rgb.b)
                      ),
                      <.button(
                        ^.fontSize := "8px",
                        ^.onClick --> p.proxy.dispatch(RemoveColor(i)), "remove"
                      )
                    )
                },
                <.td(
                  ^.verticalAlign := "top",
                  <.button(^.onClick --> p.proxy.dispatch(AddColor(Color(LCH(avgLuminance, avgChroma, freeHue), groupId))), "+")
                )
              )
          }
        )
      )
    }
  }

  private val component = ReactComponentB[Props]("PaletteView")
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[RootModel]) = component(Props(proxy))
}