package pigment

import scala.scalajs.js
import js.annotation._
import org.scalajs.dom._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import diode._
import diode.react._

import scala.util.Try

object PaletteView {

  case class Props(proxy: ModelProxy[RootModel]) {
    def palette = proxy.value.palette
  }

  class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props) = {
      <.table(
        <.thead(<.tr(<.th())),
        <.tbody(
          <.tr(
            p.palette map { col =>
              val rgb = col.lab.toRGB
              <.td(
                <.div(^.width := "50px", ^.height := "50px",
                  ^.backgroundColor := col.toCSS),
                <.pre(
                  ^.fontFamily := "monospace",
                  ^.fontSize := "11px",
                  ^.textAlign := "center",
                  ^.margin := "0px",
                  s"#${rgb.toHEX}"
                ),
                <.pre(
                  ^.fontFamily := "monospace",
                  ^.fontSize := "8px",
                  ^.textAlign := "center",
                  ^.margin := "0px",
                  "%3d,%3d,%3d" format (rgb.r, rgb.g, rgb.b)
                )
              )
            }
          )
        )
      )
    }
  }

  private val component = ReactComponentB[Props]("PaletteView")
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[RootModel]) = component(Props(proxy))
}
