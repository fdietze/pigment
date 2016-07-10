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
              <.td(
                <.div(^.width := 50, ^.height := 50, ^.backgroundColor := col.toCSS),
                <.div(
                  ^.fontFamily := "monospace",
                  ^.fontSize := "11px",
                  ^.textAlign := "center",
                  s"#${col.lab.toRGB.toHEX}"
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
