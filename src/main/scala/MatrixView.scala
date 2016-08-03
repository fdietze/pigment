package pigment

import scala.scalajs.js
import js.annotation._
import org.scalajs.dom._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import diode._
import diode.react._

import scala.util.Try

object MatrixView {

  case class Props(proxy: ModelProxy[RootModel]) {
    def colors = proxy.value.colorScheme.colors
  }

  class Backend($: BackendScope[Props, Unit]) {

    def renderCell(a: LAB, b: LAB) = {
      <.div(
        ^.width := "40px",
        ^.height := "40px",
        ^.position := "relative",
        <.div(
          ^.width := "20px",
          ^.height := "20px",
          ^.position := "absolute",
          ^.top := "0px",
          ^.left := "0px",
          ^.backgroundColor := a.toCSS
        ),
        <.div(
          ^.width := "20px",
          ^.height := "20px",
          ^.position := "absolute",
          ^.top := "0px",
          ^.left := "20px",
          ^.backgroundColor := b.toCSS
        ),
        <.div(
          ^.width := "40px",
          ^.height := "18px",
          ^.paddingTop := "2px",
          ^.position := "absolute",
          ^.top := "20px",
          ^.left := "0px",
          ^.backgroundColor := b.toCSS,
          ^.fontFamily := "Monospace",
          ^.fontSize := "10px",
          ^.color := a.toCSS,
          "h0i7ka"
        )
      )
    }
    def render(p: Props) = {
      import p._
      <.table(
        ^.borderSpacing := "0px",
        // <.thead(<.tr(<.th())),
        <.tbody(
          colors map { a =>
            <.tr(
              colors map { b =>
                <.td(
                  ^.padding := "0px",
                  renderCell(a.lab, b.lab)
                )
              }
            )
          }
        )
      )
    }
  }

  private val component = ReactComponentB[Props]("MatrixView")
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[RootModel]) = component(Props(proxy))
}
