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

object ExportView {

  case class Props(proxy: ModelProxy[RootModel]) {
    def model = proxy.value
    def scheme = model.colorScheme
    def colors = model.colorScheme.colors
    def groups = model.colorScheme.groups
    def locked = model.locked
  }

  class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props) = {
      <.div(
        p.groups.toSeq.map {
          case (groupId, group) if group.size > 0 =>
            <.table(
              <.thead(<.tr(<.th())),
              <.tbody(
                group.zipWithIndex.map {
                  case (col, i) =>
                    <.tr(
                      <.td(
                        ^.textAlign := "center",
                        <.div(^.width := "20px", ^.height := "20px",
                          ^.backgroundColor := col.toCSS)
                      ),
                      <.td(
                        <.pre(
                          ^.fontFamily := "monospace",
                          ^.fontSize := "7px",
                          ^.margin := "0px",
                          "LAB(%3d,%3d,%3d)" format (col.lab.l.toInt, col.lab.a.toInt, col.lab.b.toInt)
                        )
                      ),
                      <.td(
                        <.input(
                          ^.`type` := "text",
                          ^.fontFamily := "monospace",
                          ^.size := 7,
                          ^.value := s"#${col.rgb.toHex}",
                          ^.onChange ==> { (e: ReactEventI) =>
                            val parsed = js.Dynamic.global.d3.rgb(e.target.value)
                            val rgb = RGB(parsed.r.asInstanceOf[Double], parsed.g.asInstanceOf[Double], parsed.b.asInstanceOf[Double])
                            console.log(rgb.toString)
                            p.proxy.dispatchCB(UpdateColor(ColorIndex(groupId, i), rgb))
                          }
                        )
                      )
                    )
                }
              )
            )
        }
      )
    }
  }

  private val component = ReactComponentB[Props]("ExportView")
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[RootModel]) = component(Props(proxy))
}
