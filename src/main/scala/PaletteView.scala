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
    def model = proxy.value
    def scheme = model.colorScheme
    def colors = model.colorScheme.colors
    def groups = model.colorScheme.groups
    def locked = model.locked
  }

  class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props) = {
      <.div(
        <.table(
          <.thead(<.tr(<.th())),
          <.tbody(
            <.tr(
              // <.td(
              //   p.groups.keys.max + 1
              // ),
              <.td(
                <.button(^.onClick --> p.proxy.dispatchCB(AddColor(p.scheme.nextGroupId + 1, LCH(50, 50, 50))), "+")
              )
            ),
            p.groups.toSeq.map {
              case (groupId, group) if group.size > 0 =>
                val avgLuminance = group.map(_.lab.luminance).sum / group.size
                val avgChroma = group.map(_.lch.chroma).sum / group.size
                val hues = group.map(_.lch.hue)
                val huesOver = hues :+ (hues.min + 2 * PI)
                val freeHue = if (huesOver.size == 0)
                  50
                else if (huesOver.size < 2)
                  huesOver.head + PI
                else
                  huesOver.sorted.sliding(2).maxBy { case Seq(a, b) => b - a }.sum / 2

                <.tr(
                  // <.td(groupId),
                  group.zipWithIndex.map {
                    case (col, i) =>
                      val rgb = col.rgb
                      <.td(
                        ^.textAlign := "center",
                        <.div(^.width := "50px", ^.height := "50px",
                          ^.backgroundColor := col.toCSS),
                        <.pre(
                          ^.fontFamily := "monospace",
                          ^.fontSize := "8px",
                          ^.margin := "0px",
                          "%3d,%3d,%3d" format (col.lab.l.toInt, col.lab.a.toInt, col.lab.b.toInt)
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
                          ^.onClick --> p.proxy.dispatchCB(RemoveColor(ColorIndex(groupId, i))), "remove"
                        )
                      // <.br(),
                      // if (p.locked(ColorIndex(groupId, i)))
                      //   <.button(
                      //   ^.fontSize := "6px",
                      //   ^.onClick --> p.proxy.dispatchCB(RemoveLock(ColorIndex(groupId, i))), "unlock"
                      // )
                      // else
                      //   <.button(
                      //     ^.fontSize := "6px",
                      //     ^.onClick --> p.proxy.dispatchCB(SetLock(ColorIndex(groupId, i))), "lock"
                      //   )
                      )
                  },
                  <.td(
                    ^.verticalAlign := "top",
                    <.button(^.onClick --> p.proxy.dispatchCB(AddColor(groupId, LCH(avgLuminance, avgChroma, freeHue))), "+")
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
