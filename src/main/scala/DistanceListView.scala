package pigment

import scala.scalajs.js
import js.annotation._
import org.scalajs.dom._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import diode._
import diode.react._

import scala.util.Try

object DistanceListView {

  case class Props(proxy: ModelProxy[RootModel]) {
    def colors = proxy.value.colorScheme.colors
    def groups = proxy.value.colorScheme.groups
  }

  class Backend($: BackendScope[Props, Unit]) {

    def pair(a: LAB, b: LAB) = {
      <.div(
        ^.width := "40px",
        ^.height := "20px",
        ^.position := "relative",
        <.div(
          ^.width := "20px",
          ^.height := "20px",
          ^.position := "absolute",
          ^.top := "0px",
          ^.left := "0px",
          ^.backgroundColor := a.toCSS
        ),
        <.span(
          ^.width := "20px",
          ^.height := "20px",
          ^.position := "absolute",
          ^.top := "0px",
          ^.left := "20px",
          ^.backgroundColor := b.toCSS
        )
      )
    }
    def render(p: Props) = {
      def pairs(xs: Seq[Color]): Seq[(Color, Color)] = (for (i <- xs.indices; j <- xs.indices if i < j) yield { (xs(i), xs(j)) })
      def pairs2(xs: Seq[Color], ys: Seq[Color]): Seq[(Color, Color)] = (for (x <- xs; y <- ys) yield { (x, y) })

      val intraGroupPairs: Seq[Seq[(Color, Color)]] = p.groups.values.toSeq map pairs
      val interGroupPairs: Seq[Seq[(Color, Color)]] = p.groups.values.toSeq.combinations(2).toSeq.map { case Seq(xs, ys) => pairs2(xs, ys) }
      val groupPairs = (intraGroupPairs ++ interGroupPairs).map { pairs =>
        val sorted = pairs.sortBy { case (a, b) => ColorDistance.ciede2000(a.lab, b.lab) }
        val limited = if (sorted.size <= 10) sorted else sorted.take(5) ++ sorted.takeRight(5)
        limited
      }

      // println(groups.mkString("\n"))
      <.div(
        ^.display := "flex",
        ^.flexWrap := "wrap",
        groupPairs.map { pairs =>
          <.div(
            pairs.map {
              case (a, b) =>
                pair(a.lab, b.lab)
            }
          )
        }
      )
    }
  }

  private val component = ReactComponentB[Props]("DistanceListView")
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[RootModel]) = component(Props(proxy))
}
