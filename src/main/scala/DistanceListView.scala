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
    def palette = proxy.value.palette
    def groups = proxy.value.groups
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
      val groups = p.groups.values ++ p.groups.values.toSeq.combinations(2).map(_.flatten)
      val groupPairs = groups.map { group =>
        val colors = group.map(_._1)
        val pairs = (for (i <- 0 until colors.size; j <- 0 until colors.size if i < j) yield { (colors(i), colors(j)) })
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
