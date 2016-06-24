package pigment

import scala.scalajs.js
import js.annotation._
import js.JSConverters._
import org.scalajs.dom._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import org.singlespaced.d3js
import org.singlespaced.d3js._
import org.singlespaced.d3js.d3
import org.singlespaced.d3js.Ops._
import org.singlespaced.d3js.Link
import org.singlespaced.d3js.forceModule.Force

import diode._
import diode.react._

import scala.util.Try

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

object DistanceD3GraphView extends graphView.GraphView[Vertex, DiEdge] {
  override def linkDistance(e: DiEdge[Vertex]) = ColorDistance.ciede2000(e.source.color, e.target.color) * 2
  override def linkStrength(e: DiEdge[Vertex]) = 5
  override def styleVertices(sel: VertexSelection) = {
    super.styleVertices(sel)
      .attr("r", 10.0)
      .style("fill", (d: D3Vertex) => d.v.color.toCSS)
  }
}

object DistanceGraphView {

  case class Props(proxy: ModelProxy[RootModel]) {
    def palette = proxy.value.palette
  }

  class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props) = {
      p.proxy.wrap(_.graph)(DistanceD3GraphView(_, 400, 400))
    }
  }

  private val component = ReactComponentB[Props]("DistanceListView")
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[RootModel]) = component(Props(proxy))
}
