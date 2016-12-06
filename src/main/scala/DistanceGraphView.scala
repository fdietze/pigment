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

import pharg._

import fdietze.scalajs.react.components.D3ForceLayout

object DistanceGraphView extends D3ForceLayout[Color, Edge[Color]] {
  val scale = 1.3
  val radius = 10.0

  override val reuseVertexCoordinatesOnUpdate = true
  override val panAndZoom = false
  override def linkDistance(p: Props, e: Edge[Color]) = ColorDistance.ciede2000(e.in.lab, e.out.lab) * scale //TODO: .rgb.lab to get perceived distance between rgb values
  override def charge(p: Props, v: Color) = 0
  override def linkStrength(p: Props, e: Edge[Color]) = 2
  override def styleVertices(p: Props, sel: VertexSelection) = {
    super.styleVertices(p, sel)
      .attr("r", radius)
      .style("fill", (d: D3Vertex) => d.v.toCSS)
  }
  override def styleEdges(p: Props, sel: EdgeSelection) = {
    super.styleEdges(p, sel)
      .style("stroke-dasharray", (d: D3Edge) => s"${radius * 2 - 2} 2")
      .style("stroke-dashoffset", (d: D3Edge) => s"${-2 - radius}")
  }
}
