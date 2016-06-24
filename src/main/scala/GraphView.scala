package graphView

import collection.mutable
import scala.scalajs.js
import scala.scalajs.js.JSApp
import org.scalajs.dom
import org.scalajs.dom._

import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom.ext.KeyCode

import scala.scalajs.js.Dynamic.global
import scala.annotation.meta.field
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import diode._
import diode.ActionResult.ModelUpdate
import diode.react._
import org.singlespaced.d3js
import org.singlespaced.d3js.d3
import org.singlespaced.d3js.Ops._
import org.singlespaced.d3js.Link
import org.singlespaced.d3js.forceModule.Force

import js.JSConverters._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

trait GraphView[V, E[X] <: DiEdgeLikeIn[X]] {

  class D3Vertex(
    val v: V
  ) extends d3js.forceModule.Node

  class D3Edge(
    val e: E[V],
    @(JSExport @field) var source: D3Vertex,
    @(JSExport @field) var target: D3Vertex
  ) extends d3js.Link[D3Vertex]

  type VertexSelection = d3js.selection.Update[D3Vertex]
  type EdgeSelection = d3js.selection.Update[D3Edge]

  case class Props(
    proxy: ModelProxy[Graph[V, E]],
    width: Double,
    height: Double
  ) {
    def graph = proxy.value
  }

  case class State(
    force: Force[D3Vertex, D3Edge],
    var vertexData: js.Array[D3Vertex],
    var edgeData: js.Array[D3Edge],
    var vertexSel: js.UndefOr[VertexSelection] = js.undefined,
    var edgeSel: js.UndefOr[EdgeSelection] = js.undefined
  )

  def charge(v: V): Double = -30
  def linkDistance(v: E[V]): Double = 20
  def linkStrength(v: E[V]): Double = 3

  private def initialState(p: Props): State = {
    import p._
    State(
      force = d3.layout.force[D3Vertex, D3Edge]()
        .charge((d: D3Vertex, _: Double) => charge(d.v))
        .linkDistance((d: D3Edge, _: Double) => linkDistance(d.e))
        .linkStrength((d: D3Edge, _: Double) => linkStrength(d.e))
        .size((width, height)),
      vertexData = js.Array(),
      edgeData = js.Array()
    )
  }

  val vertexGroupRef = Ref[raw.HTMLCanvasElement]("vertices")
  val edgeGroupRef = Ref[raw.HTMLCanvasElement]("edges")

  def renderVertexArea(p: Props, s: State) = {
    import p._
    <.svg.svg(
      ^.width := s"${width}px",
      ^.height := s"${height}px",
      ^.position := "absolute",
      ^.ref := "vertices"
    )
  }

  def renderEdgeArea(p: Props, s: State) = {
    import p._
    <.svg.svg(
      ^.width := s"${width}px",
      ^.height := s"${height}px",
      ^.position := "absolute",
      ^.ref := "edges"
    )
  }

  def render(p: Props, s: State) = {
    import p._
    <.div(
      <.div(
        ^.width := s"${width}px",
        ^.height := s"${height}px",
        ^.position := "relative",
        ^.border := "1px solid #DDD",
        renderEdgeArea(p, s),
        renderVertexArea(p, s)
      )
    )
  }

  def vertexTag = "circle"
  def lineTag = "circle"

  def positionVertices(sel: VertexSelection): VertexSelection = {
    sel
      .attr("cx", (d: D3Vertex) => d.x)
      .attr("cy", (d: D3Vertex) => d.y)
  }

  def positionEdges(sel: EdgeSelection): EdgeSelection = {
    sel
      .attr("x1", (d: D3Edge) => d.source.x)
      .attr("y1", (d: D3Edge) => d.source.y)
      .attr("x2", (d: D3Edge) => d.target.x)
      .attr("y2", (d: D3Edge) => d.target.y)
  }

  def styleVertices(sel: VertexSelection): VertexSelection = {
    sel
      .attr("r", (d: D3Vertex) => 5)
      .style("fill", "steelblue")
  }

  class Backend($: BackendScope[Props, State]) {

    def updateData(p: Props, s: State) = Callback {
      import p._
      import s._

      val oldVertices = vertexData.map(d => d.v -> d).toMap
      val newVertices = p.graph.nodes.map((v: Graph[V, E]#NodeT) => v.value).map { v =>
        oldVertices.get(v) match {
          case Some(d3v) => d3v
          case None => new D3Vertex(v)
        }
      }.toJSArray

      val vertexMap = newVertices.map(d => d.v -> d).toMap
      val oldEdges = edgeData.map(d => (d.e -> d)).toMap
      val newEdges = p.graph.edges.map { e_inner: Graph[V, E]#EdgeT =>
        val e = e_inner.toOuter
        oldEdges.get(e) match {
          case Some(d3e) =>
            d3e
          case None =>
            new D3Edge(e, vertexMap(e.source), vertexMap(e.target))
        }
      }.toJSArray

      vertexData = newVertices
      edgeData = newEdges
    }

    def updateVisualization(p: Props, s: State) = Callback {
      import p._
      import s._
      lazy val domVerticesSel = d3.select(vertexGroupRef($).get).selectAll(vertexTag)
      lazy val domEdgesSel = d3.select(edgeGroupRef($).get).selectAll("line")

      vertexSel = vertexSel.getOrElse(domVerticesSel).data(vertexData)
      for (v <- vertexSel) {
        v.exit().remove()
        v.enter().append(vertexTag)
        styleVertices(v)
        positionVertices(v)
      }

      edgeSel = edgeSel.getOrElse(domEdgesSel).data(edgeData)
      edgeSel.get.exit().remove()
      edgeSel.get.enter().append("line")
      positionEdges(edgeSel.get)
        .style("stroke", "#666")
        .style("stroke-width", 2)

      force.nodes(vertexData).links(edgeData)
      force.start()
    }

    def update(p: Props, s: State): Callback = updateData(p, s) >> updateVisualization(p, s)

    def registerTick(s: State) = Callback {
      import s._

      force.on("tick", (e: Event) => {
        positionVertices(vertexSel.get)
        positionEdges(edgeSel.get)
        ()
      })
    }

    def stopForce(s: State) = Callback {
      s.force.stop()
    }

  }

  private val component = ReactComponentB[Props]("SmartComponent")
    .initialState_P(initialState)
    .backend(new Backend(_))
    .renderPS((_, p, s) => render(p, s))
    .componentDidMount(c => c.backend.update(c.props, c.state) >> c.backend.registerTick(c.state))
    .shouldComponentUpdate(c => { c.$.backend.update(c.currentProps, c.currentState).runNow(); false }) // let d3 update, instead of react
    .componentWillUnmount(c => c.backend.stopForce(c.state))
    .build

  def apply(proxy: ModelProxy[Graph[V, E]], width: Double, height: Double) = component(Props(proxy, width, height))
}
