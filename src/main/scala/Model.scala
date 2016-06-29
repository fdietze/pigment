package pigment

import diode._
import diode.react._

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

// Model
case class RootModel(palette: IndexedSeq[LAB] = IndexedSeq.empty) {
  lazy val vertices = palette
  lazy val edges = vertices.combinations(2).map { case IndexedSeq(source, target) => DiEdge(source, target) }.toSeq
  // ColorDistance.ciede2000(source.color, target.color)
  lazy val graph = Graph.from[LAB, DiEdge](vertices, edges)
}

// case class Vertex(color: LAB)
// case class Edge[V](source: V, target: V, distance: Double) extends DiEdgeLikeIn[V]

// Actions
case class AddColor(color: LAB) extends Action
case class UpdateColor(index: Int, color: LAB) extends Action

// Circuit
object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  // define initial value for the application model
  def initialModel = RootModel(Array(
    LAB(30, -75, 20),
    LAB(60, 15, 12),
    LAB(70, 100, 0),
    LAB(30, 0, 0),
    LAB(50, 1, 0)
  ))

  val paletteHandler = new ActionHandler(zoomRW(_.palette)((m, v) => m.copy(palette = v))) {
    override def handle = {
      case AddColor(c) => updated(value :+ c)
      case UpdateColor(index, c) => updated(value.updated(index, c))
    }
  }

  override val actionHandler = composeHandlers(paletteHandler)
}
