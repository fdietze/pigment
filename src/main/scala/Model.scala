package pigment

import diode._
import diode.react._

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

// Model
case class RootModel(colorScheme: ColorScheme) {
  lazy val groups = colorScheme.colors.zipWithIndex.groupBy { case (col, i) => col.group }
  lazy val vertices = colorScheme.colors
  lazy val edges = vertices.combinations(2).map { case IndexedSeq(source, target) => DiEdge(source, target) }.toSeq
  lazy val graph = Graph.from[Color, DiEdge](vertices, edges)
}

case class ColorScheme(colors: IndexedSeq[Color] = IndexedSeq.empty)

// Actions
case class AddColor(color: Color) extends Action
case class UpdateColor(index: Int, color: Color) extends Action
case class RemoveColor(index: Int) extends Action

// Circuit
object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  // define initial value for the application model
  def initialModel = RootModel(ColorScheme(Array(
    Color(LAB(20, -22, -15), group = 1),
    Color(LAB(86, 38, 71)),
    Color(LAB(75, 79, 0)),
    Color(LAB(75, -32, -57)),
    Color(LAB(75, -75, 27))
  )))

  val colorSchemeHandler = new ActionHandler(zoomRW(_.colorScheme.colors)((m, v) => m.copy(colorScheme = m.colorScheme.copy(colors = v)))) {
    override def handle = {
      case AddColor(c) => updated(value :+ c)
      case UpdateColor(index, c) => updated(value.updated(index, c))
      case RemoveColor(i) => updated(value.take(i) ++ value.drop(i + 1))
    }
  }

  override val actionHandler = composeHandlers(colorSchemeHandler)
}
