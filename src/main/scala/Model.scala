package pigment

import diode._
import diode.react._

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

// Model
case class RootModel(colorScheme: ColorScheme)

case class ColorScheme(groups: Map[Int, IndexedSeq[Color]] = Map.empty.withDefaultValue(IndexedSeq.empty)) {
  lazy val colors = groups.values.flatten.toSeq

  private def vertices = colors
  private def edges = vertices.combinations(2).map { case Seq(source, target) => DiEdge(source, target) }.toSeq
  lazy val graph = Graph.from[Color, DiEdge](vertices, edges)
}

// Actions
case class AddColor(group: Int, color: Color) extends Action
case class UpdateColor(group: Int, index: Int, color: Color) extends Action
case class RemoveColor(group: Int, index: Int) extends Action

// Circuit
object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  // define initial value for the application model
  def initialModel = RootModel(
    ColorScheme(Map(
      0 -> Array(
        LAB(20, -22, -15)
      ),
      1 -> Array(
        LAB(86, 38, 71),
        LAB(75, 79, 0),
        LAB(75, -32, -57),
        LAB(75, -75, 27)
      )
    ))
  )

  val colorSchemeHandler = new ActionHandler(zoomRW(_.colorScheme.groups)((m, v) => m.copy(colorScheme = m.colorScheme.copy(groups = v)))) {
    override def handle = {
      case AddColor(group, color) => updated(value.updated(group, value(group) :+ color))
      case UpdateColor(group, index, color) => updated(value.updated(group, value(group).updated(index, color)))
      case RemoveColor(group, index) => updated(value.updated(group, value(group).take(index) ++ value(group).drop(index + 1)))
    }
  }

  override val actionHandler = composeHandlers(colorSchemeHandler)
}
