package pigment

import diode._
import diode.react._

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

// Model
case class RootModel(palette: IndexedSeq[Color] = IndexedSeq.empty) {
  lazy val groups = palette.zipWithIndex.groupBy { case (col, i) => col.group }
  lazy val vertices = palette
  lazy val edges = vertices.combinations(2).map { case IndexedSeq(source, target) => DiEdge(source, target) }.toSeq
  lazy val graph = Graph.from[Color, DiEdge](vertices, edges)
}

case class Color(lab: LAB, group: Int = 0) {
  def l = lab.l
  def a = lab.a
  def b = lab.b
  def luminance = lab.luminance
  def chroma = lab.chroma
  def hue = lab.hue
  def hueHint = lab.hueHint
  def toCSS = lab.toCSS
  def isGray = lab.isGray
  def withChroma(c: Double) = copy(lab = lab.withChroma(c))
}

// Actions
case class AddColor(color: Color) extends Action
case class UpdateColor(index: Int, color: Color) extends Action
case class RemoveColor(index: Int) extends Action

// Circuit
object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  // define initial value for the application model
  def initialModel = RootModel(Array(
    Color(LAB(20, -22, -15), group = 1),
    Color(LAB(86, 38, 71)),
    Color(LAB(75, 79, 0)),
    Color(LAB(75, -32, -57)),
    Color(LAB(75, -75, 27))
  ))

  val paletteHandler = new ActionHandler(zoomRW(_.palette)((m, v) => m.copy(palette = v))) {
    override def handle = {
      case AddColor(c) => updated(value :+ c)
      case UpdateColor(index, c) => updated(value.updated(index, c))
      case RemoveColor(i) => updated(value.take(i) ++ value.drop(i + 1))
    }
  }

  override val actionHandler = composeHandlers(paletteHandler)
}
