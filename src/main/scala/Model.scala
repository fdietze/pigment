package pigment

import diode._
import diode.react._

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

// Model
case class RootModel(palette: IndexedSeq[Color] = IndexedSeq.empty) {
  lazy val vertices = palette
  lazy val edges = vertices.combinations(2).map { case IndexedSeq(source, target) => DiEdge(source, target) }.toSeq
  // ColorDistance.ciede2000(source.color, target.color)
  lazy val graph = Graph.from[Color, DiEdge](vertices, edges)
}

case class Color(lab: LAB) {
  def l = lab.l
  def a = lab.a
  def b = lab.b
  def luminance = lab.luminance
  def chroma = lab.chroma
  def hue = lab.hue
  def toCSS = lab.toCSS
  def isGray = lab.isGray
  def withChroma(c: Double) = copy(lab = lab.withChroma(c))
}

// Actions
case class AddColor(color: Color) extends Action
case class UpdateColor(index: Int, color: Color) extends Action

// Circuit
object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  // define initial value for the application model
  def initialModel = RootModel(Array(
    Color(LAB(30, -75, 20)),
    Color(LAB(60, 15, 12)),
    Color(LAB(70, 100, 0)),
    Color(LAB(30, 0, 0)),
    Color(LAB(50, 1, 0))
  ))

  val paletteHandler = new ActionHandler(zoomRW(_.palette)((m, v) => m.copy(palette = v))) {
    override def handle = {
      case AddColor(c) => updated(value :+ c)
      case UpdateColor(index, c) => updated(value.updated(index, c))
    }
  }

  override val actionHandler = composeHandlers(paletteHandler)
}
