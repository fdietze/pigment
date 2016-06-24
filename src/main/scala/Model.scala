package pigment

import diode._
import diode.react._

// Model
case class RootModel(palette: IndexedSeq[LAB] = IndexedSeq.empty)

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
