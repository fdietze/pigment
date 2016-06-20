package pigment

import diode._
import diode.react._

// Model
case class RootModel(colorArea: ColorArea, palette: IndexedSeq[LAB] = IndexedSeq.empty)
case class ColorArea(luminance: Double, chroma: Double)

// Actions
case class UpdateLuminance(luminance: Double)
case class UpdateChroma(chroma: Double)
case class AddColor(color: LAB)
case class UpdateColor(index: Int, color: LAB)

// Circuit
object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  // define initial value for the application model
  def initialModel = RootModel(ColorArea(70, 100), Array(
    LAB(30, -75, 20),
    LAB(60, 15, 12),
    LAB(70, 100, 0),
    LAB(30, 0, 0),
    LAB(50, 1, 0)
  ))

  val colorAreaHandler = new ActionHandler(zoomRW(_.colorArea)((m, v) => m.copy(colorArea = v))) {
    override def handle = {
      case UpdateLuminance(l) => updated(value.copy(luminance = l))
      case UpdateChroma(l) => updated(value.copy(chroma = l))
    }
  }

  val paletteHandler = new ActionHandler(zoomRW(_.palette)((m, v) => m.copy(palette = v))) {
    override def handle = {
      case AddColor(c) => updated(value :+ c)
      case UpdateColor(index, c) => updated(value.updated(index, c))
    }
  }

  override val actionHandler = composeHandlers(colorAreaHandler, paletteHandler)
}
