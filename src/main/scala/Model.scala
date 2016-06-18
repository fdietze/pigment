package rainbow

import diode._
import diode.react._

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  // define initial value for the application model
  def initialModel = RootModel(ColorArea(70))

  val colorAreaHandler = new ActionHandler(zoomRW(_.colorArea)((m, v) => m.copy(colorArea = v))) {
    override def handle = {
      case UpdateLuminance(l) =>
        updated(value.copy(luminance = l))
    }
  }

  override val actionHandler = composeHandlers(colorAreaHandler)
}

case class RootModel(colorArea: ColorArea)
case class ColorArea(luminance: Int)
case class UpdateLuminance(luminance: Int)
