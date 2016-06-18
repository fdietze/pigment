package rainbow

import scala.scalajs.js
import org.scalajs.dom._

import japgolly.scalajs.react._

object Main extends js.JSApp {
  def main {
    val colorArea = AppCircuit.connect(_.colorArea)(a => ColorAreaView(a))
    ReactDOM.render(colorArea, document.getElementById("container"))
  }
}
