package pigment

import scala.scalajs.js
import org.scalajs.dom._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import diode._
import diode.react._

object Main extends js.JSApp {
  def main {
    val mainView = AppCircuit.connect(m => m)(m => MainView(m))
    ReactDOM.render(mainView, document.getElementById("container"))
  }

  val MainView = ReactComponentB[ModelProxy[RootModel]]("MainView")
    .render_P { m =>
      <.div(
        ^.display := "flex",
        ^.flex := "1 1 auto",
        ColorAreaView(m),
        MatrixPreview(m)
      )
    }
    .build
}
