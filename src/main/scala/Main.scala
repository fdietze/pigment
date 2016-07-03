package pigment

import scala.scalajs.js
import org.scalajs.dom._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import diode._
import diode.react._

object Main extends js.JSApp {
  def main {
    // ColorDistance.tests()
    val mainView = AppCircuit.connect(m => m)
    ReactDOM.render(mainView(m => MainView(m)), document.getElementById("container"))
  }

  val MainView = ReactComponentB[ModelProxy[RootModel]]("MainView")
    .render_P { m =>
      <.div(
        ^.display := "flex",
        ^.flex := "1 1 auto",
        ChromaView(m),
        LuminanceView(m),
        DistanceListView(m),
        m.wrap(_.graph)(DistanceGraphView(_, 200, 200)),
        MatrixPreview(m)
      )
    }
    .build
}
