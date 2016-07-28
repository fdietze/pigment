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

  val modelConnect = AppCircuit.connect(m => m)

  val MainView = ReactComponentB[ModelProxy[RootModel]]("MainView")
    .render_P { m =>
      <.div(
        PaletteView(m),
        <.div(
          ^.display := "flex",
          ^.flex := "1 1 auto",
          ^.flexWrap := "wrap",
          ChromaCircleView(m),
          ChromaView(m),
          LuminanceView(m)
        ),
        <.div(
          ^.display := "flex",
          ^.flex := "1 1 auto",
          ^.flexWrap := "wrap",
          modelConnect(g => DistanceGraphView(g.value.graph, 200, 200)),
          DistanceListView(m),
          MatrixView(m)
        )
      )
    }
    .build
}
