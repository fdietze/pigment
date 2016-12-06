package pigment

import scala.scalajs.js
import org.scalajs.dom._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import diode._
import diode.react._

import vectory._

class SaveProcessor extends ActionProcessor[RootModel] {
  import boopickle.Default._
  import js.typedarray._
  import js.typedarray.TypedArrayBufferOps._
  import js.Dynamic.global
  import java.nio.ByteBuffer

  implicit class ByteBufferOpt(data: ByteBuffer) {
    def toArrayBuffer: ArrayBuffer = {
      if (data.hasTypedArray()) {
        // get relevant part of the underlying typed array
        data.typedArray().subarray(data.position, data.limit).buffer
      } else {
        // fall back to copying the data
        val tempBuffer = ByteBuffer.allocateDirect(data.remaining)
        val origPosition = data.position
        tempBuffer.put(data)
        data.position(origPosition)
        tempBuffer.typedArray().buffer
      }
    }
  }

  def process(dispatch: diode.Dispatcher, action: Any, next: Any => diode.ActionResult[RootModel], currentModel: RootModel): ActionResult[RootModel] = {
    val byteBuffer = Pickle.intoBytes(currentModel)
    val buffer = new Uint8Array(byteBuffer.toArrayBuffer, 0, byteBuffer.remaining)
    val s = new StringBuilder(buffer.length)
    for (i <- 0 until buffer.length) {
      s ++= global.String.fromCharCode(buffer(i)).asInstanceOf[String]
    }
    val encoded = window.btoa(s.result)
    window.location.hash = s"#${encoded}"

    // call the next processor
    next(action)
  }
}

object Main extends js.JSApp {
  def main {
    // ColorDistance.tests()
    val mainView = AppCircuit.connect(m => m)
    ReactDOM.render(mainView(m => MainView(m)), document.getElementById("container"))

    val saveProcessor = new SaveProcessor
    AppCircuit.addProcessor(saveProcessor)
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
          <.div(
            ^.width := "200px",
            ^.height := "200px",
            modelConnect(g => DistanceGraphView(g.value.colorScheme.graph, Vec2(200, 200), None))
          ),
          DistanceListView(m),
          MatrixView(m)
        // OptimizationView(m)
        )
      )
    }
    .build
}
