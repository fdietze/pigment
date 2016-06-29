package pigment

import scala.scalajs.js
import js.annotation._
import org.scalajs.dom._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import diode._
import diode.react._

@JSExportAll
trait TestTrait {
  val y = 2
}

class D3Vertex() extends TestTrait

object Main extends js.JSApp {
  def main {
    val a = new D3Vertex()
    println("before TypeError")
    println(a) // works fine with fastOptJS, but with fullOptJS throws: Uncaught TypeError: this.y is not a function
    println("after TypeError")
  }

  val mainView = AppCircuit.connect(m => m)

  case class Props()
  case class State()
  private val component1 = ReactComponentB[Props]("SmartComponent")
    .initialState(State())
    .render(_ => <.div(<.div(^.width := s"50px")))
    .build

  case class Props2(proxy: ModelProxy[RootModel])
  private val component = ReactComponentB[Props2]("DistanceListView")
    .render_P(p => p.proxy.wrap(_.vertices)(_ => component1(Props())))
    .build

  val MainView = ReactComponentB[ModelProxy[RootModel]]("MainView")
    .render_P { m =>
      <.div(
        component(Props2(m))
      )
    }
    .build
}

case class RootModel(palette: IndexedSeq[LAB] = IndexedSeq.empty) {
  def vertices = palette.map(Vertex(_))
}

case class Vertex(color: LAB)

case class AddColor(color: LAB) extends Action

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = RootModel(Array(LAB()))

  val paletteHandler = new ActionHandler(zoomRW(_.palette)((m, v) => m.copy(palette = v))) {
    override def handle = {
      case AddColor(c) => updated(value :+ c)
    }
  }

  override val actionHandler = composeHandlers(paletteHandler)
}

case class LAB()

@JSExport
object ColorConversion {

  @JSExport
  @inline final def labToRGB(l: Double, a: Double, b: Double): Array[Int] = {
    Array(1)
  }
}
