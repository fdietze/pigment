package pigment

import scala.scalajs.js
import js.annotation._
import org.scalajs.dom._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import diode._
import diode.react._

import scala.util.Try

import Math._

object ChromaCircleView extends ColorCanvasView {
  val chromaCircleRadius = width * 0.4
  val chromaCircleBorder = 2.0

  case class State(
    chroma: Double = 100,
    luminance: Double = 70,
    dragState: DragState = DragState()
  ) {
    def zoom = chromaCircleRadius / chroma
    def withDragState(ds: DragState) = copy(dragState = ds)
  }

  class Backend(val $: BackendScope[Props, State]) extends BgFgBackend[State] {
    def colorX(color: Color, s: State) = (if (color.isGray) 0 else s.zoom * color.lab.a) + width / 2
    def colorY(color: Color, s: State) = (if (color.isGray) 0 else s.zoom * color.lab.b) + height / 2
    def colorAt(x: Double, y: Double, s: State): LAB = {
      val a = (x - width / 2) / s.zoom
      val b = (y - height / 2) / s.zoom
      val l = s.luminance
      def hue = ((PI * 2) + atan2((y - height / 2), (x - width / 2))) % (PI * 2)
      LAB(l, a, b, hueHint = hue)
    }

    def drawForegroundOnCanvas(fgCanvas: raw.HTMLCanvasElement, p: Props, s: State) {
      import p._
      import s._
      import CanvasHelpers._

      val ctx = fgCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

      ctx.asInstanceOf[js.Dynamic].resetTransform() //TODO: add to scalajs.dom library
      ctx.clearRect(0, 0, fgCanvas.width, fgCanvas.height)

      // circle on chroma:
      percentCirle(ctx, width / 2, height / 2, chromaCircleRadius, width = chromaCircleBorder, luminance / 100.0)

      if (chroma > 0) {
        for (color <- colors) {
          drawColor(ctx, color, color.lab.luminance / 100.0, luminance / 100.0, s)
        }
      } else { // chroma is zero => infinite zoom, only draw gray colors
        for (color <- colors if color.isGray) {
          drawColor(ctx, color, color.lab.luminance / 100.0, luminance / 100.0, s)
        }
      }
    }

    override def onDragStart(s: State, draggable: Draggable, x: Double, y: Double): Callback = {
      val (_, _, col) = draggable
      val newState = s.copy(
        luminance = col.lab.luminance,
        dragState = DragState(
          dragging = Some(draggable),
          startX = x,
          startY = y,
          offsetX = colorX(col, s) - x,
          offsetY = colorY(col, s) - y
        )
      )
      $.setState(newState) >> drawBackground(newState)
    }

    override def onDrag(draggable: Draggable, x: Double, y: Double, p: Props, s: State): Callback = {
      val (groupId, i, _) = draggable
      val newCol = colorAt(x, y, s)
      val newState = s.copy(dragState = s.dragState.copy(dragging = Some((groupId, i, newCol))))
      $.setState(newState) >>
        p.proxy.dispatch(UpdateColor(groupId, i, newCol)) >>
        drawForeground(p, s)
    }

    override def onMouseWheel(draggable: Option[Draggable], deltaY: Double, p: Props, s: State): Callback = {
      import p._
      import s._
      draggable match {
        case Some((groupId, i, col)) =>
          val col = groups(groupId)(i)
          val newCol = col.lab.copy(l = (col.lab.l - deltaY / 10.0).max(0).min(100))
          val newState = s.copy(luminance = newCol.l)
          proxy.dispatch(UpdateColor(groupId, i, newCol)) >> $.setState(newState) >> drawBackground(newState)

        case None =>
          val newState = s.copy(chroma = (chroma + deltaY / 10.0).max(0).min(128))
          $.setState(newState) >> drawBackground(newState)
      }
    }
  }

  private val component = ReactComponentB[Props]("ChromaCircleView")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(c => c.backend.draw(c.props, c.state))
    .componentDidUpdate(c => c.$.backend.draw(c.currentProps, c.currentState))
    .shouldComponentUpdate(c => { c.$.backend.drawForeground(c.currentProps, c.currentState).runNow(); false })
    .build

  def apply(proxy: ModelProxy[RootModel]) = component(Props(proxy))
}
