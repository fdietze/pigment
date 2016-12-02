package pigment

import scala.scalajs.js
import js.annotation._
import org.scalajs.dom._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import diode._
import diode.react._

import scala.util.Try

import CanvasHelpers._
import Math._

object LuminanceView extends ColorCanvasView {
  case class State(
    chroma: Double = 100,
    hueShift: Double = 0,
    dragState: DragState = DragState()
  ) {
    def withDragState(ds: DragState) = copy(dragState = ds)
  }

  class Backend(val $: BackendScope[Props, State]) extends BgFgBackend[State] {
    def colorX(color: Color, s: State) = ((color.lch.hue + s.hueShift + (PI * 2)) % (PI * 2)) / (PI * 2) * width
    def colorY(color: Color, s: State) = ((100 - color.lab.luminance) / 100) * height
    def colorAt(x: Double, y: Double, s: State) = {
      val hue = ((x / width) * (Math.PI * 2) - s.hueShift + (PI * 2)) % (PI * 2)
      val l = 100 - (y / height * 100)
      LCH(l, s.chroma, hue)
    }

    def drawForegroundOnCanvas(fgCanvas: raw.HTMLCanvasElement, p: Props, s: State) {
      import p._
      import s._

      val ctx = fgCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

      ctx.asInstanceOf[js.Dynamic].resetTransform() //TODO: add to scalajs.dom library
      ctx.clearRect(0, 0, fgCanvas.width, fgCanvas.height)

      for (color <- colors) {
        drawColor(ctx, color, color.lch.chroma / 128.0, s.chroma / 128.0, s)
      }
    }

    override def onMouseWheel(draggable: Option[Draggable], deltaY: Double, p: Props, s: State): Callback = {
      import p._
      import s._
      draggable match {
        case Some((groupId, i, col)) =>
          val col = groups(groupId)(i)
          val newCol = col.lch.copy(c = (col.lch.chroma - deltaY / 10.0).max(0).min(128))
          val newState = s.copy(chroma = newCol.chroma)
          proxy.dispatchCB(UpdateColor(ColorIndex(groupId, i), newCol)) >> $.setState(newState) >> drawBackground(newState)

        case None =>
          val newState = s.copy(hueShift = (hueShift + deltaY / 100.0) % (PI * 2))
          $.setState(newState) >> draw(p, newState)
      }
    }

    override def onDragStart(s: State, draggable: Draggable, x: Double, y: Double): Callback = {
      val (_, _, col) = draggable
      val newState = s.copy(
        chroma = col.lch.chroma,
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
        p.proxy.dispatchCB(UpdateColor(ColorIndex(groupId, i), newCol)) >>
        drawForeground(p, s)
    }

  }

  private val component = ReactComponentB[Props]("LuminanceView")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(c => c.backend.draw(c.props, c.state))
    .componentDidUpdate(c => c.$.backend.draw(c.currentProps, c.currentState))
    .shouldComponentUpdate(c => { c.$.backend.drawForeground(c.currentProps, c.currentState).runNow(); false })
    .build

  def apply(proxy: ModelProxy[RootModel]) = component(Props(proxy))
}
