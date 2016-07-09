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

object ChromaView extends ColorCanvasView {
  case class State(
    luminance: Double = 100,
    dragState: DragState = DragState()
  ) {
    def withDragState(ds: DragState) = copy(dragState = ds)
  }

  type Draggable = (Color, Int)

  class Backend(val $: BackendScope[Props, State]) extends BgFgBackend[State] {
    def colorX(color: Color, s: State) = (if (color.isGray) color.hueHint else color.hue) / (Math.PI * 2) * width
    def colorY(color: Color, s: State) = ((128 - color.chroma) / 128) * height
    def colorAt(x: Double, y: Double, s: State) = {
      val hue = (x / width) * (Math.PI * 2)
      val chroma = 128 - (y / height * 128)
      val a = Math.cos(hue) * chroma
      val b = Math.sin(hue) * chroma
      LAB(s.luminance, a, b, hueHint = hue)
    }

    override def hitDraggable(x: Double, y: Double, p: Props, s: State): Option[(Color, Int)] = {
      import p._
      import s._
      palette.zipWithIndex.find {
        case (col, i) =>
          val cx = colorX(col, s)
          val cy = colorY(col, s)
          val r = colorRadius + colorBorder / 2.0
          (x - cx) * (x - cx) + (y - cy) * (y - cy) <= r * r
      }
    }

    def drawForeground(p: Props, s: State) = Callback {
      import p._
      import s._

      val ctx = fgCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

      ctx.asInstanceOf[js.Dynamic].resetTransform() //TODO: add to scalajs.dom library
      ctx.clearRect(0, 0, fgCanvas.width, fgCanvas.height)

      val colors = dragState.dragging match {
        case Some((col, i)) => palette.updated(i, col)
        case None => palette
      }
      for (color <- colors) {
        drawColor(ctx, color, color.luminance / 100.0, s.luminance / 100.0, s)
      }
    }

    override def onMouseWheel(draggable: Option[Draggable], deltaY: Double, p: Props, s: State): Callback = {
      import p._
      import s._
      draggable match {
        case Some((col, i)) =>
          val col = palette(i)
          val newCol = col.copy(lab = col.lab.copy(l = (col.luminance - deltaY / 10).max(0).min(100)))
          val newState = s.copy(luminance = newCol.luminance)
          proxy.dispatch(UpdateColor(i, newCol)) >> $.setState(newState) >> drawBackground(newState)

        case None =>
          Callback.empty
      }
    }

    override def onDragStart(s: State, draggable: Draggable, x: Double, y: Double): Callback = {
      val (col, _) = draggable
      val newState = s.copy(
        luminance = col.luminance,
        DragState(
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
      val (col, i) = draggable
      val newCol = col.copy(lab = colorAt(x, y, s))
      val newState = s.copy(dragState = s.dragState.copy(dragging = Some((newCol, i))))
      $.setState(newState) >>
        p.proxy.dispatch(UpdateColor(i, newCol)) >>
        drawForeground(p, s)
    }

  }

  private val component = ReactComponentB[Props]("ChromaView")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(c => c.backend.draw(c.props, c.state))
    .componentDidUpdate(c => c.$.backend.draw(c.currentProps, c.currentState))
    .shouldComponentUpdate(c => { c.$.backend.drawForeground(c.currentProps, c.currentState).runNow(); false })
    .build

  def apply(proxy: ModelProxy[RootModel]) = component(Props(proxy))
}
