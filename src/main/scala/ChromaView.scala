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

object ChromaView extends ColorCanvasView {
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

  type Draggable = (Color, Int)

  class Backend(val $: BackendScope[Props, State]) extends BgFgBackend[State] {
    def colorX(color: Color, s: State) = (if (color.isGray) 0 else s.zoom * color.a) + width / 2
    def colorY(color: Color, s: State) = (if (color.isGray) 0 else s.zoom * color.b) + height / 2
    def colorAt(x: Double, y: Double, s: State): LAB = {
      val a = (x - width / 2) / s.zoom
      val b = (y - height / 2) / s.zoom
      val l = s.luminance
      def hue = ((PI * 2) + atan2((y - height / 2), (x - width / 2))) % (PI * 2)
      LAB(l, a, b, hueHint = hue)
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
      import CanvasHelpers._

      val ctx = fgCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

      ctx.asInstanceOf[js.Dynamic].resetTransform() //TODO: add to scalajs.dom library
      ctx.clearRect(0, 0, fgCanvas.width, fgCanvas.height)

      // circle on chroma:
      percentCirle(ctx, width / 2, height / 2, chromaCircleRadius, width = chromaCircleBorder, luminance / 100.0)

      val colors = dragState.dragging match {
        case Some((col, i)) => palette.updated(i, col)
        case None => palette
      }
      if (chroma > 0) {
        for (color <- colors) {
          drawColor(ctx, color, color.luminance / 100.0, luminance / 100.0, s)
        }
      } else { // chroma is zero => infinite zoom, only draw gray colors
        for (color <- colors if color.isGray) {
          drawColor(ctx, color, color.luminance / 100.0, luminance / 100.0, s)
        }
      }
    }

    override def onDragStart(s: State, draggable: Draggable, x: Double, y: Double): Callback = {
      val (col, _) = draggable
      val newState = s.copy(
        luminance = col.luminance,
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
      val (col, i) = draggable
      val newCol = col.copy(lab = colorAt(x, y, s))
      val newState = s.copy(dragState = s.dragState.copy(dragging = Some((newCol, i))))
      $.setState(newState) >>
        p.proxy.dispatch(UpdateColor(i, newCol)) >>
        drawForeground(p, s)
    }

    override def onMouseWheel(draggable: Option[Draggable], deltaY: Double, p: Props, s: State): Callback = {
      import p._
      import s._
      draggable match {
        case Some((col, i)) =>
          val col = palette(i)
          val newCol = col.copy(lab = col.lab.copy(l = (col.l - deltaY / 10.0).max(0).min(100)))
          val newState = s.copy(luminance = newCol.l)
          proxy.dispatch(UpdateColor(i, newCol)) >> $.setState(newState) >> drawBackground(newState)

        case None =>
          val newState = s.copy(chroma = (chroma + deltaY / 10.0).max(0).min(128))
          $.setState(newState) >> drawBackground(newState)
      }
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
