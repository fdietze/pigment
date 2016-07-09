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
  }

  def colorX(color: Color, s: State) = (if (color.isGray) 0 else s.zoom * color.a) + width / 2
  def colorY(color: Color, s: State) = (if (color.isGray) 0 else s.zoom * color.b) + height / 2
  def colorAt(x: Double, y: Double, s: State): LAB = {
    val a = (x - width / 2) / s.zoom
    val b = (y - height / 2) / s.zoom
    val l = s.luminance
    def hue = ((PI * 2) + atan2((y - height / 2), (x - width / 2))) % (PI * 2)
    LAB(l, a, b, hueHint = hue)
  }

  type Draggable = (Color, Int)

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

  class Backend(val $: BackendScope[Props, State]) extends BgFgBackend {
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

    //TODO: native drag event
    def handleMouseDown(p: Props)(e: ReactMouseEventH) = $.state.flatMap { s =>
      import p._
      import s._

      val (mx, my) = mouseOffset(e)

      hitDraggable(mx, my, p, s) match {
        case Some(target @ (col, i)) =>
          val newState = s.copy(
            luminance = col.luminance,
            dragState = DragState(
              dragging = Some(target),
              startX = mx,
              startY = my,
              offsetX = colorX(col, s) - mx,
              offsetY = colorY(col, s) - my
            )
          )
          $.setState(newState) >> drawBackground(newState)
        case None =>
          Callback.empty
      }
    }

    def handleMouseMove(p: Props)(e: ReactMouseEventH) = $.state.flatMap { s =>
      import p._
      import s._
      val (mx, my) = mouseOffset(e)

      import dragState.{offsetX, offsetY}
      dragState.dragging match {
        case Some((col, i)) =>
          val newCol = col.copy(lab = colorAt(mx + offsetX, my + offsetY, s))
          val newState = s.copy(dragState = dragState.copy(dragging = Some((newCol, i))))
          $.setState(newState) >>
            proxy.dispatch(UpdateColor(i, newCol)) >>
            drawForeground(p, s)

        case None =>
          Callback.empty
      }
    }

    def handleMouseUp(p: Props)(e: ReactMouseEventH) = $.state.flatMap { s =>
      import p._
      import s._

      $.modState(_.copy(
        dragState = dragState.copy(dragging = None)
      ))
    }

    def handleMouseWheel(p: Props)(e: ReactWheelEventH) = $.state.flatMap { s =>
      import p._
      import s._

      val (mx, my) = mouseOffset(e)
      // TODO: cross browser deltaY
      // val deltaY = mouseDeltaY(e)
      // val deltaY = if (e.nativeEvent.detail < 0 || e.nativeEvent.asInstanceOf[js.Dynamic].wheelDelta.asInstanceOf[Int] > 0) -1 else 1
      val deltaY = e.deltaY
      // console.log(deltaY, e.deltaY)

      e.preventDefault()

      hitDraggable(mx, my, p, s) match {
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
