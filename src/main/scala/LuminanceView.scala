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

object LuminanceView extends ColorCanvasView {
  case class State(
    chroma: Double = 100,
    dragState: DragState = DragState()
  )

  type Draggable = (Color, Int)

  def colorX(color: Color, s: State) = (if (color.isGray) color.hueHint else color.hue) / (Math.PI * 2) * width
  def colorY(color: Color, s: State) = ((100 - color.l) / 100) * height
  def colorAt(x: Double, y: Double, s: State) = {
    val hue = (x / width) * (Math.PI * 2)
    val a = Math.cos(hue) * s.chroma
    val b = Math.sin(hue) * s.chroma
    val l = 100 - (y / height * 100)
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

  class Backend(val $: BackendScope[Props, State]) extends BgFgBackend {

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
        drawColor(ctx, color, color.chroma / 128.0, s.chroma / 128.0, s)
      }
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
          val newCol = col.withChroma((col.chroma - deltaY / 10).max(0).min(128))
          val newState = s.copy(chroma = newCol.chroma)
          proxy.dispatch(UpdateColor(i, newCol)) >> $.setState(newState) >> drawBackground(newState)

        case None =>
          Callback.empty
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
            chroma = col.chroma,
            DragState(
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
