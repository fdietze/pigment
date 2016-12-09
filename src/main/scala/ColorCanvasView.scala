package pigment

import scala.scalajs.js
import js.annotation._
import org.scalajs.dom._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import diode._
import diode.react._

import CanvasHelpers._

trait ColorCanvasView {
  val width = 200.0
  val height = 200.0

  val colorRadius = 10.0
  val colorBorder = 2.0

  // TODO: type Draggable = (ColorIndex, Color)
  type Draggable = (Int, Int, Color)

  case class DragState(
    dragging: Option[Draggable] = None,
    startX: Double = 0,
    startY: Double = 0,
    offsetX: Double = 0,
    offsetY: Double = 0
  )

  case class Props(proxy: ModelProxy[RootModel]) {
    def colors = proxy.value.colorScheme.colors
    def groups = proxy.value.colorScheme.groups
  }

  trait BgFgBackend[State <: { val dragState: DragState; def withDragState(dragState: DragState): State }] {
    def $: BackendScope[Props, State]

    def colorX(color: Color, s: State): Double
    def colorY(color: Color, s: State): Double
    def colorAt(x: Double, y: Double, state: State): Color

    def hitDraggable(x: Double, y: Double, p: Props, s: State): Option[(Int, Int, Color)] = {
      import p._
      import s._
      {
        for (
          (groupId, group) <- groups;
          (color, i) <- group.zipWithIndex if {
            val cx = colorX(color, s)
            val cy = colorY(color, s)
            val r = colorRadius + colorBorder / 2.0
            (x - cx) * (x - cx) + (y - cy) * (y - cy) <= r * r
          }
        ) yield (groupId, i, color)
      }.headOption
    }

    def mouseOffset(e: ReactMouseEventH): (Double, Double) = {
      val rect = e.target.getBoundingClientRect
      val mx = e.clientX - rect.left
      val my = e.clientY - rect.top
      (mx, my)
    }

    val bgCanvasRef = Ref[raw.HTMLCanvasElement]("canvas-bg")
    val fgCanvasRef = Ref[raw.HTMLCanvasElement]("canvas-fg")

    lazy val bgCanvas = bgCanvasRef($).get
    lazy val fgCanvas = fgCanvasRef($).get

    def drawBackground(s: State): Callback = Callback {
      drawBackgroundOnCanvas(bgCanvas, s)
    }
    def drawForeground(p: Props, s: State) = Callback {
      drawForegroundOnCanvas(fgCanvas, p, s)
    }

    def draw(p: Props, s: State) = Callback {
      drawBackgroundOnCanvas(bgCanvas, s)
      drawForegroundOnCanvas(fgCanvas, p, s)
    }

    def drawForegroundOnCanvas(fgCanvas: raw.HTMLCanvasElement, props: Props, state: State)
    def drawBackgroundOnCanvas(bgCanvas: raw.HTMLCanvasElement, state: State) {
      val ctx = bgCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
      val imageData = ctx.createImageData(bgCanvas.width, bgCanvas.height)

      val data = imageData.data.asInstanceOf[js.Array[Int]]
      // val start = System.nanoTime
      for (x <- 0 until bgCanvas.width; y <- 0 until bgCanvas.height) {
        val i = (y * bgCanvas.width + x) * 4
        val color = colorAt(x, y, state).rgb
        data(i + 0) = color.ri
        data(i + 1) = color.gi
        data(i + 2) = color.bi
        data(i + 3) = 255
      }
      // val duration = System.nanoTime - start
      // println(s"${duration / 1000000}ms")
      ctx.putImageData(imageData, 0, 0)
    }

    def drawColor(ctx: CanvasRenderingContext2D, color: Color, a: Double, refa: Double, s: State) {
      percentCirle(
        ctx,
        x = colorX(color, s),
        y = colorY(color, s),
        r = colorRadius,
        width = colorBorder,
        a = a,
        refa = Some(refa),
        fillColor = Some(color.toCSS)
      )
    }

    def onMouseWheel(draggable: Option[Draggable], deltaY: Double, p: Props, s: State): Callback = Callback.empty
    def onDrag(draggable: Draggable, x: Double, y: Double, p: Props, s: State): Callback = Callback.empty
    def onDragStart(s: State, draggable: Draggable, x: Double, y: Double): Callback = Callback.empty

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

      onMouseWheel(hitDraggable(mx, my, p, s), deltaY, p, s)
    }

    //TODO: native drag event
    def handleMouseDown(p: Props)(e: ReactMouseEventH) = $.state.flatMap { s =>
      import p._
      import s._

      val (mx, my) = mouseOffset(e)

      hitDraggable(mx, my, p, s) match {
        case Some(draggable) =>
          onDragStart(s, draggable, mx, my)

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
        case Some(draggable) =>
          onDrag(draggable, mx + offsetX, my + offsetY, p, s)
        case None =>
          Callback.empty
      }
    }

    def handleMouseUp(p: Props)(e: ReactMouseEventH) = $.state.flatMap { s =>
      val updateCBOpt = s.dragState.dragging.map { draggable =>
        val (groupId, i, color) = draggable
        p.proxy.dispatchCB(UpdateColor(ColorIndex(groupId, i), color))
      }
      val clearDragstateCB = $.modState(_.withDragState(s.dragState.copy(dragging = None)))
      updateCBOpt match {
        case Some(updateCB) => updateCB >> clearDragstateCB
        case None => clearDragstateCB
      }
    }

    val widthAttr = "width".reactAttr
    val heightAttr = "height".reactAttr
    def render(p: Props) = {
      <.div(
        <.div(
          ^.width := s"${width}px",
          ^.height := s"${height}px",
          ^.position := "relative",
          <.canvas(
            ^.ref := "canvas-bg",
            widthAttr := width,
            heightAttr := height,
            ^.position := "absolute"
          ),
          <.canvas(
            ^.ref := "canvas-fg",
            widthAttr := width,
            heightAttr := height,
            ^.position := "absolute",
            ^.cursor := "default",
            ^.onWheel ==> handleMouseWheel(p),
            ^.onMouseDown ==> handleMouseDown(p),
            ^.onMouseMove ==> handleMouseMove(p),
            ^.onMouseUp ==> handleMouseUp(p)
          )
        )
      )
    }
  }
}
