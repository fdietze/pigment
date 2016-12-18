package pigment

import diode._
import diode.react._

import concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import scala.scalajs.js
import org.scalajs.dom._
import js.Dynamic.{global => g}

import pharg._

// Model
case class RootModel(
  colorScheme: ColorScheme = ColorScheme(),
  locked: Set[ColorIndex] = Set.empty,
  fitnessFunction: FitnessFunction = FitnessFunction()
)

//TODO: how to provide factory for case class? to retain case class features, like extractors, equals, ...
// report as scala-bug?
case class ColorScheme(groups: collection.immutable.Map[Int, IndexedSeq[Color]] = Map.empty) {
  def apply(groupId: Int): IndexedSeq[Color] = groups.getOrElse(groupId, IndexedSeq.empty)
  def updated(groupId: Int, f: IndexedSeq[Color] => IndexedSeq[Color]): ColorScheme = copy(groups.updated(groupId, f(apply(groupId))))

  def apply(i: ColorIndex): Color = groups(i.groupId)(i.index)
  def updated(i: ColorIndex, color: Color): ColorScheme = updated(i.groupId, (g: IndexedSeq[Color]) => g.updated(i.index, color))

  def nextGroupId = if (groups.isEmpty) 0 else (groups.keys.max + 1)

  lazy val indices: Seq[ColorIndex] = groups.keys.toSeq.flatMap(groupId => groups(groupId).indices.map(i => ColorIndex(groupId, i)))
  lazy val colors = groups.values.flatten.toSeq

  private def vertices = colors.distinct
  private def edges = vertices.combinations(2).map { case Seq(source, target) => Edge(source, target) }
  lazy val graph = DirectedGraph(vertices.toSet, edges.toSet)
}

case class ColorIndex(groupId: Int, index: Int)

// Actions
trait LiveAction extends Action // does not trigger ExportHash or Undo

case class SetColorScheme(colorScheme: ColorScheme) extends Action

case class AddColor(group: Int, color: Color) extends Action
case class LiveUpdateColor(colorIndex: ColorIndex, color: Color) extends LiveAction
case class UpdateColor(colorIndex: ColorIndex, color: Color) extends Action
case class RemoveColor(colorIndex: ColorIndex) extends Action

case class SetLock(colorIndex: ColorIndex) extends Action
case class RemoveLock(colorIndex: ColorIndex) extends Action

case class AddTerm(term: Term) extends Action
case class UpdateTerm(index: Int, term: Term) extends Action
case class RemoveTerm(index: Int) extends Action

case object ExportHash extends Action
case object ImportHash extends Action

// Circuit
object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = RootModel()

  val colorSchemeHandler = new ActionHandler(zoomRW(_.colorScheme)((m, v) => m.copy(colorScheme = v))) {
    override def handle = {
      case SetColorScheme(scheme) => updated(scheme)
      case AddColor(groupId, color) => updated(value.updated(groupId, _ :+ color))
      case UpdateColor(cindex, color) => updated(value.updated(cindex, color))
      case LiveUpdateColor(cindex, color) => updated(value.updated(cindex, color))
      case RemoveColor(i @ ColorIndex(groupId, index)) =>
        //TODO: bug: locks are shifted after removal
        val without = value.updated(groupId, g => (g.take(index) ++ g.drop(index + 1)))
        val withoutEmpty = ColorScheme(without.groups.filterNot(_._2.isEmpty))
        updated(
          withoutEmpty,
          Effect(Future.successful(RemoveLock(i)))
        )
    }
  }

  val lockHandler = new ActionHandler(zoomRW(_.locked)((m, v) => m.copy(locked = v))) {
    override def handle = {
      case SetLock(i) => updated(value + i)
      case RemoveLock(i) =>
        println(s"REMOVE LOCK $i")
        updated(value - i)
    }
  }

  val fitnessFunctionHandler = new ActionHandler(zoomRW(_.fitnessFunction.terms)((m, v) => m.copy(fitnessFunction = m.fitnessFunction.copy(terms = v)))) {
    override def handle = {
      case AddTerm(term) => updated(value :+ term)
      case UpdateTerm(i, term) =>
        println(value.updated(i, term)); updated(value.updated(i, term))
      case RemoveTerm(i) => updated(value.take(i) ++ value.drop(i + 1))
    }
  }

  override val actionHandler = composeHandlers(colorSchemeHandler, fitnessFunctionHandler, lockHandler)
}

class ExportProcessor extends ActionProcessor[RootModel] {
  window.addEventListener(
    "hashchange",
    (e: Event) => AppCircuit.dispatch(ImportHash)
  )

  def process(dispatch: diode.Dispatcher, action: Any, next: Any => diode.ActionResult[RootModel], currentModel: RootModel): ActionResult[RootModel] = {
    val codec = Base64Codec
    action match {
      case ImportHash =>
        // println("importing from hash...")
        val encoded = g.decodeURIComponent(window.location.hash.tail).asInstanceOf[String]
        codec.decode(encoded).map { newModel =>
          // println("importing from hash successful.")
          ActionResult.ModelUpdate(newModel)
        } getOrElse
          ActionResult.NoChange

      case ExportHash =>
        // println("exporting to hash.")
        val encoded = codec.encode(currentModel)
        val hash = s"#${g.encodeURIComponent(encoded).asInstanceOf[String]}"

        // history.pushState
        // this does not trigger the event "hashchang"
        // and as a nice byproduct it makes the back-button an undo-button.
        window.history.pushState(null, null, hash)
        // window.location.hash = hash

        ActionResult.NoChange

      case a: LiveAction =>
        next(a)

      case a: Action =>
        // Every model update triggers ExportHash
        AppCircuit.dispatch(ExportHash)

        // call the next processor
        next(a)
    }
  }
}
