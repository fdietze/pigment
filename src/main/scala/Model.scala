package pigment

import diode._
import diode.react._

import concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import scala.scalajs.js
import org.scalajs.dom._

import pharg._

// Model
case class RootModel(
  colorScheme: ColorScheme = ColorScheme(),
  locked: Set[ColorIndex] = Set.empty,
  fitnessFunction: FitnessFunction = FitnessFunction()
)

//TODO: how to provide factory for case class? to retain case class features, like extractors, equals, ...
// report as scala-bug?
case class ColorScheme(groups: collection.immutable.Map[Int, IndexedSeq[Color]] = Map.empty.withDefaultValue(IndexedSeq.empty)) {
  def apply(i: Int) = groups.getOrElse(i, Nil)
  def apply(i: ColorIndex) = groups(i.groupId)(i.index)
  def nextGroupId = if (groups.isEmpty) 0 else (groups.keys.max + 1)
  lazy val indices: Seq[ColorIndex] = groups.keys.toSeq.flatMap(groupId => groups(groupId).indices.map(i => ColorIndex(groupId, i)))
  lazy val colors = groups.values.flatten.toSeq

  private def vertices = colors
  private def edges = vertices.combinations(2).map { case Seq(source, target) => Edge(source, target) }.toSeq
  lazy val graph = DirectedGraph(vertices.toSet, edges.toSet)
}

case class ColorIndex(groupId: Int, index: Int)

// Actions
case class SetColorScheme(colorScheme: ColorScheme) extends Action

case class AddColor(group: Int, color: Color) extends Action
case class UpdateColor(colorIndex: ColorIndex, color: Color) extends Action
case class RemoveColor(colorIndex: ColorIndex) extends Action

case class SetLock(colorIndex: ColorIndex) extends Action
case class RemoveLock(colorIndex: ColorIndex) extends Action

case class AddTerm(term: Term) extends Action
case class UpdateTerm(index: Int, term: Term) extends Action
case class RemoveTerm(index: Int) extends Action

// case object ExportHash extends Action
case object ImportHash extends Action

// Circuit
object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = RootModel()

  val colorSchemeHandler = new ActionHandler(zoomRW(_.colorScheme.groups)((m, v) => m.copy(colorScheme = m.colorScheme.copy(groups = v)))) {
    override def handle = {
      case SetColorScheme(scheme) => updated(scheme.groups)
      case AddColor(group, color) => updated(value.updated(group, value(group) :+ color))
      case UpdateColor(ColorIndex(group, index), color) => updated(value.updated(group, value(group).updated(index, color)))
      case RemoveColor(i @ ColorIndex(group, index)) =>
        //TODO: bug: locks are shifted after removal
        val without = value.updated(group, (value(group).take(index) ++ value(group).drop(index + 1)))
        val withoutEmpty = without.filterNot(_._2.isEmpty)
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
  // val dispatchImport: js.Function1[Event, Unit] = { e: Event => AppCircuit.dispatch(ImportHash) }
  // window.addEventListener("hashchange", dispatchImport, false)

  def process(dispatch: diode.Dispatcher, action: Any, next: Any => diode.ActionResult[RootModel], currentModel: RootModel): ActionResult[RootModel] = {
    action match {
      case ImportHash =>
        println("importing from hash...")
        export.fromJson(window.location.hash.tail).map { newModel =>
          println("importing from hash successful.")
          ActionResult.ModelUpdate(newModel)
        } getOrElse
          ActionResult.NoChange
      case _ =>

        //TODO: call after action, not before action / or provide separate ExportHash action, because of performance reasons
        import scala.scalajs.js
        import org.scalajs.dom._
        // println("exporting to hash.")
        val encoded = export.toJson(currentModel)
        val hash = s"#$encoded"
        // window.history.pushState(null, null, hash)
        window.location.hash = hash //TODO: don't trigger hashchange event

        // call the next processor
        next(action)
    }
  }
}
