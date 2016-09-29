package pigment

import diode._
import diode.react._

import concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

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

// Circuit
object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = RootModel(
    ColorScheme(Map[Int, IndexedSeq[Color]](
      0 -> Array(
        LAB(15, -12, -12),
        LAB(20, -12, -12),
        LAB(92, -0, 10),
        LAB(97, 0, 10)
      ),
      1 -> Array(
        LAB(60, 10, 65),
        LAB(50, 50, 55),
        LAB(50, 65, 45),
        LAB(50, 65, -5),
        LAB(50, 15, -45),
        LAB(55, -10, -45),
        LAB(60, -35, -5),
        LAB(60, -20, 65)
      )
    ).withDefaultValue(IndexedSeq.empty)),
    locked = Set(ColorIndex(0, 0), ColorIndex(0, 1), ColorIndex(0, 2), ColorIndex(0, 3)),
    FitnessFunction(List(
      Approximation(InterGroupDistance(0, 1), 40),
      Statistics(Maximize, Min, IntraGroupDistance(1), List())
    ))
  )

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
