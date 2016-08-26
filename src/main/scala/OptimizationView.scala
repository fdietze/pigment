package pigment

import scala.scalajs.js
import js.annotation._
import org.scalajs.dom._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import diode._
import diode.react._

import scala.util.Try

import frenetic._

import Math._

object OptimizationView {

  case class Props(proxy: ModelProxy[RootModel]) {
    def model = proxy.value
    def colorScheme = model.colorScheme
    def fitnessFunction = model.fitnessFunction
    def locked = model.locked
  }

  case class State(best: Option[ColorScheme] = None)

  case class ColorMutation(config: GAConfig, strength: Double = 1.0) extends MutationOp[ColorScheme] {
    var locked: Set[ColorIndex] = Set.empty
    import config._
    def mutColor(color: Color): Color = {
      val lab = color.lab
      val newL = (lab.l + random.r.nextGaussian * strength).min(100).max(0)
      val newA = (lab.a + random.r.nextGaussian * strength).min(128).max(-128)
      val newB = (lab.b + random.r.nextGaussian * strength).min(128).max(-128)
      LAB(newL, newA, newB)
    }

    def apply(scheme: Genotype): Option[Genotype] = {
      import scheme._
      val colorIndices = scheme.indices.toSet -- locked
      random.selectOpt(colorIndices).map {
        case ci @ ColorIndex(groupId, index) =>
          val color = scheme(ci)
          val newColor = mutColor(color)
          ColorScheme(groups.updated(groupId, groups(groupId).updated(index, newColor)))
      }
    }
  }

  case class GAConfig() extends Config[ColorScheme] { config =>
    type Genotype = ColorScheme

    override val populationSize: Int = 50
    override val tournamentSize = 4
    override def mutationCount(g: Genotype) = random.nextInt(1, 5)

    val seed = 0
    override val parallel: Boolean = false

    val baseGenotype = ColorScheme()
    var mutationOperators = ColorMutation(config, strength = 5.0) :: Nil

    var model: RootModel = RootModel()

    def calculateFitness(g: Genotype, prefix: String): Double = {
      model.fitnessFunction(g)
    }
  }

  class Backend($: BackendScope[Props, State]) {
    val gaConfig = GAConfig()
    val ga = GeneticAlgorithm(gaConfig)

    def initGA(p: Props, s: State) = Callback {
      gaConfig.model = p.model
      gaConfig.mutationOperators.head.locked = p.model.locked

      if (s.best.isEmpty || s.best.get != p.colorScheme) {
        println("init GA")
        ga.population = List.fill(ga.config.populationSize)(p.colorScheme)
      }
    }

    def stepGA(p: Props, steps: Int = 1) = {
      val best = ga.runFor(steps)
      $.setState(State(Some(best))) >> p.proxy.dispatch(SetColorScheme(best))
    }

    def renderGoal(p: Props, s: State, term: Statistics, i: Int) = {
      import p.proxy.dispatch

      def goalToString(goal: Goal) = {
        goal match {
          case Minimize => "Minimize"
          case Maximize => "Maximize"
          // case Approximate(_) => "Approximate"
        }
      }
      def stringToGoal(goal: String) = {
        goal match {
          case "Minimize" => Minimize
          case "Maximize" => Maximize
          // case "Approximate" => Approximate(50)
        }
      }
      <.span(
        <.select(
          ^.value := goalToString(term.goal),
          ^.onChange ==> ((e: ReactEventI) => dispatch(UpdateTerm(i, term.copy(goal = stringToGoal(e.target.value))))),
          List("Minimize", "Maximize").map { goal =>
            <.option(goal, ^.value := goal)
          }
        )
      )
    }

    def renderMeasure(p: Props, s: State, term: Statistics, i: Int) = {
      import p.proxy.dispatch
      def measureToString(measure: Measure) = {
        measure match {
          case Min => "Min"
          case Max => "Max"
          case Mean => "Mean"
          case StdDev => "StdDev"
        }
      }
      def stringToMeasure(measure: String) = {
        measure match {
          case "Min" => Min
          case "Max" => Max
          case "Mean" => Mean
          case "StdDev" => StdDev
        }
      }
      <.select(
        ^.value := measureToString(term.measure),
        ^.onChange ==> ((selected: ReactEventI) => dispatch(UpdateTerm(i, term.copy(measure = stringToMeasure(selected.target.value))))),
        List("Min", "Max", "Mean", "StdDev").map { measure =>
          <.option(measure, ^.value := measure)
        }
      )
    }

    def renderTarget(p: Props, s: State, term: Statistics, i: Int) = {
      import p.proxy.dispatch
      val firstGroupId = p.colorScheme.groups.keys.headOption.getOrElse(0)
      val secondGroupId = p.colorScheme.groups.keys.drop(1).headOption.getOrElse(1)
      def targetToString(target: Target) = {
        target match {
          case Chroma(_) => "Chroma"
          case Luminance(_) => "Luminance"
          case IntraGroupDistance(_) => "IntraGroupDistance"
          case InterGroupDistance(_, _) => "InterGroupDistance"
        }
      }
      def stringToTarget(target: String) = {
        target match {
          case "Chroma" => Chroma(firstGroupId)
          case "Luminance" => Luminance(firstGroupId)
          case "IntraGroupDistance" => IntraGroupDistance(firstGroupId)
          case "InterGroupDistance" => InterGroupDistance(firstGroupId, secondGroupId)
        }
      }
      def groupSelector(selected: Int, action: (Int) => Action) = {
        <.select(
          ^.value := selected,
          ^.onChange ==> ((selected: ReactEventI) => dispatch(action(selected.target.value.toInt))),
          p.colorScheme.groups.keys.toSeq.map { g =>
            <.option(g.toString, ^.value := g.toString)
          }
        )
      }
      <.span(
        <.select(
          ^.value := targetToString(term.target),
          ^.onChange ==> ((selected: ReactEventI) => dispatch(UpdateTerm(i, term.copy(target = stringToTarget(selected.target.value))))),
          List("Chroma", "Luminance", "IntraGroupDistance", "InterGroupDistance").map { target =>
            <.option(target, ^.value := target)
          }
        ),
        term.target match {
          case Chroma(groupId) => groupSelector(selected = groupId, (g) => UpdateTerm(i, term.copy(target = Chroma(g))))
          case Luminance(groupId) => groupSelector(selected = groupId, (g) => UpdateTerm(i, term.copy(target = Luminance(g))))
          case IntraGroupDistance(groupId) => groupSelector(selected = groupId, (g) => UpdateTerm(i, term.copy(target = IntraGroupDistance(g))))
          case InterGroupDistance(groupIdA, groupIdB) => <.span(
            groupSelector(selected = groupIdA, (g) => UpdateTerm(i, term.copy(target = InterGroupDistance(g, groupIdB)))),
            groupSelector(selected = groupIdB, (g) => UpdateTerm(i, term.copy(target = InterGroupDistance(g, groupIdA))))
          )
        }
      )
    }

    //TODO: make dry
    def renderTarget(p: Props, s: State, term: Approximation, i: Int) = {
      import p.proxy.dispatch
      val firstGroupId = p.colorScheme.groups.keys.headOption.getOrElse(0)
      val secondGroupId = p.colorScheme.groups.keys.drop(1).headOption.getOrElse(1)
      def targetToString(target: Target) = {
        target match {
          case Chroma(_) => "Chroma"
          case Luminance(_) => "Luminance"
          case IntraGroupDistance(_) => "IntraGroupDistance"
          case InterGroupDistance(_, _) => "InterGroupDistance"
        }
      }
      def stringToTarget(target: String) = {
        target match {
          case "Chroma" => Chroma(firstGroupId)
          case "Luminance" => Luminance(firstGroupId)
          case "IntraGroupDistance" => IntraGroupDistance(firstGroupId)
          case "InterGroupDistance" => InterGroupDistance(firstGroupId, secondGroupId)
        }
      }
      def groupSelector(selected: Int, action: (Int) => Action) = {
        <.select(
          ^.value := selected,
          ^.onChange ==> ((selected: ReactEventI) => dispatch(action(selected.target.value.toInt))),
          p.colorScheme.groups.keys.toSeq.map { g =>
            <.option(g.toString, ^.value := g.toString)
          }
        )
      }
      <.span(
        <.select(
          ^.value := targetToString(term.target),
          ^.onChange ==> ((selected: ReactEventI) => dispatch(UpdateTerm(i, term.copy(target = stringToTarget(selected.target.value))))),
          List("Chroma", "Luminance", "IntraGroupDistance", "InterGroupDistance").map { target =>
            <.option(target, ^.value := target)
          }
        ),
        term.target match {
          case Chroma(groupId) => groupSelector(selected = groupId, (g) => UpdateTerm(i, term.copy(target = Chroma(g))))
          case Luminance(groupId) => groupSelector(selected = groupId, (g) => UpdateTerm(i, term.copy(target = Luminance(g))))
          case IntraGroupDistance(groupId) => groupSelector(selected = groupId, (g) => UpdateTerm(i, term.copy(target = IntraGroupDistance(g))))
          case InterGroupDistance(groupIdA, groupIdB) => <.span(
            groupSelector(selected = groupIdA, (g) => UpdateTerm(i, term.copy(target = InterGroupDistance(g, groupIdB)))),
            groupSelector(selected = groupIdB, (g) => UpdateTerm(i, term.copy(target = InterGroupDistance(g, groupIdA))))
          )
        }
      )
    }

    def render(p: Props, s: State) = {
      import p.proxy.dispatch
      val firstGroupId = p.colorScheme.groups.keys.headOption.getOrElse(0)
      <.div(
        p.fitnessFunction.terms.zipWithIndex.map {
          case (term, i) =>
            <.div(
              <.button("-", ^.onClick --> dispatch(RemoveTerm(i))),
              term match {
                case term: Statistics =>
                  <.span(
                    renderGoal(p, s, term, i),
                    renderMeasure(p, s, term, i),
                    renderTarget(p, s, term, i)
                  )
                case term: Approximation =>
                  <.span(
                    "Approximate",
                    renderTarget(p, s, term, i),
                    <.input(^.`type` := "number", ^.value := term.targetValue, ^.width := "3em",
                      ^.onChange ==> ((e: ReactEventI) => dispatch(UpdateTerm(i, term.copy(targetValue = e.target.value.toDouble)))))
                  )

              },
              ": ", "%.0f" format term.result(p.colorScheme, Min),
              " ", "%.0f" format term.result(p.colorScheme, Mean),
              " ", "%.0f" format term.result(p.colorScheme, Max),
              " ", "%.0f" format term.result(p.colorScheme, StdDev)
            )
        },
        <.button("+S", ^.onClick --> dispatch(AddTerm(Statistics(Maximize, Min, Chroma(firstGroupId))))),
        <.button("+A", ^.onClick --> dispatch(AddTerm(Approximation(Chroma(firstGroupId), 70)))),
        "%.0f" format p.fitnessFunction(p.colorScheme),
        <.br(),
        <.button(^.onClick --> stepGA(p), "step"),
        <.button(^.onClick --> stepGA(p, 10), "10"),
        <.button(^.onClick --> stepGA(p, 100), "100")
      )
    }
  }

  private val component = ReactComponentB[Props]("OptimizationView")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(c => c.backend.initGA(c.props, c.state))
    .componentDidUpdate(c => c.$.backend.initGA(c.currentProps, c.currentState))
    .build

  def apply(proxy: ModelProxy[RootModel]) = component(Props(proxy))
}
