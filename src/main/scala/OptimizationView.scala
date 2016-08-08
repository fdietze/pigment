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
      val colorIndices = (for ((groupId, group) <- groups.toSeq; index <- group.indices) yield ColorIndex(groupId, index)).toSet -- locked
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
    override val tournamentSize = 3
    override def mutationCount(g: Genotype) = random.nextInt(1, 4)

    val seed = 0
    override val parallel: Boolean = false

    val baseGenotype = ColorScheme()
    var mutationOperators = ColorMutation(config, strength = 10.0) :: Nil

    var model: RootModel = RootModel()

    def calculateFitness(g: Genotype, prefix: String): Double = model.fitnessFunction(g)
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

    def render(p: Props, s: State) = {
      <.div(
        // <.button(^.onClick --> initGA(p, s), "init"),
        <.button(^.onClick --> stepGA(p), "step"),
        <.button(^.onClick --> stepGA(p, 10), "10"),
        <.button(^.onClick --> stepGA(p, 100), "100"),
        <.br(),
        p.fitnessFunction(p.colorScheme),
        p.fitnessFunction.terms.map { term =>
          <.div(term.toString, ": ", "%.0f" format term.result(p.colorScheme))
        }
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
