package net.doxxx.solver

import scala.annotation.tailrec
import scala.concurrent.duration._

object FFXIVCraftingHQ extends App {

  case class Action (durabilityCost: Int,
                     cpCost: Int,
                     successRate: Double,
                     qualityIncrease: Int,
                     progressIncrease: Int)

  val NoAction = Action(0, 0, 0, 0, 0)
  val BasicSynth = Action(10, 0, 0.9, 0, 26)
  val BasicTouch = Action(10, 18, 0.7, 65, 0)
  val MastersMend = Action(-30, 94, 1, 0, 0)
  val InnerQuiet = Action(0, 18, 1, 0, 0)
  val SteadyHand = Action(0, 22, 1, 0, 0)
  val HastyTouch = Action(10, 22, 0.5, 65, 0)
  val WasteNot = Action(0, 56, 1, 0, 0)

  val actions = IndexedSeq(
    NoAction, BasicSynth, BasicTouch, MastersMend //, InnerQuiet, SteadyHand, HastyTouch, WasteNot
  )

  val startDurability: Int = 60
  val startCP: Int = 190
  val startQuality: Int = 0
  val startProgress: Int = 55

  def specimenBuilder(actions: Iterable[Action]): Vector[Action] = actions.toVector

  case class State(durability: Int, cp: Int, quality: Double, progress: Double) {
    def apply(action: Action) = copy(
      durability - action.durabilityCost,
      cp - action.cpCost,
      quality + action.qualityIncrease * action.successRate,
      progress - action.progressIncrease * action.successRate
    )
  }

  val penalty = 10000

  def fitnessFunc(steps: Vector[Action]): Double = {
    @tailrec
    def eval(steps: List[Action], states: List[State]): List[State] = steps match {
      case Nil => states
      case action :: tail => eval(tail, states.head.apply(action) :: states)
    }

    val initState = State(startDurability, startCP, startQuality, startProgress)
    val states = eval(steps.toList, List(initState))
    val finalState :: intermediateStates = states
    val durabilityViolations = intermediateStates.count(s => s.durability <= 0 || s.durability > startDurability)
    val progressViolations = intermediateStates.count(_.progress < 0)
    val cpViolations = states.count(_.cp < 0)
    val finalDurabilityPenalty = if (finalState.durability < 0) penalty else 0
    val finalProgressPenalty = if (finalState.progress > 0) penalty else 0

    (finalState.quality
      - (durabilityViolations + progressViolations + cpViolations) * penalty
      - finalDurabilityPenalty
      - finalProgressPenalty)
  }

  val timeLimitTest = Experiment.timeLimitTest(60, SECONDS)
  val fitnessTest = Experiment.fitnessPercentageTest(0.9, fitnessFunc)
  def stopCondition(specimens: List[Vector[Action]]): Boolean = {
    timeLimitTest() || fitnessTest(specimens)
  }

  val genePool = actions.toArray
  val experiment = new Experiment[Action, Vector[Action]](
    mutationRate = 0.01,
    population = 500,
    genePool,
    specimenBuilder,
    fitnessFunc,
    stopCondition
  )

  // Original best sequence from Excel:
  // 4, 1, 0, 6, 0, 1, 3, 6, 0, 5, 2, 2, 6, 2, 1
  val archetype = Vector(0, 1, 0, 0, 0, 1, 3, 0, 0, 0, 2, 2, 0, 2, 1).map(actions)

  println(s"archetype fitness = ${fitnessFunc(archetype)}")

  val start = System.currentTimeMillis()
  val (evolvedSpecimens, epoch) = experiment.evolution(experiment.randomPool(archetype))
  val elapsed = System.currentTimeMillis() - start

  println()

  val best = evolvedSpecimens.maxBy(fitnessFunc)
  val bestPretty = best.filter(_ != NoAction).map(a => actions.indexOf(a)).mkString("[", ",", "]")
  println(s"$bestPretty => ${fitnessFunc(best)}")
  println(s"Generations: ${epoch+1}")
  println(s"Total time: ${elapsed/1000}s")
  println(s"Avg time per generation: ${elapsed/(epoch+1)}ms")
}
