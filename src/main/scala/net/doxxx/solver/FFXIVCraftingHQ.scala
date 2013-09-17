package net.doxxx.solver

import scala.annotation.tailrec

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

  def specimenBuilder(actions: Iterable[Action]): Seq[Action] = actions.toSeq

  case class State(durability: Int, cp: Int, quality: Double, progress: Double) {
    def apply(action: Action) = copy(
      durability - action.durabilityCost,
      cp - action.cpCost,
      quality + action.qualityIncrease * action.successRate,
      progress - action.progressIncrease * action.successRate
    )
  }

  val penalty = 10000

  def fitnessFunc(steps: Seq[Action]): Double = {
    @tailrec
    def eval(steps: List[Action], states: List[State]): List[State] = steps match {
      case Nil => states
      case action :: tail => eval(tail, states.head.apply(action) :: states)
    }

    val initState = State(startDurability, startCP, startQuality, startProgress)
    val states = eval(steps.toList, List(initState))
    val finalState :: intermediateStates = states
    val durabilityViolations = intermediateStates.count(_.durability <= 0)
    val cpViolations = states.count(_.cp < 0)
    val finalDurabilityPenalty = if (finalState.durability < 0) penalty else 0

    finalState.quality - (durabilityViolations + cpViolations) * penalty - finalDurabilityPenalty
  }

  def stopCondition(specimens: List[Seq[Action]]): Boolean = {
    System.currentTimeMillis() - start > 60000
  }

  val genePool = actions.toArray
  val petri = new GeneticExploration[Action, Seq[Action]](
    mutationRate = 0.1,
    population = 500,
    genePool,
    specimenBuilder,
    fitnessFunc,
    stopCondition
  )

//  val archetype = Seq(4, 1, 0, 6, 0, 1, 3, 6, 0, 5, 2, 2, 6, 2, 1).map(actions)
  val archetype = Seq(0, 1, 0, 0, 0, 1, 3, 0, 0, 0, 2, 2, 0, 2, 1).map(actions)

  println(s"archetype fitness = ${fitnessFunc(archetype)}")

  val start = System.currentTimeMillis()
  val (evolvedSpecimens, epoch) = petri.evolution(petri.randomPool(archetype))
  val elapsed = System.currentTimeMillis() - start

  println()

  val best = evolvedSpecimens.maxBy(fitnessFunc)
  val bestPretty = best.map(a => actions.indexOf(a)).mkString("[", ",", "]")
  println(s"$bestPretty => ${fitnessFunc(best)}")
  println(s"Generations: ${epoch+1}")
  println(s"Total time: ${elapsed/1000}s")
  println(s"Avg time per generation: ${elapsed/(epoch+1)}ms")
}
