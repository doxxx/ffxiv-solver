package net.doxxx.solver.models

import scala.annotation.tailrec
import scala.concurrent.duration._
import net.doxxx.solver.Experiment

class FFXIVCraftingHQ(charLevel: Int,
                      recipeLevel: Int,
                      baseCraftsmanship: Int,
                      baseControl: Int,
                      baseProgressIncrease: Int,
                      baseQualityIncrease: Int,
                      startDurability: Int,
                      startCP: Int,
                      startQuality: Int,
                      difficulty: Int,
                      archetype: Vector[String]) {

  case class Action (name: String,
                     durabilityCost: Int,
                     cpCost: Int,
                     successRate: Double,
                     qualityIncrease: Int,
                     progressIncrease: Int)

  val NoAction = Action("NOP", 0, 0, 0, 0, 0)
  val BasicSynth = Action("BS", 10, 0, 0.9, 0, baseProgressIncrease)
  val BasicTouch = Action("BT", 10, 18, 0.7, baseQualityIncrease, 0)
  val MastersMend = Action("MM", -30, 94, 1, 0, 0)
  val InnerQuiet = Action("IQ", 0, 18, 1, 0, 0) // toggle, quality increases will increase control
  val SteadyHand = Action("SH", 0, 22, 1, 0, 0) // Improves action success rate by 20% for the next five steps
  val HastyTouch = Action("HT", 10, 0, 0.5, baseQualityIncrease, 0)
  val WasteNot = Action("WN", 0, 56, 1, 0, 0) // Reduces loss of durability by 50% for the next four steps

  val actions = IndexedSeq(
    NoAction, BasicSynth, BasicTouch, MastersMend, SteadyHand, HastyTouch
  )
  val actionMap: Map[String,Action] = actions.map(a => a.name -> a).toMap

  def specimenBuilder(actions: Iterable[Action]): Vector[Action] = actions.toVector

  case class State(durability: Int, cp: Int, quality: Double, progress: Double, steadyHand: Int,
                    craftsmanship: Double, control: Double) {
    def apply(action: Action) = {
      val successRate = math.min(1, action.successRate + (if (steadyHand > 0) 0.2 else 0))
      copy(
        durability - action.durabilityCost,
        cp - action.cpCost,
        quality + action.qualityIncrease * successRate,
        progress - action.progressIncrease * successRate,
        steadyHand = if (action == SteadyHand) 5 else math.max(0, steadyHand - 1),
        craftsmanship = craftsmanship,
        control = control
      )
    }
  }

  val penalty = 10000

  def fitnessFunc(steps: Vector[Action]): Double = {
    @tailrec
    def eval(steps: List[Action], states: List[State]): List[State] = steps match {
      case Nil => states
      case action :: tail => eval(tail, states.head.apply(action) :: states)
    }

    val initState = State(startDurability, startCP, startQuality, difficulty, 0, baseCraftsmanship, baseControl)
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
  val stagnationTest = Experiment.stagnationTest(0.9, fitnessFunc)
  val convergenceTest = Experiment.convergenceTest(1000, fitnessFunc)

  def stopCondition(specimens: List[Vector[Action]]): Boolean = {
    timeLimitTest() || stagnationTest(specimens)
  }

  val genePool = actions.toArray

  def run() {
    val experiment = new Experiment[Action, Vector[Action]](
      mutationRate = 0.01,
      population = 500,
      genePool,
      specimenBuilder,
      fitnessFunc,
      stopCondition
    )

    val archetypeGenes = archetype.map(actionMap)
    println(s"archetype fitness = ${fitnessFunc(archetypeGenes)}")

    val start = System.currentTimeMillis()
    val (evolvedSpecimens, epoch) = experiment.evolution(experiment.randomPool(archetypeGenes))
    val elapsed = System.currentTimeMillis() - start

    println()

    val best = evolvedSpecimens.maxBy(fitnessFunc)
    val bestPretty = best.filter(_ != NoAction).map(_.name).mkString("[", " ", "]")
    println(s"$bestPretty => ${fitnessFunc(best)}")
    println(s"Generations: ${epoch+1}")
    println(s"Total time: ${elapsed/1000}s")
    println(s"Avg time per generation: ${elapsed/(epoch+1)}ms")
  }
}

object FFXIVCraftingHQ extends App {
  // Blank archetype: NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP
  // BS SH BT BT BT BT MM BS BS => ~234
  // BS SH HT HT BT MM SH HT BT HT BS BS => 299
  // SH BT BT HT BT HT MM BT BS BS BS => 312
  val archetype = "NOP NOP NOP NOP NOP SH BT BT HT BT HT MM BT BS BS BS".split(' ').toVector
  val model = new FFXIVCraftingHQ(
    charLevel = 12,
    recipeLevel = 12,
    baseCraftsmanship = 10,
    baseControl = 10,
    baseProgressIncrease = 26,
    baseQualityIncrease = 65,
    startDurability = 60,
    startCP = 190,
    startQuality = 0,
    difficulty = 55,
    archetype = archetype
  )
  model.run()
}
