package net.doxxx.solver.models.ffxivcrafting

import scala.concurrent.duration._
import net.doxxx.solver.{BreedingStrategies, Solver}

abstract class SimulationBase(charLevel: Int,
                               recipeLevel: Int,
                               baseCraftsmanship: Int,
                               baseControl: Int,
                               startDurability: Int,
                               startCP: Int,
                               startQuality: Int,
                               difficulty: Int,
                               availableActions: Seq[String],
                               archetype: Vector[String]) {

  import SimulationBase._

  def specimenBuilder(actions: Iterable[Action]): Vector[Action] = actions.toVector

  def simulate(steps: Vector[Action]): (Double, Vector[(Action, State)])

  def fitnessFunc(steps: Vector[Action]): Double

  val timeLimitTest = Solver.timeLimitTest(5, MINUTES)
  val stagnationTest = Solver.stagnationTest[Action, Vector[Action]](0.9)
  val convergenceTest = Solver.convergenceTest[Action, Vector[Action]](1000)

  def stopCondition(specimens: List[(Vector[Action], Double)]): Boolean = {
    timeLimitTest() || stagnationTest(specimens)
  }

  val actions = availableActions.map(actionMap).toVector
  val genePool = actions.toArray

  val maxNameLength = actions.map(_.name.length).max
  val actionFormatSpec = s"%-${maxNameLength}s"

  def prettyAction(a: Action): String = {
    actionFormatSpec.format(a.name)
  }

  def run(): (List[Vector[Action]], Int, Long) = {
    val solver = new Solver[Action, Vector[Action]](
      mutationRate = 0.01,
      population = 500,
      genePool,
      specimenBuilder,
      fitnessFunc,
      stopCondition,
      BreedingStrategies.Transposition
    )

    val archetypeGenes = archetype.map(actionMap)

    val start = System.currentTimeMillis()
    val (evolvedSpecimens, epoch) = solver.evolution(solver.randomPool(archetypeGenes))
    val elapsed = System.currentTimeMillis() - start

    (evolvedSpecimens, epoch, elapsed)
  }
}

object SimulationBase {

  case class Action (name: String,
                     durabilityCost: Int,
                     cpCost: Int,
                     successRate: Double,
                     qualityEfficiency: Double,
                     progressEfficiency: Double)

  val NoAction = Action("NOP", 0, 0, 0, 0, 0)
  val BasicSynth = Action("BS", 10, 0, 0.9, 0, 1)
  val BasicTouch = Action("BT", 10, 18, 0.7, 1, 0)
  val MastersMend = Action("MM", -30, 92, 1, 0, 0)
  val InnerQuiet = Action("IQ", 0, 18, 1, 0, 0) // toggle, quality increases will increase control
  val SteadyHand = Action("SH", 0, 22, 1, 0, 0) // Improves action success rate by 20% for the next 5 steps
  val HastyTouch = Action("HT", 10, 0, 0.5, 1, 0)
  val Rumination = Action("RU", 0, 0, 1, 0, 0) // Removes IQ and returns CP proportional to # of control increases
  val StandardTouch = Action("ST", 10, 38, 0.8, 1.25, 0)
  val StandardSynthesis = Action("SS", 10, 15, 0.9, 0, 1.5)
  val AdvancedTouch = Action("AT", 10, 52, 0.9, 1.5, 0)
  val MastersMend2 = Action("MM2", -60, 150, 1, 0, 0)
  val Manipulation = Action("MP", 0, 88, 1, 0, 0) // Returns 10 durability every step for the next 3 steps
  //  val WasteNot = Action("WN", 0, 56, 1, 0, 0) // Reduces loss of durability by 50% for the next 4 steps
  //  val ComfortZone = Action("CZ", 0, 58, 1, 0, 0) // Returns 8 CP after each step for the next 10 steps
  //  val WasteNot2 = Action("WN2", 0, 95, 1, 0, 0) // Reduces loss of durability by 50% for the next 8 steps
  //  val GreatStrides = Action("GS", 0, 32, 1, 0, 0) // Doubles the efficiency of a 'Touch' action, consuming the Great Strides effect. Lasts 3 steps.
  //  val ElementalBrand = Action("EB", 10, 15, 0.9, 0, 2) // Increases progress. Progress efficiency is doubled when recipe affinity matches element.
  //  val ByregotsBlessing = Action("BB", 10, 24, 0.9, 1, 0) // Increases quality by 100% + 20% for each bonus to control granted by IQ

  val allActions = Seq(
    NoAction, BasicSynth, BasicTouch, MastersMend, InnerQuiet, SteadyHand, HastyTouch, Rumination, StandardTouch,
    StandardSynthesis, AdvancedTouch, MastersMend2, Manipulation
  )
  val actionMap: Map[String, Action] = allActions.map(a => a.name -> a).toMap

  trait State
}
