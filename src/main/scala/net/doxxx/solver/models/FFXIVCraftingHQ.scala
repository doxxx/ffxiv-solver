package net.doxxx.solver.models

import scala.annotation.tailrec
import scala.concurrent.duration._
import net.doxxx.solver.Solver

class FFXIVCraftingHQ(charLevel: Int,
                      recipeLevel: Int,
                      baseCraftsmanship: Int,
                      baseControl: Int,
                      startDurability: Int,
                      startCP: Int,
                      startQuality: Int,
                      difficulty: Int,
                      archetype: Vector[String]) {

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
//  val WasteNot = Action("WN", 0, 56, 1, 0, 0) // Reduces loss of durability by 50% for the next 4 steps
//  val Manipulation = Action("MP", 0, 88, 1, 0, 0) // Returns 10 durability every step for the next 3 steps
//  val ComfortZone = Action("CZ", 0, 58, 1, 0, 0) // Returns 8 CP after each step for the next 10 steps
//  val WasteNot2 = Action("WN2", 0, 95, 1, 0, 0) // Reduces loss of durability by 50% for the next 8 steps
//  val GreatStrides = Action("GS", 0, 32, 1, 0, 0) // Doubles the efficiency of a 'Touch' action, consuming the Great Strides effect. Lasts 3 steps.
//  val ElementalBrand = Action("EB", 10, 15, 0.9, 0, 2) // Increases progress. Progress efficiency is doubled when recipe affinity matches element.
//  val ByregotsBlessing = Action("BB", 10, 24, 0.9, 1, 0) // Increases quality by 100% + 20% for each bonus to control granted by IQ

  val actions = IndexedSeq(
    NoAction, BasicSynth, BasicTouch, MastersMend, SteadyHand, HastyTouch, InnerQuiet, Rumination
  )
  val actionMap: Map[String,Action] = actions.map(a => a.name -> a).toMap

  def specimenBuilder(actions: Iterable[Action]): Vector[Action] = actions.toVector

  /*
  Level.Difference = Crafter.Level - Synth.Level

  For -5 <= Level.Difference < 0,
  Level.Correction.Factor = 0.10 * Level.Difference
  For 0 < Level.Difference <= 5,
  Level.Correction.Factor = 0.05 * Level.Difference
  For 5 < Level.Difference <=15,
  Level.Correction.Factor = (0.022 * Level.Difference) + 0.15

  Base.Progress = 0.21 * (Craftsmanship) + 1.6
  Level.Corrected.Progress = Base.Progress * (1 + Level.Correction.Factor)



  For -5 <= Level.Difference <= 0,
  Level.Correction.Factor = 0.05 * Level.Difference

  Base.Quality = 0.36 * (Craftsmanship) + 34
  Level.Corrected.Quality = Base.Quality * (1 + Level.Correction.Factor)

  */

  case class State(durability: Int, cp: Int, quality: Double, progress: Double, steadyHand: Int,
                    craftsmanship: Double, control: Double, innerQuiet: Boolean, innerQuietCount: Double)
  {
    def apply(action: Action) = {
      copy(
        calcDurability(action),
        calcCP(action),
        calcQuality(action),
        calcProgress(action),
        calcSteadyHand(action),
        calcCraftsmanship(action),
        calcControl(action),
        calcInnerQuiet(action),
        calcInnerQuietCount(action)
      )
    }


    def successRate(action: Action): Double = {
      math.min(1, action.successRate + (if (steadyHand > 0) 0.2 else 0))
    }

    def effectiveProgressIncrease: Int = {
      val levelDiff = charLevel - recipeLevel
      val correctionFactor = {
        if (levelDiff >= -5 && levelDiff <= 0) 0.10 * levelDiff
        else if (levelDiff > 0 && levelDiff <= 5) 0.05 * levelDiff
        else if (levelDiff > 5) 0.022 * levelDiff + 0.15
        else 0
      }
      val baseProgress = 0.21 * craftsmanship + 1.6

      math.round(baseProgress * (1 + correctionFactor)).toInt
    }

    def effectiveQualityIncrease: Int = {
      val levelDiff = charLevel - recipeLevel
      val correctionFactor = {
        if (levelDiff >= -5 && levelDiff <= 0) 0.05 * levelDiff
        else 0
      }
      val baseQuality = 0.36 * control + 34

      math.round(baseQuality * (1 + correctionFactor)).toInt
    }

    def ruminationCPIncrease(action: Action): Int = {
      if (action == Rumination && innerQuietCount > 0) {
        math.round(math.min(60, (21 * innerQuietCount - math.pow(innerQuietCount, 2) + 10) / 2)).toInt
      }
      else 0
    }

    def calcDurability(action: Action): Int = {
      math.min(startDurability, durability - action.durabilityCost)
    }

    def calcCP(action: Action): Int = {
      math.min(startCP, cp - action.cpCost + ruminationCPIncrease(action))
    }

    def calcQuality(action: Action): Double = {
      quality + effectiveQualityIncrease * action.qualityEfficiency * successRate(action)
    }

    def calcProgress(action: Action): Double = {
      progress - effectiveProgressIncrease * action.progressEfficiency * successRate(action)
    }

    def calcSteadyHand(action: Action): Int = {
      if (action == SteadyHand) 5 else math.max(0, steadyHand - 1)
    }

    def calcCraftsmanship(action: Action): Double = {
      craftsmanship
    }

    def calcControl(action: Action): Double = {
      baseControl + baseControl * calcInnerQuietCount(action) * 0.2
    }

    def calcInnerQuiet(action: Action): Boolean = (action, innerQuiet) match {
      case (InnerQuiet, false) => true
      case (InnerQuiet, true) => true
      case (Rumination, true) => false
      case (_, _) => innerQuiet
    }

    def calcInnerQuietCount(action: Action): Double = {
      {
        if (action == Rumination)
          0
        else if (innerQuiet && action.qualityEfficiency > 0)
          innerQuietCount + 1 * successRate(action)
        else
          innerQuietCount
      }
    }

  }

  val penalty = 10000

  def calcFitness(steps: Vector[Action]): (Double, Vector[(Action,State)]) = {
    @tailrec
    def eval(steps: List[Action], states: List[State]): List[State] = steps match {
      case Nil => states
      case action :: tail => eval(tail, states.head.apply(action) :: states)
    }

    val initState = State(startDurability, startCP, startQuality, difficulty, 0, baseCraftsmanship, baseControl, false, 0)
    val states = eval(steps.toList, List(initState))
    val finalState :: intermediateStates = states
    val durabilityViolations = intermediateStates.count(s => s.durability <= 0 || s.durability > startDurability)
    val progressViolations = intermediateStates.count(_.progress < 0)
    val cpViolations = states.count(_.cp < 0)
    val finalDurabilityPenalty = if (finalState.durability < 0) penalty else 0
    val finalProgressPenalty = if (finalState.progress > 0) penalty else 0

    val fitness = (finalState.quality - (durabilityViolations + progressViolations + cpViolations) * penalty
      - finalDurabilityPenalty
      - finalProgressPenalty)

    (fitness, steps.zip(states.reverse.tail))
  }

  def fitnessFunc(steps: Vector[Action]): Double = {
    val (fitness, _) = calcFitness(steps)
    fitness
  }

  val timeLimitTest = Solver.timeLimitTest(60, SECONDS)
  val stagnationTest = Solver.stagnationTest(0.9, fitnessFunc)
  val convergenceTest = Solver.convergenceTest(1000, fitnessFunc)

  def stopCondition(specimens: List[Vector[Action]]): Boolean = {
    timeLimitTest() || stagnationTest(specimens)
  }

  val genePool = actions.toArray

  val maxNameLength = actions.map(_.name.length).max
  val actionFormatSpec = s"%-${maxNameLength}s"
  def prettyAction(a: Action): String = {
    actionFormatSpec.format(a.name)
  }

  def prettyState(s: State): String = {
    "durability=%-3d cp=%-3d quality=%-4.0f progress=%-3.0f -- %s".format(s.durability, s.cp, s.quality, s.progress, s.toString)
  }

  def run() {
    val experiment = new Solver[Action, Vector[Action]](
      mutationRate = 0.01,
      population = 500,
      genePool,
      specimenBuilder,
      fitnessFunc,
      stopCondition
    )

    val archetypeGenes = archetype.map(actionMap)
    val (archetypeFitness, archetypeStates) = calcFitness(archetypeGenes)
    println(s"Archetype fitness = $archetypeFitness")
    println(archetypeStates.map { case (a, s) => s"${prettyAction(a)} => ${prettyState(s)}"}.mkString("\n"))

    println()

    val start = System.currentTimeMillis()
    val (evolvedSpecimens, epoch) = experiment.evolution(experiment.randomPool(archetypeGenes))
    val elapsed = System.currentTimeMillis() - start

    println()

    val best = evolvedSpecimens.maxBy(fitnessFunc)
    val (fitness, states) = calcFitness(best)
    val bestPretty = best.filter(_ != NoAction).map(_.name).mkString("[", " ", "]")
    println(s"$bestPretty => ${fitness}")
    println(s"Generations: ${epoch+1}")
    println(s"Total time: ${elapsed/1000}s")
    println(s"Avg time per generation: ${elapsed/(epoch+1)}ms")
    println()
    println(states.map { case (a, s) => s"${prettyAction(a)} => ${prettyState(s)}"}.mkString("\n"))
  }
}

object FFXIVCraftingHQ extends App {
  // Blank archetype: NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP NOP
  // IQ BT BT BT BT BS MM RU BS BS BT BS => 241.5
  // IQ BS BS BT BT HT MM SH BT HT BS BS
  // IQ BS BS BT BT HT MM SH BT HT NOP BS
  // IQ SH BS BS BT BT HT MM SH HT HT HT BS => 353.5
  // IQ SH BS BS BT HT MM SH HT HT HT BT BS => 354.0
  val archetype = "NOP NOP NOP NOP NOP NOP IQ SH BS BS BT BT HT MM SH HT HT HT BS".split(' ').toVector
  val model = new FFXIVCraftingHQ(
    charLevel = 12,
    recipeLevel = 12,
    baseCraftsmanship = 83,
    baseControl = 86,
    startDurability = 60,
    startCP = 190,
    startQuality = 0,
    difficulty = 53,
    archetype = archetype
  )
  model.run()
}
