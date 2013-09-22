package net.doxxx.solver.models.ffxivcrafting

import scala.util.Random
import scala.annotation.tailrec
import net.doxxx.solver.models.FFXIVCraftingHQ
import net.doxxx.solver.models.FFXIVCraftingHQ._

class MonteCarloSimulation(charLevel: Int,
                           recipeLevel: Int,
                           baseCraftsmanship: Int,
                           baseControl: Int,
                           startDurability: Int,
                           startCP: Int,
                           startQuality: Int,
                           difficulty: Int,
                           availableActions: Seq[String],
                           archetype: Vector[String])
  extends FFXIVCraftingHQ(charLevel, recipeLevel, baseCraftsmanship, baseControl, startDurability, startCP,
    startQuality, difficulty, availableActions, archetype)
{

  case class MCSState(prevSucceeded: Boolean, durability: Int, cp: Int, quality: Double, progress: Double, steadyHand: Int,
                      craftsmanship: Double, control: Double, innerQuiet: Boolean, innerQuietCount: Double,
                      manipulation: Int) extends State
  {

    override def toString = "durability=%-3d cp=%-3d quality=%-4.0f progress=%-3.0f".format(durability, cp, quality, progress)

    def apply(action: Action) = {
      if (durability <= 0 || progress <= 0) {
        this
      }
      else {
        val succeeded = Random.nextFloat <= successRate(action)
        copy(
          succeeded,
          calcDurability(action, succeeded),
          calcCP(action, succeeded),
          calcQuality(action, succeeded),
          calcProgress(action, succeeded),
          calcSteadyHand(action, succeeded),
          calcCraftsmanship(action, succeeded),
          calcControl(action, succeeded),
          calcInnerQuiet(action, succeeded),
          calcInnerQuietCount(action, succeeded),
          calcManipulation(action, succeeded)
        )
      }
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

    def calcDurability(action: Action, succeeded: Boolean): Int = {
      val bonus = if (manipulation > 0) 10 else 0
      math.min(startDurability, durability - action.durabilityCost + bonus)
    }

    def calcCP(action: Action, succeeded: Boolean): Int = {
      math.min(startCP, cp - action.cpCost + ruminationCPIncrease(action))
    }

    def calcQuality(action: Action, succeeded: Boolean): Double = {
      if (succeeded)
        quality + math.round(effectiveQualityIncrease * action.qualityEfficiency)
      else
        quality
    }

    def calcProgress(action: Action, succeeded: Boolean): Double = {
      if (succeeded)
        progress - math.round(effectiveProgressIncrease * action.progressEfficiency)
      else
        progress
    }

    def calcSteadyHand(action: Action, succeeded: Boolean): Int = {
      if (action == SteadyHand) 5 else math.max(0, steadyHand - 1)
    }

    def calcCraftsmanship(action: Action, succeeded: Boolean): Double = {
      craftsmanship
    }

    def calcControl(action: Action, succeeded: Boolean): Double = {
      math.round(baseControl * (1 + calcInnerQuietCount(action, succeeded) * 0.2))
    }

    def calcInnerQuiet(action: Action, succeeded: Boolean): Boolean = (action, innerQuiet) match {
      case (InnerQuiet, false) => true
      case (InnerQuiet, true) => true
      case (Rumination, true) => false
      case (_, _) => innerQuiet
    }

    def calcInnerQuietCount(action: Action, succeeded: Boolean): Double = {
      if (action == Rumination)
        0
      else if (innerQuiet && action.qualityEfficiency > 0 && succeeded)
        innerQuietCount + 1
      else
        innerQuietCount
    }

    def calcManipulation(action: Action, succeeded: Boolean): Int = {
      if (action == Manipulation) 3 else math.max(0, manipulation - 1)
    }

  }

  val penalty = 10000

  def simulate(steps: Vector[Action]): (Double, Vector[(Action,MCSState)]) = {
    @tailrec
    def eval(steps: List[Action], states: List[MCSState]): List[MCSState] = steps match {
      case Nil => states
      case action :: tail => eval(tail, states.head.apply(action) :: states)
    }

    val initState = MCSState(
      prevSucceeded = true,
      durability = startDurability,
      cp = startCP,
      quality = startQuality,
      progress = difficulty,
      steadyHand = 0,
      craftsmanship = baseCraftsmanship,
      control = baseControl,
      innerQuiet = false,
      innerQuietCount = 0,
      manipulation = 0
    )
    val states = eval(steps.toList, List(initState))
    val finalState :: intermediateStates = states
    val durabilityViolations = intermediateStates.count(s => s.durability <= 0 || s.durability > startDurability)
    val cpViolations = states.count(_.cp < 0)
    val ruminationSeqViolations = math.max(0, steps.segmentLength({ _ == Rumination}, 0) - 1)
    val finalDurabilityPenalty = if (finalState.durability < 0) penalty else 0
    val finalProgressPenalty = if (finalState.progress > 0) penalty else 0
    val finalActionPenalty = if (steps.last != BasicSynth) penalty else 0

    val fitness = (finalState.quality
      - (durabilityViolations + cpViolations + ruminationSeqViolations) * penalty
      - finalDurabilityPenalty
      - finalProgressPenalty
      - finalActionPenalty)

    (fitness, steps.zip(states.reverse.tail))
  }

  val monteCarloRounds = 10000
  def fitnessFunc(steps: Vector[Action]): Double = {
//    val start = System.currentTimeMillis()
    val results = (1 to monteCarloRounds).par.map { _ =>
      val (fitness, _) = simulate(steps)
      fitness
    }
//    val elapsed = System.currentTimeMillis() - start
//    println(s"Monte Carlo elapsed time = $elapsed ms")

    results.sum / monteCarloRounds
  }

}

object MonteCarloSimulation extends App {
  val availableActions = Seq("NOP", "BS", "BT", "HT", "MM", "SH", "IQ", "MP")
  val archetype = "NOP NOP NOP NOP NOP BS IQ SH BT HT MP SH HT HT HT BT BS BS".split(' ').toVector

  val model = new MonteCarloSimulation(
    charLevel = 12,
    recipeLevel = 12,
    baseCraftsmanship = 83,
    baseControl = 86,
    startDurability = 60,
    startCP = 190,
    startQuality = 0,
    difficulty = 53,
    availableActions = availableActions,
    archetype = archetype
  )

  val archetypeGenes = archetype.map(FFXIVCraftingHQ.actionMap)

  val archetypeFitness = model.fitnessFunc(archetypeGenes)
  println(s"Archetype fitness = $archetypeFitness")

  val (_, archetypeStates) = model.simulate(archetypeGenes)
  println(archetypeStates.map { case (a, s) => s"${a.name} => $s"}.mkString("\n"))

  println()

  val (evolvedSpecimens, epoch, elapsed) = model.run()

  val fitnesses = evolvedSpecimens.map(model.fitnessFunc)
  val (best, fitness) = evolvedSpecimens.zip(fitnesses).maxBy(_._2)

  val (_, actionStates) = model.simulate(best.filter(_ != NoAction))
  val trimmedActionStates = actionStates.takeWhile {
    case (a, s) => s.durability >= 0 && s.progress >= 0
  }

  val bestPretty = trimmedActionStates.map(_._1.name).mkString("[", " ", "]")
  println(s"$bestPretty => $fitness")
  println(s"Generations: ${epoch+1}")
  println(s"Total time: ${elapsed/1000}s")
  println(s"Avg time per generation: ${elapsed/(epoch+1)}ms")
  println()

  println(trimmedActionStates.map { case (a, s) => s"${a.name} => $s"}.mkString("\n"))

}
