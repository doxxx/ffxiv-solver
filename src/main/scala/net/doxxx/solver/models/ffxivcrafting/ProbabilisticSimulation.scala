package net.doxxx.solver.models.ffxivcrafting

import net.doxxx.solver.models.FFXIVCraftingHQ
import net.doxxx.solver.models.FFXIVCraftingHQ._
import scala.annotation.tailrec

class ProbabilisticSimulation(charLevel: Int,
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

  case class PSState(durability: Int, cp: Int, quality: Double, progress: Double, steadyHand: Int,
                   craftsmanship: Double, control: Double, innerQuiet: Boolean, innerQuietCount: Double) extends State
  {
    override def toString = "durability=%-3d cp=%-3d quality=%-4.0f progress=%-3.0f".format(durability, cp, quality, progress)

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

  def simulate(steps: Vector[Action]): (Double, Vector[(Action,PSState)]) = {
    @tailrec
    def eval(steps: List[Action], states: List[PSState]): List[PSState] = steps match {
      case Nil => states
      case action :: tail => eval(tail, states.head.apply(action) :: states)
    }

    val initState = PSState(
      durability = startDurability,
      cp = startCP,
      quality = startQuality,
      progress = difficulty,
      steadyHand = 0,
      craftsmanship = baseCraftsmanship,
      control = baseControl,
      innerQuiet = false,
      innerQuietCount = 0
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

  def fitnessFunc(steps: Vector[Action]): Double = {
    val (fitness, _) = simulate(steps)
    fitness
  }

}

object ProbabilisticSimulation extends App {
  val availableActions = Seq("NOP", "BS", "BT", "MM", "SH", "IQ")

  val archetype = "NOP NOP NOP NOP NOP NOP HT BT BT SH HT IQ SH HT MM BS BS BS IQ".split(' ').toVector
  val model = new ProbabilisticSimulation(
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

  val (archetypeFitness, archetypeStates) = model.simulate(archetypeGenes)
  println(s"Archetype fitness = $archetypeFitness")

  println(archetypeStates.map { case (a, s) => s"$a => $s"}.mkString("\n"))

  println()

  val (evolvedSpecimens, epoch, elapsed) = model.run()

  val fitnesses = evolvedSpecimens.map(model.fitnessFunc)
  val (best, fitness) = evolvedSpecimens.zip(fitnesses).maxBy(_._2)
  val bestPretty = best.filter(_ != NoAction).map(_.name).mkString("[", " ", "]")
  println(s"$bestPretty => $fitness")
  println(s"Generations: ${epoch+1}")
  println(s"Total time: ${elapsed/1000}s")
  println(s"Avg time per generation: ${elapsed/(epoch+1)}ms")
  println()
}
