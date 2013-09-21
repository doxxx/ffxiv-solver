package net.doxxx.solver

import scala.util.Random
import scala.annotation.tailrec
import scala.concurrent.duration.TimeUnit
import net.doxxx.solver.BreedingStrategies.BreedingStrategy

class Solver[Gene, Specimen <% Iterable[Gene]]
(mutationRate: Double,
 population: Int,
 genePool: IndexedSeq[Gene],
 specimenBuilder: Iterable[Gene] => Specimen,
 fitnessCalc: Specimen => Double,
 stopCondition: List[(Specimen, Double)] => Boolean,
 breedingStrategy: BreedingStrategy)
{
  def randomGenes: Stream[Gene] = genePool(Random.nextInt(genePool.length)) #:: randomGenes
  def newSpecimen(len: Int): Specimen = specimenBuilder(randomGenes.take(len))

  type Pool = List[Specimen]
  type Fitness = (Specimen, Double)
  type NormalizedFitness = (Specimen, Double, Double)
  type FitnessPool = List[Fitness]

  def randomPool(archetype: Specimen): Pool =
    archetype :: (1 until population).map(_ => newSpecimen(archetype.size)).toList

  def evalFitness(pool: Pool): FitnessPool = pool.zip(pool.par.map(fitnessCalc).toList)

  def evolution(pool: Pool): (Pool, Int) = {
    evolution(evalFitness(pool))
  }

  @tailrec
  final def evolution(pool: FitnessPool, epoch: Int = 0, elapsedTime: Long = 0): (Pool, Int) = {
    val start = System.currentTimeMillis()
    val newGeneration = popReproduction(pool)

    if (stopCondition(newGeneration)) {
      (newGeneration.map(_._1), epoch)
    }
    else {
      val newElapsedTime = elapsedTime + (System.currentTimeMillis() - start)

      val timePerEpoch = newElapsedTime.toDouble / (epoch + 1)
      val epochsPerReport = math.max(1, (1000 / timePerEpoch).toInt)
      if (epoch % epochsPerReport == 0) {
        val usedMemory = sys.runtime.totalMemory() - sys.runtime.freeMemory()
        val (_, bestFitness) = pool.maxBy(_._2)
        println("Generation #%d - %dMB -- %1.1fms per generation -- %1.1f".format(epoch, usedMemory/1024/1024, timePerEpoch, bestFitness))
      }

      evolution(newGeneration, epoch + 1, newElapsedTime)
    }
  }

  /*
  Generate the initial population of individuals randomly - first Generation
  Evaluate the fitness of each individual in that population
  Repeat on this generation until termination (time limit, sufficient fitness achieved, etc.):
    Select the best-fit individuals for reproduction - parents
    Breed new individuals through crossover and mutation operations to give birth to offspring
    Evaluate the individual fitness of new individuals
    Replace least-fit population with new individuals
  */

  def popReproduction(pool: FitnessPool): FitnessPool = {
    val best = selectBest(pool)
    val children = breed(best, population - best.size)
    best ::: children
  }

  def normalize(pool: FitnessPool): List[NormalizedFitness] = {
    val (_, min) = pool.minBy(_._2)
    val (_, max) = pool.maxBy(_._2)
    pool.map{ case (s, f) => (s, f, (f - ((max + min) / 2)) / ((max - min) / 2)) }
  }

  def selectBest(pool: FitnessPool): FitnessPool = {
    normalize(pool).par.filter {
      case (s, f, nf) => nf > 0
    }.map {
      case (s, f, _) => (s, f)
    }.toList
  }

  import BreedingStrategies._

  def breed(pool: FitnessPool, count: Int): FitnessPool = {
    evalFitness(
      breedingStrategy match {
        case Crossover =>
          randomPairs(pool).take(count).map {
            case (a, b) => crossover(a, b)
          }.toList
        case Transposition =>
          randomIndividuals(pool).take(count).map(transpose).toList
      }
    )
  }

  def randomPairs(pool: FitnessPool): Stream[(Specimen,Specimen)] =
    (pool(Random.nextInt(pool.size))._1, pool(Random.nextInt(pool.size))._1) #:: randomPairs(pool)

  def randomIndividuals(pool: FitnessPool): Stream[Specimen] =
    pool(Random.nextInt(pool.size))._1 #:: randomIndividuals(pool)

  def crossover(a: Specimen, b: Specimen): Specimen =
    mutate(specimenBuilder(a.zip(b).map(gene =>
      if (Random.nextFloat >= 0.5) gene._1 else gene._2)))

  def transpose(s: Specimen): Specimen = {
    val si = s.toIndexedSeq
    val i1 = Random.nextInt(si.length)
    val i2 = Random.nextInt(si.length)
    mutate(specimenBuilder(si.patch(i1, si.slice(i2, i2+1), 1)))
  }

  def mutate(s: Specimen): Specimen =
    specimenBuilder(s.map(gene =>
      if (mutationRate > Random.nextFloat) randomGenes.head else gene))
}

object Solver {

  private class FitnessHistory {
    var values: List[Double] = Nil
  }

  def convergenceTest[Gene, Specimen <% Iterable[Gene]](fitnessHistory: Int) = {
    val history = new FitnessHistory;
    { (specimens: List[(Specimen,Double)]) => Boolean
      val (_, bestFitness) = specimens.maxBy(_._2)
      history.values = (bestFitness :: history.values).take(fitnessHistory)
      history.values.count(_ == bestFitness) == fitnessHistory
    }
  }

  def stagnationTest[Gene, Specimen <% Iterable[Gene]](fitnessThreshold: Double) = {
    { (specimens: List[(Specimen,Double)]) => Boolean
      val (_, bestFitness) = specimens.maxBy(_._2)
      specimens.count(_._2 == bestFitness) / specimens.size.toDouble > fitnessThreshold
    }
  }

  def timeLimitTest(limit: Long, unit: TimeUnit, start: Long = System.currentTimeMillis()): () => Boolean =
  { () =>
    System.currentTimeMillis() - start > unit.toMillis(limit)
  }

}

object BreedingStrategies extends Enumeration {
  val Crossover = Value("Crossover")
  val Transposition = Value("Transposition")

  type BreedingStrategy = Value
}
