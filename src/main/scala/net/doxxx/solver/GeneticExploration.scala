package net.doxxx.solver

import scala.util.Random
import scala.annotation.tailrec

class GeneticExploration[Gene, Specimen <% Iterable[Gene]]
(val mutationRate: Double,
 val population: Int,
 genePool: Array[Gene],
 specimenBuilder: Iterable[Gene] => Specimen,
 fitnessF: Specimen => Double,
 stopCondition: List[Specimen] => Boolean)
{
  def randomGenes: Stream[Gene] = genePool(Random.nextInt(genePool.length)) #:: randomGenes
  def newSpecimen(len: Int): Specimen = specimenBuilder(randomGenes.take(len))

  type Pool = List[Specimen]

  def randomPool(archetype: Specimen): Pool =
    archetype :: (1 until population).map(_ => newSpecimen(archetype.size)).toList

  @tailrec
  final def evolution(pool: Pool, epoch: Int = 0): (Pool, Int) = {
    if (epoch % 100 == 0) {
      val usedMemory = sys.runtime.totalMemory() - sys.runtime.freeMemory()
      println(s"Generation #$epoch - ${usedMemory/1024/1024}MB")
    }
    val newGeneration = popReproduction(pool)
    if (stopCondition(newGeneration)) (newGeneration, epoch)
    else evolution(newGeneration, epoch + 1)
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

  def popReproduction(pool: Pool): Pool = {
    val fitnessPool = evalFitness(pool)
    val best = selectBest(fitnessPool)
    val parents = best.zip(Random.shuffle(best))
    val children = breed(parents)
    val specimenSize = pool.head.size // TODO: Select new specimen size differently?
    val randomNewSpecimens = (1 to (population - children.size)).map { _ => newSpecimen(specimenSize) }.toList
    children ::: randomNewSpecimens
  }

  type Fitness = (Specimen, Double)
  type FitnessPool = List[Fitness]

  def evalFitness(pool: Pool): FitnessPool = pool.zip(normalize(pool.par.map(fitnessF).toList))

  def normalize(values: Seq[Double]): Seq[Double] = {
    val min = values.min
    val max = values.max
    values.map(v => (v - ((max + min) / 2)) / ((max - min) / 2))
  }

  def selectBest(pool: FitnessPool): Pool = {
    pool.par.filter {
      case (s, f) => f > 0
    }.map {
      case (s, f) => s
    }.toList
  }

  def breed(parents: List[(Specimen, Specimen)]): Pool = {
    parents.map {
      case (a, b) => crossover(a, b)
    }
  }

  def crossover(a: Specimen, b: Specimen): Specimen =
    mutate(specimenBuilder(a.zip(b).map(gene =>
      if (Random.nextFloat >= 0.5) gene._1 else gene._2)))

  def mutate(s: Specimen): Specimen =
    specimenBuilder(s.map(gene =>
      if (mutationRate > Random.nextFloat) randomGenes.head else gene))
}
