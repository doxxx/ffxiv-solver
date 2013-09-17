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
    if (epoch % 10 == 0) {
      println(s"Generation #$epoch")
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

  type MatePool = List[(Specimen, Double)]

  def matePool(pool: Pool): MatePool = {
    val fitnesses = pool.map(fitnessF).toArray
    pool.zip(renormalize(fitnesses))
  }

  def renormalize(vector: Array[Double]): Array[Double] = {
    val sum = vector.sum
    vector.map(_ / sum)
  }

  def popReproduction(pool: Pool): Pool = {
    val mp = matePool(pool)
    (1 to population).par.map(_ =>
      crossover(monteCarlo(mp), monteCarlo(mp))).toList
  }

  def monteCarlo[A](weightedList: List[(A, Double)]): A =
    weightedList(Random.nextInt(weightedList.length)) match {
      case (s, f) if f > Random.nextFloat => s
      case _ => monteCarlo(weightedList)
    }

  def crossover(a: Specimen, b: Specimen): Specimen =
    mutate(specimenBuilder(a.zip(b).map(gene =>
      if (Random.nextFloat >= 0.5) gene._1 else gene._2)))

  def mutate(s: Specimen): Specimen =
    specimenBuilder(s.map(gene =>
      if (mutationRate > Random.nextFloat) randomGenes.head else gene))
}
