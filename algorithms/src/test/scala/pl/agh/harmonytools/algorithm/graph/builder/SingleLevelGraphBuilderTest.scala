package pl.agh.harmonytools.algorithm.graph.builder

import org.scalatest.{FunSuite, Matchers}
import pl.agh.harmonytools.algorithm.evaluator.{Connection, ConnectionEvaluator, IRule}
import pl.agh.harmonytools.algorithm.generator.LayerGenerator
import pl.agh.harmonytools.algorithm.graph.builders.SingleLevelGraphBuilder

class SingleLevelGraphBuilderTest extends FunSuite with Matchers {
  object MockGenerator extends LayerGenerator[Int, Int] {
    override def generate(input: Int): List[Int] =
      (0 to input).toList
  }

  object MockEvaluator extends ConnectionEvaluator[Int] {
    override protected val connectionSize: Int         = 2
    override protected val softRules: List[IRule[Int]] = List.empty
    override protected val hardRules: List[IRule[Int]] = List.empty

    override def evaluateHardRules(connection: Connection[Int]): Boolean =
      connection.prev != 0 && connection.current != 2

    override def evaluateSoftRules(connection: Connection[Int]): Int = 0
  }

  test("Building test") {
    val firstContent = -1
    val lastContent  = -2
    val graphBuilder = new SingleLevelGraphBuilder[Int, Int](firstContent, lastContent)
    graphBuilder.withGenerator(MockGenerator)
    graphBuilder.withEvaluator(MockEvaluator)
    graphBuilder.withGeneratorInput(List(1, 2, 3))
    val graph = graphBuilder.build()
    graph.printEdges()
    graph.getLayers.size shouldBe 3
    graph.getLayers.foreach(layer => layer.getNodeList.count(_.getContent == 2) shouldBe 0)
    graph.getLayers.take(2).foreach(layer => layer.getNodeList.count(_.getContent == 0) shouldBe 0)
    graph.getLayers.take(2).foreach(layer => layer.getNodeList.size shouldBe 1)
    graph.getLayers.last.getNodeList.size shouldBe 3
    graph.getFirst.getContent shouldBe firstContent
    graph.getLast.getContent shouldBe lastContent
  }
}
