package pl.agh.harmonytools.algorithm.graph.builder

import org.scalatest.{FunSuite, Matchers}
import pl.agh.harmonytools.algorithm.evaluator.{Connection, ConnectionEvaluator, IRule}
import pl.agh.harmonytools.algorithm.generator.{GeneratorInput, LayerGenerator}
import pl.agh.harmonytools.algorithm.graph.builders.SingleLevelGraphBuilder
import pl.agh.harmonytools.algorithm.graph.node.NodeContent

class SingleLevelGraphBuilderTest extends FunSuite with Matchers {
  object MockGenerator extends LayerGenerator[Content, GenInput] {
    override def generate(input: GenInput): List[Content] =
      (0 to input.value).map(Content).toList
  }

  object MockEvaluator extends ConnectionEvaluator[Content] {
    override protected val connectionSize: Int             = 2
    override protected val softRules: List[IRule[Content]] = List.empty
    override protected val hardRules: List[IRule[Content]] = List.empty

    override def evaluateHardRules(connection: Connection[Content]): Boolean =
      connection.prev.value != 0 && connection.current.value != 2

    override def evaluateSoftRules(connection: Connection[Content]): Int = 0
  }

  case class Content(value: Int) extends NodeContent {
    override def isRelatedTo(other: NodeContent): Boolean = ???
  }

  case class GenInput(value: Int) extends GeneratorInput

  test("Building test") {
    val firstContent = Content(-1)
    val lastContent  = Content(-2)
    val graphBuilder = new SingleLevelGraphBuilder[Content, GenInput](firstContent, lastContent)
    graphBuilder.withGenerator(MockGenerator)
    graphBuilder.withEvaluator(MockEvaluator)
    graphBuilder.withGeneratorInput(List(1, 2, 3).map(GenInput))
    val graph = graphBuilder.build()
    graph.printEdges()
    graph.getLayers.size shouldBe 3
    graph.getLayers.foreach(layer => layer.getNodeList.count(_.getContent.value == 2) shouldBe 0)
    graph.getLayers.take(2).foreach(layer => layer.getNodeList.count(_.getContent.value == 0) shouldBe 0)
    graph.getLayers.take(2).foreach(layer => layer.getNodeList.size shouldBe 1)
    graph.getLayers.last.getNodeList.size shouldBe 3
    graph.getFirst.getContent shouldBe firstContent
    graph.getLast.getContent shouldBe lastContent
  }
}
