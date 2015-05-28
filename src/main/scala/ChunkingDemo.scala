import ml.wolfe._
import ml.wolfe.nlp.{Sentence, Document}
import ml.wolfe.nlp.io.CoNLLReader
import ml.wolfe.term.Argmaxer._
import ml.wolfe.term.LearningObjective._
import ml.wolfe.term.TermImplicits._
import ml.wolfe.term._
import ml.wolfe.ui.D3FG

object ChunkingDemo extends App {
  val n = 5
  val Y = Seqs(Bools, 0, n)
  def model(length: IntTerm)(y: Y.Term) = {
    sum(0 until length) { i => I(y(i))} +
      sum(0 until length - 1) { i => I(y(i) <-> ! y(i + 1))}
  }
  val mpParams = MaxProductParameters(10)
  val result = argmax(Y)(y => model(5)(y) subjectTo (y.length === 5) argmaxBy maxProduct(mpParams)).evalResult()
  D3FG.display(result.factorGraphs.head)
}