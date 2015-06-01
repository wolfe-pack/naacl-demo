package ml.wolfe.examples
import cc.factorie.la.DenseTensor1
import ml.wolfe.Index
import ml.wolfe.nlp.Document
import ml.wolfe.term.{VectorDom, TermImplicits, DiscreteDom, Dom}
import TermImplicits._

/**
 * Created by luke on 31/05/15.
 */
object TaggingExample {

  import cc.factorie.la.DenseTensor1
  def preloadWeights()(implicit index:Index, weightsDom:VectorDom, Tags: DiscreteDom[Symbol]) = new DenseTensor1(Seq(
    feature('match, 1.1, Tags.Const('I_PER) -> Tags.Const('B_PER)),
    feature('location,  1.1, Tags.Const('B_LOC)),
    feature('lastName,  0.7, Tags.Const('B_PER)),
    feature('lastName,  0.3, Tags.Const('I_PER)),
    feature('firstName, 2, Tags.Const('B_PER)),
    feature('pair, 0.9, Tags.Const('B_PER) -> Tags.Const('I_PER)),
    feature('bias, 1, Tags.Const('O))
  ).map(_.eval()).reduce(_+_))


  implicit class DocWrapper(val d:Document) {
    def withTags(tags:Seq[Symbol]) = {
      d.copy(sentences =
        (d.sentences zip d.sentences.scanLeft(0)(_ + _.tokens.length)).map {
          case (sen, off) => sen.copy(tokens = sen.tokens.zip(tags.drop(off)).map{
            case(tok, tag) => tok.copy(posTag = tag.name)
          })
        }
      )
    }
  }

}
