import ml.wolfe.term._
import ml.wolfe.term.TermImplicits._
import org.sameersingh.htmlgen.{HTML, RawHTML}
import ml.wolfe.ui.Json._

import scala.util.Random

/**
 * @author Luke Hewitt
 */
object D3FG {

  val fgScriptLocation = "/assets/javascripts/factorgraph/fg.js"

  private def wrapCode(id: String, fgData:String): HTML = {
    val script =
      s"""
         |<script type="text/javascript">
         |             var fgScriptLocation;
         |             if (typeof fgScriptLocation === 'undefined') {
         |                fgScriptLocation = "$fgScriptLocation";
         |                head.js(fgScriptLocation);
         |             }
         |             head.ready(function() {
         |                var fgData = ${indent(29, fgData)};
         |                FG.create("$id", fgData);
         |             });
         |    </script>
      """.stripMargin

    val html =
      s"""
         | <div id="$id"></div>
         |
         | <link rel="stylesheet" type="text/css" href="/assets/stylesheets/factorgraph/fg.css"/>
         |$script
      """.stripMargin
    println(html)
    RawHTML(html)
  }

  def display[NodeContent, EdgeContent, FactorContent](fg: FG[NodeContent, EdgeContent, FactorContent]) = {

    val nodesSeq = fg.nodes.toList
    val nodesIndex = nodesSeq.map(_._2).zipWithIndex.toMap
    val nodes = nodesSeq.map{case (atom, node) =>
      toJson(Map(
        "type"      -> toJson("node"),
        "text"      -> toJson(""/*atom.toString*/),
        "hoverhtml" -> toJson(msgString(atom,
          node.content.asInstanceOf[MaxProductBP#NodeContent].belief)
        ),
        "x"         -> toJson(Math.random()*600),
        "y"         -> toJson(Math.random()*400),
        "fixed"     -> toJson(false)
      ))
    }
    val nNodes = fg.nodes.size

    val factorsSeq = fg.factors.toList
    val factorsIndex = factorsSeq.zipWithIndex.map{ case((t, f), i) => (f, i+nNodes)}.toMap
    val factors = factorsSeq.map{case (term, factor) =>
      toJson(Map(
        "type"      -> "factor",
        "hoverhtml" -> term.toString
      ))
    }

    val edgesSeq = fg.edges.toList
    val edgesIndex = edgesSeq.zipWithIndex.toMap
    val links = edgesSeq.map { e =>
      toJson(Map(
        "target" -> nodesIndex(e.node),
        "source" -> factorsIndex(e.factor)
      ))
    }

    val schedule = fg.messageHistory.take(50).map { ms =>
      toJson(ms.map{ m =>
        toJson(Map(
          "edge"      -> toJson(edgesIndex(m.edge)),
          "direction" -> toJson(m.direction),
          "msg"       -> toJson(msgString(m.edge.node.variable, m.message))
        ))
      })
    }

    val data = toJson(Map(
      "width" -> toJson(650),
      "height" -> toJson(250),
      "graph" -> toJson(Map(
        "nodes" -> (nodes ++ factors),
        "links" -> links
      )),
      "schedule" -> toJson(schedule)
    ))

    val id = "fg" + Random.nextLong.toString
    wrapCode(id, data.str)
  }

  def msgString(atom:AnyGroundAtom, msg: Msg): String = {
    msg.disc.map(discMsg => {
      "<table class=\'potentialtable\'>" +
        "<tr><td><i>" + atom.toString + "</i></td><td></td></tr>" +
        discMsg.msg.zipWithIndex.map { case (m, i) =>
          "<tr><td>" + atom.domain.indexToValue(i) + "</td><td>" + m + "</td></tr>"
        }.reduce(_+_) +
        "</table>"
    }).reduce(_+_)
  }



  def main(args:Array[String]) {
    import ml.wolfe.term.TermImplicits._
    import Argmaxer._

    val n = 5
    val Y = Seqs(Bools, 0, n)
    def model(length: IntTerm)(y: Y.Term) = {
      sum(0 until length) { i => I(y(i))} +
        sum(0 until length - 1) { i => I(y(i) <-> ! y(i + 1))}
    }
    val mpParams = MaxProductParameters(1)
    val result = argmax(Y)(y => model(5)(y) subjectTo (y.length === 5) argmaxBy maxProduct(mpParams)).evalResult()

    val fg = result.factorGraphs.head
    display(fg)
  }



}