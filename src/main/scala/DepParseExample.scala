import ml.wolfe.term._
import org.sameersingh.htmlgen.ConverterUtils._
import org.sameersingh.htmlgen.Custom.Matrix
import org.sameersingh.htmlgen.{RawHTML, HTML}

/**
 * @author riedel
 */
object DepParseExample {

  import TermImplicits._

  def toMatrix(words:Seq[String],marginals:VarSeqDom[VarSeqDom[Bools.type]]#Marginals) = {
    val length = words.length//marginals.length.maxBy(_._2)._1
    val values = for (m <- 0 until length) yield for (h <- 0 until length) yield {
      val belief = marginals(m)(h)
      val prob = math.exp(belief(true)) / (math.exp(belief(true)) + math.exp(belief(false)))
      prob
    }
    println(values.mkString("\n"))
    Matrix(values, words, words)
  }

  implicit def matrix[M](m: Matrix[M]): HTML = {
    val indentLevel = 0
    val sb = new StringBuilder
    sb.append(indent(indentLevel) + "<table class=\"matrix\">\n")
    val cells = m.data.map(_.map(m extr _)).flatten
    val min = cells.min
    val max = cells.max
    val dimC = 250.0/(m.cols)
    val dimR = 250.0/(m.rows)
    def opacity(d: Double) = d
    sb.append(indent(indentLevel+1) + "<thead>\n")
    if(!m.colNames.isEmpty) {
      // column names
      sb.append(indent(indentLevel+1) + "<tr class=\"matrixRow\">\n")
      if(!m.rowNames.isEmpty)
        sb.append(indent(indentLevel+1) + "<th></th>\n")
      for(j <- 0 until m.cols) {
        sb.append(indent(indentLevel+2) + "<th class=\"rotate\"><div><span>%s</span></div></th>\n" format (m.colNames(j)))
      }
      sb.append(indent(indentLevel+1) + "</tr>\n")
    }
    sb.append(indent(indentLevel+1) + "</thead>\n")
    sb.append(indent(indentLevel+1) + "<tbody>\n")
    for(i <- 0 until m.rows) {
      sb.append(indent(indentLevel+1) + "<tr class=\"matrixRow\">\n") // style="width:100%%;height:%fpx" format(dimR)
      if(!m.rowNames.isEmpty)
        sb.append(indent(indentLevel+2) + "<th><div><span>%s</span></div></th>\n" format (m.rowNames(i)))
      for(j <- 0 until m.cols) {
        val o = opacity(m.cell(i,j))
        sb.append(indent(indentLevel+2) + "<td class=\"matrixCell\" style=\"opacity:%f\"/>\n" format(o)) //width:%fpx;height:100%%;

      }
      sb.append(indent(indentLevel+1) + "</tr>\n")
    }
    sb.append(indent(indentLevel+1) + "</tbody>\n")
    sb.append(indent(indentLevel) + "</table>\n")
    RawHTML(sb.mkString)
  }

}

object MLNExample extends App {

  import TermImplicits._
  import Argmaxer._

  val p = Seq('Anna, 'Bob, 'Charlie)
  val friendsSet = Set(('Anna, 'Bob),('Anna, 'Charlie)).flatMap(x => Set(x,x.swap))
  implicit val Persons = p.toDom
  val persons = p.toConst
  implicit val Friends = FullMaps(Persons, Persons, Bools)
  val friends = (for(p1 <- Persons.values; p2 <- Persons.values) yield (p1->p2)->friendsSet((p1,p2))).toMap.toConst

  @domain case class World(smokes: Pred[Symbol], cancer: Pred[Symbol])
  implicit val Worlds = World.Values(Preds(Persons), Preds(Persons))

  def mln(w: Worlds.Term): DoubleTerm = {
    import w._
    sum(persons) {
      p => -2.0 * I(cancer(p))
    } + sum(persons) {
      p => 1.0 * I(smokes(p) --> cancer(p))
    } + sum(persons) { p1 => sum(persons) { p2 =>
      1.0 * I(friends(p1, p2) --> (smokes(p1) <-> smokes(p2)))
    }}
  }
  def evidence(world: Worlds.Term) = world.smokes('Anna.toConst) //true

  implicit val mpParams = BPParameters(10)
  val mu2 = argmax(Worlds) { w => mln(w) subjectTo evidence(w) argmaxBy maxProduct}
  mu2.eval()
}
