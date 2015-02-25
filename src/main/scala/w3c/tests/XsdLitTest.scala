package w3c.tests

import w3c.xsd.XsdLiteralSyntax
import XsdLiteralSyntax._
import XsdLiteralSyntax.ops._


/**
 *
 *
 * @author Matthew Pocock
 */
object XsdLitTest {

  // note: this is broken - it's to illustrate the typeclasses
  implicit object integer extends XsdLiteralSyntax[Int] {
    val xsdType = "xsd:integer"

    override def render(lit: Int) = s"$lit^$xsdType"

    override def parse(s: String) = s.toInt
  }

  def main(args: Array[String]): Unit = {

    val rImpl = implicitly[XsdLiteralSyntax[Int]].render(42)
    println(s"Rendered using an implicitly summoning: $rImpl")

    val pImpl = implicitly[XsdLiteralSyntax[Int]].parse("42")
    println(s"Parsed using an implicit summoning: $pImpl")

    val rStx = "42"^integer
    println(s"Rendered using syntax: $rStx")

    val pStx = 42.render
    println(s"Parsed using syntax: $pStx")
  }

}