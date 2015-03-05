package w3c.tests

import w3c.typeclass.{ThrowableFailure, FailureTree}
import w3c.xsd._
import LexicalMapping._
import LexicalMapping.ops._
import scala.util.Try
import scalaz._
import Scalaz._
import FailureTree._

/**
 *
 *
 * @author Matthew Pocock
 */
object XsdLitTest {

//  // note: this is broken - it's to illustrate the typeclasses
//  implicit object integer extends LexicalMapping[Int] {
//    val xsdType = "xsd:integer"
//
//    override def render(lit: Int) = s"$lit^$xsdType"
//
//    override def parse(s: String) = Try { s.toInt } match {
//      case scala.util.Success(i) => i.success
//      case scala.util.Failure(t) => t.failureTree.failure
//    }
//  }
//
//  def main(args: Array[String]): Unit = {
//
//    val rImpl = implicitly[LexicalMapping[Int]].render(42)
//    println(s"Rendered using an implicitly summoning: $rImpl")
//
//    val pImpl = implicitly[LexicalMapping[Int]].parse("42")
//    println(s"Parsed using an implicit summoning: $pImpl")
//
//    val rStx = "42"^integer
//    println(s"Rendered using syntax: $rStx")
//
//    val pStx = 42.render
//    println(s"Parsed using syntax: $pStx")
//
//    val nonString = "hiMum"^integer
//    println(s"Parsed non-string as string: $nonString")
//  }

}
