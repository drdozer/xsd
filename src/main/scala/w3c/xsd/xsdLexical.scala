package w3c.xsd

import shapeless._
import shapeless.PolyDefns.~>
import shapeless.Coproduct
import shapeless.ops.{coproduct, hlist}
import simulacrum.typeclass
import w3c.typeclass.{StringFailure, FailureTree, Caster, AllImplicitly}

import scalaz._
import Scalaz._
import FailureTree._


/**
 * A lexical space for the datatype `DT`.
 *
 * A lexical space is a subset of all strings of characters. The subset is defined by those strings for which `parse`
 * succeeds.
 *
 * see: http://www.w3.org/TR/xmlschema-2/#lexical-space
 *
 * @author Matthew Pocock
 */
@typeclass trait LexicalSpace[DT] {
  def render(dt: DT): String
  def parse(s: String): Validation[FailureTree, DT]

  // laws:
  // parse(render(dt)) === dt # data round-trips to the lexical scope
  // parse(render(parse(s))) = parse(s) # lexicals are equivalent through their associated value
}

object LexicalSpace {

  implicit class LexicalSyntax(val _s: String) extends AnyVal {
    def ^[DT](implicit syntax: LexicalSpace[DT]): Validation[FailureTree, DT] = syntax.parse(_s)
    def ^^[DT](implicit syntax: LexicalSpace[DT]): DT = ^(syntax).fold(
      f => throw new IllegalStateException(s"Failed to parse ${_s} with the lexical space ${syntax}: " + f),
      dt => dt
    )
  }


}

trait WithLexicalSpace[xs <: XsdAnyType with XsdBuiltIn] {

  protected val namedTypes: NamedTypes[xs]
  import namedTypes._


  /**
   * A lexical space that represents space-separated values from the underlying list element's lexical space.
   */
  def listLexicalSpace[L, E]
  (implicit
   lhq: HasQName[L],
   ehq: HasQName[E],
   listOf: ListDatatype.Aux[L, E],
   eLex: LexicalSpace[E]): LexicalSpace[L] = new LexicalSpace[L] {
    // render a list by rendering the elements, separated by a string
    override def render(dt: L) = listOf.elements(dt).map(eLex render _).mkString(" ")

    // parse a list by splitting on elements and parsing those
    override def parse(s: String) = {
      val allParsed = s.split( """\s+""").map(eLex parse _).to[List]

      val withErrorIndexes = allParsed.zipWithIndex.map {
        case (p, i) => p.leftMap(f => (s"Parse failed at $i", f :: Nil).failureTree.wrapNel)
      }

      val combined = withErrorIndexes.sequence[({type l[a] = ValidationNel[FailureTree, a]})#l, E]

      combined.map(listOf.fromElements(_ :_*)).leftMap(fs =>
        StringFailure(s"Could not parse ${qnameOf[L]} as list of ${qnameOf[E]}", fs.list))
    }
  }

  type fx[T] = T => String
  object toRender extends (LexicalSpace ~> fx) {
    def apply[DT](ls: LexicalSpace[DT]): DT => String = (dt: DT) => ls.render(dt)
  }

  type ft[X] = String => Validation[FailureTree, X]
  object toParse extends (LexicalSpace ~> ft) {
    def apply[DT](ls: LexicalSpace[DT]): String => Validation[FailureTree, DT] = (s: String) => ls.parse(s)
  }

  object mergeResults extends Poly {
    implicit def caseRR[T, Ts <: Coproduct] = use((lhs: Validation[FailureTree, T], rhs: ValidationNel[FailureTree, Ts]) =>
      lhs.leftMap(_.wrapNel) findSuccess rhs)
  }

  /**
   * A lexical space that represents the union of some memberTypes.
   */
  def unionLexicalSpace[
  U, Ts <: Coproduct, Is <: HList,
  RendFs <: HList, As <: Coproduct,
  ParseFs <: HList, Strings <: HList, Parseds <: HList]
  (implicit
   unionDatatype: UnionDatatype.Aux[U, Ts],
   lexicalSpaces: AllImplicitly.Aux[LexicalSpace, Ts, Is],
   renderMapper: hlist.Mapper.Aux[toRender.type, Is, RendFs],
   zipRenderers: w3c.typeclass.ZipApply.Aux[RendFs, Ts, As],
   foldOutString: coproduct.Folder.Aux[PolyDefns.identity.type, As, String],
   parseMapper: hlist.Mapper.Aux[toParse.type, Is, ParseFs],
   constMapper: hlist.ConstMapper.Aux[String, ParseFs, Strings],
   zipParsers: shapeless.ops.hlist.ZipApply.Aux[ParseFs, Strings, Parseds],
   foldOutParseResult: hlist.RightFolder.Aux[
     Parseds, ValidationNel[FailureTree, CNil], mergeResults.type, Validation[FailureTree, Ts]]): LexicalSpace[U] =
    new LexicalSpace[U]
    {
      override def render(dt: U) = {
        val renderers = renderMapper(lexicalSpaces.out)
        val dtCP = unionDatatype.asCoproduct(dt)
        val rendered = zipRenderers(renderers, dtCP)
        foldOutString.apply(rendered)
      }

      override def parse(s: String) = {
        val parsers = parseMapper(lexicalSpaces.out)
        val ss = constMapper(s, parsers)
        val parsed = zipParsers.apply(parsers, ss)
        val ts = foldOutParseResult.apply(parsed, "No lexical space found".failureTree.failureNel[CNil])
        ts.map(unionDatatype.fromCoproduct)
      }
    }

  def restrictionLexicalSpace[
  R, BO]
  (implicit
   restricted: Restricted.AuxBO[R, BO],
   lexicalSpaceBO: LexicalSpace[BO],
   caster: Caster.Aux[BO, R]): LexicalSpace[R] = new LexicalSpace[R] {
    override def render(dt: R) = lexicalSpaceBO.render(caster.upcast(dt))

    override def parse(s: String) = lexicalSpaceBO.parse(s) flatMap caster.downcast
  }

}
