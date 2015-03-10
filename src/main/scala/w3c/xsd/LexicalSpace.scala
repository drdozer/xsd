package w3c.xsd

import shapeless._
import shapeless.PolyDefns.~>
import shapeless.Coproduct
import shapeless.ops.{coproduct, hlist}
import simulacrum._
import w3c.typeclass.{StringFailure, FailureTree, Caster, AllImplicitly}

import scalaz._
import Scalaz._
import FailureTree._

/**
 * API for repersenting a lexical space.
 *
 * See: http://www.w3.org/TR/xmlschema11-2/#lexical-space
 */
trait LexicalSpace[xs <: SpecialAndPrimitiveTypes] {

  self : Datatypes[xs] =>
  
  @typeclass trait Literal[L <: xs#literal] {
    @op("^") def parseLiteral[DT](lit: L)(implicit syntax: LexicalMapping[DT]): Validation[FailureTree, DT] =
      syntax.parse(lit)
    @op("^^") def parseLiteral0[DT](lit: L)(implicit syntax: LexicalMapping[DT], hasQName: HasQName[DT]): DT =
      parseLiteral(lit)(syntax).fold(
        f => throw new IllegalStateException(s"Failed to parse $lit with the lexical space for ${qnameOf[DT]}: " + f),
        dt => dt
      )

    def literalString(lit: L): String
    def apply(s: String): xs#literal
  }

  implicit class LiteralStrings(sc: StringContext) {
    def lit(args: Any*) = literalLiteral(sc.s(args: _*))
  }

  import Literal.ops._

  implicit def literalLiteral: Literal[xs#literal]

  /**
   * A lexical mapping for the datatype `DT`.
   *
   * A lexical mapping is a subset of all strings of characters. The subset is defined by those strings for which `parse`
   * succeeds.
   *
   * see: http://www.w3.org/TR/xmlschema-2/#lexical-space
   *
   * @author Matthew Pocock
   */
  @typeclass trait LexicalMapping[DT] {
    def render(dt: DT): xs#literal
    def parse(lit: xs#literal): Validation[FailureTree, DT]

    trait LexicalMappingLaw {
      protected val valueSpace: ValueSpace[xs]
      protected val saptd: SpecialAndPrimitiveTypesDeclarations[xs]

      import valueSpace._
      import saptd._

      def parseRenderSucceedEqual(dt: DT)(implicit eq: Equality[DT, DT]): Boolean =
        parse(render(dt)).fold(
          f => false,   // should not fail
          s => eq.equal(s, dt) == booleanValueSpace.trueValue) // should be equal to the input

      def parseRenderParseRenderIdentity(dt: DT)(idt: Identity[DT, DT]): Boolean = {
        val pr = parse(render(dt))
        val prpr = pr flatMap (x => parse(render(x)))

        pr.fold(
          f => false, // should not fail the first time
          prS => prpr.fold(
            f => false, // should not fail the second time
            prprS => idt.identical(prS, prprS) == booleanValueSpace.trueValue // should be identical as cannonical
          )
        )
      }
    }

    def lexicalMapplingLaw(implicit
                           _valueSpace: ValueSpace[xs],
                           _saptd: SpecialAndPrimitiveTypesDeclarations[xs]) =
      new LexicalMappingLaw {
        override protected val valueSpace: ValueSpace[xs] = _valueSpace
        override protected val saptd: SpecialAndPrimitiveTypesDeclarations[xs] = _saptd
      }
  }

  /**
   * A type has a qname associated with it.
   */
  @typeclass trait HasQName[T] {
    def qname: xs#QName
  }

  object HasQName {
    def apply[T](qname: String)
                (implicit
                 qnameLexicalMapping: LexicalMapping[xs#QName],
                 qnameHasQName: HasQName[xs#QName]): HasQName[T] =
      apply[T](literalLiteral(qname).^^[xs#QName])

    def apply[T](qn: xs#QName): HasQName[T] =
    {
      new HasQName[T] {
        override def qname = qn
      }
    }
  }

  /**
   * Summon the qname for a type.
   */
  def qnameOf[T](implicit hq: HasQName[T]): xs#QName = hq.qname


  /**
   * A lexical space that represents space-separated values from the underlying list element's lexical space.
   */
  def listLexicalMapping[L, E]
  (implicit
   lhq: HasQName[L],
   ehq: HasQName[E],
   listValueSpace: ListValueSpace[L, E],
   eLex: LexicalMapping[E]): LexicalMapping[L] = new LexicalMapping[L] {
    // render a list by rendering the elements, separated by a string
    override def render(dt: L) = literalLiteral(listValueSpace.elements(dt).map(eLex render _).mkString(" "))

    // parse a list by splitting on elements and parsing those
    override def parse(lit: xs#literal) = {
      val allParsed = lit.literalString.split( """\s+""").map(s => eLex parse literalLiteral(s)).to[scala.collection.immutable.List]

      val withErrorIndexes = allParsed.zipWithIndex.map {
        case (p, i) => p.leftMap(f => (s"Parse failed at $i", f :: Nil).failureTree.wrapNel)
      }

      val combined = withErrorIndexes.sequence[({type l[a] = ValidationNel[FailureTree, a]})#l, E]

      combined.map(listValueSpace.fromElements(_ :_*)).leftMap(fs =>
        StringFailure(s"Could not parse ${qnameOf[L]} as list of ${qnameOf[E]}", fs.list))
    }
  }

  type fx[T] = T => xs#literal
  object toRender extends (LexicalMapping ~> fx) {
    def apply[DT](ls: LexicalMapping[DT]): DT => xs#literal =
      (dt: DT) => ls.render(dt)
  }

  type ft[X] = xs#literal => Validation[FailureTree, X]
  object toParse extends (LexicalMapping ~> ft) {
    def apply[DT](ls: LexicalMapping[DT]): xs#literal => Validation[FailureTree, DT] =
      (lit: xs#literal) => ls.parse(lit)
  }

  object mergeResults extends Poly {
    implicit def caseRR[T, Ts <: Coproduct] = use((lhs: Validation[FailureTree, T], rhs: ValidationNel[FailureTree, Ts]) =>
      lhs.leftMap(_.wrapNel) findSuccess rhs)
  }

  /**
   * A lexical space that represents the union of some memberTypes.
   */
  def unionLexicalMapping[
  U, Ts <: Coproduct, Is <: HList,
  RendFs <: HList, As <: Coproduct,
  ParseFs <: HList, Strings <: HList, Parseds <: HList]
  (implicit
   unionValueSpace: UnionValueSpace[U, Ts],
   lexicalSpaces: AllImplicitly.Aux[LexicalMapping, Ts, Is],
   renderMapper: hlist.Mapper.Aux[toRender.type, Is, RendFs],
   zipRenderers: w3c.typeclass.ZipApply.Aux[RendFs, Ts, As],
   foldOutLiteral: coproduct.Folder.Aux[PolyDefns.identity.type, As, xs#literal],
   parseMapper: hlist.Mapper.Aux[toParse.type, Is, ParseFs],
   constMapper: hlist.ConstMapper.Aux[xs#literal, ParseFs, Strings],
   zipParsers: shapeless.ops.hlist.ZipApply.Aux[ParseFs, Strings, Parseds],
   foldOutParseResult: hlist.RightFolder.Aux[
     Parseds, ValidationNel[FailureTree, CNil], mergeResults.type, Validation[FailureTree, Ts]]): LexicalMapping[U] =
    new LexicalMapping[U]
    {
      override def render(dt: U) = {
        val renderers = renderMapper(lexicalSpaces.out)
        val dtCP = unionValueSpace.asCoproduct(dt)
        val rendered = zipRenderers(renderers, dtCP)
        foldOutLiteral.apply(rendered)
      }

      override def parse(lit: xs#literal) = {
        val parsers = parseMapper(lexicalSpaces.out)
        val ss = constMapper(lit, parsers)
        val parsed = zipParsers.apply(parsers, ss)
        val ts = foldOutParseResult.apply(parsed, "No lexical space found".failureTree.failureNel[CNil])
        ts.map(unionValueSpace.fromCoproduct)
      }
    }

  def restrictionLexicalMapping[
  R, BO]
  (implicit
   restricted: Restricted.AuxBO[R, BO],
   lexicalSpaceBO: LexicalMapping[BO],
   caster: Caster.Aux[BO, R]): LexicalMapping[R] = new LexicalMapping[R] {
    override def render(dt: R) = lexicalSpaceBO.render(caster.upcast(dt))

    override def parse(lit: xs#literal) = lexicalSpaceBO.parse(lit) flatMap caster.downcast
  }

}
