package w3c.xsd

import shapeless._
import shapeless.PolyDefns.~>
import shapeless.ops.{coproduct, hlist}
import simulacrum.typeclass
import w3c.typeclass.AllImplicitly

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
  def parse(s: String): LexicalSpace.Result[DT]

  // laws:
  // parse(render(dt)) === dt # data round-trips to the lexical scope
  // parse(render(parse(s))) = parse(s) # lexicals are equivalent through their associated value
}

object LexicalSpace {

  /**
   * A lexical space that represents space-separated values from the underlying list element's lexical space.
   */
  def listLexicalSpace[L, E]
  (implicit
   listOf: ListDatatype.Aux[L, E],
   eLex: LexicalSpace[E]): LexicalSpace[L] = new LexicalSpace[L] {
    // render a list by rendering the elements, separated by a string
    override def render(dt: L) = listOf.elements(dt).map(eLex render _).mkString(" ")

    // parse a list by splitting on elements and parsing those
    override def parse(s: String) = {
      val parsed = s.split("""\s+""").map(eLex parse _).to[Seq]
      val (errors, successes) = parsed.partition(_.isLeft)
      if(errors.isEmpty) Right(listOf.fromElements(successes.map(_.right.get) :_*))
      else Left(errors.flatMap(_.left.get))
    }
  }

  type fx[T] = T => String
  object toRender extends (LexicalSpace ~> fx) {
    def apply[DT](ls: LexicalSpace[DT]): DT => String = (dt: DT) => ls.render(dt)
  }

  type ft[X] = String => Result[X]
  object toParse extends (LexicalSpace ~> ft) {
    def apply[DT](ls: LexicalSpace[DT]): String => Result[DT] = (s: String) => ls.parse(s)
  }

  object mergeResults extends Poly {
    implicit def caseRR[T, Ts <: Coproduct] = use((lhs: Result[T], rhs: Result[Ts]) => (lhs, rhs) match {
      case (Left(tErrs), Left(tsErrs)) => Left(tErrs ++ tsErrs)
      case (Left(_), Right(ts)) => Right(Inr(ts))
      case (Right(t), _) => Right(Inl(t))
    })
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
    foldOutParseResult: hlist.RightFolder.Aux[Parseds, Result[CNil], mergeResults.type, Result[Ts]]): LexicalSpace[U] = new LexicalSpace[U]
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
      val ts = foldOutParseResult.apply(parsed, Left(Seq()))
      ts.right.map(unionDatatype.fromCoproduct)
    }
  }

  type Result[T] = Either[Seq[String], T]

  implicit class LexicalSyntax(val _s: String) extends AnyVal {
    def ^[DT](syntax: LexicalSpace[DT]): Result[DT] = syntax.parse(_s)
  }

}
