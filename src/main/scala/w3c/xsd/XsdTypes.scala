package w3c.xsd

import shapeless.PolyDefns.{~>, ->}
import shapeless.ops.coproduct
import shapeless.ops.coproduct.Folder
import shapeless.ops.hlist.Mapper
import shapeless._
import simulacrum.typeclass
import w3c.typeclass.{ZipApply, AllImplicitly}

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

  def listLexicalSpace[L, E]
  (implicit
   listOf: ListDatatype[L, E],
   eLex: LexicalSpace[E]): LexicalSpace[L] = new LexicalSpace[L] {
    // render a list by rendering the elements, separated by a string
    override def render(dt: L) = listOf.elements(dt).map(eLex render _).mkString(" ")

    // parse a list by splitting on elements and parsing those
    override def parse(s: String) = {
      val parsed = s.split("""\s+""").map(eLex parse _)
      val (errors, successes) = parsed.partition(_.isLeft)
      if(errors.isEmpty) Right(listOf.fromElements(successes.map(_.right.get) :_*))
      else Left(errors.flatMap(_.left.get))
    }
  }

  type fx[T] = T => String
  object toRender extends (LexicalSpace ~> fx) {
    def apply[DT](ls: LexicalSpace[DT]): DT => String = (dt: DT) => ls.render(dt)
  }

  def unionLexicalSpace[U, Ts <: Coproduct, Is <: HList, Fs <: HList, As <: Coproduct]
  (implicit
   unionDatatype: UnionDatatype[U, Ts],
   lexicalSpaces: AllImplicitly.Aux[LexicalSpace, Ts, Is],
   mapper: Mapper.Aux[toRender.type, Is, Fs],
   zipApply: ZipApply.Aux[Fs, Ts, As],
   folder: Folder.Aux[PolyDefns.identity.type, As, String]): LexicalSpace[U] = new LexicalSpace[U]
  {
    override def render(dt: U) = {
      val renderers = lexicalSpaces.out.map(toRender)
      val dtCP = unionDatatype.asCoproduct(dt)
      val zipped = zipApply(renderers, dtCP)
      folder.apply(zipped)
    }

    override def parse(s: String) = ???
  }

  type Result[T] = Either[Seq[String], T]

  implicit class LexicalSyntax(val _s: String) extends AnyVal {
    def ^[DT](syntax: LexicalSpace[DT]): Result[DT] = syntax.parse(_s)
  }

}


/**
 * A facet
 * @tparam DT
 */
trait Facet[DT]

trait FundamentalFacet[DT] extends Facet[DT]

trait Constraint[DT] extends Facet[DT]

/**
 * Witness that `T` is a datatype.
 *
 * For `T` to be a datatype, it must have an associated instance of `LexicalSpace[T]`
 *
 * See: http://www.w3.org/TR/xmlschema-2/#typesystem
 *
 * @author Matthew Pocock
 */
trait Datatype[T]


/**
 * Witness that `T` is an atomic datatype.
 *
 * See: http://www.w3.org/TR/xmlschema-2/#atomic
 *
 * @tparam T
 */
trait AtomicDatatype[T]

/**
 * Witness that `L` is a list type, with elements of type `E`
 *
 * See: http://www.w3.org/TR/xmlschema-2/#list-datatypes
 *
 * @tparam L  the type that is an XSD list
 * @tparam E  the element type of the list
 */
trait ListDatatype[L, E] {
  def size: Int
  def elements(l: L): Seq[E]
  def fromElements(es: E*): L
}

object ListDatatype {
  def apply[L : Datatype, E : AtomicDatatype]
  (implicit dt: ListDatatype[L, E]): ListDatatype[L, E] = dt
}


/**
 * Witness that `U` is a union datatype, with elements of the types in the coproduct `Ts`.
 *
 * See: http://www.w3.org/TR/xmlschema-2/#union-datatypes
 *
 * @tparam U    the xsd union datatype
 * @tparam Ts   a coproduct of atomic types that make up this union
 */
trait UnionDatatype[U, Ts <: Coproduct] {
  def asCoproduct(u: U): Ts
}

object UnionDatatype {

  type AllAtomicDatatype[Ts <: Coproduct] = AllImplicitly[AtomicDatatype, Ts]

  def apply[U : Datatype, Ts <: Coproduct : AllAtomicDatatype]
  (implicit dt: UnionDatatype[U, Ts]): UnionDatatype[U, Ts] = dt


}