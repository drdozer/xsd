package w3c.xsd

import shapeless._
import w3c.typeclass.{SomeExists, AllImplicitly, Caster}

// a Datatype is: Atomic | List | Union
// a Datatype is: Primitive | Derived


/**
 * Witness that `T` is a datatype.
 *
 * For `T` to be a datatype, it must have an associated instance of `LexicalSpace[T]`
 *
 * See: http://www.w3.org/TR/xmlschema-2/#typesystem
 *
 * @author Matthew Pocock
 */
sealed trait Datatype[DT]



/**
 * Witness that `T` is an atomic datatype.
 *
 * See: http://www.w3.org/TR/xmlschema-2/#atomic
 */
trait AtomicDatatype[DT] extends Primitive[DT]


// Special, primitive and derived datatypes are disjoint and covering.
sealed trait DatatypeType[DT]

/**
 * A special (ur-) type.
 */
sealed trait Special[DT] extends DatatypeType[DT]

object Special {
  def witness[DT]: Special[DT] = new Special[DT] {}
}

/**
 * Witness that the datatype `DT` is a primitive type.
 *
 * See: http://www.w3.org/TR/xmlschema-2/#primitive-vs-derived
 */
trait Primitive[DT] extends DatatypeType[DT]


/**
 * Witness that the datatype `DT` is a derived type.
 *
 * See: http://www.w3.org/TR/xmlschema-2/#primitive-vs-derived
 */
trait Derived[DT] extends DatatypeType[DT]


sealed trait BuiltIn[DT] extends DatatypeType[DT]

object BuiltIn {
  implicit def specialIsBuiltIn[DT : Special]: BuiltIn[DT] = new BuiltIn[DT] {}
  implicit def primitiveIsBuiltIn[DT : Primitive] : BuiltIn[DT] =  new BuiltIn[DT] {}
}

trait UserDefined[DT] extends DatatypeType[DT]

/**
 * Witness that `L` is a list type, with elements of type `E`
 *
 * See: http://www.w3.org/TR/xmlschema-2/#list-datatypes
 *
 * @tparam L  the type that is an XSD list
 */
trait ListDatatype[L] extends Derived[L] {
  /**
   * The element type of this list datatype.
   */
  type E

  def elements(l: L): Seq[E]
  def fromElements(es: E*): L
}

object ListDatatype {

  type Aux[L, E0] = ListDatatype[L] { type E = E0 }

  def apply[L : Datatype, E : AtomicDatatype]
  (implicit dt: ListDatatype.Aux[L, E]): ListDatatype.Aux[L, E] = dt


}


/**
 * Witness that `U` is a union datatype, with elements of the types in the coproduct `Ts`.
 *
 * See: http://www.w3.org/TR/xmlschema-2/#union-datatypes
 *
 * @tparam U    the xsd union datatype
 */
trait UnionDatatype[U] extends Derived[U] {
  /** A coproduct of atomic types that make up this union */
  type Ts <: Coproduct

  def asCoproduct(u: U): Ts
  def fromCoproduct(ts: Ts): U
}

object UnionDatatype {

  /**
   * A datatype can be a type within a union if it is either atomic or a list.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#union-datatypes
   */
  type Unionable[UT] = SomeExists[AtomicDatatype[UT] :+: ListDatatype[UT] :+: CNil]


  type Aux[U, Ts0 <: Coproduct] = UnionDatatype[U] { type Ts = Ts0 }

  type AllUnionable[Ts <: Coproduct] = AllImplicitly[Unionable, Ts]

  def apply[U : Datatype, Ts <: Coproduct : AllUnionable]
  (implicit dt: UnionDatatype.Aux[U, Ts]): UnionDatatype.Aux[U, Ts] = dt

}



/**
 * Witness that the datatype `R` is derived by restriction.
 *
 * See: http://www.w3.org/TR/xmlschema-2/#restriction
 *
 * @tparam R
 */
trait Restricted[R] extends Derived[R] {
  /**
   * The type that this restriction is based on.
   */
  type BasedOn

  /**
   * The type of the zero or more applied facets.
   *
   * These facet applications are over the type `BasedOn`, rather than `R`.
   * For an instance of `BasedOn` to be an instance of `R`,
   *
   */
  type AppliedFacets <: HList

  /**
   * The applied facets.
   */
  def facets: AppliedFacets
}

object Restricted {
  type AuxBO[R, BO] = Restricted[R] { type BasedOn = BO }
}


/**
 * Witness that `F` is a facet.
 *
 * See: http://www.w3.org/TR/xmlschema-2/#facets
 */
sealed trait Facet[F] {
  type FA[DT] <: FacetApplication[F, DT]
}

/**
 * Witness that `F` is a fundamental facet.
 *
 * See: http://www.w3.org/TR/xmlschema-2/#fundamental-facets
 */
trait FundamentalFacet[F] extends Facet[F]

/** Witness that `F` is a constraint facet.
  *
  * See: http://www.w3.org/TR/xmlschema-2/#non-fundamental
  */
trait ConstrainingFacet[F] extends Facet[F]


// F : Facet
// DT : Datatype
trait FacetApplication[F, DT]
{
  type requires
}

trait RestrictionApplication[F, DT] extends FacetApplication[F, DT]
{
  type restrictionValue
  val byValue: restrictionValue
}

object RestrictionApplication {
  type Aux[F, DT, RV] = RestrictionApplication[F, DT] { type restrictionValue = RV }
}
