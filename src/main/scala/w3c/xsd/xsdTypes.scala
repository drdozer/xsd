package w3c.xsd

import shapeless._
import simulacrum.typeclass
import w3c.typeclass.{SomeExist, AllImplicitly}

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
trait AtomicDatatype[DT] extends Datatype[DT]



// Primitive and derived datatypes are disjoint and covering. Every datatype is either primitive or derived.

/**
 * Witness that the datatype `DT` is a primitive type.
 *
 * See: http://www.w3.org/TR/xmlschema-2/#primitive-vs-derived
 */
trait Primitive[DT]


/**
 * Witness that the datatype `DT` is a derived type.
 *
 * See: http://www.w3.org/TR/xmlschema-2/#primitive-vs-derived
 */
trait Derived[DT]


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

// List facets:
//·length·
//·maxLength·
//·minLength·
//·enumeration·
//·pattern·
//·whiteSpace·



/**
 * Witness that a datatype can be a member of a union datatype.
 *
 * A datatype can be a type within a union if it is either atomic or a list.
 *
 * See: http://www.w3.org/TR/xmlschema-2/#union-datatypes
 *
 * @tparam UT  the type that can be within a union
 */
trait Unionable[UT]

object Unionable {
  implicit def atomicIsUninoable[A : AtomicDatatype]: Unionable[A] = new Unionable[A] {}
  implicit def listIsUnionable[L : ListDatatype]: Unionable[L] = new Unionable[L] {}
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

  type Unionable[UT] = SomeExist[AtomicDatatype[UT] :+: ListDatatype[UT] :+: CNil]


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
trait Restricted[R] extends Datatype[R] {
  /**
   * The type that this restriction is based on.
   */
  type BasedOn

  /**
   * The type of the zero or more applied facets.
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

trait BaseTypeOps[xs <: XsdAnyType] {
  trait BaseTypeOf[DT] {
    type BaseType
  }

  object BaseTypeOf {
    type Aux[DT, BT] = BaseTypeOf[DT] { type BaseType = BT }

    implicit def baseTypeOfRestricted[R, BO](implicit restricted: Restricted.AuxBO[R, BO]): BaseTypeOf.Aux[R, BO] =
      new BaseTypeOf[R] { type BaseType = BO }

    implicit def baseTypeOfUnrestricted[DT]: BaseTypeOf.Aux[DT, xs#anySimpleType] = new BaseTypeOf[DT] {
      type BaseType = xs#anySimpleType
    }
  }
}
