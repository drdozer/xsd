package w3c.xsd

import shapeless._
import shapeless.ops.hlist.HNilHKernel
import simulacrum.{op, typeclass}
import w3c.typeclass.{AllExist, SomeExist, Caster}

/**
 * The xsd fundamental facets.
 *
 * http://www.w3.org/TR/xmlschema-2/#rf-fund-facets
 */
trait Facets {

  /*
   * The xsd fundamental facets.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#rf-fund-facets
   */
  type equal
  type ordered
  type bounded
  type cardinality
  type numeric

  /*
   * The xsd constraint facets.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#rf-facets
   */
  type length
  type minLength
  type maxLength
  type pattern
  type enumeration
  type whiteSpace
  type maxInclusive
  type maxExclusive
  type minExclusive
  type minInclusive
  type totalDigits
  type fractionDigits

}

trait FactesProvider[ff <: Facets, xs <: XsdAnyType with XsdBuiltIn] {

  implicit val baseTypeOps: BaseTypeOps[xs]
  import baseTypeOps._

  /**
   * The equality fundamental facet.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#equal
   */
  implicit def equalIsFundamentalFacet: EqualFacet
  type EqualFacet = FundamentalFacet[ff#equal] {
    type FA[DT] <: FacetApplication[ff#equal, DT] {
      type requires = Equality[DT]
    }
  }

  /**
   * Equality typeclass.
   *
   * This is defined for all datatypes.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#equal
   */
  @typeclass trait Equality[T] {
    @op("===") def equal(lhs: T, rhs: T): Boolean
    @op("!==") def notEqual(lhs: T, rhs: T): Boolean
  }

  object Equality {

    /**
     * The equality of a restricted datatype is consistent with the equality of the datatype that was restricted.
     *
     * Any custom provider of equality must be consistent with this definition.
     */
    implicit def restrictedEquality[R, DF]
    (implicit res: Restricted.AuxBO[R, DF], eqDF: Equality[DF], caster: Caster[DF, R]): Equality[R] =
      new Equality[R] {
        @op("===")
        override def equal(lhs: R, rhs: R) = eqDF.equal(caster.upcast(lhs), caster.upcast(rhs))

        @op("!==")
        override def notEqual(lhs: R, rhs: R) = eqDF.notEqual(caster.upcast(lhs), caster.upcast(rhs))
      }
  }

  trait EqualityFacetApplication

  implicit def orderedIsFundamentalFacet: FundamentalFacet[ff#ordered]
  type OrderedFacet = FundamentalFacet[ff#ordered] {
    type FA[DT] <: FacetApplication[ff#ordered, DT] {
      type requires = SomeExist[PartiallyOrder[DT] :+: FullyOrdered[T] :+: CNil]
    }
  }

  /**
   * Ordered typeclass.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#rf-ordered
   */
  trait Ordered[T] extends Equality[T] {
    @op("<") def lt(lhs: T, rhs: T): Boolean
    @op("<=") def lteq(lhs: T, rhs: T): Boolean
  }

  trait PartiallyOrder[T] extends Ordered[T] {
    @op("<>") def incomparable(lhs: T, rhs: T)
  }

  trait FullyOrdered[T] extends Ordered[T]


  implicit def boundedIsFundamentalFacet: BoundedFacet
  type BoundedFacet = FundamentalFacet[ff#bounded] {
    type FA[DT] <: FacetApplication[ff#bounded, DT] {
      type requires = Bounded[DT]
    }
  }

  /**
   *
   * See: http://www.w3.org/TR/xmlschema-2/#rf-bounded
   */
  trait Bounded[T]


  /**
   * See: http://www.w3.org/TR/xmlschema-2/#rf-cardinality
   */
  implicit def cardinalityIsFundamentalFacet: CardinalityFacet
  type CardinalityFacet = FundamentalFacet[ff#cardinality] {
    type FA[DT] <: FacetApplication[ff#cardinality, DT] {
      type requires = SomeExist[Finite[DT] :+: CountablyInfinite[DT] :+: CNil]
    }
  }

  /**
   * http://www.w3.org/TR/xmlschema-2/#dc-cardinality
   */
  sealed trait Cardinality[T]

  trait Finite[T] extends Cardinality[T]

  object Finite {
    /**
     * When {variety} is ·atomic· and {value} of {base type definition} is finite, then {value} is finite.
     *
     * See: http://www.w3.org/TR/xmlschema-2/#rf-bounded
     */
    def atomicDerivedFromFinite[DT, BT]
    (implicit
     atomic: AtomicDatatype[DT],
     baseTypeOf: BaseTypeOf.Aux[DT, BT],
     baseTypeIsFinite: Finite[BT]): Finite[DT] =
      new Finite[DT] {}

    /**
     * When {variety} is ·atomic· and {value} of {base type definition} is countably infinite and either of the following conditions are true, then {value} is finite; else {value} is countably infinite:

     one of ·length·, ·maxLength·, ·totalDigits· is among {facets},
     all of the following are true:
     one of ·minInclusive· or ·minExclusive· is among {facets}
     one of ·maxInclusive· or ·maxExclusive· is among {facets}
     either of the following are true:
     ·fractionDigits· is among {facets}
     {base type definition} is one of date, gYearMonth, gYear, gMonthDay, gDay or gMonth or any type ·derived· from them
     */
    def atomicInfiniteFinite[DT, BT]
    (implicit
     atomic: AtomicDatatype[DT],
     baseTypeOf: BaseTypeOf.Aux[DT, BT],
     baseTypeIsCountablyInfinite: CountablyInfinite[BT],
     someLengthOrMax: SomeExist[
       (FacetApplication[ff#length, DT] :+:
         FacetApplication[ff#maxLength, DT] :+:
         FacetApplication[ff#totalDigits, DT] :+: CNil) :+:
         (AllExist[
           SomeExist[FacetApplication[ff#minInclusive, DT] :+: FacetApplication[ff#minExclusive, DT] :+: CNil] ::
             SomeExist[FacetApplication[ff#maxInclusive, DT] :+: FacetApplication[ff#maxExclusive, DT] :+: CNil] ::
             SomeExist[FacetApplication[ff#fractionDigits, DT] :+: ops.coproduct.Selector[
               xs#date :+: xs#gYearMonth :+: xs#gYear :+: xs#gMonthDay :+: xs#gDay :+: xs#gMonth :+: CNil, DT]] :: HNil]) :+: CNil]
      ): Finite[DT] = new Finite[DT] {}
  }

  trait CountablyInfinite[T] extends Cardinality[T]

  /**
   * The datatype is numeric.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#rf-numeric
   * @return
   */
  implicit def numeric: FundamentalFacet[ff#numeric]
  type NumericFacet = FundamentalFacet[ff#numeric] {
    type FA[DT] <: FacetApplication[ff#numeric, DT] {
      type requires = Numeric[DT]
    }
  }

  trait Numeric[T]


  /**
   * The datatype is constrained by length.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#rf-length
   */
  implicit def lengthIsConstrainingFacet: LengthFacet
  type LengthFacet = ConstrainingFacet[ff#length] {
    type FA[DT] <: RestrictionApplication[ff#length, DT] {
      type requires = HasLength[DT]
      type restrictionValue = xs#nonNegativeInteger
    }
  }

  /**
   * See: http://www.w3.org/TR/xmlschema-2/#length-validation-rules
   */
  implicit def validateLength[DT](restriction: RestrictionApplication.Aux[ff#length, DT, xs#nonNegativeInteger])
                                 (implicit hasLength: HasLength[DT]): FacetValidator[DT] =
    new FacetValidator[DT] {
      def isValid(dt: DT): Boolean = implicitly[Equality[xs#nonNegativeInteger]].equal(
        hasLength.length(dt),
        restriction.byValue)
    }

  implicit def minLengthIsConstrainingFacet: MinLengthFacet
  type MinLengthFacet = ConstrainingFacet[ff#minLength] {
    type FA[DT] <: RestrictionApplication[ff#minLength, DT] {
      type requires = HasLength[DT]
      type restrictionValue = xs#nonNegativeInteger
    }
  }
  
  implicit def validateMinLength[DT](restriction: RestrictionApplication.Aux[ff#minLength, DT, xs#nonNegativeInteger])
                                    (implicit hasLength: HasLength[DT]): FacetValidator[DT] =
    new FacetValidator[DT] {
      override def isValid(dt: DT): Boolean = implicitly[Ordered[xs#nonNegativeInteger]].lt(
        hasLength.length(dt),
        restriction.byValue)
    }

  /**
   * Provides access to the length of instances of a datatype.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#rf-length
   */
  @typeclass trait HasLength[T] {
    def length(t: T): xs#nonNegativeInteger
  }

  /**
   * For string and datatypes ·derived· from string, length is measured in units of characters as defined in
   * [http://www.w3.org/TR/xmlschema-2/#XML | XML 1.0 (Second Edition)].
   *
   * For string and datatypes ·derived· from string, length will not always coincide with "string length" as perceived
   * by some users or with the number of storage units in some digital representation. Therefore, care should be taken
   * when specifying a value for length and in attempting to infer storage requirements from a given value for length.
   */
  implicit def stringHasLength: HasLength[xs#string]

  /**
   * For anyURI, length is measured in units of characters (as for string).
   */
  implicit def anyURIHasLength: HasLength[xs#anyURI]

  /**
   * For hexBinary and datatypes ·derived· from it, length is measured in octets (8 bits) of binary data.
   */
  implicit def hexBinaryHasLength: HasLength[xs#hexBinary]

  /**
   * For base64Binary and datatypes ·derived· from it, length is measured in octets (8 bits) of binary data.
   */
  implicit def base64BinaryHasLength: HasLength[xs#base64Binary]

  /**
   * For datatypes ·derived· by ·list·, length is measured in number of list items.
   */
  implicit def listHasLength[L](implicit asList: ListDatatype[L])

}