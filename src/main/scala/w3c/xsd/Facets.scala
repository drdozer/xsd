package w3c.xsd

import shapeless._
import simulacrum.{op, typeclass}
import w3c.typeclass.{AllExists, Caster, SomeExists}

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

trait FacetTypeclasses[ff <: Facets, xs <: SpecialAndPrimitiveTypes with NumericTypes] {

  self : Datatypes[xs] =>

  /**
   * All xsd types, except the ur-types, have a base type.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#dt-anySimpleType
   */
  trait BaseTypeOf[DT] {
    type BaseType
  }

  object BaseTypeOf {
    type Aux[DT, BT] = BaseTypeOf[DT] { type BaseType = BT }

    /**
     * The base type of a restricted type is the type restricted.
     */
    implicit def baseTypeOfRestricted[R, BO](implicit restricted: Restricted.AuxBO[R, BO]): BaseTypeOf.Aux[R, BO] =
      new BaseTypeOf[R] { type BaseType = BO }

    /**
     * The base type of a primitive type is `xs#anySimpleType`.
     */
    implicit def baseTypeOfPrimitive[DT : Primitive]: BaseTypeOf.Aux[DT, xs#anySimpleType] = new BaseTypeOf[DT] {
      type BaseType = xs#anySimpleType
    }
  }


  /**
   * Validate a value against a facet.
   */
  trait FacetValidator[DT] {
    def isValid(dt: DT): xs#boolean
  }


  /**
   * Witness that a type has an inclusive upper bound.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#dt-inclusive-upper-bound
   */
  trait InclusiveUpperBound[T] {
    def bound: T
  }

  /**
   * Witness that a type has an exclusive upper bound.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#dt-exclusive-upper-bound
   */
  trait ExclusiveUpperBound[T] {
    def bound: T
  }

  /**
   * Witness that a type has an inclusive lower bound.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#dt-inclusive-lower-bound
   */
  trait InclusiveLowerBound[T] {
    def bound: T
  }

  /**
   * Witness that a type has an exclusive lower bound.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#dt-exclusive-lower-bound
   */
  trait ExclusiveLowerBound[T] {
    def bound: T
  }

  /**
   * Witness that a type is bounded.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#rf-bounded
   */
  trait Bounded[T]
  
  object Bounded {
    /**
     * See: See: http://www.w3.org/TR/xmlschema-2/#dt-bounded
     */
    def boundedByBounds[T]
    (implicit
     ordered: Ordered[T],
     hasUpperBound: SomeExists[InclusiveUpperBound[T] :+: ExclusiveUpperBound[T] :+: CNil],
     hasLowerBound: SomeExists[InclusiveLowerBound[T] :+: ExclusiveLowerBound[T] :+: CNil]): Bounded[T] =
      new Bounded[T] {}
  }


  /**
   * Witness that a type has a cardinality.
   *
   * http://www.w3.org/TR/xmlschema-2/#dc-cardinality
   */
  sealed trait Cardinality[T]


  /**
   * Witness that a type is finite.
   */
  trait Finite[T] extends Cardinality[T]

  object Finite {
    /**
     * When {variety} is ·atomic· and {value} of {base type definition} is finite, then {value} is finite.
     *
     * See: http://www.w3.org/TR/xmlschema-2/#rf-bounded
     */
    def atomicDerivedFromFinite[DT, BT]
    (implicit
     atomic: Atomic[DT],
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
     atomic: Atomic[DT],
     baseTypeOf: BaseTypeOf.Aux[DT, BT],
     baseTypeIsCountablyInfinite: CountablyInfinite[BT],
     someLengthOrMax: SomeExists[
       (FacetApplication[ff#length, DT] :+:
                FacetApplication[ff#maxLength, DT] :+:
                FacetApplication[ff#totalDigits, DT] :+: CNil) :+:
         (AllExists[
           SomeExists[FacetApplication[ff#minInclusive, DT] :+: FacetApplication[ff#minExclusive, DT] :+: CNil] ::
             SomeExists[FacetApplication[ff#maxInclusive, DT] :+: FacetApplication[ff#maxExclusive, DT] :+: CNil] ::
             SomeExists[FacetApplication[ff#fractionDigits, DT] :+: ops.coproduct.Selector[
               xs#date :+: xs#gYearMonth :+: xs#gYear :+: xs#gMonthDay :+: xs#gDay :+: xs#gMonth :+: CNil, DT] :+: CNil] :: HNil]) :+:
         CNil]
      ): Finite[DT] = new Finite[DT] {}
  }

  /**
   * Witness that a type is countably infinite.
   */
  trait CountablyInfinite[T] extends Cardinality[T]

  object CountablyInfinite {
    /**
     * All types that can not be proven to be finite are treated as countably infinite.
     */
    def notFinite[T](implicit isntFinite: ¬[Finite[T]]): CountablyInfinite[T] = new CountablyInfinite[T] {}
  }


  /**
   * Witness that a type is numeric.
   */
  trait Numeric[T]

  /**
   * Provides access to the length of instances of a datatype.
   *
   * The length is represented as an `xs#nonNegativeInteger`.
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
  implicit def listHasLength[L](implicit asList: List[L])

}

trait FactesProvider[ff <: Facets, xs <: SpecialAndPrimitiveTypes with NumericTypes] {
  protected implicit val facetTypeclasses: FacetTypeclasses[ff, xs]
  protected val valueSpace: ValueSpace[xs]

  import facetTypeclasses._
  import valueSpace._

  /**
   * The equality facet is fundamental.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#equal
   */
  implicit def equalIsFundamentalFacet: EqualFacet

  /**
   * The equality facet must be applied to a type with an `Equality` witness.
   * 
   * All xs types must have equality defined. 
   * 
   * See: http://www.w3.org/TR/xmlschema-2/#equal
   */
  type EqualFacet = FundamentalFacet[ff#equal] {
    type FA[DT] <: FacetApplication[ff#equal, DT] {
      type requires = Equality[DT, DT]
    }
  }


  /**
   * The ordered facet is fundamental.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#rf-ordered
   */
  implicit def orderedIsFundamentalFacet: FundamentalFacet[ff#ordered]

  /**
   * The ordered facet must be applied to a type with either a `PartialOrder` or `FullyOrdered` witness.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#dt-ordered
   */
  type OrderedFacet = FundamentalFacet[ff#ordered] {
    type FA[DT] <: FacetApplication[ff#ordered, DT] {
      type requires = SomeExists[PartiallyOrdered[DT, DT] :+: FullyOrdered[DT, DT] :+: CNil]
    }
  }


  /**
   * The bounded facet is fundamental.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#rf-bounded
   */
  implicit def boundedIsFundamentalFacet: BoundedFacet

  /**
   * The bounded facet must be applied to a type with a `Bounded` witness.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#dt-bounded
   */
  type BoundedFacet = FundamentalFacet[ff#bounded] {
    type FA[DT] <: FacetApplication[ff#bounded, DT] {
      type requires = Bounded[DT]
    }
  }


  /**
   * See: http://www.w3.org/TR/xmlschema-2/#rf-cardinality
   */
  implicit def cardinalityIsFundamentalFacet: CardinalityFacet
  type CardinalityFacet = FundamentalFacet[ff#cardinality] {
    type FA[DT] <: FacetApplication[ff#cardinality, DT] {
      type requires = SomeExists[Finite[DT] :+: CountablyInfinite[DT] :+: CNil]
    }
  }


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


  /**
   * The length facet is constraining.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#rf-length
   */
  implicit def lengthIsConstrainingFacet: LengthFacet

  /**
   * The length facet must be applied to a datatype with a `HasLength` instance and is parameterised by a
   * `nonNegativeInteger` defining the constrained length.
   */
  type LengthFacet = ConstrainingFacet[ff#length] {
    type FA[DT] <: RestrictionApplication[ff#length, DT] {
      type requires = HasLength[DT]
      type restrictionValue = xs#nonNegativeInteger
    }
  }

  /**
   * The length facet is constraining.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#length-validation-rules
   */
  implicit def validateLength[DT](restriction: RestrictionApplication.Aux[ff#length, DT, xs#nonNegativeInteger])
                                 (implicit
                                  hasLength: HasLength[DT],
                                   equality: Equality[xs#nonNegativeInteger, xs#nonNegativeInteger]): FacetValidator[DT] =
    new FacetValidator[DT] {
      def isValid(dt: DT): xs#boolean = equality.equal(
        hasLength.length(dt),
        restriction.byValue)
    }

  /**
   * The minLength facet is constraining.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#rf-minLength
   */
  implicit def minLengthIsConstrainingFacet: MinLengthFacet
  type MinLengthFacet = ConstrainingFacet[ff#minLength] {
    type FA[DT] <: RestrictionApplication[ff#minLength, DT] {
      type requires = HasLength[DT]
      type restrictionValue = xs#nonNegativeInteger
    }
  }


  /**
   * See: http://www.w3.org/TR/xmlschema-2/#cvc-minLength-valid
   */
  implicit def validateMinLength[DT](restriction: RestrictionApplication.Aux[ff#minLength, DT, xs#nonNegativeInteger])
                                    (implicit
                                     hasLength: HasLength[DT],
                                     ordered: Ordered[xs#nonNegativeInteger, xs#nonNegativeInteger]): FacetValidator[DT] =
    new FacetValidator[DT] {
      override def isValid(dt: DT): xs#boolean = ordered.lt(
        hasLength.length(dt),
        restriction.byValue)
    }

}