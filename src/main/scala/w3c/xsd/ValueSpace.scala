package w3c.xsd

import simulacrum.{op, typeclass}
import w3c.typeclass.{DoesNotExist, Caster}

/**
 * API for representing and working with XSD value spaces.
 *
 * See: http://www.w3.org/TR/xmlschema11-2/#value-space
 *
 * @tparam xs   the XSD implementation
 */
trait ValueSpace[xs <: SpecialAndPrimitiveTypes] {

  /**
   * The globally-defined identity relation.
   *
   * This is defined for all datatypes. The identity check is defined in terms of `xs#boolean`
   *
   * See: http://www.w3.org/TR/xmlschema11-2/#identity
   *
   * @tparam T1  the type of the first item
   * @tparam T2  the type of the second item
   */
  trait Identity[T1, T2] {
    protected def booleanValueSpace: BooleanValueSpace[xs#boolean]

    def identical(lhs: T1, rhs: T2): xs#boolean
    def distinct(lhs: T1, rhs: T2): xs#boolean = booleanValueSpace.!(identical(lhs, rhs))

    trait IdentityLaw {
      def identityIsSymmetric(lhs: T1, rhs: T2)(implicit
                                                t21I: Identity[T2, T1]): Boolean =
        identical(lhs, rhs) == t21I.identical(rhs, lhs)

      def identityIsConsistentWithEquality(lhs: T1, rhs: T2)(implicit
                                                            equality: Equality[T1, T2]): Boolean =
        // if two items are identical, they must be equal
        if(identical(lhs, rhs) == booleanValueSpace.trueValue)
          equality.equal(lhs, rhs) == booleanValueSpace.trueValue
        else
          true
    }

    def identityLaw: IdentityLaw = new IdentityLaw {}
  }

  object Identity {

    /**
     * Values from different primitives are never identical.
     *
     * <blockquote>
     * In the identity relation defined herein, values from different ·primitive· datatypes' ·value spaces· are made
     * artificially distinct if they might otherwise be considered identical.
     * </blockquote>
     */
    implicit def differentPrimitivesAreNotIdentical[T1, T2]
    (implicit
     _booleanValueSpace: BooleanValueSpace[xs#boolean],
     t1IsPrimitive: Primitive[T1],
     t2IsPrimitive: Primitive[T2],
     t1t2AreDifferent: DoesNotExist[T1 =:= T2]): Identity[T1, T2] =
      new Identity[T1, T2] {
        override protected def booleanValueSpace = _booleanValueSpace
        override def identical(lhs: T1, rhs: T2): xs#boolean = booleanValueSpace.falseValue
      }

    /**
     * Restrictions are considered to be part of the identity space of the type they restrict: lhs case.
     *
     * <blockquote>
     * Datatypes ·constructed· by ·facet-based restriction· do not create new values; they define subsets of some
     * ·primitive· datatype's ·value space·.
     * </blockquote>
     */
    implicit def restrictedAreIdenticalByBaseT1[T1, T2, BO]
    (implicit
     _booleanValueSpace: BooleanValueSpace[xs#boolean],
     t1IsRestriction: Restricted.AuxBO[T1, BO],
     idBOT2: Identity[BO, T2],
     t1BOCaster: Caster.Aux[BO, T1]): Identity[T1, T2] =
      new Identity[T1, T2] {
        override protected def booleanValueSpace = _booleanValueSpace
        override def identical(lhs: T1, rhs: T2) = idBOT2.identical(t1BOCaster.upcast(lhs), rhs)
      }


    /**
     * Restrictions are considered to be part of the identity space of the type they restrict: rhs case.
     *
     * <blockquote>
     * Datatypes ·constructed· by ·facet-based restriction· do not create new values; they define subsets of some
     * ·primitive· datatype's ·value space·.
     * </blockquote>
     */
    implicit def restrictedAreIdenticalByBaseT2[T1, T2, BO]
    (implicit
     _booleanValueSpace: BooleanValueSpace[xs#boolean],
     t2IsRestriction: Restricted.AuxBO[T2, BO],
     idBOT1: Identity[T1, BO],
     t1BOCaster: Caster.Aux[BO, T2]): Identity[T1, T2] =
      new Identity[T1, T2] {
        override protected def booleanValueSpace = _booleanValueSpace
        override def identical(lhs: T1, rhs: T2) = idBOT1.identical(lhs, t1BOCaster.upcast(rhs))
      }


    /**
     * Lists are identical if they contain identical elements, and are also identical if they are both empty, regardless
     * of their element type.
     *
     * <blockquote>
     * Given a list A and a list B, A and B are the same list if they are the same sequence of atomic values. The
     * necessary and sufficient conditions for this identity are that A and B have the same length and that the items of A
     * are pairwise identical to the items of B.
     *
     * Note: It is a consequence of the rule just given for list identity that there is only one empty list. An empty list
     * declared as having ·item type· decimal and an empty list declared as having ·item type· string are not only equal
     * but identical.
     * </blockquote>
     */
    implicit def listIdentity[L1, E1, L2, E2]
    (implicit
     _booleanValueSpace: BooleanValueSpace[xs#boolean],
     l1IsListOfE1: ListDatatype.Aux[L1, E1],
     l2IsListOfE2: ListDatatype.Aux[L2, E2],
     e12id: Identity[E1, E2]): Identity[L1, L2] =
      new Identity[L1, L2] {
        override protected def booleanValueSpace = _booleanValueSpace
        override def identical(lhs: L1, rhs: L2): xs#boolean = {
          val e1 = l1IsListOfE1.elements(lhs)
          val e2 = l2IsListOfE2.elements(rhs)

          if(
            (e1.isEmpty && e2.isEmpty) ||
              (e1.size == e2.size && e1.zip(e2).map((e12id.identical(_, _)).tupled).forall(_ == booleanValueSpace.trueValue)))
            booleanValueSpace.trueValue
          else
            booleanValueSpace.falseValue
        }
      }

  }

  /**
   * Witness that a type has equality.
   *
   * This is defined for all datatypes. The equality check is defined in terms of `xs#boolean`.
   *
   * http://www.w3.org/TR/xmlschema11-2/#equality
   */
  trait Equality[T1, T2] {
    protected def booleanValueSpace: BooleanValueSpace[xs#boolean]

    @op("===") def equal(lhs: T1, rhs: T2): xs#boolean
    @op("≠") def notEqual(lhs: T1, rhs: T2): xs#boolean = booleanValueSpace.!(equal(lhs, rhs))

    trait EqualityLaw {
      def equalityIsSymmetric(lhs: T1, rhs: T2)(implicit
                                                t21E: Equality[T2, T1]): Boolean =
        equal(lhs, rhs) == t21E.equal(rhs, lhs)
    }

    def equalityLaw: EqualityLaw = new EqualityLaw {}
  }

  object Equality extends LowPriorityEqualityImplicits {
    /**
     * Values based upon different primitives are never equal.
     *
     * <blockquote>
     * In the equality relation defined herein, values from different primitive data spaces are made artificially
     * unequal even if they might otherwise be considered equal.
     * </blockquote>
     */
    implicit def differentPrimitivesAreNotEqual[T1, T2]
    (implicit
     _booleanSpace: BooleanValueSpace[xs#boolean],
     t1IsPrimitive: Primitive[T1],
     t2IsPrimitive: Primitive[T2],
     t1t2AreDifferent: DoesNotExist[T1 =:= T2]): Equality[T1, T2] =
      new Equality[T1, T2] {
        override def booleanValueSpace = _booleanSpace
        override def equal(lhs: T1, rhs: T2) = booleanValueSpace.falseValue
      }

    /**
     * Restricted types use the equality of their base type: LHS.
     */
    implicit def restrictedAreEqualByBaseT1[T1, T2, BO]
    (implicit
     _booleanValueSpace: BooleanValueSpace[xs#boolean],
     t1IsRestriction: Restricted.AuxBO[T1, BO],
     eqBOT2: Equality[BO, T2],
     t1BOCaster: Caster.Aux[BO, T1]): Equality[T1, T2] =
      new Equality[T1, T2] {
        override protected def booleanValueSpace = _booleanValueSpace
        override def equal(lhs: T1, rhs: T2) = eqBOT2.equal(t1BOCaster.upcast(lhs), rhs)
      }

    /**
     * Restricted types use the equality of their base type: RHS.
     */
    implicit def restrictedAreEqualByBaseT2[T1, T2, BO]
    (implicit
     _booleanValueSpace: BooleanValueSpace[xs#boolean],
     t2IsRestriction: Restricted.AuxBO[T2, BO],
     eqBOT1: Equality[T1, BO],
     t1BOCaster: Caster.Aux[BO, T2]): Equality[T1, T2] =
      new Equality[T1, T2] {
        override protected def booleanValueSpace = _booleanValueSpace
        override def equal(lhs: T1, rhs: T2) = eqBOT1.equal(lhs, t1BOCaster.upcast(rhs))
      }

    /**
     * List equality.
     *
     * <blockquote>
     * Two lists A and B are equal if and only if they have the same length and their items are pairwise equal. A list
     * of length one containing a value V1 and an atomic value V2 are equal if and only if V1 is equal to V2.
     * </blockquote>
     */

    implicit def listEquality[L1, E1, L2, E2]
    (implicit
     _booleanValueSpace: BooleanValueSpace[xs#boolean],
     l1IsListOfE1: ListDatatype.Aux[L1, E1],
     l2IsListOfE2: ListDatatype.Aux[L2, E2],
     e12eq: Equality[E1, E2]): Equality[L1, L2] =
      new Equality[L1, L2] {
        override protected def booleanValueSpace = _booleanValueSpace
        override def equal(lhs: L1, rhs: L2): xs#boolean = {
          val e1 = l1IsListOfE1.elements(lhs)
          val e2 = l2IsListOfE2.elements(rhs)

          if(
            (e1.isEmpty && e2.isEmpty) ||
              (e1.size == e2.size && e1.zip(e2).map((e12eq.equal(_, _)).tupled).forall(_ == booleanValueSpace.trueValue)))
            booleanValueSpace.trueValue
          else
            booleanValueSpace.falseValue
        }
      }
  }

  trait LowPriorityEqualityImplicits {

    /**
     * Infer an equality relation from an identity relation.
     *
     * <blockquote>
     * The equality relation for most datatypes is the identity relation.
     * </blockquote>
     */
    implicit def equalityFromIdentity[T1, T2](implicit
                                              _booleanValueSpace: BooleanValueSpace[xs#boolean],
                                              identity: Identity[T1, T2]): Equality[T1, T2] =
      new Equality[T1, T2] {
        override def booleanValueSpace = _booleanValueSpace
        override def equal(lhs: T1, rhs: T2) = identity.identical(lhs, rhs)
      }
  }

  /**
   * Witness that a type is Ordered. Ordered types will be either Partially or fully ordered.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#rf-ordered
   */
  trait Ordered[T] extends Equality[T, T] {
    @op("<") def lt(lhs: T, rhs: T): xs#boolean
    @op("≤") def lteq(lhs: T, rhs: T): xs#boolean
    @op(">") def gt(lhs: T, rhs: T): xs#boolean
    @op("≥") def gteq(lhs: T, rhs: T): xs#boolean
  }

  /**
   * Witness that a type is partially ordered.
   */
  trait PartiallyOrdered[T] extends Ordered[T] {
    @op("<>") def incomparable(lhs: T, rhs: T): xs#boolean
  }

  /**
   * Witness that a type is fully ordered.
   */
  trait FullyOrdered[T] extends Ordered[T]

  @typeclass trait BooleanValueSpace[B] {
    def trueValue: B
    def falseValue: B

    def ! (b: B): B = if(b == trueValue) falseValue else trueValue
  }
}