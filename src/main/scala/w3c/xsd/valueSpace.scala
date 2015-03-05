package w3c.xsd

import simulacrum.{op, typeclass}
import w3c.typeclass.Caster

trait ValueSpace[xs <: XsdBuiltIn] {

  /**
   * Witness that a type has equality.
   *
   * This is defined for all datatypes. The equality check is defined in terms of `xs#boolean`.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#equal
   */
  @typeclass trait Equality[T] {
    @op("===") def equal(lhs: T, rhs: T): xs#boolean
  }

  object Equality {

    /**
     * The equality of a restricted datatype is consistent with the equality of the datatype that was restricted.
     *
     * Any custom provider of equality must be consistent with this definition.
     */
    implicit def restrictedEquality[R, DF]
    (implicit res: Restricted.AuxBO[R, DF], eqDF: Equality[DF], caster: Caster.Aux[DF, R]): Equality[R] =
      new Equality[R] {
        @op("===")
        override def equal(lhs: R, rhs: R) = eqDF.equal(caster.upcast(lhs), caster.upcast(rhs))
      }
  }

  /**
   * Witness that a type is Ordered. Ordered types will be either Partially or fully ordered.
   *
   * See: http://www.w3.org/TR/xmlschema-2/#rf-ordered
   */
  trait Ordered[T] extends Equality[T] {
    @op("<") def lt(lhs: T, rhs: T): xs#boolean
    @op("<=") def lteq(lhs: T, rhs: T): xs#boolean
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

}