package w3c.xsd

import shapeless.ops.coproduct.Basis
import shapeless.{:+:, CNil}
import w3c.typeclass.>:~>

/**
 *
 *
 * @author Matthew Pocock
 */
trait NumericTypes {

  self : BuiltInPrimitives =>

  type integer

  type nonPositiveInteger
  type long
  type nonNegativeInteger
  type negativeInteger

  type int
  type short
  type byte

  type unsignedLong
  type positiveInteger
  type unsignedInt
  type unsignedShort
  type unsignedByte

}

trait NumericTypesHierarchy[xsd <: AnyType with BuiltInPrimitives with NumericTypes] {

  self: BuiltInPrimitivesHierarchy[xsd] =>

  type decimalSubtypes_NumericTypes = xsd#integer :+: CNil
  def decimalDescendantsHasNumericTypes: Basis[decimalDescendents, decimalSubtypes_NumericTypes]

  def integerHierarchy: xsd#integer >:~> (xsd#nonPositiveInteger :+: xsd#long :+: xsd#nonNegativeInteger :+: CNil)

  def nonPositiveIntegerHierarchy: xsd#nonPositiveInteger >:~> xsd#negativeInteger

  def longHierarchy: xsd#long >:~> xsd#int

  def intHierarchy: xsd#int >:~> xsd#short

  def shortHierarchy: xsd#short >:~> xsd#byte

  def nonNegativeIntegerHierarchy: xsd#nonPositiveInteger >:~> (xsd#unsignedLong :+: xsd#positiveInteger :+: CNil)

  def unsignedLongHierarchy: xsd#unsignedLong >:~> xsd#unsignedInt

  def unsignedIntHierarchy: xsd#unsignedInt >:~> xsd#unsignedShort

  def unsignedShortHierarchy: xsd#unsignedShort >:~> xsd#unsignedByte

}