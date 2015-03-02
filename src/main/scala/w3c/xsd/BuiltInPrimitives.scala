package w3c.xsd

import shapeless.ops.coproduct.Basis
import shapeless.{Coproduct, CNil, :+:}
import simulacrum.typeclass
import w3c.typeclass.>:~>

/**
 *
 *
 * @author Matthew Pocock
 */
trait BuiltInPrimitives {

  self : XsdAnyType =>

  type string
  type duration
  type dateTime
  type time
  type date
  type gYearMonth
  type gYear
  type gMonthDay
  type gDay
  type gMonth
  type boolean
  type base64Binary
  type hexBinary
  type float
  type decimal
  type double
  type anyURI
  type QName
  type NOTATION

}

@typeclass trait BuiltInPrimitivesHierarchy[xs <: XsdAnyType with BuiltInPrimitives] {

  self: AnyTypeHierarchy[xs] =>

  // our extensions of anySimpleTypeHierarcy is this coproduct
  type SimpleTypeSubtypes_BuiltInPrimitives =
    xs#string :+:
    xs#duration :+:
    xs#dateTime :+:
    xs#time :+:
    xs#date :+:
    xs#gYearMonth :+:
    xs#gYear :+:
    xs#gMonthDay :+:
    xs#gDay :+:
    xs#gMonth :+:
    xs#boolean :+:
    xs#base64Binary :+:
    xs#hexBinary :+:
    xs#float :+:
    xs#decimal :+:
    xs#double :+:
    xs#anyURI :+:
    xs#QName :+:
    xs#NOTATION :+: CNil

  // this is a witness that SimpleTypesSubtypes contains the subtypes we declare here
  def simpleTypesHasBuiltInPrimitives: Basis[anySimpleTypesDescendents, SimpleTypeSubtypes_BuiltInPrimitives]

  type stringDescendents <: Coproduct
  def stringHierarchy: xs#string >:~> stringDescendents

  type decimalDescendents <: Coproduct
  def decimalHierarchy: xs#decimal >:~> decimalDescendents

}

@typeclass trait BuiltInPrimitivesTypes[xs <: XsdAnyType with BuiltInPrimitives] {
  implicit def stringIsPrimitive: Primitive[xs#string]
  implicit def durationIsPrimitive: Primitive[xs#duration]
  implicit def dateTimeIsPrimitive: Primitive[xs#dateTime]
  implicit def timeIsPrimitive: Primitive[xs#dateTime]
  implicit def dateIsPrimitive: Primitive[xs#date]
  implicit def gYearMonthIsPrimitive: Primitive[xs#gYearMonth]
  implicit def gYearIsPrimitive: Primitive[xs#gYear]
  implicit def gMonthDayIsPrimitive: Primitive[xs#gMonthDay]
  implicit def gDayIsPrimitive: Primitive[xs#gDay]
  implicit def gMonthIsPrimitive: Primitive[xs#gMonth]
  implicit def booleanIsPrimitive: Primitive[xs#boolean]
  implicit def base64BinaryIsPrimitive: Primitive[xs#base64Binary]
  implicit def hexBinaryIsPrimitive: Primitive[xs#hexBinary]
  implicit def floatIsPrimitive: Primitive[xs#float]
  implicit def decimalIsPrimitive: Primitive[xs#decimal]
  implicit def doubleIsPrimitive: Primitive[xs#double]
  implicit def anyURIIsPrimitive: Primitive[xs#anyURI]
  implicit def QNameIsPrimitive: Primitive[xs#QName]
  implicit def NOTATIONIsPrimitive: Primitive[xs#NOTATION]
}