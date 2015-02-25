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

  self : AnyType =>

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

@typeclass trait BuiltInPrimitivesHierarchy[xsd <: AnyType with BuiltInPrimitives] {

  self: AnyTypeHierarchy[xsd] =>

  // our extensions of anySimpleTypeHierarcy is this coproduct
  type SimpleTypeSubtypes_BuiltInPrimitives =
    xsd#string :+:
    xsd#duration :+:
    xsd#dateTime :+:
    xsd#time :+:
    xsd#date :+:
    xsd#gYearMonth :+:
    xsd#gYear :+:
    xsd#gMonthDay :+:
    xsd#gDay :+:
    xsd#gMonth :+:
    xsd#boolean :+:
    xsd#base64Binary :+:
    xsd#hexBinary :+:
    xsd#float :+:
    xsd#decimal :+:
    xsd#double :+:
    xsd#anyURI :+:
    xsd#QName :+:
    xsd#NOTATION :+: CNil

  // this is a witness that SimpleTypesSubtypes contains the subtypes we declare here
  def simpleTypesHasBuiltInPrimitives: Basis[anySimpleTypesDescendents, SimpleTypeSubtypes_BuiltInPrimitives]

  type stringDescendents <: Coproduct
  def stringHierarchy: xsd#string >:~> stringDescendents

  type decimalDescendents <: Coproduct
  def decimalHierarchy: xsd#decimal >:~> decimalDescendents

}
