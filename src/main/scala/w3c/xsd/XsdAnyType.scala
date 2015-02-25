package w3c
package xsd

import shapeless.Coproduct
import typeclass._
import simulacrum._

trait AnyType {

  type anyType
  type anySimpleType

}

@typeclass trait AnyTypeHierarchy[xsd <: AnyType] {

  def anyTypeHierarchy: xsd#anyType >:~> xsd#anySimpleType

  type SimpleTypeSubtypes
  def anySimpleTypeHierarchy: xsd#anySimpleType >:~> SimpleTypeSubtypes

}

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

  def simpleTypesHasSubtypes:  Coproduct.`xsd#string, xsd#duration, xsd#dateTime, xsd#time, xsd#date, xsd#gYearMonth, xsd#gYear, xsd#gMonthDay, xsd#gDay, xsd#gMonth, xsd#boolean, xsd#base64Binary, xsd#hexBinary, xsd#float, xsd#decimal, xsd#double, xsd#anyURI, xsd#QName, xsd#NOTATION`.T

  type StringSubtypes
  def stringHierarchy: xsd#string >:~> StringSubtypes

  type DecimalSubtypes
  def decimalHierarchy: xsd#decimal >:~> DecimalSubtypes

}

trait StringTypes {

  self : BuiltInPrimitives =>

  type normalizedString
    type token
    type language
    type Name
    type NMTOKEN
    type NCName
    type ID
    type IDREF
    type ENTITY
    type IDREFS
    type ENTITIES

}

@typeclass trait StringTypesHierarchy[xsd <: AnyType with BuiltInPrimitives with StringTypes] {


  def stringHierarchy: xsd#string >:~> xsd#normalizedString


}