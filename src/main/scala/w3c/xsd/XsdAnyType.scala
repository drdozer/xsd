package w3c
package xsd

import shapeless.{:+:, CNil, Coproduct}
import typeclass._

trait XsdAnyType {

  type anyType
  type anySimpleType

}

trait AnyTypeHierarchy[xsd <: XsdAnyType] {

  def anyTypeHierarchy: xsd#anyType >:~> xsd#anySimpleType

  type anySimpleTypesDescendents <: Coproduct
  def anySimpleTypeHierarchy: (xsd#anySimpleType >:~> anySimpleTypesDescendents)#sealing

}

trait AnyTypeDatatypeQNames[xs <: XsdAnyType with XsdBuiltIn] {

  protected val namedTypes: NamedTypes[xs]
  import namedTypes._
  import LexicalSpace._

  implicit def anyTypeQName: HasQName[xs#anyType] = HasQName("xsd:anyType")
  implicit def anySimpleTypeIri: HasQName[xs#anySimpleType] = HasQName("xsd:anySimpleType")

}