package w3c
package xsd

import shapeless.{:+:, CNil, Coproduct}
import shapeless.ops.coproduct.Basis
import typeclass._
import simulacrum._

trait XsdAnyType {

  type anyType
  type anySimpleType

}

@typeclass trait AnyTypeHierarchy[xsd <: XsdAnyType] {

  def anyTypeHierarchy: xsd#anyType >:~> xsd#anySimpleType

  type anySimpleTypesDescendents <: Coproduct
  def anySimpleTypeHierarchy: (xsd#anySimpleType >:~> anySimpleTypesDescendents)#sealing

}

@typeclass trait AnyTypeDatatypeIris[xsd <: XsdAnyType] {

  implicit def anyTypeIri: DatatypeIri[xsd#anyType] = DatatypeIri("xsd:anyType")
  implicit def anySimpleTypeIri: DatatypeIri[xsd#anySimpleType] = DatatypeIri("xsd:anySimpleType")

}