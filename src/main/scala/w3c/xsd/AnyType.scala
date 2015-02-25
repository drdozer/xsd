package w3c
package xsd

import shapeless.{:+:, CNil, Coproduct}
import shapeless.ops.coproduct.Basis
import typeclass._
import simulacrum._

trait AnyType {

  type anyType
  type anySimpleType

}

@typeclass trait AnyTypeHierarchy[xsd <: AnyType] {

  def anyTypeHierarchy: xsd#anyType >:~> xsd#anySimpleType

  type anySimpleTypesDescendents <: Coproduct
  def anySimpleTypeHierarchy: (xsd#anySimpleType >:~> anySimpleTypesDescendents)#sealing

}

