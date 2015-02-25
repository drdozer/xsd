package w3c.xsd

import shapeless.{:+:, CNil}
import shapeless.ops.coproduct.Basis
import simulacrum._
import w3c.typeclass.>:~>


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

  self: BuiltInPrimitivesHierarchy[xsd] =>

  type stringSubtypes_StringTypes = xsd#normalizedString :+: CNil
  def stringDescendantsHasStringTypes: Basis[stringDescendents, stringSubtypes_StringTypes]

  def normalizedStringHierarchy: xsd#normalizedString >:~> xsd#token

  def tokenHierarchy: xsd#token >:~> (xsd#language :+: xsd#Name :+: xsd#NMTOKEN :+: CNil)

  def NameHierarchy: xsd#Name >:~> xsd#NCName

  def NCNameHierarchy: xsd#NCName >:~> (xsd#ID :+: xsd#IDREF :+: xsd#ENTITY :+: CNil)

}

