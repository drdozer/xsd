package w3c.xsd

import shapeless.{HNil, ::, Coproduct, CNil, :+:}
import w3c.typeclass.{SomeExists, AllExists, >:~>}

/**
 *
 *
 * @author Matthew Pocock
 */
trait SpecialAndPrimitiveTypes {

  // special types
  type anyType
  type anySimpleType


  // primitive types
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

trait BuiltInPrimitivesHierarchy[xs <: SpecialAndPrimitiveTypes] {

  def anyTypeHierarchy: xs#anyType >:~> xs#anySimpleType

  def anySimpleTypeHierarchy: (xs#anySimpleType >:~> anySimpleTypesDescendents)#sealing
  
  // our extensions of anySimpleTypeHierarcy is this coproduct
  type anySimpleTypesDescendents =
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

  type stringDescendents <: Coproduct
  def stringHierarchy: xs#string >:~> stringDescendents

  type decimalDescendents <: Coproduct
  def decimalHierarchy: xs#decimal >:~> decimalDescendents

}

/**
 * Witness trait for type declarations.
 *
 * This provides all the witnesses to demonstrate that a type is correctly and fully described.
 */
trait Declarations[xs <: SpecialAndPrimitiveTypes] {

  protected val namedTypes: NamedTypes[xs]
  protected val valueSpace: ValueSpace[xs]

  import namedTypes._
  import valueSpace._

  type datatypeWitness[DT] = SomeExists[Special[DT] :+: Primitive[DT] :+: Derived[DT] :+: CNil]
  type datastructureWitness[DT] = SomeExists[AtomicDatatype[DT] :+: ListDatatype[DT] :+: UnionDatatype[DT] :+: CNil]
  type definitionScope[DT] = SomeExists[BuiltIn[DT] :+: UserDefined[DT] :+: CNil]
  type typeWitness[DT] = AllExists[
    HasQName[DT] ::
    LexicalMapping[DT] ::
    datatypeWitness[DT] ::
    datastructureWitness[DT] ::
    Identity[DT] ::
    Equality[DT] :: HNil]



}

trait SpecialAndPrimitiveTypesDeclarations[xs <: SpecialAndPrimitiveTypes] extends Declarations[xs] {

  protected val namedTypes: NamedTypes[xs]
  import namedTypes._
  import LexicalMapping._

  val boolean_true: xs#boolean = "true".^^[xs#boolean]
  val boolean_false: xs#boolean = "false".^^[xs#boolean]


  implicit def anyTypeHasQName: HasQName[xs#anyType] = HasQName("xs:anyType")
  implicit def anyTypeIsSpecial: Special[xs#anyType] = Special.witness[xs#anyType]

  implicit def anySimpleTypeHasQName: HasQName[xs#anySimpleType] = HasQName("xs:anySimpleType")
  implicit def anySimpleTypeIsSpecial: Special[xs#anySimpleType] = Special.witness[xs#anySimpleType]


  implicit def stringHasQName: HasQName[xs#string] = HasQName("xs:string")
  implicit def stringLexicalSpace: LexicalMapping[xs#string]
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

  implicit def booleanLexicalMapping: LexicalMapping[xs#boolean]
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