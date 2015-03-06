package w3c.xsd

import shapeless.{HNil, ::, Coproduct, CNil, :+:}
import w3c.typeclass._

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

trait SpecialAndPrimitiveTypesDeclarations[xs <: SpecialAndPrimitiveTypes] {

  self : LexicalSpace[xs] with ValueSpace[xs] =>
//
//  type datatypeWitness[DT] = SomeExists[
//    Special[DT] :+:
//      AllExists[Primitive[DT] :: Identity[DT, DT] :: HNil] :+:
//      Derived[DT] :+:
//      CNil]
//
//  type datastructureWitness[DT] = SomeExists[AtomicDatatype[DT] :+: ListDatatype[DT] :+: UnionDatatype[DT] :+: CNil]
//  type definitionScope[DT] = SomeExists[BuiltIn[DT] :+: UserDefined[DT] :+: CNil]
//  type typeWitness[DT] = AllExists[
//    HasQName[DT] ::
//    LexicalMapping[DT] ::
//    datatypeWitness[DT] ::
//    datastructureWitness[DT] ::
//    Equality[DT] :: HNil]


  implicit val anyTypeHasQName: HasQName[xs#anyType] = HasQName("xs:anyType")
  implicit def anyTypeLexicalMapping: LexicalMapping[xs#anyType]
  implicit val anyTypeIsSpecial: Special[xs#anyType] = Special.witness[xs#anyType]

  implicit val anySimpleTypeHasQName: HasQName[xs#anySimpleType] = HasQName("xs:anySimpleType")
  implicit def anySimpleTypeLexicalMapping: LexicalMapping[xs#anySimpleType]
  implicit val anySimpleTypeIsSpecial: Special[xs#anySimpleType] = Special.witness[xs#anySimpleType]


  implicit val stringHasQName: HasQName[xs#string] = HasQName("xs:string")
  implicit def stringLexicalMapping: LexicalMapping[xs#string]
  implicit def stringIsPrimitive: Primitive[xs#string]

  implicit val durationHasQName: HasQName[xs#duration] = HasQName("xs:duration")
  implicit def durationLexicalMapping: LexicalMapping[xs#duration]
  implicit def durationIsPrimitive: Primitive[xs#duration]

  implicit val dateTimeHasQName: HasQName[xs#dateTime] = HasQName("xs:dateTime")
  implicit def dateTimeLexicalMapping: LexicalMapping[xs#dateTime]
  implicit def dateTimeIsPrimitive: Primitive[xs#dateTime]

  implicit val timeHasQName: HasQName[xs#time] = HasQName("xs:time")
  implicit def timeLexicalMapping: LexicalMapping[xs#time]
  implicit def timeIsPrimitive: Primitive[xs#dateTime]

  implicit val dateHasQName: HasQName[xs#date] = HasQName("xs:date")
  implicit def dateLexicalMapping: LexicalMapping[xs#date]
  implicit def dateIsPrimitive: Primitive[xs#date]

  implicit val gYearMonthHasQName: HasQName[xs#gYearMonth] = HasQName("xs:gYearMonth")
  implicit def gYearMonthLexicalMapping: LexicalMapping[xs#gYearMonth]
  implicit def gYearMonthIsPrimitive: Primitive[xs#gYearMonth]

  implicit val gYearHasQName: HasQName[xs#gYear] = HasQName("xs:gYear")
  implicit def gYearLexicalMapping: LexicalMapping[xs#gYear]
  implicit def gYearIsPrimitive: Primitive[xs#gYear]

  implicit val gMonthDayHasQName: HasQName[xs#gMonthDay] = HasQName("xs:gMonthDay")
  implicit def gMonthDayLexicalMapping: LexicalMapping[xs#gMonthDay]
  implicit def gMonthDayIsPrimitive: Primitive[xs#gMonthDay]

  implicit val gDayHasQName: HasQName[xs#gDay] = HasQName("xs:gDay")
  implicit def gDayLexicalMapping: LexicalMapping[xs#gDay]
  implicit def gDayIsPrimitive: Primitive[xs#gDay]

  implicit val gMonthHasQName: HasQName[xs#gMonth] = HasQName("xs:gMonth")
  implicit def gMonthLexicalMapping: LexicalMapping[xs#gMonth]
  implicit def gMonthIsPrimitive: Primitive[xs#gMonth]

  implicit val booleanHasQName: HasQName[xs#boolean] = HasQName("xs:boolean")
  implicit def booleanLexicalMapping: LexicalMapping[xs#boolean]
  implicit def booleanIsPrimitive: Primitive[xs#boolean]
  implicit def booleanValueSpace: BooleanValueSpace[xs#boolean] = new BooleanValueSpace[xs#boolean] {
    override val trueValue: xs#boolean  = "true".^^[xs#boolean]
    override val falseValue: xs#boolean = "false".^^[xs#boolean]
    override def booleanFrom(b: Boolean) = if(b) trueValue else falseValue
  }

  implicit val base64BinaryHasQName: HasQName[xs#base64Binary] = HasQName("xs:base64Binary")
  implicit def base64LexicalMapping: LexicalMapping[xs#base64Binary]
  implicit def base64BinaryIsPrimitive: Primitive[xs#base64Binary]

  implicit val hexBinaryHasQName: HasQName[xs#hexBinary] = HasQName("xs:hexBinary")
  implicit def hexBinaryLexicalMapping: LexicalMapping[xs#hexBinary]
  implicit def hexBinaryIsPrimitive: Primitive[xs#hexBinary]

  implicit val floatHasQName: HasQName[xs#float] = HasQName("xs:float")
  implicit def floatLexicalMapping: LexicalMapping[xs#float]
  implicit def floatIsPrimitive: Primitive[xs#float]

  implicit val decimalHasQName: HasQName[xs#decimal] = HasQName("xs:decimal")
  implicit def decimalLexicalMapping: LexicalMapping[xs#decimal]
  implicit def decimalIsPrimitive: Primitive[xs#decimal]

  implicit val doubleHasQName: HasQName[xs#double] = HasQName("xs:double")
  implicit def doubleLexicalMapping: LexicalMapping[xs#double]
  implicit def doubleIsPrimitive: Primitive[xs#double]

  implicit val anyURIHasQName: HasQName[xs#anyURI] = HasQName("xs:anyURI")
  implicit def anyURILexicalMapping: LexicalMapping[xs#anyURI]
  implicit def anyURIIsPrimitive: Primitive[xs#anyURI]

  implicit val QNameHasQName: HasQName[xs#QName] = HasQName("xs:QName")
  implicit def QNameLexicalMapping: LexicalMapping[xs#QName]
  implicit def QNameIsPrimitive: Primitive[xs#QName]

  implicit val NOTATIONHasQName: HasQName[xs#NOTATION] = HasQName("xs:NOTATION")
  implicit def NOTATIONLexicalMapping: LexicalMapping[xs#NOTATION]
  implicit def NOTATIONIsPrimitive: Primitive[xs#NOTATION]

}