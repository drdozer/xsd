package w3c.xsd

import shapeless.ops.coproduct.Basis
import shapeless.{HNil, ::, Coproduct, CNil, :+:}
import w3c.typeclass._

/**
 *
 *
 * @author Matthew Pocock
 */
trait SpecialAndPrimitiveTypes {

  /**
   * The type of literals.
   *
   * <blockquote>
   * A sequence of zero or more characters in the Universal Character Set (UCS) which may or may not prove upon
   * inspection to be a member of the ·lexical space· of a given datatype and thus a ·lexical representation· of a given
   * value in that datatype's ·value space·, is referred to as a literal. The term is used indifferently both for
   * character sequences which are members of a particular ·lexical space· and for those which are not.
   * </blockquote>
   *
   * See: http://www.w3.org/TR/xmlschema11-2/#dt-literal
   */
  type literal

  // special types
  type anyType
  type anySimpleType
  type anyAtomicType


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

  def anySimpleTypeHierarchy: (xs#anySimpleType >:~> anySimpleTypesDescendants)#sealing

  /**
   * The descendants of `anySimpleType`.
   */
  type anySimpleTypesDescendants <: Coproduct

  /**
   * The built-in descendants of `anySimpleType`.
   */
  type anySimpleTypeBuiltInDescendants =
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

  /**
   * Witness that the descendants of `anySimpleType` contains all of the built-in descendants.
   */
  implicit def anySimpleTypeDescendantsContainsBuiltIn: Basis[anySimpleTypesDescendants, anySimpleTypeBuiltInDescendants]

  type stringDescendents <: Coproduct
  def stringHierarchy: xs#string >:~> stringDescendents

  type decimalDescendents <: Coproduct
  def decimalHierarchy: xs#decimal >:~> decimalDescendents

}

trait SpecialAndPrimitiveTypesDeclarations[xs <: SpecialAndPrimitiveTypes] {

  self : Datatypes[xs] with LexicalSpace[xs] with ValueSpace[xs] =>

  import Literal.ops._

  implicit val anyTypeHasQName: HasQName[xs#anyType] = HasQName[xs#anyType]("xs:anyType")
  implicit val anyTypeIsSpecial: Special[xs#anyType] = Special.witness[xs#anyType]
  implicit val anyTypeIsBuiltIn: BuiltIn[xs#anyType] = BuiltIn.witness[xs#anyType]

  implicit val anySimpleTypeHasQName: HasQName[xs#anySimpleType] = HasQName[xs#anySimpleType]("xs:anySimpleType")
  implicit def anySimpleTypeLexicalMapping: LexicalMapping[xs#anySimpleType] // any literal to any atomic datatype or list
  implicit val anySimpleTypeIsSpecial: Special[xs#anySimpleType] = Special.witness[xs#anySimpleType]
  implicit val anySimpleTypeIsBuiltIn: BuiltIn[xs#anySimpleType] = BuiltIn.witness[xs#anySimpleType]

  implicit val anyAtomicTypeHasQName: HasQName[xs#anyAtomicType] = HasQName[xs#anyAtomicType]("xs:anyAtomicType")
  implicit def anyAtomicTypeLexicalMapping: LexicalMapping[xs#anyAtomicType] // any primitive
  implicit val anyAtomicTypeIsSpecial: Special[xs#anyAtomicType] = Special.witness[xs#anyAtomicType]
  implicit val anyAtomicTypeIsBuiltIn: BuiltIn[xs#anyAtomicType] = BuiltIn.witness[xs#anyAtomicType]


  implicit val stringHasQName: HasQName[xs#string] = HasQName[xs#string]("xs:string")
  implicit def stringLexicalMapping: LexicalMapping[xs#string] // in an identity relation with xs#literal
  implicit val stringIsPrimitive: Primitive[xs#string] = Primitive.witness[xs#string]
  implicit val stringIsBuiltIn: BuiltIn[xs#string] = BuiltIn.witness[xs#string]

  implicit val durationHasQName: HasQName[xs#duration] = HasQName[xs#duration]("xs:duration")
  implicit def durationLexicalMapping: LexicalMapping[xs#duration]
  implicit val durationIsPrimitive: Primitive[xs#duration] = Primitive.witness[xs#duration]
  implicit val durationIsBuiltIn: BuiltIn[xs#duration] = BuiltIn.witness[xs#duration]

  implicit val dateTimeHasQName: HasQName[xs#dateTime] = HasQName[xs#dateTime]("xs:dateTime")
  implicit def dateTimeLexicalMapping: LexicalMapping[xs#dateTime]
  implicit val dateTimeIsPrimitive: Primitive[xs#dateTime] = Primitive.witness[xs#dateTime]
  implicit val dateTimeIsBuiltIn: BuiltIn[xs#dateTime] = BuiltIn.witness[xs#dateTime]

  implicit val timeHasQName: HasQName[xs#time] = HasQName[xs#time]("xs:time")
  implicit def timeLexicalMapping: LexicalMapping[xs#time]
  implicit val timeIsPrimitive: Primitive[xs#dateTime] = Primitive.witness[xs#dateTime]
  implicit val timeIsBuiltIn: BuiltIn[xs#dateTime] = BuiltIn.witness[xs#dateTime]

  implicit val dateHasQName: HasQName[xs#date] = HasQName[xs#date]("xs:date")
  implicit def dateLexicalMapping: LexicalMapping[xs#date]
  implicit val dateIsPrimitive: Primitive[xs#date] = Primitive.witness[xs#date]
  implicit val dateIsBuiltIn: BuiltIn[xs#date] = BuiltIn.witness[xs#date]

  implicit val gYearMonthHasQName: HasQName[xs#gYearMonth] = HasQName[xs#gYearMonth]("xs:gYearMonth")
  implicit def gYearMonthLexicalMapping: LexicalMapping[xs#gYearMonth]
  implicit val gYearMonthIsPrimitive: Primitive[xs#gYearMonth] = Primitive.witness[xs#gYearMonth]
  implicit val gYearMonthIsBuiltIn: BuiltIn[xs#gYearMonth] = BuiltIn.witness[xs#gYearMonth]

  implicit val gYearHasQName: HasQName[xs#gYear] = HasQName[xs#gYear]("xs:gYear")
  implicit def gYearLexicalMapping: LexicalMapping[xs#gYear]
  implicit val gYearIsPrimitive: Primitive[xs#gYear] = Primitive.witness[xs#gYear]
  implicit val gYearIsBuiltIn: BuiltIn[xs#gYear] = BuiltIn.witness[xs#gYear]

  implicit val gMonthDayHasQName: HasQName[xs#gMonthDay] = HasQName[xs#gMonthDay]("xs:gMonthDay")
  implicit def gMonthDayLexicalMapping: LexicalMapping[xs#gMonthDay]
  implicit val gMonthDayIsPrimitive: Primitive[xs#gMonthDay] = Primitive.witness[xs#gMonthDay]
  implicit val gMonthDayIsBuiltIn: BuiltIn[xs#gMonthDay] = BuiltIn.witness[xs#gMonthDay]

  implicit val gDayHasQName: HasQName[xs#gDay] = HasQName[xs#gDay]("xs:gDay")
  implicit def gDayLexicalMapping: LexicalMapping[xs#gDay]
  implicit val gDayIsPrimitive: Primitive[xs#gDay] = Primitive.witness[xs#gDay]
  implicit val gDayIsBuiltIn: BuiltIn[xs#gDay] = BuiltIn.witness[xs#gDay]

  implicit val gMonthHasQName: HasQName[xs#gMonth] = HasQName[xs#gMonth]("xs:gMonth")
  implicit def gMonthLexicalMapping: LexicalMapping[xs#gMonth]
  implicit val gMonthIsPrimitive: Primitive[xs#gMonth] = Primitive.witness[xs#gMonth]
  implicit val gMonthIsBuiltIn: BuiltIn[xs#gMonth] = BuiltIn.witness[xs#gMonth]

  implicit val booleanHasQName: HasQName[xs#boolean] = HasQName[xs#boolean]("xs:boolean")
  implicit def booleanLexicalMapping: LexicalMapping[xs#boolean]
  implicit val booleanIsPrimitive: Primitive[xs#boolean] = Primitive.witness[xs#boolean]
  implicit val booleanIsBuiltIn: BuiltIn[xs#boolean] = BuiltIn.witness[xs#boolean]
  implicit def booleanValueSpace: BooleanValueSpace[xs#boolean] = new BooleanValueSpace[xs#boolean] {
    override val trueValue: xs#boolean  = lit"true".^^[xs#boolean]
    override val falseValue: xs#boolean = lit"false".^^[xs#boolean]
    override def booleanFrom(b: Boolean) = if(b) trueValue else falseValue
  }

  implicit val base64BinaryHasQName: HasQName[xs#base64Binary] = HasQName[xs#base64Binary]("xs:base64Binary")
  implicit def base64LexicalMapping: LexicalMapping[xs#base64Binary]
  implicit val base64BinaryIsPrimitive: Primitive[xs#base64Binary] = Primitive.witness[xs#base64Binary]
  implicit val base64BinaryIsBuiltIn: BuiltIn[xs#base64Binary] = BuiltIn.witness[xs#base64Binary]

  implicit val hexBinaryHasQName: HasQName[xs#hexBinary] = HasQName[xs#hexBinary]("xs:hexBinary")
  implicit def hexBinaryLexicalMapping: LexicalMapping[xs#hexBinary]
  implicit val hexBinaryIsPrimitive: Primitive[xs#hexBinary] = Primitive.witness[xs#hexBinary]
  implicit val hexBinaryIsBuiltIn: BuiltIn[xs#hexBinary] = BuiltIn.witness[xs#hexBinary]

  implicit val floatHasQName: HasQName[xs#float] = HasQName[xs#float]("xs:float")
  implicit def floatLexicalMapping: LexicalMapping[xs#float]
  implicit val floatIsPrimitive: Primitive[xs#float] = Primitive.witness[xs#float]
  implicit val floatIsBuiltIn: BuiltIn[xs#float] = BuiltIn.witness[xs#float]

  implicit val decimalHasQName: HasQName[xs#decimal] = HasQName[xs#decimal]("xs:decimal")
  implicit def decimalLexicalMapping: LexicalMapping[xs#decimal]
  implicit val decimalIsPrimitive: Primitive[xs#decimal] = Primitive.witness[xs#decimal]
  implicit val decimalIsBuiltIn: BuiltIn[xs#decimal] = BuiltIn.witness[xs#decimal]

  implicit val doubleHasQName: HasQName[xs#double] = HasQName[xs#double]("xs:double")
  implicit def doubleLexicalMapping: LexicalMapping[xs#double]
  implicit val doubleIsPrimitive: Primitive[xs#double] = Primitive.witness[xs#double]
  implicit val doubleIsBuiltIn: BuiltIn[xs#double] = BuiltIn.witness[xs#double]

  implicit val anyURIHasQName: HasQName[xs#anyURI] = HasQName[xs#anyURI]("xs:anyURI")
  implicit def anyURILexicalMapping: LexicalMapping[xs#anyURI]
  implicit val anyURIIsPrimitive: Primitive[xs#anyURI] = Primitive.witness[xs#anyURI]
  implicit val anyURIIsBuiltIn: BuiltIn[xs#anyURI] = BuiltIn.witness[xs#anyURI]

  implicit val QNameHasQName: HasQName[xs#QName] = HasQName[xs#QName]("xs:QName")
  implicit def QNameLexicalMapping: LexicalMapping[xs#QName]
  implicit val QNameIsPrimitive: Primitive[xs#QName] = Primitive.witness[xs#QName]
  implicit val QNameIsBuiltIn: BuiltIn[xs#QName] = BuiltIn.witness[xs#QName]

  implicit val NOTATIONHasQName: HasQName[xs#NOTATION] = HasQName[xs#NOTATION]("xs:NOTATION")
  implicit def NOTATIONLexicalMapping: LexicalMapping[xs#NOTATION]
  implicit val NOTATIONIsPrimitive: Primitive[xs#NOTATION] = Primitive.witness[xs#NOTATION]
  implicit val NOTATIONIsBuiltIn: BuiltIn[xs#NOTATION] = BuiltIn.witness[xs#NOTATION]

}