package w3c.xsd

/**
 *
 *
 * @author Matthew Pocock
 */
trait XsdBuiltIn
  extends SpecialAndPrimitiveTypes
          with StringTypes
          with NumericTypes

trait XsdBuiltInHierarchy[xsd <: XsdBuiltIn]
  extends BuiltInPrimitivesHierarchy[xsd]
          with StringTypesHierarchy[xsd]
          with NumericTypesHierarchy[xsd]
{
  override type stringDescendents = stringSubtypes_StringTypes
  override type decimalDescendents = decimalSubtypes_NumericTypes
}