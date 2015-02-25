package w3c.xsd

/**
 *
 *
 * @author Matthew Pocock
 */
trait XsdBuiltIn
  extends AnyType
          with BuiltInPrimitives
          with StringTypes
          with NumericTypes

trait XsdBuiltInHierarchy[xsd <: XsdBuiltIn]
  extends AnyTypeHierarchy[xsd]
          with BuiltInPrimitivesHierarchy[xsd]
          with StringTypesHierarchy[xsd]
          with NumericTypesHierarchy[xsd]
{
  override type anySimpleTypesDescendents = SimpleTypeSubtypes_BuiltInPrimitives
  override type stringDescendents = stringSubtypes_StringTypes
  override type decimalDescendents = decimalSubtypes_NumericTypes
}