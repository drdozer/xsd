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
