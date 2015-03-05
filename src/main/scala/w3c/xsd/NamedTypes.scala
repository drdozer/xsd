package w3c.xsd

import simulacrum.typeclass

/**
 * Association of types with QNames.
 */
trait NamedTypes[xs <: SpecialAndPrimitiveTypes] {

  protected implicit def QNameLexicalSpace: LexicalMapping[xs#QName]

  /**
   * A type has a qname associated with it.
   */
  @typeclass trait HasQName[T] {
    def qname: xs#QName
  }

  object HasQName {
    def apply[T](qname: String): HasQName[T] = {
      import LexicalMapping._
      val qn = qname.^^[xs#QName]
      new HasQName[T] {
        override def qname = qn
      }
    }
  }

  /**
   * Summon the qname for a type.
   */
  def qnameOf[T](implicit hq: HasQName[T]): xs#QName = hq.qname

}