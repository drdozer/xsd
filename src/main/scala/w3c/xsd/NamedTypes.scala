package w3c.xsd

import simulacrum.typeclass

trait NamedTypes[xs <: XsdBuiltIn] {

  protected implicit def QNameLexicalSpace: LexicalSpace[xs#QName]

  /**
   * A type has a qname associated with it.
   */
  @typeclass trait HasQName[T] {
    def qname: xs#QName
  }

  object HasQName {
    def apply[T](qname: String): HasQName[T] = {
      import LexicalSpace._
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