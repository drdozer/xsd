package w3c.xsd

/**
 *
 *
 * @author Matthew Pocock
 */
sealed trait DatatypeIri[DT] {
  def typeIri: String
}

object DatatypeIri {
  def apply[DT](iri: String): DatatypeIri[DT] = new DatatypeIri[DT] {
    override def typeIri = iri
  }

  def apply[DT](implicit di: DatatypeIri[DT]): DatatypeIri[DT] = di
}