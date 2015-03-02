package w3c.xsd

/**
 * Witness that `F` is a facet.
 *
 * See: http://www.w3.org/TR/xmlschema-2/#facets
 */
sealed trait Facet[F] {
  type FA[DT] <: FacetApplication[F, DT]
}

/**
 * Witness that `F` is a fundamental facet.
 *
 * See: http://www.w3.org/TR/xmlschema-2/#fundamental-facets
 */
trait FundamentalFacet[F] extends Facet[F]

/** Witness that `F` is a constraint facet.
  *
  * See: http://www.w3.org/TR/xmlschema-2/#non-fundamental
  */
trait ConstrainingFacet[F] extends Facet[F]


// F : Facet
// DT : Datatype
trait FacetApplication[F, DT]
{
  type requires
}

trait RestrictionApplication[F, DT] extends FacetApplication[F, DT]
{
  type restrictionValue
  val byValue: restrictionValue
}

object RestrictionApplication {
  type Aux[F, DT, RV] = RestrictionApplication[F, DT] { type restrictionValue = RV }
}

trait FacetValidator[DT] {
  def isValid(dt: DT): Boolean
}