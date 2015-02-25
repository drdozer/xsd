package w3c.xsd

import simulacrum._

/**
 *
 *
 * @author Matthew Pocock
 */
@typeclass trait XsdLiteralSyntax[Lit] {
  def render(lit: Lit): String
  def parse(s: String): Lit
}

object XsdLiteralSyntax {
  implicit class StringLiteralSyntax(val _s: String) extends AnyVal {
    def ^[Lit](syntax: XsdLiteralSyntax[Lit]): Lit = syntax.parse(_s)
  }
}