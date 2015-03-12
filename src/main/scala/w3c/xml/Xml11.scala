package w3c.xml

import org.parboiled2.RuleTrace.{ZeroOrMore, CharRange}
import org.parboiled2._
import shapeless.{:+:, HNil, ::}
import w3c.typeclass.{Companion1, Companion}

import scala.language.implicitConversions

/**
 * See: http://www.w3.org/TR/xml/
 */
trait Xml11 {

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-document
   */
  type document

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Char
   */
  type Char

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-RestrictedChar
   */

  /**
   * See: http://www.w3.org/TR/xml/#NT-S
   */
  type S

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-NameStartChar
   */
  type NameStartChar

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-NameChar
   */
  type NameChar

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Name
   */
  type Name

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Names
   */
  type Names

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Nmtoken
   */
  type Nmtoken

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Nmtokens
   */
  type Nmtokens

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-EntityValue
   */
  type EntityValue

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-AttValue
   */
  type AttValue

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-SystemLiteral
   */
  type SystemLiteral

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-PubidLiteral
   */
  type PubidLiteral

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-PubidChar
   */
  type PubidChar

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-CharData
   */
  type CharData

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Comment
   */
  type Comment

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-PI
   */
  type PI

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-PITarget
   */
  type PITarget

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-CDSect
   */
  type CDSect

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-CDStart
   */
  type CDStart

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-CData
   */
  type CData

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-CDEnd
   */
  type CDEnd

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-prolog
   */
  type prolog

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-XMLDecl
   */
  type XMLDecl

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-VersionInfo
   */
  type VersionInfo

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Eq
   */
  type Eq

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-VersionNum
   */
  type VersionNum

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Misc
   */
  type Misc

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-doctypedecl
   */
  type doctypedecl

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-DeclSep
   */
  type DeclSep

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-intSubset
   */
  type intSubset

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-markupdecl
   */
  type markupdecl

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-extSubset
   */
  type extSubset

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-extSubsetDecl
   */
  type extSubsetDecl

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-SDDecl
   */
  type SDDecl

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-element
   */
  type element

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-STag
   */
  type STag

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Attribute
   */
  type Attribute

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-ETag
   */
  type ETag

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-content
   */
  type content

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-EmptyElemTag
   */
  type EmptyElemenTag

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-elementdecl
   */
  type elementdecl

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-contentspec
   */
  type contentspec

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-children
   */
  type children

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-cp
   */
  type cp

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-choice
   */
  type choice

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-seq
   */
  type seq

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Mixed
   */
  type Mixed

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-AttlistDecl
   */
  type AttlistDecl

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-AttDef
   */
  type AttDef

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-AttType
   */
  type AttType

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-StringType
   */
  type StringType

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-TokenizedType
   */
  type TokenizedType

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-EnumeratedType
   */
  type EnumeratedType

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-NotationType
   */
  type NotationType

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Enumeration
   */
  type Enumeration

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-DefaultDecl
   */
  type DefaultDecl

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-conditionalSect
   */
  type conditionalSect

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-includeSect
   */
  type includeSect

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-ignoreSect
   */
  type ignoreSect

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-ignoreSectContents
   */
  type ignoreSectContents

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Ignore
   */
  type Ignore

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-CharRef
   */
  type CharRef

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Reference
   */
  type Reference

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-EntityRef
   */
  type EntityRef

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-PEReference
   */
  type PEReference

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-EntityDecl
   */
  type EntityDecl

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-GEDecl
   */
  type GEDecl

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-PEDecl
   */
  type PEDecl

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-EntityDef
   */
  type EntityDef

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-PEDef
   */
  type PEDef

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-ExternalID
   */
  type ExternalID

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-NDataDecl
   */
  type NDataDecl

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-TextDecl
   */
  type TextDecl

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-extParsedEnt
   */
  type extParsedEnt

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-EncodingDecl
   */
  type EncodingDecl

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-EncName
   */
  type EncName

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-NotationDecl
   */
  type NotationDecl

  /**
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-PublicID
   */
  type PublicID

}

trait Xml11Companions[xml <: Xml11] {

  trait document_Companion extends Companion[xml#document] {
    type Args = xml#prolog :: xml#element :: Seq[xml#Misc] :: HNil
  }
  implicit def document: document_Companion


//  trait Char_Companion extends Companion1[xml#Char] {
//    type A = xml#CodePoint
//  }
//  implicit def Char: Char_Companion
//
//  trait S_Companion extends Companion1[xml#S] {
//    type A = xml#CodePoint
//  }


}


trait Xml11Productions[xml <: Xml11] extends Parser {

  type CodePoint = Int

  val isHighSurrogate = CharPredicate.from(Character.isHighSurrogate)
  val isLowSurrogate = CharPredicate.from(Character.isLowSurrogate)
  val isDefined = CharPredicate.from(Character.isDefined)

  def codePoint: Rule1[CodePoint] = rule {
    (isHighSurrogate ~ isLowSurrogate ~ push(Character.toCodePoint(charAt(-2), charAt(-1)))) |
      (isDefined ~ push(charAt(-1).toInt))
  }

  trait CodePointPredicate extends (CodePoint => Boolean)

  def CodePointRange(min: CodePoint, max: CodePoint): CodePointPredicate = new CodePointPredicate {
    override def apply(v1: CodePoint) = (v1 >= min) && (v1 <= max)
  }

  implicit def codePointPredicateAsRule(cpp: CodePointPredicate): Rule[HNil, HNil] = rule {
    codePoint ~> ((cp: CodePoint) => if(cpp(cp)) MATCH else MISMATCH)
  }

  /**
   *
   * document	   ::=   	( prolog element Misc* ) - ( Char* RestrictedChar Char* )
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-document
   */
  lazy val P_document: Rule0 = rule {
    //!(zeroOrMore(P_Char) ~ P_RestrictedChar ~ zeroOrMore(P_Char)) ~
      (P_prolog ~ P_element ~ zeroOrMore(P_Misc)) }


  /**
   * Char	   ::=   	[#x1-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Char
   */
  lazy val P_Char: Rule0 = rule {
    CharPredicate('\u0001' to '\uD7FF') |
      CharPredicate('\uE000' to '\uFFFD') |
      CodePointRange(0x10000, 0x10FFFF)
  }

  /**
   * RestrictedChar	   ::=   	[#x1-#x8] | [#xB-#xC] | [#xE-#x1F] | [#x7F-#x84] | [#x86-#x9F]
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-RestrictedChar
   */
  lazy val P_RestrictedChar: Rule0 = rule {
    CharPredicate('\u0001' to '\u0008') |
      CharPredicate('\u000B' to '\u000C') |
      CharPredicate('\u000E' to '\u001F') |
      CharPredicate('\u007F' to '\u0084') |
      CharPredicate('\u0086' to '\u009F')
  }

  /**
   * S	   ::=   	(#x20 | #x9 | #xD | #xA)+
   *
   * See: http://www.w3.org/TR/xml/#NT-S
   */
  lazy val P_S: Rule0 = rule {
    oneOrMore(ch('\u0020') | '\u0009' | '\u000D' | '\u000A')
  }


  /**
   * ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-NameStartChar
   */
  lazy val P_NameStartChar: Rule0 = rule {
    ch(':') |
      CharPredicate('A' to 'Z') |
      '_'|
      CharPredicate('a' to 'z') |
      CharPredicate('\u00C0' to '\u00D6') |
      CharPredicate('\u00D8' to '\u00F6') |
      CharPredicate('\u00F8' to '\u02FF') |
      CharPredicate('\u0370' to '\u037D') |
      CharPredicate('\u037F' to '\u1FFF') |
      CharPredicate('\u200C' to '\u200D') |
      CharPredicate('\u2070' to '\u218F') |
      CharPredicate('\u2C00' to '\u2FEF') |
      CharPredicate('\u3001' to '\uD7FF') |
      CharPredicate('\uF900' to '\uFDCF') |
      CharPredicate('\uFDF0' to '\uFFFD') |
      CodePointRange(0x10000, 0xEFFFF)
  }


  /**
   * NameChar	   ::=   	NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300 to x036F] | [#x203F to x2040]
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-NameChar
   */
  lazy val P_NameChar: Rule0 = rule {
    P_NameStartChar |
      '-' |
      '.' |
      CharPredicate('0' to '9') |
      '\u00B7' |
      CharPredicate('\u0300' to '\u036F') |
      CharPredicate('\u203F' to '\u2040')
  } 

  /**
   * Name	   ::=   	NameStartChar (NameChar)*
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Name
   */
  lazy val P_Name: Rule0 = rule {
    P_NameStartChar ~ zeroOrMore(P_NameChar)
  }

  /**
   * Names	   ::=   	Name (#x20 Name)*
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Names
   */
  lazy val P_Names: Rule0 = rule {
    P_Name ~ zeroOrMore('\u0020' ~ P_Name)
  }

  /**
   * Nmtoken	   ::=   	(NameChar)+
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Nmtoken
   */
  lazy val P_Nmtoken: Rule0 = rule {
    oneOrMore(P_NameChar)
  }

  /**
   * Nmtokens	   ::=   	Nmtoken (#x20 Nmtoken)*
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Nmtokens
   */
  lazy val P_Nmtokens: Rule0 = rule {
    P_Nmtoken ~ zeroOrMore('\u0020' ~ P_Nmtoken)
  }

  /**
   * EntityValue	   ::=   	'"' ([^%&"] | PEReference | Reference)* '"'
   |  "'" ([^%&'] | PEReference | Reference)* "'"
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-EntityValue
   */
  lazy val P_EntityValue: Rule0 = rule {
    (ch('"') ~ zeroOrMore(CharPredicate("%&\"").negated | P_PEReference | P_Reference) ~ '"') |
      (ch('\'') ~ zeroOrMore(CharPredicate("%&\"").negated | P_PEReference | P_Reference) ~ '\'')
  }

  /**
   * AttValue	   ::=   	'"' ([^<&"] | Reference)* '"'
   |  "'" ([^<&'] | Reference)* "'"
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-AttValue
   */
  lazy val P_AttValue: Rule0 = rule {
    (ch('"') ~ zeroOrMore(CharPredicate("%&\"").negated | P_Reference) ~ '"') |
      ('\'' ~ zeroOrMore(CharPredicate("%&\"").negated | P_Reference) ~ '\'')
  }

  /**
   * SystemLiteral	   ::=   	('"' [^"]* '"') | ("'" [^']* "'")
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-SystemLiteral
   */
 lazy val P_SystemLiteral: Rule0 = rule {
    (ch('"') ~ zeroOrMore(CharPredicate('"').negated) ~ ch('"')) |
      (ch('\'') ~ zeroOrMore(CharPredicate('"').negated) ~ ch('\''))
  }

  /**
   * PubidLiteral	   ::=   	'"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
   *
   * http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-PubidLiteral
   */
  lazy val P_PubidLiteral: Rule0 = rule {
    (ch('"') ~ zeroOrMore(P_PubidChar) ~ ch('"')) |
          (ch('\'') ~ zeroOrMore(P_PubidChar) ~ ch('\''))
  }

  /**
   * PubidChar	   ::=   	#x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-PubidChar
   */
  lazy val P_PubidChar: Rule0 = rule {
    ch('\u0020') |
      '\u000D' |
      '\u000A' |
      (CharPredicate('a' to 'z') ++ CharPredicate('A' to 'Z') ++ CharPredicate('0' to '9')) |
    CharPredicate("-'()+,./:=?;!*#@$_%")
  }

  /**
   * CharData	   ::=   	[^<&]* - ([^<&]* ']]>' [^<&]*)
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-CharData
   */
  lazy val P_CharData: Rule0 = rule {
    zeroOrMore(!"]]>" ~ CharPredicate("<&").negated)
  }

  /**
   * Comment	   ::=   	'<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Comment
   */
  lazy val P_Comment: Rule0 = rule {
    "<!--" ~ zeroOrMore(!"--" ~ P_Char) ~ "-->"
  }

  /**
   * '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-PI
   */
  lazy val P_PI: Rule0 = rule {
    "<?" ~ P_PITarget ~ optional(P_S ~ zeroOrMore(!"?>" ~ P_Char)) ~ "?>"
  }

  /**
   * PITarget	   ::=   	Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-PITarget
   */
  lazy val P_PITarget: Rule0 = rule {
    capture(P_Name) ~> ((n : String) => if(n.toLowerCase() == "xml") MISMATCH else MATCH)
  }

  /**
   * CDSect	   ::=   	CDStart CData CDEnd
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-CDSect
   */
  lazy val P_CDSect: Rule0 = rule {
    P_CDStart ~ P_CData ~ P_CDEnd
  }

  /**
   * CDStart	   ::=   	'<![CDATA['
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-CDStart
   */
  lazy val P_CDStart: Rule0 = rule {
    "<![CDATA["
  }

  /**
   * CData	   ::=   	(Char* - (Char* ']]>' Char*))
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-CData
   */
  lazy val P_CData: Rule0 = rule {
    zeroOrMore(!"]]" ~ P_Char)
  }

  /**
   * CDEnd	   ::=   	']]>'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-CDEnd
   */
  lazy val P_CDEnd: Rule0 = rule {
    "]]>"
  }

  /**
   * prolog	   ::=   	XMLDecl Misc* (doctypedecl Misc*)?
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-prolog
   */
  lazy val P_prolog: Rule0 = rule {
    P_XMLDecl ~ zeroOrMore(P_Misc) ~ optional(P_doctypedecl ~ zeroOrMore(P_Misc))
  }

  /**
   * XMLDecl	   ::=   	'<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-XMLDecl
   */
  lazy val P_XMLDecl: Rule0 = rule {
    "<?xml" ~ P_VersionInfo ~ optional(P_EncodingDecl) ~ optional(P_SDDecl) ~ optional(P_S) ~ "?>"
  }

  /**
   * VersionInfo	   ::=   	S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-VersionInfo
   */
  lazy val P_VersionInfo: Rule0 = rule {
    P_S ~ "version" ~ P_Eq ~ (('\'' ~ P_VersionNum ~ '\'') | ('"' ~ P_VersionNum ~ '"'))
  }

  /**
   * Eq	   ::=   	S? '=' S?
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Eq
   */
  lazy val P_Eq: Rule0 = rule {
    optional(P_S) ~ '=' ~ optional(P_S)
  }

  /**
   * VersionNum	   ::=   	'1.1'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-VersionNum
   */
  lazy val P_VersionNum: Rule0 = rule {
    "1.1"
  }

  /**
   * Misc	   ::=   	Comment | PI | S
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Misc
   */
  lazy val P_Misc: Rule0 = rule {
    P_Comment | P_PI | P_S
  }

  /**
   * doctypedecl	   ::=   	'<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-doctypedecl
   */
  lazy val P_doctypedecl: Rule0 = rule {
    "<!DOCTYPE" ~ P_S ~ P_Name ~
      optional(P_S ~ P_ExternalID) ~
      optional(P_S) ~
      optional('[' ~ P_intSubset ~ ']' ~ optional(P_S)) ~
      '>'
  }

  /**
   * DeclSep	   ::=   	PEReference | S
   *
   * http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-DeclSep
   */
  lazy val P_DeclSep: Rule0 = rule {
    P_PEReference | P_S
  }

  /**
   * intSubset	   ::=   	(markupdecl | DeclSep)*
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-intSubset
   */
  lazy val P_intSubset: Rule0 = rule {
    zeroOrMore(P_markupdecl | P_DeclSep)
  }

  /**
   * markupdecl	   ::=   	elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-markupdecl
   */
  lazy val P_markupdecl: Rule0 = rule {
    P_elementdecl |
      P_AttlistDecl |
      P_EntityDecl |
      P_NotationDecl |
      P_PI |
      P_Comment
  }

  /**
   * extSubset	   ::=   	TextDecl? extSubsetDecl
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-extSubset
   */
  lazy val P_extSubset: Rule0 = rule {
    optional(P_TextDecl) ~ P_extSubsetDecl
  }

  /**
   * extSubsetDecl	   ::=   	( markupdecl | conditionalSect | DeclSep)*
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-extSubsetDecl
   */
  lazy val P_extSubsetDecl: Rule0 = rule {
    zeroOrMore(P_markupdecl | P_conditionalSect | P_DeclSep)
  }

  /**
   * SDDecl	   ::=   	S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-SDDecl
   */
  lazy val P_SDDecl: Rule0 = rule {
    P_S ~ "standalone" ~ P_Eq ~ (('\'' ~ ("yes" | "no") ~ '\'') | ('"' ~ ("yes" | "no") ~ '"'))
  }

  /**
   * element	   ::=   	EmptyElemTag
   | STag content ETag
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-element
   */
  lazy val P_element: Rule0 = rule {
    P_EmptyElemTag |
      (P_STag ~ P_content ~ P_ETag)
  }

  /**
   * STag	   ::=   	'<' Name (S Attribute)* S? '>'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-STag
   */
  lazy val P_STag: Rule0 = rule {
    ch('<') ~ P_Name ~ zeroOrMore(P_S ~ P_Attribute) ~ optional(P_S) ~ '>'
  }

  /**
   * Attribute	   ::=   	Name Eq AttValue
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Attribute
   */
  lazy val P_Attribute: Rule0 = rule {
    P_Name ~ P_Eq ~ P_AttValue
  }

  /**
   * ETag	   ::=   	'</' Name S? '>'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-ETag
   */
  lazy val P_ETag: Rule0 = rule {
    "</" ~ P_Name ~ optional(P_S) ~ '>'
  }

  /**
   * content	   ::=   	CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-content
   */
  lazy val P_content: Rule0 = rule {
    optional(P_CharData) ~ zeroOrMore((P_element | P_Reference | P_CDSect | P_PI | P_Comment) ~ optional(P_CharData))
  }

  /**
   * EmptyElemTag	   ::=   	'<' Name (S Attribute)* S? '/>'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-EmptyElemTag
   */
  lazy val P_EmptyElemTag: Rule0 = rule {
    ch('<') ~ P_Name ~ zeroOrMore(P_S ~ P_Attribute) ~ optional(P_S) ~ "/>"
  }

  /**
   * elementdecl	   ::=   	'<!ELEMENT' S Name S contentspec S? '>'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-elementdecl
   */
  lazy val P_elementdecl: Rule0 = rule {
    "<!ELEMENT" ~ P_S ~ P_Name ~ P_S ~ P_contentspec ~ optional(P_S) ~ '>'
  }

  /**
   * contentspec	   ::=   	'EMPTY' | 'ANY' | Mixed | children
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-contentspec
   */
  lazy val P_contentspec: Rule0 = rule {
    "EMPTY" | "ANY" | P_Mixed | P_children
  }

  /**
   * children	   ::=   	(choice | seq) ('?' | '*' | '+')?
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-children
   */
  lazy val P_children: Rule0 = rule {
    (P_choice | P_seq) ~ optional(ch('?') | '*' | '+')
  }

  /**
   * cp	   ::=   	(Name | choice | seq) ('?' | '*' | '+')?
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-cp
   */
  lazy val P_cp: Rule0 = rule {
    (P_Name | P_choice | P_seq) ~ optional(ch('?') | '*' | '+')
  }

  /**
   * choice	   ::=   	'(' S? cp ( S? '|' S? cp )+ S? ')'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-choice
   */
  lazy val P_choice: Rule0 = rule {
    ch('(') ~ optional(P_S) ~ P_cp ~ oneOrMore(optional(P_S) ~ '|' ~ optional(P_S) ~ P_cp) ~ optional(P_S) ~ ')'
  }

  /**
   * seq	   ::=   	'(' S? cp ( S? ',' S? cp )* S? ')'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-seq
   */
  lazy val P_seq: Rule0 = rule {
    ch('(') ~ optional(P_S) ~ P_cp ~ zeroOrMore(optional(P_S) ~ ',' ~ optional(P_S) ~ P_cp) ~ optional(P_S) ~ ')'
  }

  /**
   * Mixed	   ::=   	'(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'
   | '(' S? '#PCDATA' S? ')'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Mixed
   */
  val P_Mixed: Rule0 = rule {
    ( ch('(') ~ optional(P_S) ~ "#PCDATA" ~ zeroOrMore(optional(P_S) ~ '|' ~ optional(P_S) ~ P_Name) ~ optional(P_S) ~ ")*") |
      (ch('(') ~ optional(P_S) ~ "#PCDATA" ~ optional(P_S) ~ ')')
  }

  /**
   * AttlistDecl	   ::=   	'<!ATTLIST' S Name AttDef* S? '>'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-AttlistDecl
   */
  lazy val P_AttlistDecl: Rule0 = rule {
    "<!ATTLIST" ~ P_S ~ P_Name ~ zeroOrMore(P_AttDef) ~ optional(P_S) ~ '>'
  }

  /**
   * AttDef	   ::=   	S Name S AttType S DefaultDecl
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-AttDef
   */
  lazy val P_AttDef: Rule0 = rule {
    P_S ~ P_Name ~ P_S ~ P_AttType ~ P_S ~ P_DefaultDecl
  }

  /**
   * AttType	   ::=   	StringType | TokenizedType | EnumeratedType
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-AttType
   */
  lazy val P_AttType: Rule0 = rule {
    P_StringType | P_TokenizedType | P_EnumeratedType
  }

  /**
   * StringType	   ::=   	'CDATA'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-StringType
   */
  lazy val P_StringType: Rule0 = rule {
    "CDATA"
  }

  /**
   * TokenizedType	   ::=   	'ID'
   | 'IDREF'
   | 'IDREFS'
   | 'ENTITY'
   | 'ENTITIES'
   | 'NMTOKEN'
   | 'NMTOKENS'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-TokenizedType
   */
  lazy val P_TokenizedType: Rule0 = rule {
    "ID" |
    "IDREF" |
    "IDREFS" |
    "ENTITY" |
    "ENTITIES" |
    "NMTOKEN" |
    "NMTOKENS"
  }

  /**
   * EnumeratedType	   ::=   	NotationType | Enumeration
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-EnumeratedType
   */
  lazy val P_EnumeratedType: Rule0 = rule {
    P_NotationType | P_Enumeration
  }

  /**
   * NotationType	   ::=   	'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-NotationType
   */
  lazy val P_NotationType: Rule0 = rule {
    "NOTATION" ~ P_S ~ '(' ~ optional(P_S) ~ P_Name ~ zeroOrMore(optional(P_S) ~ '|' ~ optional(P_S) ~ P_Name) ~ optional(P_S) ~ ')'
  }

  /**
   *  Enumeration	   ::=   	'(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
   *
   *  See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Enumeration
   */
  lazy val P_Enumeration: Rule0 = rule {
    ch('(') ~ optional(P_S) ~ P_Nmtoken ~ zeroOrMore(optional(P_S) ~ '|' ~ optional(P_S) ~ P_Nmtoken) ~ optional(P_S) ~ ')'
  }

  /**
   * DefaultDecl	   ::=   	'#REQUIRED' | '#IMPLIED'
   | (('#FIXED' S)? AttValue)
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-DefaultDecl
   */
  lazy val P_DefaultDecl: Rule0 = rule {
    "#REQUIRED" | "#IMPLIED" | (optional("#FIXED" ~ P_S) ~ P_AttValue)
  }

  /**
   * conditionalSect	   ::=   	includeSect | ignoreSect
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-conditionalSect
   */
  lazy val P_conditionalSect: Rule0 = rule {
    P_includeSect | P_ignoreSect
  }

  /**
   * includeSect	   ::=   	'<![' S? 'INCLUDE' S? '[' extSubsetDecl ']]>'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-includeSect
   */
  lazy val P_includeSect: Rule0 = rule {
    "<!" ~ optional(P_S) ~ "INCLUDE" ~ P_S ~ '[' ~ P_extSubsetDecl ~ "]]>"
  }

  /**
   * ignoreSect	   ::=   	'<![' S? 'IGNORE' S? '[' ignoreSectContents* ']]>'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-ignoreSect
   */
  lazy val P_ignoreSect: Rule0 = rule {
    "<![" ~ optional(P_S) ~ "IGNORE" ~ optional(P_S) ~ '[' ~ zeroOrMore(P_ignoreSectContents) ~ "]]>"
  }

  /**
   * ignoreSectContents	   ::=   	Ignore ('<![' ignoreSectContents ']]>' Ignore)*
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-ignoreSectContents
   */
  lazy val P_ignoreSectContents: Rule0 = rule {
    P_Ignore ~ zeroOrMore("<![" ~ P_ignoreSectContents ~ "]]>" ~ P_Ignore)
  }

  /**
   * Ignore	   ::=   	Char* - (Char* ('<![' | ']]>') Char*)
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Ignore
   */
  lazy val P_Ignore: Rule0 = rule {
    zeroOrMore(!"<![" ~ !"]]>" ~ P_Char)
  }

  /**
   * CharRef	   ::=   	'&#' [0-9]+ ';'
   | '&#x' [0-9a-fA-F]+ ';'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-CharRef
   */
  lazy val P_CharRef: Rule0 = rule {
    ("&#" ~ oneOrMore(CharPredicate('0' to '9')) ~ ';') |
      ("&#x" ~ oneOrMore(CharPredicate('0' to '9') ++ CharPredicate('a' to 'f') ++ CharPredicate('A' to 'F')) ~ ';')
  }

  /**
   * Reference	   ::=   	EntityRef | CharRef
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-Reference
   */
  lazy val P_Reference: Rule0 = rule {
    P_EntityRef | P_CharRef
  }

  /**
   * EntityRef	   ::=   	'&' Name ';'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-EntityRef
   */
  lazy val P_EntityRef: Rule0 = rule {
    ch('&') ~ P_Name ~ ';'
  }

  /**
   * PEReference	   ::=   	'%' Name ';'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-PEReference
   */
  lazy val P_PEReference: Rule0 = rule {
    ch('%') ~ P_Name ~ ';'
  }

  /**
   * EntityDecl	   ::=   	GEDecl | PEDecl
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-EntityDecl
   */
  lazy val P_EntityDecl: Rule0 = rule {
    P_GEDecl | P_PEDecl
  }

  /**
   * GEDecl	   ::=   	'<!ENTITY' S Name S EntityDef S? '>'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-GEDecl
   */
  lazy val P_GEDecl: Rule0 = rule {
    "<!ENTITY" ~ P_S ~ P_Name ~ P_S ~ P_EntityDef ~ optional(P_S) ~ '>'
  }

  /**
   * PEDecl	   ::=   	'<!ENTITY' S '%' S Name S PEDef S? '>'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-PEDecl
   */
  lazy val P_PEDecl: Rule0 = rule {
    "<!ENTITY" ~ P_S ~ '%' ~ P_S ~ P_Name ~ P_S ~ P_PEDef ~ optional(P_S) ~ '>'
  }

  /**
   * EntityDef	   ::=   	EntityValue | (ExternalID NDataDecl?)
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-EntityDef
   */
  lazy val P_EntityDef: Rule0 = rule {
    P_EntityValue | (P_ExternalID ~ optional(P_NDataDecl))
  }

  /**
   * PEDef	   ::=   	EntityValue | ExternalID
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-PEDef
   */
  lazy val P_PEDef: Rule0 = rule {
    P_EntityValue | P_ExternalID
  }

  /**
   * ExternalID	   ::=   	'SYSTEM' S SystemLiteral
   | 'PUBLIC' S PubidLiteral S SystemLiteral
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-ExternalID
   */
  lazy val P_ExternalID: Rule0 = rule {
    ("SYSTEM" ~ P_S ~ P_SystemLiteral) | ("PUBLIC" ~ P_S ~ P_PubidLiteral ~ P_S ~ P_SystemLiteral)
  }

  /**
   * NDataDecl	   ::=   	S 'NDATA' S Name
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-NDataDecl
   */
  lazy val P_NDataDecl: Rule0 = rule {
    P_S ~ "NDATA" ~ P_S ~ P_Name
  }

  /**
   * TextDecl	   ::=   	'<?xml' VersionInfo? EncodingDecl S? '?>'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-TextDecl
   */
  lazy val P_TextDecl: Rule0 = rule {
    "<?xml" ~ optional(P_VersionInfo) ~ P_EncodingDecl ~ optional(P_S) ~ "?>"
  }

  /**
   * extParsedEnt	   ::=   	( TextDecl? content ) - ( Char* RestrictedChar Char* )
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-extParsedEnt
   */
  lazy val P_extParsedEnt: Rule0 = rule {
    optional(P_TextDecl) ~ P_content // - ....
  }

  /**
   * EncodingDecl	   ::=   	S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-EncodingDecl
   */
  lazy val P_EncodingDecl: Rule0 = rule {
    P_S ~ "encoding" ~ P_Eq ~ (('"' ~ P_EncName ~ '"') | ('\'' ~ P_EncName ~ '\''))
  }

  /**
   * EncName	   ::=   	[A-Za-z] ([A-Za-z0-9._] | '-')*
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-EncName
   */
  lazy val P_EncName: Rule0 = rule {
    (CharPredicate('A' to 'Z') ++ CharPredicate('a' to 'z')) ~
      zeroOrMore((CharPredicate('A' to 'Z') ++ CharPredicate('a' to 'z') ++ CharPredicate('0' to '9')) | '-')
  }

  /**
   * NotationDecl	   ::=   	'<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-NotationDecl
   */
  lazy val P_NotationDecl: Rule0 = rule {
    "<!NOTATION" ~ P_S ~ P_Name ~ P_S ~ (P_ExternalID | P_PublicID) ~ optional(P_S) ~ '>'
  }

  /**
   * PublicID	   ::=   	'PUBLIC' S PubidLiteral
   *
   * See: http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-PublicID
   */
  lazy val P_PublicID: Rule0 = rule {
    "PUBLIC" ~ P_S ~ P_PubidLiteral
  }

}