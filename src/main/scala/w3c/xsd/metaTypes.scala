package w3c.xsd

import shapeless._
import shapeless.ops.coproduct.ExtendBy
import simulacrum.typeclass
import w3c.typeclass._

/**
 * API for working with the XSD datatypes.
 *
 * These together comprise the meta-type system that classify the various xs provided types.
 *
 * See: http://www.w3.org/TR/xmlschema11-2/#datatype-dichotomies
 */
trait Datatypes[xs <: SpecialAndPrimitiveTypes] {

  /**
   * Every datatype is one of; atomic, list, union.
   *
   * See: http://www.w3.org/TR/xmlschema11-2/#atomic-vs-list
   */
  type AtomicListUnion[T] = OneExists[Atomic[T] :+: List[T] :+: Union[T] :+: CNil]

  /**
   * Witness that a datatype is atomic.
   *
   * <blockquote>
   * An ·atomic· datatype has a ·value space· consisting of a set of "atomic" or elementary values.
   * </blockquote>
   *
   * See: http://www.w3.org/TR/xmlschema11-2/#atomic
   */
  trait Atomic[A]

  object Atomic {

    /**
     * The type `xs#anyAtomicType` is atomic.
     *
     * See: http://www.w3.org/TR/xmlschema11-2/#dt-atomic
     */
    implicit def isAnyAtomicType: Atomic[xs#anyAtomicType] =
      new Atomic[xs#anyAtomicType] {}

    /**
     * Any type derived from an atomic type is atomic.
     * 
     * See: http://www.w3.org/TR/xmlschema11-2/#dt-atomic
     */
    implicit def derivedFromAnyAtomicType[T, BT](implicit
                                                 hasBaseType: HasBaseType.Aux[T, BT],
                                                 btIsAtomic: Atomic[BT]): Atomic[T] =
      new Atomic[T] {}

  }


  /**
   * Witness that a type is a list.
   *
   * <blockquote>
   * ·List· datatypes are always ·constructed· from some other type; they are never ·primitive·. The ·value space· of a
   * ·list· datatype is the set of finite-length sequences of zero or more ·atomic· values where each ·atomic· value is
   * drawn from the ·value space· of the lists's ·item type· and has a ·lexical representation· containing no
   * whitespace.
   * </blockquote>
   *
   * See: http://www.w3.org/TR/xmlschema11-2/#list-datatypes
   */
  trait List[L] {
    type ItemType
  }

  object List {

    type Aux[L, I] = List[L] { type ItemType = I }

    /**
     * Construct a list type.
     */
    def apply[L, I](implicit itemTypeConstraint: ItemTypeConstraint[I]): List.Aux[L, I] =
      new List[L] { type ItemType = I }

    /**
     * Witness the item type constraint for list.
     *
     * See: http://www.w3.org/TR/xmlschema11-2/#dt-itemType
     */
    trait ItemTypeConstraint[I]

    /**
     * The item type is atomic.
     */
    implicit def itemIsAtomic[I](implicit atomic: Atomic[I]): ItemTypeConstraint[I] =
      new ItemTypeConstraint[I] {}

    /**
     * The item type is a union of atomic types.
     *
     * <blockquote>
     * If the ·item type· is a ·union·, each of its ·basic members· must be ·atomic·.
     * </blockquote>
     */
    implicit def itemIsUnionOfAtomic[I, MTs <: Coproduct](implicit
                                                          isUnion: Union.Aux[I, MTs],
                                                          allAtomic: AllImplicitly[Atomic, MTs]): ItemTypeConstraint[I] =
      new ItemTypeConstraint[I] {}

    // todo: is a type derived from a list by restriction also a list?

  }

  trait ListValueSpace[L, ItemType] {
    def elements(l: L): Seq[ItemType]
    def fromElements(items: ItemType*): L

  }


  /**
   * Witness that a type is a union.
   *
   * See: http://www.w3.org/TR/xmlschema11-2/#union-datatypes
   */
  trait Union[U] {
    /**
     * See: http://www.w3.org/TR/xmlschema11-2/#dt-memberTypes
     */
    type MemberTypes <: Coproduct
  }

  object Union {
    type Aux[U, MTs <: Coproduct] = Union[U] { type MemberTypes = MTs }

    /**
     * <blockquote>
     * Any number (zero or more) of ordinary or ·primitive· ·datatypes· can participate in a ·union· type.
     * </blockquote>
     */
    trait MemberTypeConstraint[MT]
    implicit def ordinaryIsMemberType[MT](implicit isOrdinary: Ordinary[MT]): MemberTypeConstraint[MT] =
      new MemberTypeConstraint[MT] {}
    implicit def primitiveIsMemberType[MT](implicit isPrimitive: Primitive[MT]): MemberTypeConstraint[MT] =
      new MemberTypeConstraint[MT] {}


    /**
     * Construct a union from member types.
     *
     * <pre>
     * When a union type is ·constructed· by ·union·, its ·value space·, ·lexical space·, and ·lexical mapping· are the
     * "ordered unions" of the ·value spaces·, ·lexical spaces·, and ·lexical mappings· of its ·member types·.
     * </pre>
     */
    def apply[U, MTs <: Coproduct]
    (implicit allConformToConstraint: AllImplicitly[MemberTypeConstraint, MTs]): Union.Aux[U, MTs] =
      new Union[U] { type MemberTypes = MTs }

    /**
     * Union by restriction.
     *
     * <pre>
     * When a union type is defined by ·restricting· another ·union·, its ·value space·, ·lexical space·, and ·lexical
     * mapping· are subsets of the ·value spaces·, ·lexical spaces·, and ·lexical mappings· of its ·base type·.
     * </pre>
     */
    implicit def restrictionOfUnionIsUnion[T, BT, MTs <: Coproduct]
    (implicit
     hasBaseType: HasBaseType.Aux[T, BT],
     isUnion: Union.Aux[BT, MTs]): Union.Aux[T, MTs] = new Union[T] { type MemberTypes = MTs }
  }

  trait UnionValueSpace[U, MemberTypes] {
    implicit val uIsUnion: Union[U]

    def asCoproduct(u: U): MemberTypes
    def fromCoproduct(ts: MemberTypes): U
  }

  trait UnionMembership[C <: Coproduct] {
    /**
     * See: http://www.w3.org/TR/xmlschema11-2/#dt-transitivemembership
     */
    type transitiveMembers <: Coproduct
    /**
     * See: http://www.w3.org/TR/xmlschema11-2/#dt-basicmembership
     */
    type basicMembers <: Coproduct
  }

  object UnionMembership {

    type Aux[C <: Coproduct, Tr <: Coproduct, Bs <: Coproduct] = UnionMembership[C] {
      type transitiveMembers = Tr
      type basicMembers = Bs
    }

    implicit def unionMembershipCNil: UnionMembership.Aux[CNil, CNil, CNil] = new UnionMembership[CNil] {
      type transitiveMembers = CNil
      type basicMembers = CNil
    }

    implicit def unionMembershipUnion[H, T <: Coproduct,
                                      MTs <: Coproduct, MTsTr <: Coproduct, MTsBs <: Coproduct,
                                      TUM <: Coproduct, TUMTr <: Coproduct, TUMBs <: Coproduct,
                                      Tr <: Coproduct, Bs <: Coproduct]
    (implicit
     union: Union.Aux[H, MTs],
     mtsMembership: UnionMembership.Aux[MTs, MTsTr, MTsBs],
     tumMembership: UnionMembership.Aux[TUM, TUMTr, TUMBs],
     trtr: ExtendBy.Aux[MTsTr, TUMTr, Tr],
     bsbs: ExtendBy.Aux[MTsBs, TUMBs, Bs]): UnionMembership.Aux[H :+: T, H :+: Tr, Bs] =
      new UnionMembership[H :+: T] {
        type transitiveMembers = H :+: Tr
        type basicMembers = Bs
      }

    implicit def unionMembershipNonUnion[H, T <: Coproduct, TUM <: Coproduct, TUMTr <: Coproduct, TUMBs <: Coproduct]
    (implicit
      notUnion: DoesNotExist[Union[H]],
     tumMembership: UnionMembership.Aux[TUM, TUMTr, TUMBs]): UnionMembership.Aux[H :+: T, H :+: TUMTr, H :+: TUMBs] =
      new UnionMembership[H :+: T] {
        type transitiveMembers = H :+: TUMTr
        type basicMembers = H :+: TUMBs
      }
  }

  // todo: implement constraint "The ·transitive membership· of a ·union· must not contain the ·union· itself, nor any datatype ·derived· or ·constructed· from the ·union·."

  // todo: implement active member type and active basic member. See: http://www.w3.org/TR/xmlschema11-2/#dt-active-member


  /**
   * Every datatype is one of; special, primitive, or ordinary.
   *
   * See: http://www.w3.org/TR/xmlschema11-2/#primitive-vs-derived
   */
  type SpecialPrimitiveOrdinary[T] = OneExists[Special[T] :+: Primitive[T] :+: Ordinary[T] :+: CNil]


  /**
   * Witness that `S` is a special (ur-) type.
   *
   * See: http://www.w3.org/TR/xmlschema11-2/#dt-special
   */
  sealed trait Special[S]

  object Special {
    private [xsd] def witness[S]: Special[S] = new Special[S] {}
  }


  /**
   * Witness that the datatype `DT` is a primitive type.
   *
   * <blockquote>
   * Primitive datatypes are those datatypes that are not ·special· and are not defined in terms of other datatypes;
   * they exist ab initio. All ·primitive· datatypes have anyAtomicType as their ·base type·, but their ·value· and
   * ·lexical spaces· must be given in prose; they cannot be described as ·restrictions· of anyAtomicType by the
   * application of particular ·constraining facets·.
   * </blockquote>
   *
   * See: http://www.w3.org/TR/xmlschema11-2/#dt-primitive
   */
  trait Primitive[P]

  object Primitive {
    private [xsd] def witness[P]: Primitive[P] = new Primitive[P] {}
  }


  /**
   * An ordinary datatype; not special or primitive.
   *
   * <blockquote>
   * Ordinary datatypes are all datatypes other than the ·special· and ·primitive· datatypes.  ·Ordinary· datatypes can
   * be understood fully in terms of their Simple Type Definition and the properties of the datatypes from which they
   * are ·constructed·.
   * </blockquote>
   *
   * See: http://www.w3.org/TR/xmlschema11-2/#dt-ordinary
   */
  trait Ordinary[O]

  object Ordinary {
    // todo: consider constraints or an implicit witness
    def witness[O]: Ordinary[O] = new Ordinary[O] {}
  }


  /**
   * Witness that the datatype `R` is derived by restriction.
   *
   * See: http://www.w3.org/TR/xmlschema11-2/#restriction
   *
   * @tparam R
   */
  trait Restricted[R] {
    /**
     * The type that this restriction is based on.
     */
    type BasedOn

    /**
     * The type of the zero or more applied facets.
     *
     * These facet applications are over the type `BasedOn`, rather than `R`.
     * For an instance of `BasedOn` to be an instance of `R`,
     *
     */
    type AppliedFacets <: HList

    /**
     * The applied facets.
     */
    def facets: AppliedFacets
  }

  object Restricted {
    type AuxBO[R, BT] = Restricted[R] { type BaseType = BT }
  }


  /**
   * Witness the base type of a type.
   *
   * See: http://www.w3.org/TR/xmlschema11-2/#dt-basetype
   */
  trait HasBaseType[T] {
    type BaseType
  }
  
  object HasBaseType extends LowPriorityHasBaseTypeImplicits {
    type Aux[T, BT] = HasBaseType[T] { type BaseType = BT }

    /**
     * The base type of `anySimpleType` is `anyType`.
     *
     * <blockquote>
     * The above does not preclude the Simple Type Definition for anySimpleType from having a value for its {base type
     * definition}.  (It does, and its value is anyType.)
     * </blockquote>
     */
    implicit val anySimpleTypeHasBaseType: HasBaseType.Aux[xs#anySimpleType, xs#anyType] =
      new HasBaseType[xs#anySimpleType] { type BaseType = xs#anyType }

    /**
     * The base type of a type constructed from a facet restriction is the type restricted.
     */
    implicit def restrictionHasBaseType[R, BT](implicit res: Restricted.AuxBO[R, BT]): HasBaseType.Aux[R, BT] =
      new HasBaseType[R] { type BaseType = BT }
  }

  trait LowPriorityHasBaseTypeImplicits {
    /**
     * The base type of a list is `anySimpleType`.
     */
    implicit def listHasBaseTypeXsAnySimpleType[L](implicit lIsList: List[L]): HasBaseType[L] =
      new HasBaseType[L] { type BaseType = xs#anySimpleType }

    /**
     * The base type of a union is `anySimpleType`.
     */
    implicit def unionHasBaseTypeXsAnySimpleType[U](implicit lIsUnion: Union[U]): HasBaseType[U] =
      new HasBaseType[U] { type BaseType = xs#anySimpleType }
  }


  /**
   * A datatype may be built-in or user-defined.
   *
   * See: http://www.w3.org/TR/xmlschema11-2/#built-in-vs-user-derived
   */
  type BuiltInUserDefined[T] = OneExists[BuiltIn[T] :+: UserDefined[T] :+: CNil]

  /**
   * Witness that a type is built-in.
   *
   * See: http://www.w3.org/TR/xmlschema11-2/#dt-built-in
   */
  trait BuiltIn[BI]

  object BuiltIn {
    private [xsd] def witness[BI]: BuiltIn[BI] = new BuiltIn[BI] {}
  }


  /**
   * Witness that a type is user-defined.
   *
   * See: http://www.w3.org/TR/xmlschema11-2/#dt-user-defined
   */
  trait UserDefined[UD]

  object UserDefined {
    def witness[UD]: UserDefined[UD] = new UserDefined[UD] {}
  }
}



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
