package w3c.typeclass

import shapeless._
import shapeless.ops.coproduct.{ExtendRight, ToHList}
import scala.annotation.implicitNotFound


/**
 * Encapsulates an upcast/downcast pair.
 *
 * @tparam U  the upcast type
 * @tparam D  the downcast type
 *
 * @author Matthew Pocock
 */
trait Caster[U, D]
{

  /**
   * Witness an upcast relationship from `D` to `U`.
   * 
   * This projects `D` to the more general type `U`.
   */
  def upcast(d: D): U

  /** Witness a downcast relationship from `U` to `D`.
    * 
    * This projects some instances of `U` to a more specific type `D`.
    */
  def downcast(u: U): Option[D]


  /**
   * Fold a function over `U` by applying it to the downcasted value.
   * 
   * @param u   the value to fold over
   * @param f   a function that will be applied to any downcasted value
   * @tparam X  the result type of the function folded
   * @return    the result of applying `f` to a successful downcast
   */
  def fold[X](u: U)(f: D => X): Option[X] = downcast(u).map(f)
  
}

object Caster {

  implicit def noCast[U]: Caster[U, U] = new Caster[U, U] {
    override def upcast(d: U) = d
    override def downcast(u: U) = Some(u)
  }

}



/**
 * Encapsulate a folding over a type with muliple downcastings.
 *
 * @tparam U    the upcast type
 * @tparam X    the result type
 * @tparam Fs   a `HList` of functions from downcast types of `U` to `X`
 */
@implicitNotFound("Could not find a CastFolder from ${U} that can fold to ${X} using the functions ${Fs}")
trait CastFolder[U, X, Fs] {
  def fold(u: U, fs: Fs): Option[X]
}

object CastFolder {

  /**
   * Folding over no functions produces None.
   */
  implicit def foldHNil[U, X]: CastFolder[U, X, HNil] = new CastFolder[U, X, HNil] {
    def fold(u: U, hnil: HNil) = None
  }

  /**
   * Folding over an HList starting with a function.
   *
   * If the `U` value can be downcast to the type this function accepts, then return this value. Otherwise, apply the
   * tail of the list of functions.
   */
  implicit def foldHCons[U, D, X, L <: HList]
  (implicit
   caster: Caster[U, D],
   lFolder: CastFolder[U, X, L]): CastFolder[U, X, (D => X)::L] = new CastFolder[U, X, (D => X)::L]
  {
    def fold(u: U, fs: (D => X)::L): Option[X] = caster.fold(u)(fs.head) orElse lFolder.fold(u, fs.tail)
  }

  implicit def foldCastingWithTuple[U, X, Fs <: HList, Ts]
  (implicit
   toHList: Generic.Aux[Ts, Fs],
   folder: CastFolder[U, X, Fs]): CastFolder[U, X, Ts] = new CastFolder[U, X, Ts] {
    override def fold(u: U, ts: Ts) = folder.fold(u, toHList.to(ts))
  }
}


/**
 * Witness that `U >:~> Ds` is sealed, that is, every instance of `U` is projected into `Ds`.
 *
 * @tparam U  the upcast type
 * @tparam Ds the downcast types
 */
@implicitNotFound("Could not find a sealing for ${U} with downcasts ${Ds}")
sealed trait Sealing[U, Ds]

object Sealing {
  def apply[U, Ds]: Sealing[U, Ds] = new Sealing[U, Ds] {}
}


trait SealingFolder[U, X, Fs] {
  def fold(u: U, fs: Fs): Option[X]
}

object SealingFolder {

  implicit def sealingFolder[U, Ds <: Coproduct, X, Hs <: HList, Fs <: HList]
    (implicit
     sealing: Sealing[U, Ds],
     coProdAsHList: ToHList.Aux[Ds, Hs],
     asFunctions: WithRange.Aux[Hs, X, Fs],
     folder: CastFolder[U, X, Fs]): SealingFolder[U, X, Fs] = new SealingFolder[U, X, Fs]
    {
      def fold(u: U, fs: Fs): Option[X] = folder.fold(u, fs)
    }

  implicit def foldSealingWithTuple[U, X, Fs <: HList, Ts]
  (implicit
   toHList: Generic.Aux[Ts, Fs],
   folder: SealingFolder[U, X, Fs]): SealingFolder[U, X, Ts] = new SealingFolder[U, X, Ts] {
    override def fold(u: U, ts: Ts) = folder.fold(u, toHList.to(ts))
  }

  implicit class SealingSyntax[U, Ds](_u: U) {
    def foldS[X, Fs](fs: Fs)
                    (implicit
                     sealing: Sealing[U, Ds],
                     sealingFolder: SealingFolder[U, X, Fs]): X = sealingFolder.fold(_u, fs).get
  }
}


/**
 * Witness a hierarchy relation from the type `U` to the type(s) `Ds`.
 *
 * This relation captures an extension of the idea of subtyping. The subtype(s) `Ds` share a supertype `U`.
 *
 * If `Ds` is a single type, then this implies `Caster[U, Ds]`. If `Ds` is a `Coproduct` then this implies a
 * `Caster[U, D_i]` for each `D_i` in `Ds`.
 *
 * There may be instance of the supertype `U` that do not downcast into `Ds`. To state that `Ds` covers `U`, use
 * `seal` to return a `Sealed[U, Ds].`
 *
 * @tparam U  the upcast type
 * @tparam Ds the downcast types
 */
sealed trait >:~> [U, Ds] {
  type sealing = Sealing[U, Ds]

  def fold[X, Fs](u: U, fs: Fs)(implicit folder: CastFolder[U, X, Fs]): X = folder.fold(u, fs).get
}

object >:~> {
  def apply[U, Ds]: U >:~> Ds = new >:~>[U, Ds] {}
}

object Hierarchy {
  def apply[U, Ds]: U >:~> Ds = >:~>.apply[U, Ds]
}

trait HierarchyFolder[U, X, Fs] {
  def fold(u: U, fs: Fs): Option[X]
}

object HierarchyFolder {

  implicit def hierarchyFolder[U, Ds <: Coproduct, X, DsU <: Coproduct, Hs <: HList, Fs <: HList]
    (implicit
     hierarchy: U >:~> Ds,
     uOnEnd: ExtendRight.Aux[Ds, U, DsU],
     coProdAsHList: ToHList.Aux[DsU, Hs],
     asFunctions: WithRange.Aux[Hs, X, Fs],
     folder: CastFolder[U, X, Fs]): HierarchyFolder[U, X, Fs] = new HierarchyFolder[U, X, Fs]
    {
      def fold(u: U, fs: Fs): Option[X] = folder.fold(u, fs)
    }

  implicit def foldHierarchyWithTuple[U, X, Fs <: HList, Ts]
  (implicit
   toHList: Generic.Aux[Ts, Fs],
   folder: HierarchyFolder[U, X, Fs]): HierarchyFolder[U, X, Ts] = new HierarchyFolder[U, X, Ts] {
    override def fold(u: U, ts: Ts) = folder.fold(u, toHList.to(ts))
  }

  implicit class HierarchySyntax[U, Ds](_u: U) {
    def foldH[X, Fs](fs: Fs)
                    (implicit
                     hierarchy: U >:~> Ds,
                     hierarchyFolder: HierarchyFolder[U, X, Fs]): X = hierarchyFolder.fold(_u, fs).get
  }
}