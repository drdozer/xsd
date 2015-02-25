package w3c.typeclass

import shapeless._
import shapeless.ops.coproduct.ToHList
import shapeless.ops.hlist._
import shapeless.poly._
import simulacrum.{op, typeclass}


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
  def apply(d: D): U

  /** Witness a downcast relationship from `U` to `D`.
    * 
    * This projects some instances of `U` to a more specific type `D`.
    */
  def unapply(u: U): Option[D]


  /**
   * Fold a function over `U` by applying it to the downcasted value.
   * 
   * @param u   the value to fold over
   * @param f   a function that will be applied to any downcasted value
   * @tparam X  the result type of the function folded
   * @return    the result of applying `f` to a successful downcast
   */
  def fold[X](u: U)(f: D => X): Option[X] = unapply(u).map(f)
  
}

object Caster {

  implicit def noCast[U]: Caster[U, U] = new Caster[U, U] {
    override def apply(d: U) = d
    override def unapply(u: U) = Some(u)
  }

}



/**
 * Encapsulate a folding over a type with muliple downcastings.
 *
 * @tparam U    the upcast type
 * @tparam X    the result type
 * @tparam Fs   a `HList` of functions from downcast types of `U` to `X`
 */
trait CastFolder[U, X, Fs] {
  def fold(u: U, fs: Fs): Option[X]
}

object CastFolder {

  trait fx[X] {
    type λ[T] = T => X
  }

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


  implicit def sealedFolder[U, Ds <: Coproduct, X, Hs <: HList, Fs <: HList]
  (implicit
    seal: Sealed[U, Ds],
    coProdAsHList: ToHList.Aux[Ds, Hs],
    asFunctions: Mapped.Aux[Hs, fx[X]#λ, Fs],
    folder: CastFolder[U, X, Fs]): CastFolder[U, X, Fs] = new CastFolder[U, X, Fs] {
    def fold(u: U, fs: Fs): Option[X] = folder.fold(u, fs)
  }

  implicit def foldWithTuple[U, X, Fs <: HList, Ts]
  (implicit
   toHList: Generic.Aux[Ts, Fs],
   folder: CastFolder[U, X, Fs]): CastFolder[U, X, Ts] = new CastFolder[U, X, Ts] {
    override def fold(u: U, ts: Ts) = folder.fold(u, toHList.to(ts))
  }

  implicit class FolderSyntax[U](_u: U) {
    def foldO[X, Fs](fs: Fs)(implicit folder: CastFolder[U, X, Fs]): Option[X] = folder.fold(_u, fs)
    def fold[X, Fs](fs: Fs)(implicit folder: CastFolder[U, X, Fs]): X = folder.fold(_u, fs).get
  }
}


/**
 * Witness that `U >:~> Ds` is sealed, that is, every instance of `U` is projected into `Ds`.
 *
 * @tparam U  the upcast type
 * @tparam Ds the downcast types
 */
trait Sealed[U, Ds]



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
trait >:~> [U, Ds] {
  type seal = Sealed[U, Ds]
//
//  def fold[X](u: U)(fs: Fs)(implicit folder: CastFolder[U, X, Fs]): X = folder.fold(u, Fs).getOrElse(defaut(u)).get
}
