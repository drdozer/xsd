package w3c.typeclass

import shapeless.{HList, Coproduct}
import shapeless.ops.coproduct.ToHList
import shapeless.ops.hlist.{Mapped, Tupler}
import simulacrum.typeclass

/**
 * Witness an upcast relationship from `D` to `U`.
 *
 * @tparam U  the upcast type
 * @tparam D  the downcast type
 *
 * @author Matthew Pocock
 */
trait Upcast[U, D] {
  def apply(d: D): U
}

/** Witness a downcast relationship from `U` to `D`.
 *
 * @tparam U  the upcast type
 * @tparam D  the downcast type
 *
 * @author Matthew Pocock
 */
trait Downcast[U, D] {
  def unapply(u: U): Option[D]
}

/**
 * Encapsulates an upcast/downcast pair.
 *
 * @tparam U  the upcast type
 * @tparam D  the downcast type
 *
 * @author Matthew Pocock
 */
trait Caster[U, D] extends Upcast[U, D] with Downcast[U, D]

object Caster {
  def apply[U, D](implicit upcast: Upcast[U, D], downcast: Downcast[U, D]): Caster[U, D] = new Caster[U, D] {
    override def apply(d: D) = upcast.apply(d)

    override def unapply(u: U) = downcast.unapply(u)
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
 * It is not implied or required that every `U` must have a corresponding instance in `Ds`. If `Ds` covers `U`
 *
 * @tparam U  the upcast type
 * @tparam Ds the downcast types
 */
trait >:~> [U, Ds] {
  type seal = Sealed[U, Ds]
}


/**
 * Witness a fold from `U` to `Ds`.
 *
 *
 *
 * @tparam U  the upcast type
 * @tparam Ds the downcast type
 */
trait Folder[U, Ds] {
  type Funcs[X]

  def fold[X](u: U)(f: Funcs[X]): X
}

object Folder extends Folder1 {

  trait ft[X] {
    type λ[T] = T => X
  }

  trait FAux[HL <: HList, Out0 <: HList] {
    type λ[X] = Mapped.Aux[HL, ft[X]#λ, Out0]
  }

  def foldDsSealed[U, Ds <: Coproduct, AsHList <: HList, ToFuncs <: HList]
  (s: Sealed[U, Ds])(implicit toHList: ToHList.Aux[Ds, AsHList],
                     toFuncs: FAux[AsHList, ToFuncs]): Folder[U, Ds] = new Folder[U, Ds]
  {
    type Funcs[X] = toFuncs.λ[X]

    override def fold[X](u: U)(f: Funcs[X]) = ???
  }

}

trait Folder1 {

  /**
   * Fold over a sealed hierarchy.
   *
   * This will apply a function to the downcast on successful downcasting. As this is sealed, all downcasts should be
   * successfull. If the downcast fails, an exception is raised.
   */
  def foldDSealed[U, D](s: Sealed[U, D])(implicit dc: Downcast[U, D]): Folder[U, D] = new Folder[U, D] {
    type Funcs[X] = D => X

    def fold[X](u: U)(f: Funcs[X]): X = f(dc.unapply(u).get)
  }

  /**
   * Fold over an open hierarchy.
   *
   * This will apply a function on a successful downcasting. If the value can not be downcast, a fall-through function
   * will be applied to value in its upcast type.
   *
   */
  def foldD[U, D](h: U >:~> D)(implicit dc: Downcast[U, D]): Folder[U, D] = new Folder[U, D] {
    type Funcs[X] = (
      D => X,
      U => X)

    override def fold[X](u: U)(f: Funcs[X]): X = dc.unapply(u).map(f._1).getOrElse(f._2(u))
  }

}