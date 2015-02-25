package w3c.typeclass

import shapeless._

/**
 * Whitness that a hlist `T1::T2::T3::...` is related to an hlist [X] `(X=>T1)::(X=>T2)::(X=>T3)...`.
 *
 * @tparam L  the input `HList`
 * @tparam X  the domain of the functions in `Out`
 *
 * @author Matthew Pocock
 */
trait WithDomain[L <: HList, X] {
  /**
   * A hlist type where the elements are functions with the domain `X`.
   */
  type Out <: HList
}

object WithDomain {
  def apply[L <: HList, X](implicit withDomain: WithDomain[L, X]): Aux[L, X, withDomain.Out] = withDomain

  type Aux[L <: HList, X, Out0 <: HList] = WithDomain[L, X] { type Out = Out0 }

  implicit def hnillWithDomain[X]: Aux[HNil, X, HNil] = new WithDomain[HNil, X] { type Out = HNil }

  implicit def hlistWithDomain[H, T <: HList, X, OutM <: HList]
  (implicit mt: WithDomain.Aux[T, X, OutM]): WithDomain.Aux[H :: T, X, (X => H) :: OutM] =
    new WithDomain[H :: T, X] { type Out = (X => H) :: OutM }
}



/**
 * Whitness that a hlist `T1::T2::T3::...` is related to an hlist [X] `(T1=>X)::(T2=>X)::(T3=>X)...`.
 *
 * @tparam L  the input `HList`
 * @tparam X  the domain of the functions in `Out`
 *
 * @author Matthew Pocock
 */
trait WithRange[L <: HList, X] {
  /**
   * A hlist type where the elements are functions with the range `X`.
   */
  type Out <: HList
}

object WithRange {
  def apply[L <: HList, X](implicit withRange: WithRange[L, X]): Aux[L, X, withRange.Out] = withRange

  type Aux[L <: HList, X, Out0 <: HList] = WithRange[L, X] { type Out = Out0 }

  implicit def hnillWithRange[X]: Aux[HNil, X, HNil] = new WithRange[HNil, X] { type Out = HNil }

  implicit def hlistWithRange[H, T <: HList, X, OutM <: HList]
  (implicit mt: WithRange.Aux[T, X, OutM]): WithRange.Aux[H :: T, X, (H => X) :: OutM] =
    new WithRange[H :: T, X] { type Out = (H => X) :: OutM }
}