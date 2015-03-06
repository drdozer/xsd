package w3c.typeclass

import shapeless._
import shapeless.ops.coproduct.ToHList

import scala.annotation.implicitNotFound

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


/**
 * Collect implicits for some type `I[_]` for each element of a coproduct into an HList.
 *
 * @tparam I    the implicit to collect
 * @tparam CP   the coproduct to collect for
 */
trait AllImplicitly[I[_], CP <: Coproduct] {
  type Out <: HList
  val out: Out
}

object AllImplicitly {

  def apply[I[_], CP <: Coproduct](implicit all: AllImplicitly[I, CP]): AllImplicitly[I, CP] = all

  type Aux[I[_], CP <: Coproduct, Out0] = AllImplicitly[I, CP] { type Out = Out0 }

  def nillImplicitly[I[_]]: AllImplicitly[I, CNil] = new AllImplicitly[I, CNil] {
    type Out = HNil
    val out = HNil
  }

  def cconsImplicitly[I[_], H, Tl <: Coproduct]
  (implicit ih: I[H], allT: AllImplicitly[I, Tl]): AllImplicitly[I, H :+: Tl] = new AllImplicitly[I, H :+: Tl] {
    type Out = I[H] :: allT.Out
    val out = ih :: allT.out
  }

}

@implicitNotFound("Unable to prove that an implicit for ${T} does not exist")
trait DoesNotExist[T]

object DoesNotExist extends LowPriorityDoesNotExist {
  implicit def ambiguousPriorityDoesNotExist1[T](implicit ev: T): DoesNotExist[T] = ???
  implicit def ambiguousPriorityDoesNotExist2[T](implicit ev: T): DoesNotExist[T] = ???
}

trait LowPriorityDoesNotExist {
  implicit def lowPriorityDoesNotExist[T]: DoesNotExist[T] = new DoesNotExist[T] {}
}


@implicitNotFound("Unable to find implicit witnesses for all types in ${L}")
trait AllExists[L <: HList] {
  val out: L
}

object AllExists {
  def apply[L <: HList](implicit all: AllExists[L]): AllExists[L] = all

  implicit def allHNil: AllExists[HNil] = new AllExists[HNil] {
    override val out: HNil = HNil
  }

  implicit def allHCons[H, T <: HList](implicit h: H, allTail: AllExists[T]): AllExists[H :: T] =
    new AllExists[H :: T] {
      override val out: H :: T = h :: allTail.out
    }

  implicit def allT[T, R <: HList](implicit
                                   generic: Generic.Aux[T, R],
                                   all: AllExists[R]): AllExists[R] = all
}


@implicitNotFound("Unable to find an implicit witness for at least one type in ${C}")
trait SomeExists[C <: Coproduct] {
  val out: C
}

object SomeExists {
  def apply[C <: Coproduct](implicit some: SomeExists[C]): SomeExists[C] = some

  implicit def someInl[H, Tl <: Coproduct](implicit h: H): SomeExists[H :+: Tl] =
    new SomeExists[H :+: Tl] {
      val out = Inl(h)
    }

  implicit def someInr[H, Tl <: Coproduct](implicit someTl: SomeExists[Tl]): SomeExists[H :+: Tl] =
    new SomeExists[H :+: Tl] {
      val out = Inr(someTl.out)
    }
}


@implicitNotFound("Unable to demonstrate that exactly one type in ${C} has an implicit witness")
trait OneExists[C <: Coproduct] {
  val out: C
}

object OneExists {
  def apply[C <: Coproduct](implicit one: OneExists[C]): OneExists[C] = one

  implicit def oneInl[H, Tl <: Coproduct](implicit
                                          h: H,
                                          noTl: DoesNotExist[OneExists[Tl]]): OneExists[H :+: Tl] =
    new OneExists[H :+: Tl] {
      val out = Inl(h)
    }

  implicit def oneInr[H, Tl <: Coproduct](implicit oneTl: OneExists[Tl]): OneExists[H :+: Tl] =
    new OneExists[H :+: Tl] {
      val out = Inr(oneTl.out)
    }

}

trait ZipApply[FL <: HList, AC <: Coproduct] extends DepFn2[FL, AC] { type Out <: Coproduct }

object ZipApply {
  def apply[FL <: HList, AC <: Coproduct](implicit zip: ZipApply[FL, AC]): Aux[FL, AC, zip.Out] = zip

  type Aux[FL <: HList, AC <: Coproduct, Out0 <: Coproduct] = ZipApply[FL, AC] { type Out = Out0 }

  implicit def hnilZipApply: Aux[HNil, CNil, CNil] =
    new ZipApply[HNil, CNil] {
      type Out = CNil
      def apply(fl: HNil, ac: CNil): Out = ac
    }

  implicit def hconsZipApply[H, A, LCons <: HList, CCons <: Coproduct, ZCons <: Coproduct]
  (implicit zipApply: ZipApply.Aux[LCons, CCons, ZCons]): Aux[(H => A) :: LCons, H :+: CCons, A :+: ZCons] =
    new ZipApply[(H => A) :: LCons, H :+: CCons] {
      type Out = A :+: ZCons

      override def apply(fList: (H => A) :: LCons, union: H :+: CCons): Out = union match {
        case Inl(h) => Inl(fList.head(h))
        case Inr(tl) =>Inr(zipApply(fList.tail, tl))
      }
    }
}