package w3c.tests

import shapeless._
import shapeless.ops.hlist._
import w3c.typeclass._
import CastFolder._

/**
 *
 *
 * @author Matthew Pocock
 */
object CasterTest {

  def main(args: Array[String]): Unit = {

    implicit val longIntCaster: Caster[Long, Int] = new Caster[Long, Int] {
      override def unapply(l: Long): Option[Int] = if(l > Int.MaxValue) None else Some(l.toInt)
      override def apply(i: Int) = i.toLong
    }


    implicit val longShortCaster: Caster[Long, Short] = new Caster[Long, Short] {
      override def unapply(l: Long): Option[Short] = if(l > Short.MaxValue) None else Some(l.toShort)
      override def apply(s: Short) = s.toLong
    }

    val slHFolder = implicitly[CastFolder[Long, String, (Short => String)::(Int => String)::HNil]]

    val fsH = { (s: Short) => s"Short($s)" } ::
      { (i: Int) => s"Int($i)"} :: HNil

    println(slHFolder.fold(456456445455642312L, fsH))
    println(slHFolder.fold(233566234L, fsH))
    println(slHFolder.fold(234L, fsH))

    val slTFolder = implicitly[CastFolder[Long, String, (Short=>String, Int=>String)]]

    val fsT = (
      (s: Short) => s"Short($s)",
      (i: Int) => s"Int($i)")

    println(slTFolder.fold(456456445455642312L, fsT))
    println(slTFolder.fold(233566234L, fsT))
    println(slTFolder.fold(234L, fsT))

    println(456456445455642312L.foldO(fsH))
    println(233566234L.foldO(fsH))
    println(234L.foldO(fsH))

    println(456456445455642312L.foldO(fsT))
    println(233566234L.foldO(fsT))
    println(234L.foldO(fsT))


    type shortOrInt = Coproduct.`Short, Int`.T

//    implicit val longSealed = new Sealed[Long, shortOrInt] {}
//
//    CastFolder.sealedFolder[Long, shortOrInt, String, Short::Int::HNil, (Short => String)::(Int => String)::HNil]
//    val sealedCaster = implicitly[CastFolder[Long, String, shortOrInt]]
  }

}
