package w3c.tests

import shapeless._
import shapeless.ops.hlist._
import w3c.typeclass._

/**
 *
 *
 * @author Matthew Pocock
 */
object CasterTest {

  implicit val longIntCaster: Caster[Long, Int] = new Caster[Long, Int] {
    override def downcast(l: Long): Option[Int] = if(l > Int.MaxValue) None else Some(l.toInt)
    override def upcast(i: Int) = i.toLong
  }

  implicit val longShortCaster: Caster[Long, Short] = new Caster[Long, Short] {
    override def downcast(l: Long): Option[Short] = if(l > Short.MaxValue) None else Some(l.toShort)
    override def upcast(s: Short) = s.toLong
  }

  val fsH = { (s: Short) => s"Short($s)" } ::
    { (i: Int) => s"Int($i)"} :: HNil

  val fsH1 = { (s: Short) => s"Short($s)" } ::
    { (i: Int) => s"Int($i)"} ::
    { (l: Long) => s"Long($l)" } :: HNil

  val fsT = (
    (s: Short) => s"Short($s)",
    (i: Int) => s"Int($i)")

  val fsT1 = (
    (s: Short) => s"Short($s)",
    (i: Int) => s"Int($i)",
    (l: Long) => s"Long($l)")

  type shortOrInt = Coproduct.`Short, Int`.T

  def main(args: Array[String]): Unit = {
    withCasters
//    withSealing
    withHierarchy
  }

  def withCasters: Unit = {

    val slHFolder = implicitly[CastFolder[Long, String, (Short => String)::(Int => String)::HNil]]

    println("Folding with a summoned CastFolder using a hlist of functions")
    println(slHFolder.fold(456456445455642312L, fsH))
    println(slHFolder.fold(233566234L, fsH))
    println(slHFolder.fold(234L, fsH))

    val slTFolder = implicitly[CastFolder[Long, String, (Short=>String, Int=>String)]]

    println("Folding with a summoned CastFolder using a tuple of functions")
    println(slTFolder.fold(456456445455642312L, fsT))
    println(slTFolder.fold(233566234L, fsT))
    println(slTFolder.fold(234L, fsT))

  }

  def withSealing: Unit = {
    import SealingFolder._

    implicit val longIsShortOrInt = Sealing[Long, shortOrInt]

    println("Folding over `U` using syntax from Sealed using a hlist of functions")
    //println(456456445455642312L.foldS(fsH)) // throws exception as we are assuming this to be sealed!
    println(233566234L.foldS(fsH))
    println(234L.foldS(fsH))

    println("Folding over `U` using syntax from Sealed using a tuple of functions")
    //println(456456445455642312L.foldS(fsT)) // throws exception as we are assuming this to be sealed!
    println(233566234L.foldS(fsT))
    println(234L.foldS(fsT))

  }

  def withHierarchy: Unit = {
    import HierarchyFolder._

    implicit val longHierarchy: Long >:~> shortOrInt = Hierarchy[Long, shortOrInt]

    println("Folding over `U` using syntax from Sealed using a hlist of functions")
    println(456456445455642312L.foldH(fsH1))
    println(233566234L.foldH(fsH1))
    println(234L.foldH(fsH1))

    println("Folding over `U` using syntax from Sealed using a tuple of functions")
    println(456456445455642312L.foldH(fsT1))
    println(233566234L.foldH(fsT1))
    println(234L.foldH(fsT1))

    // println(456456445455642312L.fold(fsH)) // doesn't compile, as expected
  }
}
