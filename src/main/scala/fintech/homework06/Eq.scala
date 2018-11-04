package fintech.homework06

/*
Реализовать тайп класс Eq[A] и синтаксис '===', деривацию для Map Seq Option
Опционально - разработать === для комплексных чисел с возможностью указать точность
*/

trait Eq[A] {
  def equiv(lft: A, rgt: A): Boolean
}

case class Scale(value: Int)

object Eq {

  implicit val intEq: Eq[Int] = (lft: Int, rgt: Int) => lft == rgt
  implicit val strEq: Eq[String] = (lft: String, rgt: String) => lft == rgt
  implicit val doubleEq: Eq[Double] = (lft: Double, rgt: Double) => lft == rgt

  implicit def eqSeq[A](implicit eq: Eq[A]): Eq[Seq[A]] = (lft, rgt) =>
    lft.zip(rgt).forall(values => eq.equiv(values._1, values._2))

  implicit def eqOption[T: Eq](implicit eq: Eq[T]): Eq[Option[T]] =
    (lft: Option[T], rgt: Option[T]) => (lft, rgt) match {
      case (Some(val1), Some(val2)) => eq.equiv(val1, val2)
      case (None, None) => true
      case _ => false
    }

  implicit def eqMap[A, B](implicit eqA: Eq[A], eqB: Eq[B]): Eq[Map[A, B]] = (lft, rgt) =>
      lft.values.zip(rgt.values).forall(values => eqB.equiv(values._1, values._2)) &&
        lft.keys.zip(rgt.keys).forall(keys => eqA.equiv(keys._1, keys._2))

  implicit def eqComplex(implicit scale: Scale = Scale(2), eq: Eq[Double]): Eq[ComplexNumber] =
    (lft, rgt) => eq.equiv(BigDecimal(lft.real).setScale(scale.value, BigDecimal.RoundingMode.HALF_EVEN).toDouble,
      BigDecimal(rgt.real).setScale(scale.value, BigDecimal.RoundingMode.HALF_EVEN).toDouble) &&
      eq.equiv(BigDecimal(lft.image).setScale(scale.value, BigDecimal.RoundingMode.HALF_EVEN).toDouble,
        BigDecimal(rgt.image).setScale(scale.value, BigDecimal.RoundingMode.HALF_EVEN).toDouble)

  implicit class EqClass[A](lftEq: A) {
    def ====(rgtEq: A)(implicit eq: Eq[A]): Boolean = eq.equiv(lftEq, rgtEq)
  }
}