package fintech.homework06

/*
Реализовать тайп класс Eq[A] и синтаксис '===', деривацию для Map Seq Option
Опционально - разработать === для комплексных чисел с возможностью указать точность
*/

trait Eq[A] {
  def equiv(lft: A, rgt: A): Boolean
}

object Eq {

  implicit def Equivalent[A]: Eq[A] = (lft: A, rgt: A) => lft == rgt

  implicit class EqSeq[A](lft: Seq[A]) {
    def ====(rgt: Seq[A])(implicit eq: Eq[A]): Boolean = {
      if (lft.size == rgt.size) return forEach(lft.zip(rgt), eq)
      false
    }
  }

  private def forEach[A](seq: Seq[(A, A)], eq: Eq[A]): Boolean = {
    for (pair <- seq)
      if (!eq.equiv(pair._1, pair._2)) return false
    true
  }

  implicit class EqOption[A](lft: Option[A]) {
    def ====(rgt: Option[A])(implicit eq: Eq[A]): Boolean = (lft, rgt) match {
      case (Some(x1), Some(x2)) => eq.equiv(x1, x2)
      case (None, None) => true
      case _ => false
    }
  }

  implicit class EqMap[K, V](lft: Map[K, V]) {
    def ====(rgt: Map[K, V])(implicit kEq: Eq[K], vEq: Eq[V]): Boolean = {
      if (lft.size == rgt.size) return forEach(lft.keys.zip(rgt.keys).toList, kEq) &&
         forEach(lft.values.zip(rgt.values).toList, vEq)
        false
      }
    }

  implicit class EqComplexNumber(lft: ComplexNumber) {
    def ====(rgt: ComplexNumber, scale: Int = 2)(implicit eq: Eq[BigDecimal]): Boolean = {
      val dblToBD = (double: Double) => BigDecimal(double)
      eq.equiv(dblToBD(lft.real).setScale(scale, BigDecimal.RoundingMode.HALF_EVEN),
        dblToBD(rgt.real).setScale(scale, BigDecimal.RoundingMode.HALF_EVEN)) &&
        eq.equiv(dblToBD(lft.image).setScale(scale, BigDecimal.RoundingMode.HALF_EVEN),
          dblToBD(rgt.image).setScale(scale, BigDecimal.RoundingMode.HALF_EVEN))
    }
  }
}
