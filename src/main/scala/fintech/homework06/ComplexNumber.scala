package fintech.homework06

class ComplexNumber(val real: Double, val image: Double) {
  def *(other: ComplexNumber): ComplexNumber =
    new ComplexNumber(real * other.real - image * other.image, image * other.real + real * other.image)

  def +(other: ComplexNumber): ComplexNumber = new ComplexNumber(real + other.real, image + other.image)

  override def toString: String = {
    if (real == 0)
      image.toString + "i"
    else {
      if (image > 0)
        "%s + %si".format(real.toString, image.toString)
      else if (image < 0)
        "%s - %si".format(real.toString, math.abs(image).toString)
      else
        real.toString
    }
  }

  def ~(pow: Int): ComplexNumber = {
    val argument = math.atan(image / real)
    val moduleInPower = math.pow(math.sqrt(real * real + image * image), pow)
    new ComplexNumber(math.cos(pow * argument) * moduleInPower,
      math.sin(pow * argument) * moduleInPower)
  }

  override def hashCode(): Int = 41 * (41 * real.hashCode()) + image.hashCode()

  override def equals(a: Any): Boolean = {
    a match {
      case complexNumber: ComplexNumber => complexNumber.real == real &&
        complexNumber.image == image
      case _ => false
    }
  }
}