import spire.math.Complex
import spire.implicits.*

object Fourier:
  /** DFT coefficients c_k of a closed curve, keyed by frequency k in -(n/2-1)..n/2. */
  def coefficients(curve: Double => Complex[Double], samples: Int): Map[Int, Complex[Double]] =
    val points = Vector.tabulate(samples)(n => curve(2 * math.Pi * n / samples))
    (-(samples / 2 - 1) to samples / 2).map { k =>
      val sum = (0 until samples).foldLeft(Complex.zero[Double]) { (acc, n) =>
        acc + points(n) * Complex.polar(1.0, -2 * math.Pi * k * n / samples)
      }
      k -> sum / samples.toDouble
    }.toMap
