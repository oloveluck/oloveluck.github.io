import spire.math.Complex
import spire.implicits.*
import cats.Monoid
import cats.syntax.foldable.*

/** Fourier epicycles: the DFT of a closed curve rendered as a chain of
  * rotating vectors whose tip traces the curve back out. Pure throughout:
  * `step` evolves the state, `view` describes the frame as a Scene.
  */
final class EpicycleAnimation(
  val slug: Slug,
  val title: String,
  curve: Double => Complex[Double],
  keptTerms: Int
) extends Animation:
  import EpicycleAnimation.{samples, period, omega}

  private val terms: List[(Int, Complex[Double])] =
    Fourier
      .coefficients(curve, samples)
      .toList
      .sortBy((_, c) => -c.abs)
      .take(keptTerms)

  final case class EpState(
    scaled: List[(Int, Complex[Double])],
    center: Complex[Double],
    joints: Vector[Complex[Double]],
    trail: Vector[(Double, Complex[Double])]
  )
  type S = EpState

  def init(width: Int, height: Int): S =
    val scale = math.min(width, height) / 40.0
    EpState(
      scaled = terms.map((k, c) => k -> c * scale),
      center = Complex(width / 2.0, height / 2.0),
      joints = Vector.empty,
      trail = Vector.empty
    )

  def step(t: Double, dt: Double, input: Input, s: S): S =
    val joints = s.scaled
      .scanLeft(s.center) { case (acc, (k, c)) =>
        acc + c * Complex.polar(1.0, k * omega * t)
      }
      .toVector
    val stalled = s.trail.lastOption.exists((t0, _) => t - t0 > 0.25)
    val base = if stalled then Vector.empty else s.trail
    val trail = (base :+ (t, joints.last)).dropWhile((t0, _) => t0 < t - period)
    s.copy(joints = joints, trail = trail)

  def view(width: Int, height: Int, s: S): Scene =
    val background = Scene.of(Shape.Fill("#0f0f0f"))

    val circles = s.scaled.zip(s.joints).foldMap { case ((_, coeff), centerPt) =>
      Scene.of(Shape.Circle(centerPt, coeff.abs, "#2e2e2e", 1))
    }

    val arms = Scene.of(Shape.Polyline(s.joints, "#4a4a4a", 1))

    val trail =
      val points = s.trail.map(_._2)
      val bands = 3
      val bandSize = math.max(1, points.length / bands)
      val grouped = points.grouped(bandSize).toVector
      grouped.zipWithIndex.toList.foldMap { (band, i) =>
        // bands share a boundary point so no segment goes undrawn
        val joined = if i == 0 then band else grouped(i - 1).last +: band
        Scene.of(Shape.Polyline(joined, "#9fd0e8", 2, alpha = 0.25 + 0.75 * (i + 1.0) / bands))
      }

    Monoid[Scene].combineAll(List(background, circles, arms, trail))

object EpicycleAnimation:
  private val samples = 256
  private val period = 16.0
  private val omega = 2 * math.Pi / period

  // canvas y grows downward, so both curves negate the mathematical y
  // the heart is a finite trig polynomial (16sin³θ = 12sinθ − 4sin3θ), so its
  // Fourier series is exactly 8 terms and keptTerms = 8 reproduces it perfectly
  def heartCurve(theta: Double): Complex[Double] =
    val x = 16 * math.pow(math.sin(theta), 3)
    val y = 13 * math.cos(theta) - 5 * math.cos(2 * theta) -
      2 * math.cos(3 * theta) - math.cos(4 * theta)
    Complex(x, -y)

  // a true 4-leaf clover built from four hearts: each leaflet is the heart
  // curve scaled down with its tip at the shared center, rotated 90° apart.
  // The C4 symmetry keeps the spectrum sparse (only freqs ≡ 1 mod 4 survive)
  private val leafScale = 0.6

  def cloverCurve(theta: Double): Complex[Double] =
    val u = theta / (2 * math.Pi)
    val leaf = math.min(3, math.floor(u * 4).toInt)
    val local = (u * 4 - leaf) * 2 * math.Pi
    val tip = heartCurve(math.Pi)
    val base = (heartCurve(local + math.Pi) - tip) * leafScale
    base * Complex.polar(1.0, math.Pi / 4 + leaf * math.Pi / 2)

  val heart: EpicycleAnimation =
    EpicycleAnimation(Slug("epicycles"), "fourier epicycles", heartCurve, keptTerms = 8)

  val clover: EpicycleAnimation =
    EpicycleAnimation(Slug("clover"), "fourier clover", cloverCurve, keptTerms = 48)
