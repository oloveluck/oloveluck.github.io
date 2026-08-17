import spire.math.{Complex, Quaternion}
import spire.implicits.*
import cats.data.Chain

/** Home-page background: the fourier clover — four hearts tracing as one
  * epicycle chain — on a freely spinnable 3D plane. Drag to tumble it (coin
  * spin with momentum); left alone it glides home to a gentle face-on sway.
  */
object Loveluck extends Animation:
  val slug: Slug = Slug("loveluck")
  val title: String = "loveluck"

  private val P = 10.0
  private val omega = 2 * math.Pi / P
  private val minKeep = 2.0
  private val swayPeriod = 40.0

  // coin-spin feel: drags rotate freely through edge-on and around; a flick
  // completes revolutions, and homing engages only once the spin has slowed,
  // so the shape is always either rotating or gliding back to face-on
  private val dragGain = 3.0
  private val rollGain = 1.2
  private val maxSpin = 12.0
  private val spinSmoothing = 10.0
  private val spinDecay = 1.2
  private val spinHome = 1.5

  // after momentum plays out the orientation glides home to a gentle
  // face-on sway, so a wild spin always recovers to a legible shape
  private val recoverDelay = 5.0
  private val recoverRamp = 3.0
  private val recoverRate = 1.0
  private val swayY = 0.22
  private val swayX = 0.14

  private val samples = 256

  // the strongest clover coefficients, in a fixed order (unscaled; init
  // applies the canvas scale once)
  private val baseCoeffs: List[(Int, Complex[Double])] =
    Fourier
      .coefficients(EpicycleAnimation.cloverCurve, samples)
      .toList
      .sortBy((_, c) => -c.abs)
      .take(32)

  private def smoothstep(x: Double): Double = x * x * (3 - 2 * x)

  /** The resting orientation: face-on with a slow sway. */
  private def home(t: Double): Quaternion[Double] =
    Rotation.axisAngle(Vec3(0, 1, 0), swayY * math.sin(2 * math.Pi / swayPeriod * t)) *
      Rotation.axisAngle(Vec3(1, 0, 0), swayX * math.cos(4 * math.Pi / swayPeriod * t))

  final case class LoveState(
    orientation: Quaternion[Double],
    spin: Vec3,
    sinceDrag: Double,
    coeffs: List[(Int, Complex[Double])],
    joints: Vector[Complex[Double]],
    trail: Vector[(Double, Complex[Double])],
    scale: Double
  )
  type S = LoveState

  def init(width: Int, height: Int): S =
    val scale = math.min(width, height) / 36.0
    LoveState(
      orientation = Quaternion(1.0, 0.0, 0.0, 0.0),
      spin = Vec3.zero,
      sinceDrag = recoverDelay + recoverRamp,
      coeffs = baseCoeffs.map((k, c) => k -> c * scale),
      joints = Vector.empty,
      trail = Vector.empty,
      scale = scale
    )

  /** Pen position at an arbitrary time (used to subdivide large time gaps). */
  private def penAt(tt: Double, coeffs: List[(Int, Complex[Double])]): Complex[Double] =
    coeffs.foldLeft(Complex.zero[Double]) { case (acc, (k, c)) =>
      acc + c * Complex.polar(1.0, k * omega * tt)
    }

  def step(t: Double, dt: Double, input: Input, s: S): S =
    val joints = s.coeffs
      .scanLeft(Complex.zero[Double]) { case (acc, (k, c)) =>
        acc + c * Complex.polar(1.0, k * omega * t)
      }
      .toVector
    val lastT = s.trail.lastOption.map(_._1).getOrElse(t)
    val gap = t - lastT
    // a long stall (backgrounded tab, throttling) can't be bridged smoothly —
    // restart the trail instead of drawing chords across the missing stretch
    val trail =
      if gap > 0.25 then Vector((t, joints.last))
      else
        val segments = math.min(8, math.max(1, math.ceil(gap / 0.02).toInt))
        val newPoints = (1 to segments).map { i =>
          val ti = lastT + gap * i / segments
          (ti, penAt(ti, s.coeffs))
        }
        (s.trail ++ newPoints).dropWhile((t0, _) => t0 < t - P)

    // coin spin: drag deltas rotate the shape directly (unclamped, so it
    // tumbles fully through edge-on); a flick carries capped momentum, and
    // homing pulls it face-on only once the spin has slowed
    val sinceDrag = if input.drag.isDefined then 0.0 else s.sinceDrag + dt
    val (orientation, spin) = input.drag match
      case Some((dx, dy)) if dx != 0.0 || dy != 0.0 =>
        // circular motion around the page center reads as torque about the
        // view axis, so dragging in a circle rolls the shape in-plane
        val (px, py) = input.pointer.getOrElse((0.0, 0.0))
        val roll = (px * dy - py * dx) * rollGain
        val turned = Rotation.normalize(
          Rotation.axisAngle(Vec3(0, 0, 1), roll) *
            Rotation.axisAngle(Vec3(0, 1, 0), dx * dragGain) *
            Rotation.axisAngle(Vec3(1, 0, 0), dy * dragGain) * s.orientation
        )
        val measured =
          if dt > 1e-4 then Vec3(dy * dragGain / dt, dx * dragGain / dt, roll / dt) else s.spin
        val blend = math.min(1.0, spinSmoothing * dt)
        val blended = s.spin + (measured - s.spin) * blend
        val speed = math.sqrt(blended.dot(blended))
        val capped = if speed > maxSpin then blended * (maxSpin / speed) else blended
        (turned, capped)
      case Some(_) =>
        // held still: the grip pins the shape and absorbs its momentum
        (s.orientation, Vec3.zero)
      case None =>
        val w = s.spin
        val speed = math.sqrt(w.dot(w))
        val turned =
          if speed * dt > 1e-9 then
            Rotation.normalize(Rotation.axisAngle(w, speed * dt) * s.orientation)
          else s.orientation
        val timeGate = smoothstep(math.min(1.0, math.max(0.0, (sinceDrag - recoverDelay) / recoverRamp)))
        val calmGate = 1.0 - math.min(1.0, speed / spinHome)
        val pull = 1 - math.exp(-recoverRate * timeGate * calmGate * dt)
        (Rotation.slerp(turned, home(t), pull), w * math.exp(-spinDecay * dt))

    LoveState(orientation, spin, sinceDrag, s.coeffs, joints, trail, s.scale)

  def view(width: Int, height: Int, s: S): Scene =
    val pose = Pose(Vec3.zero, s.orientation)
    val camera = Camera(4.0 * math.max(width, height))

    val circles: Scene3 = Chain.fromSeq(
      s.coeffs.zip(s.joints).map { case ((_, c), centerPt) =>
        Scene3D.circle(centerPt, c.abs, pose, "#191919", 1)
      }
    )

    val arms: Scene3 = Scene3D.of(Scene3D.planar(s.joints, pose, "#232323", 1))

    val trail: Scene3 =
      val points = s.trail.map(_._2)
      val bands = 3
      val bandSize = math.max(1, points.length / bands)
      val grouped = points.grouped(bandSize).toVector
      Chain.fromSeq(
        grouped.zipWithIndex.map { (band, i) =>
          // bands share a boundary point so no segment goes undrawn
          val joined = if i == 0 then band else grouped(i - 1).last +: band
          Scene3D.planar(joined, pose, "#5a7d8c", 2, alpha = 0.10 + 0.25 * (i + 1.0) / bands)
        }.toList
      )

    Scene.of(Shape.Fill("#0a0a0a")) ++
      Scene3D.project(camera, width, height)(circles ++ arms ++ trail)
