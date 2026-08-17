import scala.annotation.tailrec
import spire.math.Complex
import spire.implicits.*

/** Julia set for z^2 + c, with c orbiting a circle through the
  * connected/disconnected regimes. Everything here is pure: `step` builds a
  * fresh immutable pixel field each frame and `view` describes it as a Scene.
  */
object Julia extends Animation:
  val slug: Slug = Slug("julia")
  val title: String = "julia set"

  private val computeWidth = 320
  private val computeHeight = 240
  private val maxIter = 150
  private val cRadius = 0.7885
  private val angularSpeed = 0.2

  override def pixelScale: Option[(Int, Int)] = Some((computeWidth, computeHeight))

  final case class JuliaState(angle: Double, field: IArray[Int])
  type S = JuliaState

  private val re0s: IArray[Double] =
    IArray.tabulate(computeWidth)(px => -1.6 + 3.2 * px / (computeWidth - 1))
  private val im0s: IArray[Double] =
    IArray.tabulate(computeHeight)(py => 1.2 - 2.4 * py / (computeHeight - 1))

  @tailrec
  private def escape(zRe: Double, zIm: Double, cRe: Double, cIm: Double, n: Int, m2: Double): Int =
    if n >= maxIter then 255
    else if m2 > 4.0 then colorIndex(n, m2)
    else
      val re = zRe * zRe - zIm * zIm + cRe
      val im = 2 * zRe * zIm + cIm
      escape(re, im, cRe, cIm, n + 1, re * re + im * im)

  private def colorIndex(n: Int, m2: Double): Int =
    val smooth = n + 1 - math.log(0.5 * math.log(m2)) / math.log(2.0)
    // sqrt spreads the low escape counts (where most pixels land) across the palette
    val norm = math.sqrt(math.max(0.0, smooth / maxIter))
    math.min(254, (norm * 254).toInt)

  def init(width: Int, height: Int): S =
    JuliaState(angle = 0.0, field = IArray.fill(width * height)(255))

  def step(t: Double, dt: Double, input: Input, s: S): S =
    val c = Complex.polar(cRadius, s.angle)
    val (cRe, cIm) = (c.real, c.imag)
    val field = IArray.tabulate(computeWidth * computeHeight) { i =>
      val re0 = re0s(i % computeWidth)
      val im0 = im0s(i / computeWidth)
      escape(re0, im0, cRe, cIm, 0, re0 * re0 + im0 * im0)
    }
    JuliaState(s.angle + angularSpeed * dt, field)

  def view(width: Int, height: Int, s: S): Scene =
    Scene.of(Shape.PixelField(s.field, palette, computeWidth, computeHeight))

  // starts at the canvas background so the fractal exterior melts into the
  // page, rising through muted slate to a soft warm glow — packed ABGR
  private val palette: IArray[Int] =
    val stops = List(
      0.0 -> (15, 15, 15),
      0.2 -> (16, 17, 18),
      0.45 -> (44, 60, 70),
      0.7 -> (92, 116, 128),
      1.0 -> (185, 165, 135)
    )
    def channel(t: Double, pick: ((Int, Int, Int)) => Int): Int =
      val ((t0, c0), (t1, c1)) =
        stops.zip(stops.tail).find((_, hi) => t <= hi._1).getOrElse((stops.init.last, stops.last))
      val f = if t1 == t0 then 0.0 else (t - t0) / (t1 - t0)
      (pick(c0) + f * (pick(c1) - pick(c0))).toInt
    val arr = Array.tabulate(256) { i =>
      val t = i / 255.0
      val (r, g, b) = (channel(t, _._1), channel(t, _._2), channel(t, _._3))
      (255 << 24) | (b << 16) | (g << 8) | r
    }
    arr(255) = (255 << 24) | (15 << 16) | (15 << 8) | 15
    IArray.unsafeFromArray(arr)
