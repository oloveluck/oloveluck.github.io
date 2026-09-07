import scala.annotation.tailrec
import spire.math.Complex
import spire.implicits.*

/** An endless dive into the boundary of the Mandelbrot set. Each cycle picks the
  * next landmark from `targets`, falls toward it, and rises back out; the camera
  * always leaves and re-enters at the same wide framing, so the switch between
  * landmarks lands on an identical frame and is invisible.
  *
  * Depth and framing are functions of `t` alone, so `step` keeps no camera
  * state — the state is just the pixels it last built.
  */
object Mandelbrot extends Animation:
  val slug: Slug = Slug("mandelbrot")
  val title: String = "mandelbrot set"

  private val computeWidth = 224
  private val computeHeight = 168
  // One budget for the whole dive. A budget that grew with depth would move the
  // escape/interior cutoff a step at a time, and each step flips a filament of
  // pixels between the black interior and mid-palette — a sparkle crawling
  // along the boundary. How deep the dive can go and how big this can be are
  // the same question: resolving the boundary costs iterations in proportion to
  // the zoom, and a frame here has 16ms to spend. These three go together.
  private val maxIter = 110
  private val wideHalfWidth = 1.8
  private val deepHalfWidth = 1.8e-3
  /** Nepers of zoom between the two ends of a dive. */
  private val logRange = math.log(wideHalfWidth / deepHalfWidth)
  private val cycleSeconds = 32.0
  /** How many times escape counts walk the palette across the whole budget. */
  private val paletteCrossings = 2.0
  private val bandScale = paletteCrossings * 255 / math.sqrt(maxIter)
  private val interior = 255
  private val log2 = math.log(2.0)

  /** The framing at the top of every dive — the whole set, with margin. */
  private val wideCenter = Complex(-0.6, 0.0)

  /** Landmarks on the boundary, each checked to stay interesting the whole way
    * down at this zoom depth and iteration budget.
    */
  private val targets: IArray[Complex[Double]] = IArray(
    Complex(-0.7269, 0.1889),                        // spiral arm off the seahorse valley
    Complex(0.2549870375144766, 0.0005679790528465), // elephant valley
    Complex(-1.7864402, 0.0)                         // filaments on the western antenna
  )

  override def pixelScale: Option[(Int, Int)] = Some((computeWidth, computeHeight))

  type S = IArray[Int]

  /** Where the camera is looking, and how much of the plane fits across it. */
  private final case class Frame(center: Complex[Double], halfWidth: Double)

  private def smoothstep(x: Double): Double = x * x * (3 - 2 * x)

  /** Triangle wave over the cycle, eased at both ends: 0 at the wide framing,
    * 1 at the bottom of the dive, and stationary at each turn.
    */
  private def depthAt(t: Double): Double =
    val phase = (t / cycleSeconds) % 1.0
    smoothstep(if phase < 0.5 then 2 * phase else 2 * (1 - phase))

  /** The offset from the target decays at twice the zoom rate, so the landmark
    * slides to the middle of the frame early in the dive and stays there — and
    * at depth 0 the offset is the whole way back to `wideCenter`, which is what
    * makes every cycle start and end on the same frame whatever it dives into.
    */
  private def frameAt(t: Double): Frame =
    val depth = depthAt(t)
    val target = targets((t / cycleSeconds).toInt % targets.length)
    val pan = math.exp(-2 * logRange * depth)
    Frame(target + (wideCenter - target) * pan, wideHalfWidth * math.exp(-logRange * depth))

  /** The main cardioid and the period-2 bulb are the two largest interior
    * regions; naming them outright skips a full iteration budget per pixel
    * across the whole wide framing.
    */
  private def inMainBody(cRe: Double, cIm: Double): Boolean =
    val q = (cRe - 0.25) * (cRe - 0.25) + cIm * cIm
    q * (q + (cRe - 0.25)) <= 0.25 * cIm * cIm ||
      (cRe + 1) * (cRe + 1) + cIm * cIm <= 0.0625

  /** Palette index for one point of the parameter plane. The orbit runs on raw
    * doubles rather than `Complex` — it is the one piece of arithmetic that
    * runs millions of times per frame.
    */
  private def sample(cRe: Double, cIm: Double): Int =
    if inMainBody(cRe, cIm) then interior else orbit(cRe, cIm, 0.0, 0.0, 0, 0.0)

  @tailrec
  private def orbit(cRe: Double, cIm: Double, zRe: Double, zIm: Double, n: Int, m2: Double): Int =
    if m2 > 4.0 then colorIndex(n, m2)
    else if n >= maxIter then interior
    else
      val re = zRe * zRe - zIm * zIm + cRe
      val im = 2 * zRe * zIm + cIm
      orbit(cRe, cIm, re, im, n + 1, re * re + im * im)

  /** The fractional escape count, walked through the palette on a sqrt scale so
    * the contours stay evenly spread as the counts grow. Two crossings draws a
    * second contour through the mid field without the bands aliasing against
    * the filigree; the palette ends where it starts, so the wrap leaves no
    * seam. Nothing here depends on depth, so the colours hold still while the
    * camera moves.
    */
  private def colorIndex(n: Int, m2: Double): Int =
    val smooth = n + 1 - math.log(0.5 * math.log(m2)) / log2
    (math.sqrt(math.max(0.0, smooth)) * bandScale).toInt % 255

  def init(width: Int, height: Int): S = IArray.fill(width * height)(interior)

  def step(t: Double, dt: Double, input: Input, s: S): S =
    val Frame(center, halfWidth) = frameAt(t)
    val halfHeight = halfWidth * computeHeight / computeWidth
    val re0 = center.real - halfWidth
    val im0 = center.imag + halfHeight
    val reStep = 2 * halfWidth / (computeWidth - 1)
    val imStep = 2 * halfHeight / (computeHeight - 1)
    IArray.tabulate(computeWidth * computeHeight) { i =>
      sample(re0 + reStep * (i % computeWidth), im0 - imStep * (i / computeWidth))
    }

  def view(width: Int, height: Int, s: S): Scene =
    Scene.of(Shape.PixelField(s, palette, computeWidth, computeHeight))

  // a hump rather than a ramp: the palette leaves the canvas background at the
  // far field, swells through slate and steel into a warm halo that hugs the
  // set, then falls back to the background it shares with the interior — so the
  // boundary shades into the black body instead of speckling against it.
  // Packed ABGR.
  private val palette: IArray[Int] =
    val bg = (15, 15, 15)
    val stops = List(
      0.0 -> bg,
      0.14 -> (28, 36, 44),
      0.34 -> (58, 78, 90),
      0.52 -> (108, 128, 138),
      0.68 -> (168, 146, 110),
      0.78 -> (198, 178, 148),
      0.9 -> (86, 76, 64),
      1.0 -> bg
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
    arr(interior) = (255 << 24) | (bg._3 << 16) | (bg._2 << 8) | bg._1
    IArray.unsafeFromArray(arr)
