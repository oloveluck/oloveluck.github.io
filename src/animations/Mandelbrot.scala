import scala.annotation.tailrec

/** An endless dive into the boundary of the Mandelbrot set. Each cycle picks
  * the next landmark from `targets`, falls toward it, and rises back out; the
  * camera always leaves and re-enters at the same wide framing, so the switch
  * between landmarks happens while the whole set is on screen and is invisible.
  *
  * Everything is a function of `t`: depth, framing and iteration budget are all
  * derived from the clock, so `step` holds no camera state — only the pixels it
  * just built.
  */
object Mandelbrot extends Animation:
  val slug: Slug = Slug("mandelbrot")
  val title: String = "mandelbrot set"

  private val computeWidth = 320
  private val computeHeight = 240
  // deep zoom is expensive, so the iteration budget is spent where it shows:
  // wide frames escape fast, deep ones need the headroom to resolve filigree
  private val baseIter = 80
  private val extraIter = 180
  private val wideHalfWidth = 1.8
  private val deepHalfWidth = 1.8e-5
  private val logRange = math.log(wideHalfWidth / deepHalfWidth)
  private val cycleSeconds = 32.0
  private val bandScale = 26.0
  private val interior = 255

  /** The framing at the top of every dive — the whole set, with margin. */
  private val wideCenter = (-0.6, 0.0)

  /** Landmarks on the boundary, each verified to stay interesting all the way
    * down at this zoom depth and iteration budget.
    */
  private val targets: IArray[(Double, Double)] = IArray(
    (-0.7269, 0.1889),                      // spiral arm off the seahorse valley
    (0.2549870375144766, 0.0005679790528465), // elephant valley
    (-1.7864402, 0.0),                      // filaments on the western antenna
    (-1.768778833, 0.001738996)             // a small copy of the whole set
  )

  override def pixelScale: Option[(Int, Int)] = Some((computeWidth, computeHeight))

  type S = IArray[Int]

  private def smoothstep(x: Double): Double = x * x * (3 - 2 * x)

  /** Triangle wave over the cycle, eased at both ends: 0 at the wide framing,
    * 1 at the bottom of the dive, and stationary at each turn.
    */
  private def depthAt(t: Double): Double =
    val phase = (t / cycleSeconds) % 1.0
    smoothstep(if phase < 0.5 then 2 * phase else 2 * (1 - phase))

  private def targetAt(t: Double): (Double, Double) =
    targets((t / cycleSeconds).toInt % targets.length)

  /** The offset from the target decays at twice the zoom rate, so the landmark
    * slides to the middle of the frame within the first fraction of the dive
    * and stays there — and at depth 0 the framing is the same for every target.
    */
  private def camera(t: Double): (Double, Double, Double) =
    val depth = depthAt(t)
    val (tx, ty) = targetAt(t)
    val pan = math.exp(-2 * logRange * depth)
    val cx = tx + (wideCenter._1 - tx) * pan
    val cy = ty + (wideCenter._2 - ty) * pan
    (cx, cy, wideHalfWidth * math.exp(-logRange * depth))

  /** The main cardioid and the period-2 bulb are the two largest interior
    * regions; naming them outright skips a full iteration budget per pixel
    * across the whole wide framing.
    */
  private def inMainBody(cRe: Double, cIm: Double): Boolean =
    val q = (cRe - 0.25) * (cRe - 0.25) + cIm * cIm
    q * (q + (cRe - 0.25)) <= 0.25 * cIm * cIm ||
      (cRe + 1) * (cRe + 1) + cIm * cIm <= 0.0625

  @tailrec
  private def escape(
    cRe: Double, cIm: Double, zRe: Double, zIm: Double, n: Int, m2: Double, maxIter: Int
  ): Int =
    if m2 > 4.0 then colorIndex(n, m2)
    else if n >= maxIter then interior
    else
      val re = zRe * zRe - zIm * zIm + cRe
      val im = 2 * zRe * zIm + cIm
      escape(cRe, cIm, re, im, n + 1, re * re + im * im, maxIter)

  /** The fractional escape count, walked through the palette on a sqrt scale so
    * the contours stay evenly spread as the counts grow. `bandScale` is tuned to
    * just about fill the palette at the wide framing and to wrap once by the
    * bottom of the dive — enough for a second contour through the mid field,
    * not so much that the bands alias against the filigree. The palette ends
    * where it starts, so the wrap leaves no seam.
    */
  private def colorIndex(n: Int, m2: Double): Int =
    val smooth = n + 1 - math.log(0.5 * math.log(m2)) / math.log(2.0)
    (math.sqrt(math.max(0.0, smooth)) * bandScale).toInt % 255

  def init(width: Int, height: Int): S = IArray.fill(width * height)(interior)

  def step(t: Double, dt: Double, input: Input, s: S): S =
    val (cx, cy, halfWidth) = camera(t)
    val halfHeight = halfWidth * computeHeight / computeWidth
    val maxIter = (baseIter + extraIter * depthAt(t)).toInt
    val reStep = 2 * halfWidth / (computeWidth - 1)
    val imStep = 2 * halfHeight / (computeHeight - 1)
    IArray.tabulate(computeWidth * computeHeight) { i =>
      val cRe = cx - halfWidth + reStep * (i % computeWidth)
      val cIm = cy + halfHeight - imStep * (i / computeWidth)
      if inMainBody(cRe, cIm) then interior
      else escape(cRe, cIm, 0.0, 0.0, 0, 0.0, maxIter)
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
