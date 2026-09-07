import scala.annotation.tailrec
import spire.math.Complex
import spire.implicits.*

/** A tour of the Mandelbrot set in precise stills. The camera holds on one
  * framing at a time while the image resolves — coarse blocks first, then
  * every pixel, then four samples to a pixel — and once it has held for a
  * while, dissolves into the next. Nothing on the boundary ever moves between
  * frames, which is what lets the boundary be exact.
  *
  * A still is far more work than a frame can hold, so `step` spends a fixed
  * budget of iterations per frame and carries a cursor through the work. The
  * cost of a frame is then the same whatever is on screen, and how deep a
  * still can go only decides how long it takes to come into focus.
  */
object Mandelbrot extends Animation:
  val slug: Slug = Slug("mandelbrot")
  val title: String = "mandelbrot set"

  private val width = 896
  private val height = 672
  private val maxIter = 600
  /** Work spent per frame, in iterations — a few milliseconds. A sample is
    * charged its iterations plus `sampleOverhead` plus a unit per pixel it
    * paints: in the wide framing most points cost no iterations at all (the
    * cardioid test or a first-step escape), and without the overhead a frame
    * would race through hundreds of thousands of them.
    */
  private val budgetPerFrame = 1_600_000
  private val sampleOverhead = 16
  private val dissolveSeconds = 1.5
  private val holdSeconds = 9.0
  /** How many times escape counts walk the palette across the whole budget. */
  private val paletteCrossings = 2.0
  private val bandScale = paletteCrossings * 255 / math.sqrt(maxIter)
  private val interior = 255
  private val log2 = math.log(2.0)

  override def pixelScale: Option[(Int, Int)] = Some((width, height))

  /** Where the camera is looking, and how much of the plane fits across it. */
  private final case class Frame(center: Complex[Double], halfWidth: Double)

  private val stills: IArray[Frame] = IArray(
    Frame(Complex(-0.6, 0.0), 1.8),                                 // the whole set
    Frame(Complex(-0.7269, 0.1889), 0.012),                         // spiral arm off the seahorse valley
    Frame(Complex(0.2549870375144766, 0.0005679790528465), 0.0025), // elephant valley
    Frame(Complex(-0.7453, 0.1127), 0.0009),                        // a double spiral
    Frame(Complex(-1.7864402, 0.0), 0.004),                         // filaments on the western antenna
    Frame(Complex(-0.743643887037151, 0.13182590420533), 0.00005)   // deep in the seahorse valley
  )

  /** One sweep over the image. At a stride above 1 every stride-th pixel is
    * sampled and painted as a block, which is how the still comes into focus;
    * at stride 1 every pixel takes sample `sample` of its four.
    */
  private final case class Pass(stride: Int, sample: Int):
    val positions: Int = (width / stride) * (height / stride)

  private val passes: IArray[Pass] =
    IArray(Pass(8, 0), Pass(4, 0), Pass(2, 0), Pass(1, 0), Pass(1, 1), Pass(1, 2), Pass(1, 3))
  private val coarsest = passes.head.stride
  private val totalWork = passes.foldLeft(0)(_ + _.positions)

  /** `painting` is the still being resolved and `cursor` how far through the
    * passes it has got; `finished` is the still before it, kept under the
    * dissolve; `blended` is scratch for the dissolve; `shown` is whichever of
    * them is on the canvas. Packed ABGR throughout.
    *
    * These are buffers, painted in place — the one animation whose state is
    * not immutable. A fresh copy of the image per frame is a multi-megabyte
    * allocation sixty times a second, which the browser puts straight into
    * its large-object space and pays back as a garbage-collection stall every
    * few seconds. The three are allocated once and recycled between stills;
    * nothing but `step` writes them, and `view` only reads `shown`.
    */
  final case class Tour(
    index: Int,
    began: Double,
    finished: Array[Int],
    painting: Array[Int],
    cursor: Int,
    blended: Array[Int],
    shown: Array[Int]
  )
  type S = Tour

  private def smoothstep(x: Double): Double = x * x * (3 - 2 * x)

  /** The main cardioid and the period-2 bulb are the two largest interior
    * regions; naming them outright skips a full iteration budget per pixel
    * across the whole wide framing.
    */
  private def inMainBody(cRe: Double, cIm: Double): Boolean =
    val q = (cRe - 0.25) * (cRe - 0.25) + cIm * cIm
    q * (q + (cRe - 0.25)) <= 0.25 * cIm * cIm ||
      (cRe + 1) * (cRe + 1) + cIm * cIm <= 0.0625

  /** Palette index for one point of the plane, packed with the iterations it
    * cost as `(iterations << 8) | index`, so the caller can keep its budget
    * without a second return value. The orbit runs on raw doubles rather than
    * `Complex` — it is the one piece of arithmetic that runs millions of times
    * per frame.
    */
  private def sample(cRe: Double, cIm: Double): Int =
    if inMainBody(cRe, cIm) then interior else orbit(cRe, cIm, 0.0, 0.0, 0, 0.0)

  @tailrec
  private def orbit(cRe: Double, cIm: Double, zRe: Double, zIm: Double, n: Int, m2: Double): Int =
    if m2 > 4.0 then (n << 8) | colorIndex(n, m2)
    else if n >= maxIter then (n << 8) | interior
    else
      val re = zRe * zRe - zIm * zIm + cRe
      val im = 2 * zRe * zIm + cIm
      orbit(cRe, cIm, re, im, n + 1, re * re + im * im)

  /** The fractional escape count, walked through the palette on a sqrt scale so
    * the contours stay evenly spread as the counts grow. Two crossings draws a
    * second contour through the mid field without the bands crowding the
    * filigree; the palette ends where it starts, so the wrap leaves no seam.
    */
  private def colorIndex(n: Int, m2: Double): Int =
    val smooth = n + 1 - math.log(0.5 * math.log(m2)) / log2
    (math.sqrt(math.max(0.0, smooth)) * bandScale).toInt % 255

  /** Folds sample `k` (from 0) of a pixel into its running average. */
  private def average(acc: Int, next: Int, k: Int): Int =
    val r = ((acc & 0xff) * k + (next & 0xff)) / (k + 1)
    val g = (((acc >> 8) & 0xff) * k + ((next >> 8) & 0xff)) / (k + 1)
    val b = (((acc >> 16) & 0xff) * k + ((next >> 16) & 0xff)) / (k + 1)
    (255 << 24) | (b << 16) | (g << 8) | r

  /** Writes `from` faded toward `to` by `f` into `out`. */
  private def blend(from: Array[Int], to: Array[Int], f: Double, out: Array[Int]): Unit =
    val k = (f * 256).toInt
    var i = 0
    while i < out.length do
      val p = from(i)
      val q = to(i)
      val r = ((p & 0xff) * (256 - k) + (q & 0xff) * k) >> 8
      val g = (((p >> 8) & 0xff) * (256 - k) + ((q >> 8) & 0xff) * k) >> 8
      val b = (((p >> 16) & 0xff) * (256 - k) + ((q >> 16) & 0xff) * k) >> 8
      out(i) = (255 << 24) | (b << 16) | (g << 8) | r
      i += 1

  /** Spends one frame's budget painting `frame` into `out`, picking up the
    * passes at `from`; returns the cursor after the work done.
    */
  private def advance(frame: Frame, out: Array[Int], from: Int): Int =
    val halfHeight = frame.halfWidth * height / width
    val re0 = frame.center.real - frame.halfWidth
    val im0 = frame.center.imag + halfHeight
    val pixel = 2 * frame.halfWidth / width
    var cursor = from
    var budget = budgetPerFrame
    var passStart = 0
    var p = 0
    while p < passes.length && cursor >= passStart + passes(p).positions do
      passStart += passes(p).positions
      p += 1
    while budget > 0 && p < passes.length do
      val pass = passes(p)
      val stride = pass.stride
      val cols = width / stride
      val end = passStart + pass.positions
      // coarse passes sample the middle of their block; the four samples of a
      // finished pixel sit on a 2x2 grid inside it
      val dx = if stride > 1 then stride * 0.5 else 0.25 + 0.5 * (pass.sample % 2)
      val dy = if stride > 1 then stride * 0.5 else 0.25 + 0.5 * (pass.sample / 2)
      while budget > 0 && cursor < end do
        val pos = cursor - passStart
        val x = pos % cols * stride
        val y = pos / cols * stride
        val paintedByCoarserPass =
          stride > 1 && stride < coarsest && x % (2 * stride) == 0 && y % (2 * stride) == 0
        if !paintedByCoarserPass then
          val packed = sample(re0 + pixel * (x + dx), im0 - pixel * (y + dy))
          budget -= (packed >>> 8) + sampleOverhead + stride * stride
          val color = palette(packed & 0xff)
          if stride == 1 then
            val i = y * width + x
            out(i) = if pass.sample == 0 then color else average(out(i), color, pass.sample)
          else
            var row = y
            while row < y + stride do
              java.util.Arrays.fill(out, row * width + x, row * width + x + stride, color)
              row += 1
        cursor += 1
      if cursor == end then
        passStart = end
        p += 1
    cursor

  private val background = (255 << 24) | (15 << 16) | (15 << 8) | 15
  private def blank(): Array[Int] = Array.fill(width * height)(background)

  def init(width: Int, height: Int): S =
    val first = blank()
    Tour(index = 0, began = 0.0, finished = blank(), painting = first, cursor = 0, blended = blank(), shown = first)

  def step(t: Double, dt: Double, input: Input, s: S): S =
    val elapsed = t - s.began
    if s.cursor == totalWork && elapsed >= dissolveSeconds + holdSeconds then
      // the finished still slides under the dissolve; its predecessor's buffer
      // is cleared and painted over as the next one
      java.util.Arrays.fill(s.finished, background)
      Tour((s.index + 1) % stills.length, t, finished = s.painting, painting = s.finished,
        cursor = 0, blended = s.blended, shown = s.painting)
    else
      val cursor = if s.cursor < totalWork then advance(stills(s.index), s.painting, s.cursor) else s.cursor
      val shown =
        if elapsed < dissolveSeconds then
          blend(s.finished, s.painting, smoothstep(elapsed / dissolveSeconds), s.blended)
          s.blended
        else s.painting
      s.copy(cursor = cursor, shown = shown)

  def view(width: Int, height: Int, s: S): Scene =
    Scene.of(Shape.RawPixels(IArray.unsafeFromArray(s.shown), this.width, this.height))

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
    arr(interior) = background
    IArray.unsafeFromArray(arr)
