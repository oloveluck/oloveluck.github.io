import spire.math.Complex
import spire.implicits.*

/** Domain coloring of a Möbius transformation f(z) = (az + b)/(cz + d) whose
  * coefficients drift on small circles. Hue follows arg f(z) smoothly, soft
  * bands follow log2 |f(z)|, and the image of the integer grid is drawn so
  * the map's conformality is visible: the grid bends but corners stay square.
  */
object Mobius extends Animation:
  val slug: Slug = Slug("mobius")
  val title: String = "möbius flow"

  private val computeWidth = 320
  private val computeHeight = 240
  private val ln2 = math.log(2.0)

  override def pixelScale: Option[(Int, Int)] = Some((computeWidth, computeHeight))

  final case class MobiusState(field: IArray[Int])
  type S = MobiusState

  private val re0s: IArray[Double] =
    IArray.tabulate(computeWidth)(px => -2.0 + 4.0 * px / (computeWidth - 1))
  private val im0s: IArray[Double] =
    IArray.tabulate(computeHeight)(py => 1.5 - 3.0 * py / (computeHeight - 1))

  // muted cyclic hue via cosine mixing, tabulated over the angle
  private val hueTabSize = 1024
  private val hueTabR: IArray[Double] =
    IArray.tabulate(hueTabSize)(i => 108 + 42 * math.cos(2 * math.Pi * i / hueTabSize))
  private val hueTabG: IArray[Double] =
    IArray.tabulate(hueTabSize)(i => 116 + 26 * math.cos(2 * math.Pi * i / hueTabSize + 2.1))
  private val hueTabB: IArray[Double] =
    IArray.tabulate(hueTabSize)(i => 118 + 34 * math.cos(2 * math.Pi * i / hueTabSize + 4.2))

  def init(width: Int, height: Int): S =
    MobiusState(IArray.fill(width * height)((255 << 24) | (15 << 16) | (15 << 8) | 15))

  def step(t: Double, dt: Double, input: Input, s: S): S =
    val a = Complex.one[Double] + Complex.polar(0.35, 0.11 * t)
    val b = Complex.polar(0.6, 0.07 * t + 1.0)
    val c = Complex.polar(0.45, 0.05 * t + 2.0)
    val d = Complex.one[Double] + Complex.polar(0.35, 0.13 * t + 4.0)
    val (aRe, aIm) = (a.real, a.imag)
    val (bRe, bIm) = (b.real, b.imag)
    val (cRe, cIm) = (c.real, c.imag)
    val (dRe, dIm) = (d.real, d.imag)

    val field = IArray.tabulate(computeWidth * computeHeight) { i =>
      val zRe = re0s(i % computeWidth)
      val zIm = im0s(i / computeWidth)
      val nRe = aRe * zRe - aIm * zIm + bRe
      val nIm = aRe * zIm + aIm * zRe + bIm
      val mRe = cRe * zRe - cIm * zIm + dRe
      val mIm = cRe * zIm + cIm * zRe + dIm
      val m2 = mRe * mRe + mIm * mIm
      if m2 < 1e-15 then (255 << 24) | (15 << 16) | (15 << 8) | 15
      else
        val wRe = (nRe * mRe + nIm * mIm) / m2
        val wIm = (nIm * mRe - nRe * mIm) / m2
        val w2 = wRe * wRe + wIm * wIm
        val wAbs = math.sqrt(w2)

        // smooth hue from arg f(z)
        val angle = math.atan2(wIm, wRe)
        val hi = (((angle / (2 * math.Pi) + 1.0) * hueTabSize).toInt) % hueTabSize
        // soft luminance bands from log2 |f(z)|
        val v = if w2 < 1e-18 then 0.0 else math.log(w2) / (2 * ln2)
        val frac = v - math.floor(v)
        val tri = 1.0 - math.abs(2 * frac - 1)
        val lum = 0.30 + 0.42 * tri
        // conformal grid: darken near integer Re/Im lines of f(z); the lines
        // bend under the map but always cross at right angles. Fade the grid
        // out near the pole where it becomes too dense to resolve.
        val gr = math.abs(wRe - math.round(wRe))
        val gi = math.abs(wIm - math.round(wIm))
        val g = math.min(gr, gi)
        val gridStrength = math.max(0.0, 1.0 - wAbs / 8.0)
        val line = math.max(0.0, 1.0 - g / 0.05)
        val dark = 1.0 - 0.45 * line * gridStrength

        val f = lum * dark
        val r = (15 + (hueTabR(hi) - 15) * f).toInt
        val gg = (15 + (hueTabG(hi) - 15) * f).toInt
        val bb = (15 + (hueTabB(hi) - 15) * f).toInt
        (255 << 24) | (bb << 16) | (gg << 8) | r
    }
    MobiusState(field)

  def view(width: Int, height: Int, s: S): Scene =
    Scene.of(Shape.RawPixels(s.field, computeWidth, computeHeight))
