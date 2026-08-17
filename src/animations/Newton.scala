import scala.annotation.tailrec
import spire.math.Complex
import spire.implicits.*

/** Newton fractal whose polynomial degree animates: p(z) = (1−u)·z^d + u·z^(d+1) − c
  * morphs continuously between degrees (the new root slides in from infinity),
  * ping-ponging d between 2 and 6 while c orbits the unit circle. Pixels are
  * colored by the angle of the root they converge to, shaded by convergence
  * speed, so the coloring works at every degree without a root list.
  */
object Newton extends Animation:
  val slug: Slug = Slug("newton")
  val title: String = "newton fractal"

  private val computeWidth = 320
  private val computeHeight = 240
  private val maxIter = 40
  private val minDegree = 2
  private val maxDegree = 6
  private val stageSeconds = 8.0
  private val angularSpeed = 0.15
  private val hueBuckets = 8
  private val shades = 32

  override def pixelScale: Option[(Int, Int)] = Some((computeWidth, computeHeight))

  final case class NewtonState(angle: Double, field: IArray[Int])
  type S = NewtonState

  private val re0s: IArray[Double] =
    IArray.tabulate(computeWidth)(px => -1.6 + 3.2 * px / (computeWidth - 1))
  private val im0s: IArray[Double] =
    IArray.tabulate(computeHeight)(py => 1.2 - 2.4 * py / (computeHeight - 1))

  private def smoothstep(x: Double): Double = x * x * (3 - 2 * x)

  /** Ping-pong through degree blends: (2→3), (3→4), … (6→5), … (3→2). */
  private def degreeBlend(t: Double): (Int, Double) =
    val stages = 2 * (maxDegree - minDegree)
    val pos = (t / stageSeconds) % stages
    val stage = pos.toInt
    val u = smoothstep(pos - stage)
    if stage < maxDegree - minDegree then (minDegree + stage, u)
    else (maxDegree - 1 - (stage - (maxDegree - minDegree)), 1 - u)

  /** Newton iteration for (1−u)·z^d + u·z^(d+1) − c on raw doubles; returns
    * the palette index (hue bucket from the converged root's angle × shade
    * from iteration count).
    */
  @tailrec
  private def iterate(u: Double, d: Int, cRe: Double, cIm: Double, zRe: Double, zIm: Double, n: Int): Int =
    // powers accumulate in locals (nothing escapes; the function stays pure)
    var pdRe = 1.0
    var pdIm = 0.0
    var k = 0
    while k < d - 1 do
      val r = pdRe * zRe - pdIm * zIm
      pdIm = pdRe * zIm + pdIm * zRe
      pdRe = r
      k += 1
    val zdRe = pdRe * zRe - pdIm * zIm
    val zdIm = pdRe * zIm + pdIm * zRe
    val zd1Re = zdRe * zRe - zdIm * zIm
    val zd1Im = zdRe * zIm + zdIm * zRe

    val pRe = (1 - u) * zdRe + u * zd1Re - cRe
    val pIm = (1 - u) * zdIm + u * zd1Im - cIm
    if pRe * pRe + pIm * pIm < 1e-12 then
      val angle = math.atan2(zIm, zRe)
      val bucket = (((angle / (2 * math.Pi) + 1.0) * hueBuckets).toInt) % hueBuckets
      math.min(254, bucket * shades + math.min(shades - 1, n * 3))
    else if n >= maxIter then 255
    else
      val dvRe = (1 - u) * d * pdRe + u * (d + 1) * zdRe
      val dvIm = (1 - u) * d * pdIm + u * (d + 1) * zdIm
      val d2 = dvRe * dvRe + dvIm * dvIm
      if d2 < 1e-18 then 255
      else
        val qRe = (pRe * dvRe + pIm * dvIm) / d2
        val qIm = (pIm * dvRe - pRe * dvIm) / d2
        iterate(u, d, cRe, cIm, zRe - qRe, zIm - qIm, n + 1)

  def init(width: Int, height: Int): S =
    NewtonState(angle = 0.0, field = IArray.fill(width * height)(255))

  def step(t: Double, dt: Double, input: Input, s: S): S =
    val c = Complex.polar(1.0, s.angle)
    val (cRe, cIm) = (c.real, c.imag)
    val (d, u) = degreeBlend(t)
    val field = IArray.tabulate(computeWidth * computeHeight) { i =>
      iterate(u, d, cRe, cIm, re0s(i % computeWidth), im0s(i / computeWidth), 0)
    }
    NewtonState(s.angle + angularSpeed * dt, field)

  def view(width: Int, height: Int, s: S): Scene =
    Scene.of(Shape.PixelField(s.field, palette, computeWidth, computeHeight))

  // eight muted hues around the circle, each shaded from the page background
  // up to full tone (boundaries glow, basin interiors stay dark) — packed ABGR
  private val palette: IArray[Int] =
    val bg = (15, 15, 15)
    val hues = Array(
      (92, 116, 128),  // slate
      (86, 106, 142),  // steel
      (110, 100, 140), // violet
      (135, 100, 125), // mauve
      (150, 110, 95),  // rust
      (150, 126, 92),  // amber
      (122, 130, 95),  // olive
      (100, 128, 105)  // moss
    )
    val arr = new Array[Int](256)
    var idx = 0
    while idx < 255 do
      val (hr, hg, hb) = hues(idx / shades)
      val f = (idx % shades) / (shades - 1).toDouble
      val r = (bg._1 + f * (hr - bg._1)).toInt
      val g = (bg._2 + f * (hg - bg._2)).toInt
      val b = (bg._3 + f * (hb - bg._3)).toInt
      arr(idx) = (255 << 24) | (b << 16) | (g << 8) | r
      idx += 1
    arr(255) = (255 << 24) | (bg._3 << 16) | (bg._2 << 8) | bg._1
    IArray.unsafeFromArray(arr)
