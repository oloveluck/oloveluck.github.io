/** Chaos-game IFS: the Barnsley fern. A deterministic LCG (threaded through
  * state — no hidden randomness) picks affine maps by weight; visited points
  * deposit density into a grid that decays each frame, so the fern both grows
  * and sways as the stem map's coefficients drift.
  */
object Fern extends Animation:
  val slug: Slug = Slug("fern")
  val title: String = "barnsley fern"

  private val computeWidth = 320
  private val computeHeight = 240
  private val decay = 0.99
  private val depositsPerFrame = 30000
  // the classic frond map f2 is 0.851·R(−2.7°); swaying its rotation angle
  // compounds up the frond so the tips swing widest, like wind
  private val f2Scale = 0.851
  private val f2BaseAngle = -0.047
  private val swayAmp = 0.018
  private val swayRate = 0.6

  override def pixelScale: Option[(Int, Int)] = Some((computeWidth, computeHeight))

  final case class FernState(seed: Long, x: Double, y: Double, density: IArray[Double])
  type S = FernState

  def init(width: Int, height: Int): S =
    FernState(seed = 0x5eed5eed5eedL, x = 0.0, y = 0.0, density = IArray.fill(width * height)(0.0))

  def step(t: Double, dt: Double, input: Input, s: S): S =
    val theta = f2BaseAngle + swayAmp * math.sin(swayRate * t)
    val fa = f2Scale * math.cos(theta)
    val fb = -f2Scale * math.sin(theta)
    val scale = (computeHeight - 12) / 10.5
    val cx = computeWidth / 2.0

    // decay then deposit; the arrays are local until frozen
    val next = Array.tabulate(s.density.length)(i => s.density(i) * decay)
    var seed = s.seed
    var x = s.x
    var y = s.y
    var i = 0
    while i < depositsPerFrame do
      seed = seed * 6364136223846793005L + 1442695040888963407L
      val r = ((seed >>> 33) % 100).toInt
      val nx =
        if r < 1 then 0.0
        else if r < 86 then fa * x + fb * y
        else if r < 93 then 0.2 * x - 0.26 * y
        else -0.15 * x + 0.28 * y
      val ny =
        if r < 1 then 0.16 * y
        else if r < 86 then -fb * x + fa * y + 1.6
        else if r < 93 then 0.23 * x + 0.22 * y + 1.6
        else 0.26 * x + 0.24 * y + 0.44
      x = nx
      y = ny
      val px = (cx + x * scale).toInt
      val py = (computeHeight - 6 - y * scale).toInt
      if px >= 0 && px < computeWidth && py >= 0 && py < computeHeight then
        next(py * computeWidth + px) += 1.0
      i += 1

    FernState(seed, x, y, IArray.unsafeFromArray(next))

  def view(width: Int, height: Int, s: S): Scene =
    val field = IArray.tabulate(s.density.length) { i =>
      val d = s.density(i)
      if d <= 0.05 then 255
      else math.min(254, (math.log1p(d) * 28.0).toInt)
    }
    Scene.of(Shape.PixelField(field, palette, computeWidth, computeHeight))

  // background up through moss to a pale green-white at the densest veins
  private val palette: IArray[Int] =
    val stops = List(
      0.0 -> (15, 15, 15),
      0.35 -> (52, 74, 56),
      0.7 -> (100, 128, 105),
      1.0 -> (168, 190, 160)
    )
    def channel(t: Double, pick: ((Int, Int, Int)) => Int): Int =
      val ((t0, c0), (t1, c1)) =
        stops.zip(stops.tail).find((_, hi) => t <= hi._1).getOrElse((stops.init.last, stops.last))
      val f = if t1 == t0 then 0.0 else (t - t0) / (t1 - t0)
      (pick(c0) + f * (pick(c1) - pick(c0))).toInt
    val arr = Array.tabulate(256) { i =>
      val t = i / 254.0
      val (r, g, b) = (channel(t, _._1), channel(t, _._2), channel(t, _._3))
      (255 << 24) | (b << 16) | (g << 8) | r
    }
    arr(255) = (255 << 24) | (15 << 16) | (15 << 8) | 15
    IArray.unsafeFromArray(arr)
