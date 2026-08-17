import cats.data.Chain
import spire.implicits.*

/** The Lorenz attractor: two trajectories started a hair apart, integrated
  * with RK4 and drawn as fading trails on the tumbling attractor. Sensitive
  * dependence does the storytelling — they move in lockstep, then tear apart.
  */
object Lorenz extends Animation:
  val slug: Slug = Slug("lorenz")
  val title: String = "lorenz attractor"

  private val sigma = 10.0
  private val rho = 28.0
  private val beta = 8.0 / 3.0
  private val h = 0.004
  private val retention = 7.0

  final case class LorenzState(
    t: Double,
    a: Vec3,
    b: Vec3,
    trailA: Vector[(Double, Vec3)],
    trailB: Vector[(Double, Vec3)]
  )
  type S = LorenzState

  def init(width: Int, height: Int): S =
    LorenzState(
      t = 0.0,
      a = Vec3(1.0, 1.0, 20.0),
      b = Vec3(1.001, 1.0, 20.0),
      trailA = Vector.empty,
      trailB = Vector.empty
    )

  private def deriv(p: Vec3): Vec3 =
    Vec3(
      sigma * (p.y - p.x),
      p.x * (rho - p.z) - p.y,
      p.x * p.y - beta * p.z
    )

  private def rk4(p: Vec3): Vec3 =
    val k1 = deriv(p)
    val k2 = deriv(p + k1 * (h / 2))
    val k3 = deriv(p + k2 * (h / 2))
    val k4 = deriv(p + k3 * h)
    p + (k1 + k2 * 2.0 + k3 * 2.0 + k4) * (h / 6)

  private def advance(
    t: Double, dt: Double, steps: Int, start: Vec3, trail: Vector[(Double, Vec3)]
  ): (Vec3, Vector[(Double, Vec3)]) =
    val (end, added) = (1 to steps).foldLeft((start, Vector.empty[(Double, Vec3)])) {
      case ((p, acc), i) =>
        val q = rk4(p)
        (q, acc :+ (t - dt + dt * i / steps, q))
    }
    val stalled = trail.lastOption.exists((t0, _) => t - t0 > 0.25)
    val base = if stalled then Vector.empty else trail
    (end, (base ++ added).dropWhile((t0, _) => t0 < t - retention))

  def step(t: Double, dt: Double, input: Input, s: S): S =
    val steps = math.max(1, math.min(8, math.round(dt / h).toInt))
    val (na, trailA) = advance(t, dt, steps, s.a, s.trailA)
    val (nb, trailB) = advance(t, dt, steps, s.b, s.trailB)
    LorenzState(t, na, nb, trailA, trailB)

  def view(width: Int, height: Int, s: S): Scene =
    val worldScale = math.min(width, height) / 55.0
    // butterfly wings live in the attractor's x-z plane; center z on the lobes
    def toWorld(p: Vec3): Vec3 =
      Vec3(p.x * worldScale, -(p.z - rho + 1) * worldScale, p.y * worldScale)
    val pose = Pose(
      Vec3.zero,
      Rotation.normalize(
        Rotation.axisAngle(Vec3(0, 1, 0.15), 0.09 * s.t) *
          Rotation.axisAngle(Vec3(1, 0, 0.2), 0.06 * s.t)
      )
    )
    val camera = Camera(1.6 * math.max(width, height))

    def bands(trail: Vector[(Double, Vec3)], stroke: String): Scene3 =
      val points = trail.map((_, p) => pose.transform(toWorld(p)))
      val bandCount = 3
      val bandSize = math.max(1, points.length / bandCount)
      val grouped = points.grouped(bandSize).toVector
      Chain.fromSeq(
        grouped.zipWithIndex.map { (band, i) =>
          val joined = if i == 0 then band else grouped(i - 1).last +: band
          Shape3.Path3(joined, stroke, 1.4, alpha = 0.2 + 0.6 * (i + 1.0) / bandCount)
        }.toList
      )

    Scene.of(Shape.Fill("#0f0f0f")) ++
      Scene3D.project(camera, width, height)(
        bands(s.trailA, "#8ab4c8") ++ bands(s.trailB, "#c8a878")
      )
