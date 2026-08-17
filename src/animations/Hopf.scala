import spire.math.Quaternion
import spire.implicits.*
import cats.data.Chain

/** The Hopf fibration: each point of an ordinary sphere lifts to a circle of
  * unit quaternions on the 3-sphere; stereographic projection turns those
  * fibers into interlinked rings. Fibers over one circle of latitude form a
  * torus of linked rings — the latitude sweeps slowly, so the torus breathes
  * from a tight ring to a wide band while the whole structure tumbles.
  */
object Hopf extends Animation:
  val slug: Slug = Slug("hopf")
  val title: String = "hopf fibration"

  private val fibers = 36
  private val fiberSamples = 64
  private val precession = 0.1
  private val kHat = Vec3(0, 0, 1)

  type S = Double

  def init(width: Int, height: Int): S = 0.0

  def step(t: Double, dt: Double, input: Input, s: S): S = t

  /** Stereographic projection of a unit quaternion (from the pole q = 1). */
  private def stereographic(q: Quaternion[Double]): Vec3 =
    val f = 1.0 / (1.0 - math.min(q.r, 0.95))
    Vec3(q.i * f, q.j * f, q.k * f)

  /** Cyclic muted gradient around the fiber circle: slate -> warm -> slate. */
  private def fiberColor(f: Double): String =
    val g = 0.5 * (1 + math.sin(2 * math.Pi * f))
    def ch(a: Int, b: Int) = (a + g * (b - a)).toInt
    String.format("#%02x%02x%02x", ch(90, 185), ch(125, 160), ch(140, 125))

  def view(width: Int, height: Int, s: S): Scene =
    val t = s
    val worldScale = math.min(width, height) / 7.0
    // latitude sweeps south-to-north-ish, staying clear of the projection pole
    val lat = -0.45 + 0.7 * math.sin(0.16 * t)
    val pose = Pose(
      Vec3.zero,
      Rotation.normalize(
        Rotation.axisAngle(Vec3(1, 0.3, 0), 0.06 * t) *
          Rotation.axisAngle(Vec3(0, 1, 0.2), 0.045 * t)
      )
    )
    val camera = Camera(1.6 * math.max(width, height))

    val scene3: Scene3 = Chain.fromSeq((0 until fibers).toList).map { i =>
      val frac = i.toDouble / fibers
      val lon = 2 * math.Pi * frac + precession * t
      val b = Vec3(
        math.cos(lat) * math.cos(lon),
        math.cos(lat) * math.sin(lon),
        math.sin(lat)
      )
      val qb = Rotation.fromTo(kHat, b)
      val points = (0 to fiberSamples).map { j =>
        val theta = 2 * math.Pi * j / fiberSamples
        val q = qb * Quaternion(math.cos(theta), 0.0, 0.0, math.sin(theta))
        pose.transform(stereographic(q) * worldScale)
      }
      Shape3.Path3(points, fiberColor(frac), 1.2, alpha = 0.55)
    }

    Scene.of(Shape.Fill("#0f0f0f")) ++ Scene3D.project(camera, width, height)(scene3)
