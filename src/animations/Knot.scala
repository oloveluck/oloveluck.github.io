import cats.data.Chain
import spire.implicits.*

/** Torus knots: the (p, q) knot winds p times around the torus's axis while
  * circling its tube q times. The knot is drawn as a six-strand wireframe
  * tube built on its analytic tangent frame, with color bands flowing along
  * the curve; it tumbles in 3D and crossfades to the next (p, q).
  */
object Knot extends Animation:
  val slug: Slug = Slug("knot")
  val title: String = "torus knots"

  private val knots = Vector((2, 3), (2, 5), (3, 4), (3, 5), (2, 7))
  private val holdT = 9.0
  private val fadeT = 2.5
  private val stageT = holdT + fadeT
  private val segments = 40
  private val pointsPerSegment = 10
  private val strands = 6
  private val bigR = 2.0
  private val tubeR = 0.75
  private val tubeThickness = 0.16
  private val colorFlow = 0.06

  type S = Double

  def init(width: Int, height: Int): S = 0.0

  def step(t: Double, dt: Double, input: Input, s: S): S = t

  /** Point on the (p, q) torus knot plus a tube-frame offset: the offset
    * direction rotates in the plane normal to the curve's analytic tangent.
    */
  private def tubePoint(p: Int, q: Int, phi: Double, psi: Double): Vec3 =
    val ring = bigR + tubeR * math.cos(q * phi)
    val center = Vec3(ring * math.cos(p * phi), ring * math.sin(p * phi), tubeR * math.sin(q * phi))
    val ringD = -tubeR * q * math.sin(q * phi)
    val tangent0 = Vec3(
      ringD * math.cos(p * phi) - ring * p * math.sin(p * phi),
      ringD * math.sin(p * phi) + ring * p * math.cos(p * phi),
      tubeR * q * math.cos(q * phi)
    )
    val tangent = tangent0 * (1.0 / math.sqrt(tangent0.dot(tangent0)))
    val radial = Vec3(math.cos(p * phi), math.sin(p * phi), 0.0)
    val n0 = radial - tangent * radial.dot(tangent)
    val normal = n0 * (1.0 / math.sqrt(n0.dot(n0)))
    val binormal = tangent.cross(normal)
    center + (normal * math.cos(psi) + binormal * math.sin(psi)) * tubeThickness

  /** Cyclic muted gradient: slate -> warm -> slate. */
  private def bandColor(f: Double): String =
    val g = 0.5 * (1 + math.sin(2 * math.Pi * f))
    def ch(a: Int, b: Int) = (a + g * (b - a)).toInt
    String.format("#%02x%02x%02x", ch(90, 185), ch(125, 160), ch(140, 125))

  private def knotPaths(p: Int, q: Int, t: Double, scale: Double, pose: Pose, alpha: Double): Scene3 =
    Chain.fromSeq(
      for
        strand <- (0 until strands).toList
        seg <- (0 until segments).toList
      yield
        val psi = 2 * math.Pi * strand / strands
        val points = (0 to pointsPerSegment).map { j =>
          val u = (seg * pointsPerSegment + j).toDouble / (segments * pointsPerSegment)
          pose.transform(tubePoint(p, q, 2 * math.Pi * u, psi) * scale)
        }
        // bands crawl along the knot over time
        val f = seg.toDouble / segments - colorFlow * t
        Shape3.Path3(points, bandColor(f - math.floor(f)), 1.2, alpha)
    )

  def view(width: Int, height: Int, s: S): Scene =
    val t = s
    val scale = math.min(width, height) / 6.8
    val pose = Pose(
      Vec3.zero,
      Rotation.normalize(
        Rotation.axisAngle(Vec3(1, 0.2, 0), 0.11 * t) *
          Rotation.axisAngle(Vec3(0, 1, 0.3), 0.08 * t)
      )
    )
    val camera = Camera(1.6 * math.max(width, height))

    val stage = ((t / stageT) % knots.length).toInt
    val phase = t % stageT
    val (p1, q1) = knots(stage)
    val (p2, q2) = knots((stage + 1) % knots.length)

    val scene3 =
      if phase < holdT then knotPaths(p1, q1, t, scale, pose, alpha = 0.55)
      else
        val u = (phase - holdT) / fadeT
        val mix = u * u * (3 - 2 * u)
        knotPaths(p1, q1, t, scale, pose, alpha = 0.55 * (1 - mix)) ++
          knotPaths(p2, q2, t, scale, pose, alpha = 0.55 * mix)

    Scene.of(Shape.Fill("#0f0f0f")) ++ Scene3D.project(camera, width, height)(scene3)
