import spire.math.{Complex, Quaternion}
import spire.implicits.*
import cats.data.Chain

/** Pure 3D layer: planar 2D content is lifted into 3D through a Pose and a
  * Camera projects it into the existing 2D Scene. The 2D Renderer never
  * knows 3D happened.
  */
final case class Vec3(x: Double, y: Double, z: Double):
  def +(o: Vec3): Vec3 = Vec3(x + o.x, y + o.y, z + o.z)
  def -(o: Vec3): Vec3 = Vec3(x - o.x, y - o.y, z - o.z)
  def *(s: Double): Vec3 = Vec3(x * s, y * s, z * s)
  def dot(o: Vec3): Double = x * o.x + y * o.y + z * o.z
  def cross(o: Vec3): Vec3 =
    Vec3(y * o.z - z * o.y, z * o.x - x * o.z, x * o.y - y * o.x)

object Vec3:
  val zero: Vec3 = Vec3(0, 0, 0)

object Rotation:
  /** Unit quaternion rotating by `angle` radians (right-handed) about `axis`. */
  def axisAngle(axis: Vec3, angle: Double): Quaternion[Double] =
    val s = math.sin(angle / 2) / math.sqrt(axis.dot(axis))
    Quaternion(math.cos(angle / 2), axis.x * s, axis.y * s, axis.z * s)

  /** Rotate v by unit quaternion q via conjugation q·(0,v)·q*.
    * Note: applying rotation A then rotation B composes as qB * qA.
    */
  def rotate(q: Quaternion[Double], v: Vec3): Vec3 =
    val p = q * Quaternion(0.0, v.x, v.y, v.z) * q.conjugate
    Vec3(p.i, p.j, p.k)

  /** The rotation carrying unit vector a onto unit vector b (half-angle form). */
  def fromTo(a: Vec3, b: Vec3): Quaternion[Double] =
    val c = a.cross(b)
    val q = Quaternion(1.0 + a.dot(b), c.x, c.y, c.z)
    val n = math.sqrt(q.r * q.r + q.i * q.i + q.j * q.j + q.k * q.k)
    if n < 1e-9 then Quaternion(0.0, 1.0, 0.0, 0.0) // a ≈ -b: 180° about an arbitrary axis
    else q * (1.0 / n)

  /** Normalize to unit length. NOT spire's `.unit`, which divides by something
    * other than the Euclidean norm and silently rescales rotations (rotation by
    * conjugation scales space by |q|², so a drifting norm scales the drawing).
    */
  def normalize(q: Quaternion[Double]): Quaternion[Double] =
    val n = math.sqrt(q.r * q.r + q.i * q.i + q.j * q.j + q.k * q.k)
    if n < 1e-12 then Quaternion(1.0, 0.0, 0.0, 0.0) else q * (1.0 / n)

  /** Spherical interpolation between unit quaternions, shortest path. */
  def slerp(a: Quaternion[Double], b: Quaternion[Double], f: Double): Quaternion[Double] =
    val dot0 = a.r * b.r + a.i * b.i + a.j * b.j + a.k * b.k
    val (bs, dot) = if dot0 < 0 then (-b, -dot0) else (b, dot0)
    if dot > 0.9995 then normalize(a * (1 - f) + bs * f)
    else
      val theta = math.acos(math.min(1.0, dot))
      val s = math.sin(theta)
      normalize(a * (math.sin((1 - f) * theta) / s) + bs * (math.sin(f * theta) / s))

final case class Pose(position: Vec3, orientation: Quaternion[Double]):
  def transform(local: Vec3): Vec3 =
    Rotation.rotate(orientation, local) + position

enum Shape3:
  case Path3(points: Seq[Vec3], stroke: String, lineWidth: Double, alpha: Double = 1.0)

type Scene3 = Chain[Shape3]

/** Camera sits at the origin looking down +z; larger z is farther away. */
final case class Camera(distance: Double)

object Scene3D:
  val empty: Scene3 = Chain.empty
  def of(shapes: Shape3*): Scene3 = Chain.fromSeq(shapes)

  /** Lift planar (z = 0) content into 3D through a pose. */
  def planar(
    points: Seq[Complex[Double]],
    pose: Pose,
    stroke: String,
    lineWidth: Double,
    alpha: Double = 1.0
  ): Shape3 =
    Shape3.Path3(points.map(p => pose.transform(Vec3(p.real, p.imag, 0.0))), stroke, lineWidth, alpha)

  /** A circle in a plane, sampled: its projection is an ellipse, which the 2D
    * Shape.Circle cannot express, so a sampled path is the honest lift.
    */
  def circle(
    center: Complex[Double],
    radius: Double,
    pose: Pose,
    stroke: String,
    lineWidth: Double,
    alpha: Double = 1.0
  ): Shape3 =
    // sample count scales with radius so large circles stay smooth
    val samples = math.max(16, math.min(96, (radius * 0.6).toInt))
    val pts = (0 to samples).map { i =>
      val a = 2 * math.Pi * i / samples
      center + Complex(radius * math.cos(a), radius * math.sin(a))
    }
    planar(pts, pose, stroke, lineWidth, alpha)

  private val nearEps = 1e-3

  /** Pure projection into the 2D Scene: painter's sort by mean depth (far to
    * near; sortBy is stable so coplanar shapes keep their emission order),
    * perspective divide, depth-attenuated width and alpha.
    */
  def project(camera: Camera, width: Int, height: Int)(scene: Scene3): Scene =
    val d = camera.distance
    val cx = width / 2.0
    val cy = height / 2.0
    val drawable = scene.toList.collect {
      case Shape3.Path3(pts, stroke, lw, alpha) if pts.length >= 2 && pts.forall(_.z > nearEps - d) =>
        val meanZ = pts.map(_.z).sum / pts.length
        val s = d / (d + meanZ)
        val screen = pts.map { p =>
          val f = d / (d + p.z)
          Complex(cx + p.x * f, cy + p.y * f)
        }
        meanZ -> Shape.Polyline(screen, stroke, lw * s, alpha * math.min(1.0, s * s))
    }
    Chain.fromSeq(drawable.sortBy((z, _) => -z).map(_._2))
