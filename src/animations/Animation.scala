import org.scalajs.dom.{html, window, CanvasRenderingContext2D, PointerEvent}
import scala.scalajs.js

/** Pointer position normalized to [-1, 1] from viewport center (None until the
  * first pointermove), and this frame's drag movement in the same normalized
  * units (Some while a button/finger is held, None otherwise).
  */
final case class Input(pointer: Option[(Double, Double)], drag: Option[(Double, Double)])

object Input:
  val empty: Input = Input(None, None)

trait Animation:
  type S
  def slug: Slug
  def title: String

  /** Fixed backing resolution (CSS-upscaled); None means dpr-scaled to the CSS size. */
  def pixelScale: Option[(Int, Int)] = None

  def init(width: Int, height: Int): S
  def step(t: Double, dt: Double, input: Input, s: S): S
  def view(width: Int, height: Int, s: S): Scene

final class RunningAnimation:
  var rafId: Int = 0
  var cancelled: Boolean = false
  var pointer: Option[(Double, Double)] = None
  var pointerAt: Double = Double.NegativeInfinity
  var dragging: Boolean = false
  var dragLast: (Double, Double) = (0.0, 0.0)
  var dragDX: Double = 0.0
  var dragDY: Double = 0.0
  var listeners: List[(String, js.Function1[PointerEvent, Unit])] = Nil

  def cancel(): Unit =
    cancelled = true
    window.cancelAnimationFrame(rafId)
    listeners.foreach((event, fn) => window.removeEventListener(event, fn))
    listeners = Nil

object AnimationRunner:
  // dt is clamped so a backgrounded tab doesn't produce one giant integration step
  private val maxDt = 0.05

  def start(anim: Animation, canvas: html.Canvas): RunningAnimation =
    val handle = RunningAnimation()
    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    val (width, height) = anim.pixelScale match
      case Some((w, h)) =>
        canvas.width = w
        canvas.height = h
        (w, h)
      case None =>
        val dpr = window.devicePixelRatio
        val (w, h) = (canvas.clientWidth, canvas.clientHeight)
        canvas.width = (w * dpr).toInt
        canvas.height = (h * dpr).toInt
        ctx.setTransform(dpr, 0, 0, dpr, 0, 0)
        (w, h)

    val renderer = Renderer(ctx)

    val onMove: js.Function1[PointerEvent, Unit] = e =>
      handle.pointer = Some((
        e.clientX / window.innerWidth * 2 - 1,
        e.clientY / window.innerHeight * 2 - 1
      ))
      handle.pointerAt = window.performance.now()
      if handle.dragging then
        val (lx, ly) = handle.dragLast
        handle.dragDX += (e.clientX - lx) / window.innerWidth * 2
        handle.dragDY += (e.clientY - ly) / window.innerHeight * 2
        handle.dragLast = (e.clientX, e.clientY)
    val onDown: js.Function1[PointerEvent, Unit] = e =>
      handle.dragging = true
      handle.dragLast = (e.clientX, e.clientY)
    val onUp: js.Function1[PointerEvent, Unit] = _ =>
      handle.dragging = false

    handle.listeners = List(
      "pointermove" -> onMove,
      "pointerdown" -> onDown,
      "pointerup" -> onUp,
      "pointercancel" -> onUp
    )
    handle.listeners.foreach((event, fn) => window.addEventListener(event, fn))

    // a pointer sample older than this counts as "walked away"
    val pointerStaleMs = 8000.0

    // state threads through the self-scheduling closure; each frame only
    // registers the next callback, so this is not stack recursion
    def frame(s: anim.S, start: Double, last: Double): js.Function1[Double, Unit] = (ts: Double) =>
      if !handle.cancelled then
        val dt = math.min((ts - last) / 1000.0, maxDt)
        val drag =
          if handle.dragging || handle.dragDX != 0.0 || handle.dragDY != 0.0
          then Some((handle.dragDX, handle.dragDY))
          else None
        handle.dragDX = 0.0
        handle.dragDY = 0.0
        val pointer = if ts - handle.pointerAt > pointerStaleMs then None else handle.pointer
        val next = anim.step((ts - start) / 1000.0, dt, Input(pointer, drag), s)
        renderer.render(width, height, anim.view(width, height, next))
        handle.rafId = window.requestAnimationFrame(frame(next, start, ts))

    handle.rafId = window.requestAnimationFrame(
      (ts: Double) => frame(anim.init(width, height), ts, ts)(ts)
    )
    handle

object Animations:
  val all: List[Animation] =
    List(EpicycleAnimation.heart, EpicycleAnimation.clover, Julia, Newton, Mobius, Lorenz, Fern, Knot, Hopf)

  def bySlug(slug: Slug): Option[Animation] =
    all.find(_.slug.value == slug.value)
