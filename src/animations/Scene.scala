import org.scalajs.dom.{CanvasRenderingContext2D, ImageData}
import scala.scalajs.js.typedarray.Int32Array
import spire.math.Complex
import cats.data.Chain

/** A frame described as pure data. Animations build Scenes; only the
  * Renderer below ever touches the canvas.
  */
enum Shape:
  case Fill(color: String)
  case Circle(center: Complex[Double], radius: Double, stroke: String, lineWidth: Double)
  case Polyline(points: Seq[Complex[Double]], stroke: String, lineWidth: Double, alpha: Double = 1.0)
  /** Palette-indexed bitmap; palette entries are packed little-endian ABGR words. */
  case PixelField(indices: IArray[Int], palette: IArray[Int], width: Int, height: Int)
  /** Full-color bitmap of packed little-endian ABGR words. */
  case RawPixels(argb: IArray[Int], width: Int, height: Int)

type Scene = Chain[Shape]

object Scene:
  val empty: Scene = Chain.empty
  def of(shapes: Shape*): Scene = Chain.fromSeq(shapes)

/** The imperative shell: the single interpreter from Scene data to canvas calls. */
final class Renderer(ctx: CanvasRenderingContext2D):
  private var image: ImageData = null
  private var pixels: Int32Array = null

  def render(width: Int, height: Int, scene: Scene): Unit =
    scene.iterator.foreach {
      case Shape.Fill(color) =>
        ctx.fillStyle = color
        ctx.fillRect(0, 0, width, height)

      case Shape.Circle(center, radius, stroke, lineWidth) =>
        ctx.strokeStyle = stroke
        ctx.lineWidth = lineWidth
        ctx.beginPath()
        ctx.arc(center.real, center.imag, radius, 0, 2 * math.Pi)
        ctx.stroke()

      case Shape.Polyline(points, stroke, lineWidth, alpha) =>
        if points.length > 1 then
          ctx.strokeStyle = stroke
          ctx.lineWidth = lineWidth
          ctx.globalAlpha = alpha
          ctx.beginPath()
          ctx.moveTo(points.head.real, points.head.imag)
          points.tail.foreach(p => ctx.lineTo(p.real, p.imag))
          ctx.stroke()
          ctx.globalAlpha = 1.0

      case Shape.PixelField(indices, palette, w, h) =>
        if image == null then
          image = ctx.createImageData(w, h)
          pixels = new Int32Array(image.data.buffer)
        var i = 0
        while i < indices.length do
          pixels(i) = palette(indices(i))
          i += 1
        ctx.putImageData(image, 0, 0)

      case Shape.RawPixels(argb, w, h) =>
        if image == null then
          image = ctx.createImageData(w, h)
          pixels = new Int32Array(image.data.buffer)
        var i = 0
        while i < argb.length do
          pixels(i) = argb(i)
          i += 1
        ctx.putImageData(image, 0, 0)
    }
