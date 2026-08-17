//> using scala 3.8.4
//> using platform scala-js
//> using jsVersion 1.22.0
//> using jsModuleKind es
//> using jsEmitWasm true
//> using dep com.lihaoyi::scalatags::0.13.1
//> using dep org.scala-js::scalajs-dom::2.8.1
//> using dep org.typelevel::spire::0.18.0
//> using dep org.typelevel::cats-core::2.13.0

import scalatags.JsDom.all.*
import org.scalajs.dom.{document, window, html}

// Domain types
opaque type Svg = String
object Svg:
  def apply(raw: String): Svg = raw
  extension (svg: Svg) def toFrag: Frag = raw(svg)

opaque type Url = String
object Url:
  def apply(s: String): Url = s
  extension (u: Url) def value: String = u

opaque type Slug = String
object Slug:
  def apply(s: String): Slug = s
  extension (s: Slug) def value: String = s

enum LinkStyle:
  case Icon(svg: Svg)
  case Text(display: String)

final case class Link(url: Url, label: String, style: LinkStyle)

final case class Post(slug: Slug, title: String, date: String, content: Frag)

enum Route:
  case Home
  case OpinionsList
  case OpinionPost(slug: Slug)
  case LabList
  case LabAnimation(slug: Slug)

final case class SiteData(
  name: String,
  bio: String,
  socialLinks: List[Link],
  footerLinks: List[Link],
  opinions: List[Post]
)

// Extension for elegant fragment composition
extension (frags: List[Frag])
  def intersperse(sep: Frag): Frag =
    frag(frags.flatMap(f => List(sep, f)).drop(1))

// Visual assets separated from site data
object Icons:
  val github: Svg = Svg("""<svg xmlns="http://www.w3.org/2000/svg" width="20" height="20" viewBox="0 0 24 24" fill="currentColor"><path d="M12 0c-6.626 0-12 5.373-12 12 0 5.302 3.438 9.8 8.207 11.387.599.111.793-.261.793-.577v-2.234c-3.338.726-4.033-1.416-4.033-1.416-.546-1.387-1.333-1.756-1.333-1.756-1.089-.745.083-.729.083-.729 1.205.084 1.839 1.237 1.839 1.237 1.07 1.834 2.807 1.304 3.492.997.107-.775.418-1.305.762-1.604-2.665-.305-5.467-1.334-5.467-5.931 0-1.311.469-2.381 1.236-3.221-.124-.303-.535-1.524.117-3.176 0 0 1.008-.322 3.301 1.23.957-.266 1.983-.399 3.003-.404 1.02.005 2.047.138 3.006.404 2.291-1.552 3.297-1.23 3.297-1.23.653 1.653.242 2.874.118 3.176.77.84 1.235 1.911 1.235 3.221 0 4.609-2.807 5.624-5.479 5.921.43.372.823 1.102.823 2.222v3.293c0 .319.192.694.801.576 4.765-1.589 8.199-6.086 8.199-11.386 0-6.627-5.373-12-12-12z"/></svg>""")

  val linkedin: Svg = Svg("""<svg xmlns="http://www.w3.org/2000/svg" width="20" height="20" viewBox="0 0 24 24" fill="currentColor"><path d="M19 0h-14c-2.761 0-5 2.239-5 5v14c0 2.761 2.239 5 5 5h14c2.762 0 5-2.239 5-5v-14c0-2.761-2.238-5-5-5zm-11 19h-3v-11h3v11zm-1.5-12.268c-.966 0-1.75-.79-1.75-1.764s.784-1.764 1.75-1.764 1.75.79 1.75 1.764-.783 1.764-1.75 1.764zm13.5 12.268h-3v-5.604c0-3.368-4-3.113-4 0v5.604h-3v-11h3v1.765c1.396-2.586 7-2.777 7 2.476v6.759z"/></svg>""")

  val instagram: Svg = Svg("""<svg xmlns="http://www.w3.org/2000/svg" width="20" height="20" viewBox="0 0 24 24" fill="currentColor"><path d="M12 2.163c3.204 0 3.584.012 4.85.07 3.252.148 4.771 1.691 4.919 4.919.058 1.265.069 1.645.069 4.849 0 3.205-.012 3.584-.069 4.849-.149 3.225-1.664 4.771-4.919 4.919-1.266.058-1.644.07-4.85.07-3.204 0-3.584-.012-4.849-.07-3.26-.149-4.771-1.699-4.919-4.92-.058-1.265-.07-1.644-.07-4.849 0-3.204.013-3.583.07-4.849.149-3.227 1.664-4.771 4.919-4.919 1.266-.057 1.645-.069 4.849-.069zm0-2.163c-3.259 0-3.667.014-4.947.072-4.358.2-6.78 2.618-6.98 6.98-.059 1.281-.073 1.689-.073 4.948 0 3.259.014 3.668.072 4.948.2 4.358 2.618 6.78 6.98 6.98 1.281.058 1.689.072 4.948.072 3.259 0 3.668-.014 4.948-.072 4.354-.2 6.782-2.618 6.979-6.98.059-1.28.073-1.689.073-4.948 0-3.259-.014-3.667-.072-4.947-.196-4.354-2.617-6.78-6.979-6.98-1.281-.059-1.69-.073-4.949-.073zm0 5.838c-3.403 0-6.162 2.759-6.162 6.162s2.759 6.163 6.162 6.163 6.162-2.759 6.162-6.163c0-3.403-2.759-6.162-6.162-6.162zm0 10.162c-2.209 0-4-1.79-4-4 0-2.209 1.791-4 4-4s4 1.791 4 4c0 2.21-1.791 4-4 4zm6.406-11.845c-.796 0-1.441.645-1.441 1.44s.645 1.44 1.441 1.44c.795 0 1.439-.645 1.439-1.44s-.644-1.44-1.439-1.44z"/></svg>""")

// Pure data
object Data:
  val site: SiteData = SiteData(
    name = "Owen Loveluck",
    bio = "Currently working on healthcare, functional programming, and sometimes climbing mountains.",
    socialLinks = List(
      Link(Url("https://github.com/oloveluck"), "GitHub", LinkStyle.Icon(Icons.github)),
      Link(Url("https://linkedin.com/in/oloveluck"), "LinkedIn", LinkStyle.Icon(Icons.linkedin)),
      Link(Url("https://instagram.com/oloveluck"), "Instagram", LinkStyle.Icon(Icons.instagram))
    ),
    footerLinks = List(
      Link(Url("privacy.txt"), "Privacy Policy", LinkStyle.Text("privacy")),
      Link(Url("llms.txt"), "LLMs Info", LinkStyle.Text("for robots"))
    ),
    opinions = Opinions.posts
  )

// Router for hash-based navigation
object Router:
  def parse(hash: String): Route = hash match
    case "" | "#" | "#/"                  => Route.Home
    case "#/opinions"                     => Route.OpinionsList
    case s if s.startsWith("#/opinions/") => Route.OpinionPost(Slug(s.stripPrefix("#/opinions/")))
    case "#/lab"                          => Route.LabList
    case s if s.startsWith("#/lab/")      => Route.LabAnimation(Slug(s.stripPrefix("#/lab/")))
    case _                                => Route.Home

// Pure view functions
object View:
  def page(data: SiteData, route: Route): Frag =
    val content = route match
      case Route.Home         => homeWrapper(data)
      case Route.OpinionsList => opinionsPage(data)
      case Route.OpinionPost(slug) =>
        data.opinions
          .find(_.slug.value == slug.value)
          .map(postPage)
          .getOrElse(homeWrapper(data))
      case Route.LabList => labPage
      case Route.LabAnimation(slug) =>
        Animations.bySlug(slug).map(animationPage).getOrElse(labPage)
    frag(content, footer(data.footerLinks))

  def homeWrapper(data: SiteData): Frag =
    frag(
      canvas(id := "bg-canvas", cls := "bg-canvas"),
      div(cls := "wrapper")(
        h1(data.name),
        p(cls := "bio")(data.bio),
        linkBar(data.socialLinks),
        navLinks(data.opinions.nonEmpty)
      )
    )

  def navLinks(hasOpinions: Boolean): Frag =
    val links: List[Frag] =
      (if hasOpinions then List(a(href := "#/opinions")("opinions")) else Nil)
        :+ a(href := "#/lab")("lab")
    div(cls := "opinions-link")(
      links.intersperse(span(cls := "separator")("·"))
    )

  def opinionsPage(data: SiteData): Frag =
    div(cls := "opinions-wrapper")(
      div(cls := "opinions-header")(
        a(href := "#/", cls := "back-link")("← back"),
        h1("opinions")
      ),
      div(cls := "post-list")(
        data.opinions.sortBy(_.date).reverse.map(postPreview)
      )
    )

  def postPreview(post: Post): Frag =
    a(href := s"#/opinions/${post.slug.value}", cls := "post-preview")(
      span(cls := "post-date")(post.date),
      span(cls := "post-title")(post.title)
    )

  def postPage(post: Post): Frag =
    div(cls := "post-wrapper")(
      div(cls := "post-header")(
        a(href := "#/opinions", cls := "back-link")("← opinions")
      ),
      tag("article")(cls := "post-content")(
        h1(post.title),
        div(cls := "post-date")(post.date),
        div(cls := "post-body")(post.content)
      )
    )

  def labPage: Frag =
    div(cls := "opinions-wrapper")(
      div(cls := "opinions-header")(
        a(href := "#/", cls := "back-link")("← back"),
        h1("lab")
      ),
      div(cls := "post-list")(
        Animations.all.map(animPreview)
      )
    )

  def animPreview(anim: Animation): Frag =
    a(href := s"#/lab/${anim.slug.value}", cls := "post-preview")(
      span(cls := "post-title")(anim.title)
    )

  def animationPage(anim: Animation): Frag =
    div(cls := "anim-wrapper")(
      div(cls := "post-header")(
        a(href := "#/lab", cls := "back-link")("← lab")
      ),
      h1(anim.title),
      canvas(id := "anim-canvas", cls := "anim-canvas")
    )

  def linkBar(links: List[Link]): Frag =
    div(cls := "links")(links.map(renderLink))

  def renderLink(l: Link): Frag = l.style match
    case LinkStyle.Icon(svg)     => a(href := l.url.value, attr("aria-label") := l.label)(svg.toFrag)
    case LinkStyle.Text(display) => a(href := l.url.value)(display)

  def footer(links: List[Link]): Frag =
    tag("footer")(
      links.map(renderLink).intersperse(span(cls := "separator")("\u00b7"))
    )

// Entry point - reactive hash-based routing
@main def app(): Unit =
  var active: Option[RunningAnimation] = None

  def render(): Unit =
    active.foreach(_.cancel())
    active = None
    val route = Router.parse(window.location.hash)
    document.body.replaceChildren(View.page(Data.site, route).render)
    route match
      case Route.LabAnimation(slug) =>
        active = Animations.bySlug(slug).map { anim =>
          val canvas = document.getElementById("anim-canvas").asInstanceOf[html.Canvas]
          AnimationRunner.start(anim, canvas)
        }
      case Route.Home =>
        val canvas = document.getElementById("bg-canvas").asInstanceOf[html.Canvas]
        active = Some(AnimationRunner.start(Loveluck, canvas))
      case _ => ()

  render()
  window.addEventListener("hashchange", _ => render())

  // the home background canvas is viewport-sized, so it must re-mount on resize
  var resizeTimer = 0
  window.addEventListener(
    "resize",
    _ =>
      window.clearTimeout(resizeTimer)
      resizeTimer = window.setTimeout(
        () => if Router.parse(window.location.hash) == Route.Home then render(),
        150
      )
  )
