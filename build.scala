//> using scala 3.3
//> using dep com.lihaoyi::os-lib:0.11.6
//> using dep org.commonmark:commonmark:0.27.1

import org.commonmark.parser.Parser
import org.commonmark.renderer.html.HtmlRenderer
import org.commonmark.node.*

@main def build(): Unit =
  val opinionsDir = os.pwd / "src" / "opinions"
  val generatedDir = os.pwd / "src" / "generated"

  // Ensure directories exist
  if !os.exists(opinionsDir) then
    os.makeDir.all(opinionsDir)
    println(s"Created $opinionsDir")

  os.makeDir.all(generatedDir)

  // Find all markdown files
  val mdFiles = if os.exists(opinionsDir) then
    os.list(opinionsDir).filter(_.ext == "md").toList
  else
    List.empty

  // Parse each markdown file
  val parser = Parser.builder().build()
  val renderer = HtmlRenderer.builder().build()

  val posts = mdFiles.flatMap { file =>
    val content = os.read(file)
    parseFrontmatter(content) match
      case Some((frontmatter, body)) =>
        val title = frontmatter.getOrElse("title", file.baseName)
        val date = frontmatter.getOrElse("date", "")
        val slug = file.baseName
        val document = parser.parse(body)
        val html = renderer.render(document)
        Some((slug, title, date, html))
      case None =>
        println(s"Warning: Could not parse frontmatter in ${file.last}")
        None
  }

  // Generate Scala file
  val scalaCode = generateScalaCode(posts)
  os.write.over(generatedDir / "Opinions.scala", scalaCode)
  println(s"Generated ${generatedDir / "Opinions.scala"} with ${posts.size} posts")

def parseFrontmatter(content: String): Option[(Map[String, String], String)] =
  val lines = content.linesIterator.toList
  if lines.headOption.exists(_.trim == "---") then
    val endIndex = lines.drop(1).indexWhere(_.trim == "---")
    if endIndex >= 0 then
      val frontmatterLines = lines.slice(1, endIndex + 1)
      val body = lines.drop(endIndex + 2).mkString("\n")
      val frontmatter = frontmatterLines.flatMap { line =>
        line.split(":", 2) match
          case Array(key, value) => Some(key.trim -> value.trim)
          case _ => None
      }.toMap
      Some((frontmatter, body))
    else None
  else None

def generateScalaCode(posts: List[(String, String, String, String)]): String =
  val postEntries = posts.map { case (slug, title, date, html) =>
    val escapedHtml = html
      .replace("\\", "\\\\")
      .replace("\"", "\\\"")
      .replace("\n", "\\n")
    s"""  Post(Slug("$slug"), "$title", "$date", raw("$escapedHtml"))"""
  }.mkString(",\n")

  s"""|import scalatags.JsDom.all.*
      |
      |object Opinions:
      |  val posts: List[Post] = List(
      |$postEntries
      |  )
      |""".stripMargin
