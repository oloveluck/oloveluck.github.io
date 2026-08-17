//> using scala 3.8.4
//> using dep com.lihaoyi::os-lib:0.11.8

import com.sun.net.httpserver.*
import java.net.InetSocketAddress
import java.nio.file.{Files, Paths}

def runStep(description: String, command: String*): Unit =
  println(description)
  val result = os.proc(command).call(cwd = os.pwd, check = false)
  if result.exitCode != 0 then
    println(s"$description failed:\n${result.err.text()}")
    sys.exit(1)

@main def serve(args: String*): Unit =
  val port = args.headOption.flatMap(_.toIntOption).getOrElse(8000)

  runStep("Processing markdown files...", "scala-cli", "run", "build.scala")
  runStep(
    "Building Scala.js...",
    "scala-cli", "--power", "package", "--js", "src",
    "-o", "public/app", "-f"
  )

  println(s"Build complete. Starting server at http://localhost:$port")

  val server = HttpServer.create(InetSocketAddress(port), 0)
  val publicDir = Paths.get("public").toAbsolutePath

  server.createContext("/", exchange => {
    val path = exchange.getRequestURI.getPath match
      case "/" => "/index.html"
      case p   => p

    val file = publicDir.resolve(path.stripPrefix("/")).normalize()

    if file.startsWith(publicDir) && Files.exists(file) && Files.isRegularFile(file) then
      val content = Files.readAllBytes(file)
      val contentType = path match
        case p if p.endsWith(".html") => "text/html"
        case p if p.endsWith(".js")   => "application/javascript"
        case p if p.endsWith(".mjs")  => "application/javascript"
        case p if p.endsWith(".wasm") => "application/wasm"
        case p if p.endsWith(".css")  => "text/css"
        case p if p.endsWith(".svg")  => "image/svg+xml"
        case p if p.endsWith(".txt")  => "text/plain"
        case _                        => "application/octet-stream"

      exchange.getResponseHeaders.set("Content-Type", contentType)
      exchange.sendResponseHeaders(200, content.length)
      exchange.getResponseBody.write(content)
      exchange.getResponseBody.close()
    else
      exchange.sendResponseHeaders(404, 0)
      exchange.getResponseBody.close()
  })

  server.setExecutor(null)
  server.start()
  println(s"Serving public/ at http://localhost:$port (Ctrl+C to stop)")
  Thread.currentThread().join()
