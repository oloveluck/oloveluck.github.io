//> using scala 3.3
//> using dep com.lihaoyi::os-lib:0.11.6

import com.sun.net.httpserver.*
import java.net.InetSocketAddress
import java.nio.file.{Files, Paths}

@main def serve(args: String*): Unit =
  val port = args.headOption.flatMap(_.toIntOption).getOrElse(8000)
  // First build the Scala.js
  println("Building Scala.js...")
  val result = os.proc("scala-cli", "--power", "package", "--js", "src/Main.scala", "-o", "public/main.js", "-f")
    .call(cwd = os.pwd, check = false)

  if result.exitCode != 0 then
    println(s"Build failed:\n${result.err.text()}")
    sys.exit(1)

  println(s"Build complete. Starting server at http://localhost:$port")

  val server = HttpServer.create(InetSocketAddress(port), 0)
  val publicDir = Paths.get("public").toAbsolutePath

  server.createContext("/", exchange => {
    val path = exchange.getRequestURI.getPath match
      case "/" => "/index.html"
      case p   => p

    val file = publicDir.resolve(path.stripPrefix("/"))

    if Files.exists(file) && Files.isRegularFile(file) then
      val content = Files.readAllBytes(file)
      val contentType = path match
        case p if p.endsWith(".html") => "text/html"
        case p if p.endsWith(".js")   => "application/javascript"
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
