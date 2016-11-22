import akka.actor.ActorSystem
import akka.http.scaladsl.{ConnectionContext, Http, HttpsConnectionContext}
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.ContentTypes._
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.{Directive0, Route}
import akka.http.scaladsl.server.RouteResult.Complete
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Flow
import akka.util.ByteString
import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.sslconfig.akka.AkkaSSLConfig
import org.slf4j.Logger
import org.uowlog._
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.xml.Xhtml

trait TemplateRoutes {
  implicit def system: ActorSystem
  def config: Config
  def httpsPort: Int

  private implicit def executionContext = system.dispatcher

  val bindingPromise = Promise[List[ServerBinding]]()

  def adminRoute =
    pathPrefix("admin")(
      // If http, redirect to https
      scheme("http")(extractUri { uri =>
        println("Full Uri: " + uri)
        if (uri.authority.port != httpsPort) redirect(uri.withScheme("https").withPort(httpsPort), MovedPermanently)
        else reject
      }) ~
      path("shutdown")(complete {
        println("Server stopping")
        bindingPromise.future
          .flatMap(bindings => Future.sequence(bindings.map(_.unbind()))) // trigger unbinding from the ports
          .onComplete(_ => system.terminate()) // and shutdown when done
        "shutting down"
      }) ~
      pathEndOrSingleSlash(complete(HttpEntity(`text/html(UTF-8)`, Xhtml.toXhtml(
        <html><body><ul>
          <li><a href="/admin/shutdown">shutdown</a></li>
        </ul></body></html>
      ))))
    )

  def baseRoute =
    pathEndOrSingleSlash(complete(HttpEntity(`text/html(UTF-8)`, Xhtml.toXhtml(
      <html><body><ul>
        <li><a href="/health">health</a></li>
        <li><a href="/docs">docs</a></li>
        <li><a href="/admin">admin</a></li>
      </ul></body></html>
    ))))

  lazy val fullRoute = Route.seal(adminRoute ~ baseRoute)
}

trait HttpsSupport {
  def keystoreFormat   = "PKCS12"
  def keystoreName     = "alleninstitute.org.p12"
  def keystorePassword = "ChangeMeNow".toCharArray // do not store passwords in code, read them from somewhere safe!

  def httpsContext = {
    import java.io.InputStream
    import java.security.{ SecureRandom, KeyStore }
    import javax.net.ssl.{ SSLContext, TrustManagerFactory, KeyManagerFactory }

    val ks: KeyStore = KeyStore.getInstance(keystoreFormat)
    val keystore: InputStream = getClass.getClassLoader.getResourceAsStream(keystoreName)
    require(keystore != null, "Keystore required!")
    ks.load(keystore, keystorePassword)

    val keyManagerFactory: KeyManagerFactory = KeyManagerFactory.getInstance("SunX509")
    keyManagerFactory.init(ks, keystorePassword)

    val tmf: TrustManagerFactory = TrustManagerFactory.getInstance("SunX509")
    tmf.init(ks)

    val sslContext: SSLContext = SSLContext.getInstance("TLS")
    sslContext.init(keyManagerFactory.getKeyManagers, tmf.getTrustManagers, new SecureRandom)
    val https: HttpsConnectionContext = ConnectionContext.https(sslContext)

    https
  }
}

abstract class WebServer(args: Array[String]) extends TemplateRoutes with HttpsSupport {
  val config = ConfigFactory.load

  val httpPort  = 9000
  val httpsPort = 9001

  implicit val system = ActorSystem("demo", config)
  implicit val materializer = ActorMaterializer()
  private implicit val executionContext = system.dispatcher

  val httpExt = Http()
  val httpBindingFuture  = httpExt.bindAndHandle(fullRoute, "0.0.0.0", httpPort)
  val httpsBindingFuture = httpExt.bindAndHandle(fullRoute, "0.0.0.0", httpsPort, httpsContext)
  bindingPromise.completeWith(Future.sequence(List(httpBindingFuture, httpsBindingFuture)))

  println("Server started")
}

object Main {
  def main(args: Array[String]) { new WebServer(args) {} }
}
