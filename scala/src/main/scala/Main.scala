import java.io.File

import akka.actor._
import akka.http.scaladsl._
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.ws.{TextMessage, UpgradeToWebSocket, Message => WSMessage}
import akka.http.scaladsl.server.Directives._
import akka.stream._
import akka.util.Timeout
import akka.pattern.ask
import akka.stream.scaladsl._

import scala.io.StdIn
import scala.concurrent.duration._
import scala.concurrent.Future
import io.circe.parser._
import io.circe.syntax._
import Codec._
import akka.http.scaladsl.server.directives.ContentTypeResolver

object Main extends App {

  implicit val system = ActorSystem("elevator-controller")
  implicit val materializer = ActorMaterializer()
  implicit val dispatcher = system.dispatcher
  implicit val timeout = Timeout(5 seconds)

  def socket: Flow[WSMessage,WSMessage,Any] = {
    val spreadSheet = system.actorOf(Props[Controller])
    val spreadSheetSink = Sink.actorRef[Event](spreadSheet,PoisonPill)

    val in = Flow[WSMessage].collect {
                                       case TextMessage.Strict("pong") => None
                                       case TextMessage.Strict(text) =>
                                         decode[Event](text).toOption
                                     }
      .collect { case Some(msg) => msg }
      .to(spreadSheetSink)

    val out = Source.actorRef[Command](1, OverflowStrategy.fail)
      .mapMaterializedValue(spreadSheet ! _)

    Flow.fromSinkAndSource(in,out)
      .map(msg => TextMessage.Strict(msg.asJson.noSpaces))
      .keepAlive(20 seconds, () => TextMessage.Strict("ping"))
      .idleTimeout(30 seconds)
  }

  val requestHandler: HttpRequest => HttpResponse = {
    case req @ HttpRequest(HttpMethods.GET, Uri.Path(path), _, _, _) =>
      req.header[UpgradeToWebSocket] match {
        case Some(upgrade) => upgrade.handleMessages(socket)
        case None          =>
          val file = new File(path)
          if (file.exists())
            HttpResponse(entity = scala.io.Source.fromFile(file).mkString)
          else HttpResponse(404, entity = "Not found!")
      }
    case r: HttpRequest =>
      r.discardEntityBytes() // important to drain incoming HTTP Entity stream
      HttpResponse(404, entity = "Unknown resource!")
  }

  val route =
    extractUpgradeToWebSocket { _ =>
      handleWebSocketMessages(socket)
    } ~
    getFromDirectory("../gui") ~
    pathSingleSlash {
      getFromFile("../gui/elevator.svg")
    }

  val binding = Http().bindAndHandle(route,"localhost",3000)

  StdIn.readLine()
  binding.flatMap(_.unbind()).onComplete(_ => system.terminate())
}
