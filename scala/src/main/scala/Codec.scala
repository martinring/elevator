import io.circe._
import io.circe.generic.extras.semiauto._
import io.circe.generic.extras.Configuration

object Codec {
  implicit val customConfig: Configuration =
    Configuration
      .default
      .withDefaults
      .withDiscriminator("tag")

  implicit val directionEncoder: Encoder[Direction] = Encoder.encodeString.contramap {
     case Direction.Up => "Up"
     case Direction.Down => "Down"
  }

  implicit val directionDecoder: Decoder[Direction] = Decoder.decodeString.emap {
     case "Up" => Right(Direction.Up)
     case "Down" => Right(Direction.Down)
     case other => Left(s"expected 'Up' or 'Down' but found '$other'")
  }

  implicit val eventDecoder = deriveDecoder[Event]
  implicit val commandEncoder = deriveEncoder[Command]
}
