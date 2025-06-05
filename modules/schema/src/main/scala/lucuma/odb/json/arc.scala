// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Angular
import lucuma.core.math.Arc
import lucuma.core.math.Arc.Empty
import lucuma.core.math.Arc.Full
import lucuma.core.math.Arc.Partial

object arc {

  // Temporary -- this will probably need a public tag type,
  // depending on how things look in the schema.
  extension [A](a: Arc[A]) private def tag: String =
    a match
      case Empty() => "EMPTY"
      case Full() => "FULL"
      case Partial(_, _) => "PARTIAL"    

  given [A: Decoder: Angular]: Decoder[Arc[A]] = hc =>
    hc.downField("tag").as[String].flatMap:
      case "EMPTY"   => Right(Arc.Empty())
      case "FULL"    => Right(Arc.Full())
      case "PARTIAL" =>
        for 
          a <- hc.downField("start").as[A]
          b <- hc.downField("end").as[A]
        yield Arc.Partial(a, b)

  given [A: Encoder: Angular]: Encoder[Arc[A]] = a =>
    Json.obj(
      "tag"   -> a.tag.asJson,
      "start" -> Arc.start.getOption(a).asJson,
      "end"   -> Arc.end.getOption(a).asJson,
    )

}

