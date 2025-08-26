// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.ArcType
import lucuma.core.math.Angular
import lucuma.core.math.Arc

object arc {

  extension [A](a: Arc[A]) def tag: ArcType =
    a match
      case Arc.Empty() => ArcType.Empty
      case Arc.Full() => ArcType.Full
      case Arc.Partial(_, _) => ArcType.Partial

  given [A: Decoder: Angular]: Decoder[Arc[A]] = hc =>
    hc.downField("type").as[ArcType].flatMap:
      case ArcType.Empty   => Right(Arc.Empty())
      case ArcType.Full    => Right(Arc.Full())
      case ArcType.Partial =>
        for 
          a <- hc.downField("start").as[A]
          b <- hc.downField("end").as[A]
        yield Arc.Partial(a, b)

  given [A: Encoder: Angular]: Encoder[Arc[A]] = a =>
    Json.obj(
      "type"   -> a.tag.asJson,
      "start" -> Arc.start.getOption(a).asJson,
      "end"   -> Arc.end.getOption(a).asJson,
    )

}

