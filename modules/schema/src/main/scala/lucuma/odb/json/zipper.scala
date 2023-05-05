// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.eq.*
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.data.Zipper

// Where a Zipper is represented as a type with `selected: A` and `all: [A!]!`
// fields.

trait ZipperCodec {

  given [A: Decoder : Eq]: Decoder[Zipper[A]] =
    Decoder.instance { c =>
      for {
        s  <- c.downField("selected").as[A]
        i  <- c.downField("index").as[NonNegInt]
        as <- c.downField("all").as[List[A]]
        z  <- NonEmptyList
                .fromList(as)
                .map(Zipper.fromNel)
                .flatMap(_.focusIndex(i.value))
                .filter(_.focus === s)
                .toRight(DecodingFailure("The `selected` item is not a member of the `all` list of items.", c.history))
      } yield z
    }

  given [A: Encoder]: Encoder[Zipper[A]] =
    Encoder.instance { (a: Zipper[A]) =>
      Json.obj(
        "selected" -> a.focus.asJson,
        "index"    -> a.indexOfFocus.asJson,
        "all"      -> a.toList.asJson
      )
    }

}

object zipper extends ZipperCodec