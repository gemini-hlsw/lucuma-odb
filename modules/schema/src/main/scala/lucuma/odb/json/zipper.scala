// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.Eq
import cats.syntax.eq.*
import cats.syntax.option.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.data.Zipper

// Where a Zipper is represented as a type with `selected: A` and `all: [A!]!`
// fields.

trait ZipperCodec {

  given [A: Decoder : Eq]: Decoder[Zipper[A]] =
    Decoder.instance { c =>
      for
        list   <- c.downField("all").as[List[A]]
        idx    <- c.downField("index").as[Int] orElse
                    c.downField("selected").as[A].flatMap(a =>
                      list
                        .indexWhere(_ === a)
                        .some
                        .filter(_ >= 0)
                        .toRight(DecodingFailure("The `selected` item is not a member of the `all` list of items.", c.history))
                      )
        zipper <- Zipper.fromList(list)
                    .toRight(DecodingFailure("The `all` list of items must be non-empty.", c.history))
                    .flatMap(
                      _.focusIndex(idx)
                        .toRight(DecodingFailure("The `index` value is out of bounds.", c.history))
                    )
      yield zipper
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
