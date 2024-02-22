// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import io.circe.Codec
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.HCursor
import io.circe.Json
import io.circe.syntax._

trait NumericCodec {

  given Codec[BigDecimal] with {
    def apply(d: BigDecimal): Json =
      d.bigDecimal.toPlainString.asJson

    def apply(c: HCursor): Decoder.Result[BigDecimal] =
      Decoder[String].apply(c).flatMap { s =>
        Either.catchNonFatal(BigDecimal(s)).leftMap { _ =>
          DecodingFailure(s"Could not decode $s as a decimal value", c.history)
        }
      }
  }

  given Codec[PosBigDecimal] =
    Codec[BigDecimal].iemap(PosBigDecimal.from)(_.value)

}

object numeric extends NumericCodec
