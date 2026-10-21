// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.syntax.all.*
import io.circe.Decoder
import lucuma.itc.Error

/**
 * A failed legacy ITC calculation. The legacy side reports the kind of error and its data as JSON,
 * so the message text is only for display and never needs parsing.
 */
enum LegacyItcError(val message: String):
  case SourceTooBright(halfWellTime: BigDecimal, override val message: String)
      extends LegacyItcError(message)
  case WavelengthOutOfRange                  extends LegacyItcError(Error.WavelengthAtOutOfRangeMessage)
  case General(override val message: String) extends LegacyItcError(message)

object LegacyItcError:
  given Decoder[LegacyItcError] = c =>
    c.downField("errorType")
      .as[String]
      .flatMap:
        case "SourceTooBright" =>
          (c.downField("halfWellTime").as[BigDecimal], c.downField("message").as[String])
            .mapN(SourceTooBright(_, _))
        case _                 =>
          c.downField("message").as[String].map(General(_))
