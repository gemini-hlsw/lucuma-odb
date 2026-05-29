// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension

object coordinates:

  trait DecoderCoordinates:
    import rightascension.decoder.given
    import declination.decoder.given

    given Decoder[Coordinates] = hc =>
      for
        r <- hc.downField("ra").as[RightAscension]
        d <- hc.downField("dec").as[Declination]
      yield Coordinates(r, d)

  object decoder extends DecoderCoordinates

  trait InternalCodec extends DecoderCoordinates:

    protected def rightAscensionEncoder: Encoder[RightAscension]
    protected def declinationEncoder:    Encoder[Declination]

    given Encoder_Coordinates: Encoder[Coordinates] = cs =>
      Json.obj(
        "ra"  -> cs.ra.asJson(using rightAscensionEncoder),
        "dec" -> cs.dec.asJson(using declinationEncoder)
      )

  trait QueryCodec extends InternalCodec:
    override protected val rightAscensionEncoder: Encoder[RightAscension] =
      rightascension.query.Encoder_Right_Ascension

    override protected val declinationEncoder: Encoder[Declination] =
      declination.query.Encoder_Declination

  object query extends QueryCodec

  trait TransportCodec extends InternalCodec:
    override protected val rightAscensionEncoder: Encoder[RightAscension] =
      rightascension.transport.Encoder_Right_Ascension

    override protected val declinationEncoder: Encoder[Declination] =
      declination.transport.Encoder_Declination

  object transport extends TransportCodec