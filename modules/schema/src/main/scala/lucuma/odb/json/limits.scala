// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Decoder
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.CallCoordinatesLimits
import lucuma.core.model.SiteCoordinatesLimits
import lucuma.odb.json.declination.decoder.given
import lucuma.odb.json.rightascension.decoder.given

object limits {

  trait LimitDecoders {

    given Decoder[SiteCoordinatesLimits] = c =>
      for {
        raStart  <- c.downField("raStart").as[RightAscension]
        raEnd    <- c.downField("raEnd").as[RightAscension]
        decStart <- c.downField("decStart").as[Declination]
        decEnd   <- c.downField("decEnd").as[Declination]
      } yield SiteCoordinatesLimits(raStart, raEnd, decStart, decEnd)

    given Decoder[CallCoordinatesLimits] = c =>
      for {
        north <- c.downField("north").as[SiteCoordinatesLimits]
        south <- c.downField("south").as[SiteCoordinatesLimits]
      } yield CallCoordinatesLimits(north, south)

  }

  object decoder extends LimitDecoders

}
