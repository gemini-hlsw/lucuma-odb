// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import clue.GraphQLOperation
import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import lucuma.itc.ItcVersions

object VersionsQuery extends GraphQLOperation[Unit] {

  type Data      = ItcVersions
  type Variables = Unit

  override val document: String =
    """
      query Versions {
        versions {
          serverVersion
          dataVersion
        }
      }
    """

  override val varEncoder: Encoder.AsObject[Unit] =
    Encoder.AsObject[Unit]

  override val dataDecoder: Decoder[ItcVersions] =
    (c: HCursor) => c.downField("versions").as[ItcVersions]

}
