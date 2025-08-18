// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.client.codec

import io.circe.*
import io.circe.generic.semiauto.*
import lucuma.core.model.OrcidProfile

trait OrcidProfileCodec:
  import userProfile.given

  given Encoder[OrcidProfile] = deriveEncoder
  given Decoder[OrcidProfile] = deriveDecoder

object orcidProfile extends OrcidProfileCodec