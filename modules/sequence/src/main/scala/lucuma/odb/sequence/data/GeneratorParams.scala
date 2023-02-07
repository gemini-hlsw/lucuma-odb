// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data

import cats.data.NonEmptyList
import lucuma.core.model.Target
import lucuma.itc.client.SpectroscopyModeInput

enum GeneratorParams {

  case GmosNorthLongSlit(
    itc:    NonEmptyList[(Target.Id, SpectroscopyModeInput)],
    config: gmos.longslit.Config.GmosNorth
  )

  case GmosSouthLongSlit(
    itc:    NonEmptyList[(Target.Id, SpectroscopyModeInput)],
    config: gmos.longslit.Config.GmosSouth
  )

}
