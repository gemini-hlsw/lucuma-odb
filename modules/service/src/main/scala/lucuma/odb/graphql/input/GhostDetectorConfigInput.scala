// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostReadMode
import lucuma.core.model.ExposureTimeMode.TimeAndCountMode
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*


case class GhostDetectorConfigInput(
  timeAndCount:     Option[TimeAndCountMode],
  explicitBinning:  Nullable[GhostBinning],
  explicitReadMode: Nullable[GhostReadMode]
)

object GhostDetectorConfigInput:

  val Binding: Matcher[GhostDetectorConfigInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ExposureTimeModeInput.TimeAndCount.Binding.Option("timeAndCount", rEtm),
        GhostBinningBinding.Nullable("explicitBinning", rBin),
        GhostReadModeBinding.Nullable("explicitReadMode", rReadMode)
      ) => (rEtm, rBin, rReadMode).parMapN: (etm, bin, readMode) =>
        GhostDetectorConfigInput(etm, bin, readMode)