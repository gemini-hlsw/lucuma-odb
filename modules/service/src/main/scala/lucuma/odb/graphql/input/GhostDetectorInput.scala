// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostReadMode
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*


case class GhostDetectorInput(
  explicitBinning:  Nullable[GhostBinning],
  explicitReadMode: Nullable[GhostReadMode]
)

object GhostDetectorInput:

  val Binding: Matcher[GhostDetectorInput] =
    ObjectFieldsBinding.rmap:
      case List(
        GhostBinningBinding.Nullable("explicitBinning", rBin),
        GhostReadModeBinding.Nullable("explicitReadMode", rReadMode)
      ) => (rBin, rReadMode).parMapN: (bin, readMode) =>
        GhostDetectorInput(bin, readMode)