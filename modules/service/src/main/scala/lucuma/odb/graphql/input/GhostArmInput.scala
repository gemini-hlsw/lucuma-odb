// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.graphql.binding.*


case class GhostArmInput(
  exposureTimeMode: Option[ExposureTimeMode],
  detector:         Option[GhostDetectorInput]
)

object GhostArmInput:

  val Binding: Matcher[GhostArmInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ExposureTimeModeInput.Binding.Option("exposureTimeMode", rEtm),
        GhostDetectorInput.Binding.Option("detector", rDetector)
      ) => (rEtm, rDetector).parMapN: (etm, det) =>
        GhostArmInput(etm, det)