// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import grackle.Result
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.Matcher

/**
 * A science exposure time mode input is nullable so that, on a telluric calibration, null
 * can clear the user's override and return the mode derived from the science observation.
 */
object TelluricExposureTimeModeEdit:

  val NullOnlyOnTelluric: String =
    "A null 'exposureTimeMode' is only valid when editing a telluric calibration."

  /** A create has nothing derived to return to, so null is rejected. */
  def forCreate(etm: Nullable[ExposureTimeMode]): Result[Option[ExposureTimeMode]] =
    etm.fold(Matcher.validationFailure(NullOnlyOnTelluric), Result(None), e => Result(Some(e)))
