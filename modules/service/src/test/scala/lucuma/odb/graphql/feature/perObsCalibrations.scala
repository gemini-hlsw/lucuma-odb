// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.feature

import cats.Eq
import cats.derived.*
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.math.Wavelength
import lucuma.core.model.CloudExtinction
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference.Description
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.string.*
import lucuma.odb.data.EditType
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.TestUsers
import lucuma.odb.graphql.input.ProgramPropertiesInput
import lucuma.odb.graphql.query.ExecutionQuerySetupOperations
import lucuma.odb.graphql.subscription.SubscriptionUtils
import lucuma.odb.json.wavelength.decoder.given
import lucuma.odb.service.CalibrationsService
import lucuma.odb.service.Services
import lucuma.odb.service.SpecPhotoCalibrations
import lucuma.odb.service.TwilightCalibrations

import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter

class perObsCalibrations extends OdbSuite with SubscriptionUtils with ExecutionQuerySetupOperations {
  val pi       = TestUsers.Standard.pi(1, 101)
  val service  = TestUsers.service(3)

  val DefaultSnAt: Wavelength = Wavelength.fromIntNanometers(510).get

  override val validUsers = List(pi, service)
}
