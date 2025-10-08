// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.enums.TargetDisposition
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.RightAscension
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.core.model.UnnormalizedSED
import lucuma.core.model.User
import lucuma.odb.data.Existence
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.input.SiderealInput
import lucuma.odb.graphql.input.TargetEnvironmentInput
import lucuma.odb.graphql.input.TargetPropertiesInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.service.Services
import lucuma.odb.util.Codecs.*

import scala.collection.immutable.SortedMap

// Methods, etc. for making calls to the services via `withServices`
trait ServiceOperations { this: OdbSuite =>
  def defaultCreateTargetInput(name: String): TargetPropertiesInput.Create =
    TargetPropertiesInput.Create(
      name         = NonEmptyString.unsafeFrom(name),
      subtypeInfo  = SiderealInput.Create(
        ra    = RightAscension.Zero,
        dec   = Declination.Zero,
        epoch = Epoch.J2000,
        properMotion = None,
        radialVelocity = None,
        parallax = None,
        catalogInfo = None
      ),
      sourceProfile = SourceProfile.Point(
        SpectralDefinition.BandNormalized(
          sed         = UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.B5III).some,
          brightnesses = SortedMap.empty
        )
      ),
      existence    = Existence.Present
    )

  def defaultTargetEnvironmentInput4UpdateBlindoffset(name: String): TargetEnvironmentInput.Edit =
    TargetEnvironmentInput.Edit(
      explicitBase = Nullable.Absent,
      asterism = Nullable.Absent,
      none,
      blindOffsetTarget = Nullable.NonNull(defaultCreateTargetInput(name)),
      explicitBlindOffset = false
    )

  def createTargetViaServiceAs(
    user: User,
    programId: Program.Id,
    disposition: TargetDisposition,
    role: Option[CalibrationRole] = None,
    input: TargetPropertiesInput.Create = defaultCreateTargetInput("My Target")
  ): IO[Target.Id] = 
    val checkedInput = Services.asSuperUser(AccessControl.unchecked(input, programId, program_id))

    withServices(user): services =>
      services.session.transaction.use: xa =>
        (services.targetService.createTarget(checkedInput, disposition, role)(using xa))
          .flatMap:
            case Result.Success(tid) => tid.pure
            case _ => IO.raiseError[Target.Id](new Exception("createTargetViaServiceAs failed"))
  
  def updateBlindOffsetViaServiceAs(
    user: User,
    programId: Program.Id,
    observationId: Observation.Id,
    name: String // must be non-empty
  ): IO[Unit] =
    val input = defaultTargetEnvironmentInput4UpdateBlindoffset(name)
    withServices(user): services =>
      Services.asSuperUser:
        services.session.transaction.use: xa =>
          (services.blindOffsetsService.updateBlindOffset(programId, observationId, input.some)(using xa))
            .flatMap:
              case Result.Success(_) => ().pure
              case _ => IO.raiseError(new Exception("updateBlindOffsetViaServiceAs failed"))
}
