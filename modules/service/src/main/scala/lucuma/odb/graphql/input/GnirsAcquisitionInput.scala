// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import cats.syntax.unorderedFoldable.*
import eu.timepit.refined.types.numeric.PosInt
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.GnirsAcquisitionType
import lucuma.core.enums.GnirsFilter
import lucuma.core.math.Offset
import lucuma.core.model.ExposureTimeMode
import lucuma.core.syntax.string.*
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.binding.*

/**
 * Acquisition customization input shared by the GNIRS observing modes. The nullable
 * fields may be cleared back to their automatic values.
 */
case class GnirsAcquisitionInput(
  explicitFilter:           Nullable[GnirsFilter],
  explicitAcqType:          Nullable[GnirsAcquisitionType],
  coadds:                   Option[PosInt],
  skyOffset:                Option[Offset],
  explicitExposureTimeMode: Nullable[ExposureTimeMode]
)

object GnirsAcquisitionInput:

  // Signal-to-noise exposure time mode does not support coadds. When the ETM is set to
  // signal-to-noise, force coadds to 1 so a previously-set value doesn't linger. Clearing
  // the ETM back to automatic counts as signal-to-noise: a derived acquisition ETM is
  // always a signal-to-noise mode.
  private def coaddsForEtm(
    etm:    Nullable[ExposureTimeMode],
    coadds: Option[PosInt]
  ): Option[PosInt] =
    etm match
      case Nullable.NonNull(ExposureTimeMode.SignalToNoiseMode(_, _)) => PosInt.from(1).toOption
      case Nullable.Null                                             => PosInt.from(1).toOption
      case _                                                         => coadds

  // A sky offset is valid exactly when the explicit acquisition type is FAINT:
  // FAINT requires one, and any other explicit type (or clearing to AUTO) forbids
  // it. This must hold within a single input; the DB also enforces it on the row.
  private def validateSkyOffset(a: GnirsAcquisitionInput): Result[GnirsAcquisitionInput] =
    val explicitlyFaint = a.explicitAcqType match
      case Nullable.NonNull(GnirsAcquisitionType.Faint) => true
      case _                                            => false
    (a.skyOffset.isDefined, explicitlyFaint) match
      case (true, false) =>
        OdbError.InvalidArgument("'skyOffset' is only valid when 'explicitAcquisitionType' is FAINT.".some).asFailure
      case (false, true) =>
        OdbError.InvalidArgument("'explicitAcquisitionType' FAINT requires a 'skyOffset'.".some).asFailure
      case _             =>
        Result(a)

  val Binding: Matcher[GnirsAcquisitionInput] =
    ObjectFieldsBinding.rmap:
      case List(
        GnirsFilterBinding.Nullable("explicitFilter", rFilter),
        GnirsAcquisitionTypeBinding.Nullable("explicitAcquisitionType", rAcqType),
        PosIntBinding.Option("coadds", rCoadds),
        OffsetInput.Binding.Option("skyOffset", rSkyOffset),
        ExposureTimeModeInput.Binding.Nullable("explicitExposureTimeMode", rEtm)
      ) =>
        (
          rFilter.flatMap: n =>
            n.traverse: f =>
              if GnirsFilter.AcquisitionFilters.contains_(f) then f.success
              else OdbError.InvalidArgument(s"'explicitFilter' must contain one of: ${GnirsFilter.AcquisitionFilters.map(_.tag.toScreamingSnakeCase).mkString_(", ")}".some).asFailure
          ,
          rAcqType, rCoadds, rSkyOffset, rEtm
        ).parMapN(GnirsAcquisitionInput.apply)
         .map(a => a.copy(coadds = coaddsForEtm(a.explicitExposureTimeMode, a.coadds)))
         .flatMap(validateSkyOffset)
