// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.NonEmptyList
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import grackle.Result
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

/**
 * Create and edit inputs for the GNIRS IFU observing mode.
 *
 * The IFU counterpart of [[GnirsLongSlitInput]]: same shared fields, but an IFU
 * FPU and telescope configs that are plain p/q offsets rather than along-slit
 * ones, with no derived default.
 */
object GnirsIfuInput:

  private val Data: Matcher[GnirsSpectroscopyInput.Edit] =
    ObjectFieldsBinding.rmap:
      case List(
        GnirsCentralWavelengthConfigInput.Binding.List.Option("centralWavelengths", rCentralWavelengths),
        GnirsFilterBinding.Option("filter", rFilter),
        GnirsFpuIfuBinding.Option("fpu", rFpu),
        TelescopeConfigInput.Binding.List.Option("telescopeConfigs", rTelescopeConfigs),
        GnirsCameraBinding.Option("camera", rCamera),
        GnirsGratingBinding.Nullable("grating", rGrating),
        GnirsPrismBinding.Nullable("prism", rPrism),
        GnirsDeckerBinding.Nullable("explicitDecker", rDecker),
        GnirsGratingBinding.Nullable("explicitGrating", rExplGrating),
        GnirsPrismBinding.Nullable("explicitPrism", rExplPrism),
        IntBinding.Nullable("explicitFocusMotorSteps", rFocus),
        GnirsReadModeBinding.Nullable("explicitReadMode", rReadMode),
        GnirsWellDepthBinding.Nullable("explicitWellDepth", rWellDepth),
        GnirsSpectroscopyInput.AcquisitionInput.Binding.Option("acquisition", rAcq),
        TelluricTypeBinding.Option("telluricType", rTelluricType)
      ) =>
        (rCentralWavelengths, rFilter, rFpu, rTelescopeConfigs, rCamera, rGrating, rPrism,
         rDecker, rExplGrating, rExplPrism,
         rFocus, rReadMode, rWellDepth, rAcq, rTelluricType).parTupled.flatMap:
          (centralWavelengths, filter, fpu, telescopeConfigs, camera, grating, prism,
           decker, explGrating, explPrism,
           focus, readMode, wellDepth, acq, telluricType) =>
            (centralWavelengths.traverse(GnirsSpectroscopyInput.resolveWavelengths),
             telescopeConfigs.traverse(resolveTelescopeConfigs)
            ).parMapN: (ws, tcs) =>
              GnirsSpectroscopyInput.Edit(
                ws, filter, fpu.map(GnirsFpu.Spectroscopy.Ifu(_)), camera, grating, prism,
                decker, explGrating, explPrism,
                focus, readMode, wellDepth, Nullable.Absent, tcs, acq, telluricType
              )

  // A supplied list replaces the stored one wholesale, so an empty list would leave
  // the observation with no offsets at all.
  private def resolveTelescopeConfigs(
    tcs: List[TelescopeConfig]
  ): Result[NonEmptyList[TelescopeConfig]] =
    Result.fromOption(
      NonEmptyList.fromList(tcs),
      Matcher.validationProblem("'telescopeConfigs' must not be empty")
    )

  object Create:
    val Binding: Matcher[GnirsSpectroscopyInput.Create] =
      Data.rmap:
        case edit => edit.toCreate

  object Edit:
    val Binding: Matcher[GnirsSpectroscopyInput.Edit] =
      Data
