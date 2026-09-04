// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import cats.syntax.traverse.*
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.odb.graphql.binding.*

/**
 * Create and edit inputs for the GNIRS long slit observing mode.
 *
 * Long slit and IFU share a table and a service, so both parse into the same
 * `GnirsSpectroscopyInput` values; the mode follows from the FPU. What differs
 * is the FPU type and the shape of the telescope configs, which is why they are
 * separate inputs.
 */
object GnirsLongSlitInput:

  private val Data: Matcher[GnirsSpectroscopyInput.Edit] =
    ObjectFieldsBinding.rmap:
      case List(
        GnirsCentralWavelengthConfigInput.Binding.List.Option("centralWavelengths", rCentralWavelengths),
        GnirsFilterBinding.Option("filter", rFilter),
        GnirsFpuSlitBinding.Option("fpu", rFpu),
        SlitTelescopeConfigsInput.Binding.Nullable("explicitTelescopeConfigs", rTelescopeConfigs),
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
            centralWavelengths.traverse(GnirsSpectroscopyInput.resolveWavelengths).map: ws =>
              GnirsSpectroscopyInput.Edit(
                ws, filter, fpu.map(GnirsFpu.Spectroscopy.Slit(_)), camera, grating, prism,
                decker, explGrating, explPrism,
                focus, readMode, wellDepth, telescopeConfigs, None, acq, telluricType
              )

  object Create:
    val Binding: Matcher[GnirsSpectroscopyInput.Create] =
      Data.rmap:
        case edit => edit.toCreate

  object Edit:
    val Binding: Matcher[GnirsSpectroscopyInput.Edit] =
      Data
