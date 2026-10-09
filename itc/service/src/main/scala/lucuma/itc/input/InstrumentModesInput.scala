// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import lucuma.core.enums.PortDisposition
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.graphql.binding.*

trait InstrumentModesInput:
  def port: PortDisposition

  // This will return "a" exposure time mode. For most instruments this is "the" exposure time mode,
  // but for some (e.g. Ghost) there are multiple. In the case of ghost, the ITC will ignore the
  // "top level" exposure time mode and use the one in the spectroscopy mode, but we still need
  // to have one to satisfy the legacy interface.
  def exposureTimeMode: ExposureTimeMode

object InstrumentModesInput:

  val Binding: Matcher[InstrumentModesInput] =
    OneOfBinding(
      "gmosNSpectroscopy"      -> GmosNSpectroscopyInput.binding,
      "gmosSSpectroscopy"      -> GmosSSpectroscopyInput.binding,
      "gmosNImaging"           -> GmosNImagingInput.binding,
      "gmosSImaging"           -> GmosSImagingInput.binding,
      "flamingos2Spectroscopy" -> Flamingos2SpectroscopyInput.binding,
      "flamingos2Imaging"      -> Flamingos2ImagingInput.binding,
      "igrins2Spectroscopy"    -> Igrins2SpectroscopyInput.binding,
      "ghostSpectroscopy"      -> GhostSpectroscopyInput.Binding,
      "gnirsSpectroscopy"      -> GnirsSpectroscopyInput.binding,
      "gnirsImaging"           -> GnirsImagingInput.binding
    )
