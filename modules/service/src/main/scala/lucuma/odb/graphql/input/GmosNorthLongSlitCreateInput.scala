// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel._
import edu.gemini.grackle.Result
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.math.Wavelength
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*


final case class GmosNorthLongSlitCreateInput(
  grating:           GmosNorthGrating,
  filter:            Option[GmosNorthFilter],
  fpu:               GmosNorthFpu,
  centralWavelength: Wavelength
)

object GmosNorthLongSlitCreateInput {

  val Binding: Matcher[GmosNorthLongSlitCreateInput] =
    ObjectFieldsBinding.rmap {
      case List(
        GmosNorthGratingBinding.Option("grating", rGrating),
        GmosNorthFilterBinding.Option("filter", rFilter),
        GmosNorthFpuBinding.Option("fpu", rFpu),
        WavelengthInput.Binding.Option("centralWavelength", rCentralWavelength)
      ) => (rGrating, rFilter, rFpu, rCentralWavelength).parTupled.flatMap {
        case (Some(grating), filter, Some(fpu), Some(centralWavelength)) =>
          Result(GmosNorthLongSlitCreateInput(grating, filter, fpu, centralWavelength))
        case _                                                           =>
          Result.failure("grating, fpu, and centralWavelength are required when creating the GMOS North Long Slit observing mode.")
      }
    }

}

