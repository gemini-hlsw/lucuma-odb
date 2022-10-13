// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel._
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.math.Wavelength
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.enumeratedBinding

/*
# The grating field must be either specified or skipped altogether.  It cannot be unset with a null value.
grating: GmosNorthGrating

# The filter field may be unset by assigning a null value, or ignored by skipping it altogether
filter: GmosNorthFilter

# The fpu field must be either specified or skipped altogether.  It cannot be unset with a null value.
fpu: GmosNorthBuiltinFpu

# The centralWavelength field must be either specified or skipped altogether.  It cannot be unset with a null value.
centralWavelength: WavelengthInput

# The explicitXBin field may be unset by assigning a null value, or ignored by skipping it altogether
explicitXBin: GmosXBinning

# The explicitYBin field may be unset by assigning a null value, or ignored by skipping it altogether
explicitYBin: GmosYBinning

# The explicitAmpReadMode field may be unset by assigning a null value, or ignored by skipping it altogether
explicitAmpReadMode: GmosAmpReadMode

# The explicitAmpGain field may be unset by assigning a null value, or ignored by skipping it altogether
explicitAmpGain: GmosAmpGain

# The explicitRoi field may be unset by assigning a null value, or ignored by skipping it altogether
explicitRoi: GmosRoi

# The explicitWavelengthDithersNm field may be unset by assigning a null value, or ignored by skipping it altogether
explicitWavelengthDithersNm: [BigDecimal!]

# The explicitSpatialOffsets field may be unset by assigning a null value, or ignored by skipping it altogether
explicitSpatialOffsets: [OffsetComponentInput!]
*/

final case class GmosNorthLongSlitInput(
  grating:           Option[GmosNorthGrating],
  filter:            Nullable[GmosNorthFilter],
  fpu:               Option[GmosNorthFpu],
  centralWavelength: Option[Wavelength]
)

object GmosNorthLongSlitInput {

  val GmosNorthGratingBinding: Matcher[GmosNorthGrating] =
    enumeratedBinding[GmosNorthGrating]

  val GmosNorthFilterBinding: Matcher[GmosNorthFilter] =
    enumeratedBinding[GmosNorthFilter]

  val GmosNorthFpuBinding: Matcher[GmosNorthFpu] =
    enumeratedBinding[GmosNorthFpu]

  val Binding: Matcher[GmosNorthLongSlitInput] =
    ObjectFieldsBinding.rmap {
      case List(
        GmosNorthGratingBinding.Option("grating", rGrating),
        GmosNorthFilterBinding.Nullable("filter", rFilter),
        GmosNorthFpuBinding.Option("fpu", rFpu),
        WavelengthInput.Binding.Option("centralWavelength", rCentralWavelength)
      ) =>
        (rGrating, rFilter, rFpu, rCentralWavelength).parMapN(
          GmosNorthLongSlitInput.apply
        )
    }

}
