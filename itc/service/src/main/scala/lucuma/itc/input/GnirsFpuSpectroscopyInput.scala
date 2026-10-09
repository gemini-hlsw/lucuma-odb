// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.odb.graphql.binding.*

// A GNIRS spectroscopy FPU: exactly one of a long slit or an IFU (`@oneOf`).
object GnirsFpuSpectroscopyInput:

  val Binding: Matcher[GnirsFpu.Spectroscopy] =
    OneOfBinding(
      "slitWidth" -> GnirsFpuSlitBinding.map(GnirsFpu.Spectroscopy.Slit(_)),
      "ifu"       -> GnirsFpuIfuBinding.map(GnirsFpu.Spectroscopy.Ifu(_))
    )
