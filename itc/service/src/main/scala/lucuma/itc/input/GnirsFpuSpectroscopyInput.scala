// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*

// A GNIRS spectroscopy FPU: exactly one of a long slit or an IFU (`@oneOf`).
object GnirsFpuSpectroscopyInput:

  val Binding: Matcher[GnirsFpu.Spectroscopy] =
    ObjectFieldsBinding.rmap:
      case List(
            GnirsFpuSlitBinding.Option("slitWidth", slitWidth),
            GnirsFpuIfuBinding.Option("ifu", ifu)
          ) =>
        (slitWidth, ifu).parTupled.flatMap: (slitWidth, ifu) =>
          oneOrFail(
            slitWidth.map(GnirsFpu.Spectroscopy.Slit(_)) -> "slitWidth",
            ifu.map(GnirsFpu.Spectroscopy.Ifu(_))        -> "ifu"
          )
