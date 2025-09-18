// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*

trait InstrumentModesInput

object InstrumentModesInput:

  val Binding: Matcher[InstrumentModesInput] =
    ObjectFieldsBinding.rmap:
      case List(
            GmosNSpectroscopyInput.binding.Option("gmosNSpectroscopy", gmosNSpectroscopy),
            GmosSSpectroscopyInput.binding.Option("gmosSSpectroscopy", gmosSSpectroscopy),
            GmosNImagingInput.binding.Option("gmosNImaging", gmosNImaging),
            GmosSImagingInput.binding.Option("gmosSImaging", gmosSImaging),
            Flamingos2SpectroscopyInput.binding
              .Option("flamingos2Spectroscopy", flamingos2Spectroscopy),
            Flamingos2ImagingInput.binding.Option("flamingos2Imaging", flamingos2Imaging)
          ) =>
        (gmosNSpectroscopy,
         gmosSSpectroscopy,
         gmosNImaging,
         gmosSImaging,
         flamingos2Spectroscopy,
         flamingos2Imaging
        ).parTupled
          .flatMap {
            case (gmosNSpectroscopy,
                  gmosSSpectroscopy,
                  gmosNImaging,
                  gmosSImaging,
                  flamingos2Spectroscopy,
                  flamingos2Imaging
                )      =>
              oneOrFail(
                gmosNSpectroscopy      -> "gmosNSpectroscopy",
                gmosSSpectroscopy      -> "gmosSSpectroscopy",
                gmosNImaging           -> "gmosNImaging",
                gmosSImaging           -> "gmosSImaging",
                flamingos2Spectroscopy -> "flamingos2Spectroscopy",
                flamingos2Imaging      -> "flamingos2Imaging"
              )
          }
