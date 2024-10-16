// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.functor.*
import cats.syntax.parallel.*
import lucuma.core.enums.ObservingModeType
import lucuma.odb.graphql.binding.*

object ObservingModeInput {

  final case class Create(
    gmosNorthLongSlit: Option[GmosLongSlitInput.Create.North],
    gmosSouthLongSlit: Option[GmosLongSlitInput.Create.South]
  ) {

    def observingModeType: Option[ObservingModeType] =
      gmosNorthLongSlit.map(_.observingModeType)
        .orElse(gmosSouthLongSlit.map(_.observingModeType))

  }

  object Create {

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          GmosLongSlitInput.Create.North.Binding.Option("gmosNorthLongSlit", rGmosNorthLongSlit),
          GmosLongSlitInput.Create.South.Binding.Option("gmosSouthLongSlit", rGmosSouthLongSlit)
        ) =>
          (rGmosNorthLongSlit, rGmosSouthLongSlit).parTupled.flatMap {
            case (gmosNorthLongSlit, gmosSouthLongSlit) =>
              oneOrFail(
                gmosNorthLongSlit -> "gmosNorthLongSlit",
                gmosSouthLongSlit -> "gmosSouthLongSlit"
              ).as(Create(gmosNorthLongSlit, gmosSouthLongSlit))

          }
      }

  }

  final case class Edit(
    gmosNorthLongSlit: Option[GmosLongSlitInput.Edit.North],
    gmosSouthLongSlit: Option[GmosLongSlitInput.Edit.South]
  ) {

    def observingModeType: Option[ObservingModeType] =
      gmosNorthLongSlit.map(_.observingModeType)
        .orElse(gmosSouthLongSlit.map(_.observingModeType))

  }

  object Edit {

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          GmosLongSlitInput.Edit.North.Binding.Option("gmosNorthLongSlit", rGmosNorthLongSlit),
          GmosLongSlitInput.Edit.South.Binding.Option("gmosSouthLongSlit", rGmosSouthLongSlit)
        ) =>
          (rGmosNorthLongSlit, rGmosSouthLongSlit).parTupled.flatMap {
            case (gmosNorthLongSlit, gmosSouthLongSlit) =>
              oneOrFail(
                gmosNorthLongSlit -> "gmosNorthLongSlit",
                gmosSouthLongSlit -> "gmosSouthLongSlit"
              ).as(Edit(gmosNorthLongSlit, gmosSouthLongSlit))
          }
      }

  }
}
