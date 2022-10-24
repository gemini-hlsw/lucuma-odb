// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.functor.*
import cats.syntax.parallel.*
import lucuma.odb.data.Nullable
import lucuma.odb.data.ObservingModeType
import lucuma.odb.graphql.binding.*

/*
gmosNorthLongSlit: GmosNorthLongSlitInput
gmosSouthLongSlit: GmosSouthLongSlitInput
*/


object ObservingModeInput {

  final case class Create(
    gmosNorthLongSlit: Option[GmosNorthLongSlitInput.Create]
  ) {

    def observingModeType: Option[ObservingModeType] =
      gmosNorthLongSlit.map(_.observingModeType)

  }

  final case class Edit(
    gmosNorthLongSlit: Option[GmosNorthLongSlitInput.Edit]
  ) {

    def observingModeType: Option[ObservingModeType] =
      gmosNorthLongSlit.map(_.observingModeType)

  }

  val CreateBinding: Matcher[Create] =
    ObjectFieldsBinding.rmap {
      case List(
        // TODO: when we add GMOS South, then we need to make the input at most one defined
        GmosNorthLongSlitInput.CreateBinding.Option("gmosNorthLongSlit", rGmosNorthLongSlit),
        ("gmosSouthLongSlit", _)
      ) =>
        rGmosNorthLongSlit.map(Create.apply)
    }

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap {
      case List(
        GmosNorthLongSlitInput.EditBinding.Option("gmosNorthLongSlit", rGmosNorthLongSlit),
        ("gmosSouthLongSlit", _)
      ) =>
        rGmosNorthLongSlit.map(Edit.apply)
    }

}