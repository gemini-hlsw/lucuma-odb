// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.option.*
import cats.syntax.parallel.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.binding.*

object GmosImagingFilterInput:

  case class Create[F](
    filter: F,
    exposureTimeMode: Option[ExposureTimeMode]
  )

  object Create:

    val NorthBinding: Matcher[Create[GmosNorthFilter]] =
      ObjectFieldsBinding.rmap:
        case List(
          GmosNorthFilterBinding("filter", rFilter),
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rEtm)
        ) =>
          (rFilter, rEtm).parMapN(apply)

    val SouthBinding: Matcher[Create[GmosSouthFilter]] =
      ObjectFieldsBinding.rmap:
        case List(
          GmosSouthFilterBinding("filter", rFilter),
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rEtm)
        ) =>
          (rFilter, rEtm).parMapN(apply)

  case class Edit[F](
    filter: Option[F],
    exposureTimeMode: Option[ExposureTimeMode]
  ):
    def toCreate: Result[Create[F]] =
      filter.fold(OdbError.InvalidArgument("A filter must be specified".some).asFailure): f =>
        Create(f, exposureTimeMode).success

  object Edit:

    val NorthBinding: Matcher[Edit[GmosNorthFilter]] =
      ObjectFieldsBinding.rmap:
        case List(
          GmosNorthFilterBinding.Option("filter", rFilter),
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rEtm)
        ) =>
          (rFilter, rEtm).parMapN(apply)

    val SouthBinding: Matcher[Edit[GmosSouthFilter]] =
      ObjectFieldsBinding.rmap:
        case List(
          GmosSouthFilterBinding.Option("filter", rFilter),
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rEtm)
        ) =>
          (rFilter, rEtm).parMapN(apply)