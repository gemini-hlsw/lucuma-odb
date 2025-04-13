// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.foldable.*
import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.F2Decker
import lucuma.core.enums.F2Disperser
import lucuma.core.enums.F2Filter
import lucuma.core.enums.F2Fpu
import lucuma.core.enums.F2ReadMode
import lucuma.core.enums.F2ReadoutMode
import lucuma.core.enums.F2Reads
import lucuma.core.enums.ObservingModeType
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*
import lucuma.odb.sequence.f2.longslit.Config

object F2LongSlitInput {

  case class Create(
    disperser: F2Disperser,
    filter: Option[F2Filter],
    fpu: F2Fpu,
    explicitReadMode: Option[F2ReadMode]       = None,
    explicitReads: Option[F2Reads]             = None,
    explicitDecker: Option[F2Decker]           = None,
    explicitReadoutMode: Option[F2ReadoutMode] = None
  ) {
    def observingModeType: ObservingModeType =
      ObservingModeType.Flamingos2LongSlit

    /**
      * Creates a F2 long slit observing mode based on input parameters.
      */
    def toObservingMode: Config =
      Config(
        disperser,
        filter,
        fpu,
        explicitReadMode,
        explicitReads,
        explicitDecker,
        explicitReadoutMode
      )

  }

  object Create {
    private val F2Data: Matcher[(
      F2Disperser,
      Option[F2Filter],
      F2Fpu,
      Option[F2ReadMode],
      Option[F2Decker],
      Option[F2ReadoutMode],
      Option[F2Reads],
    )] =
      ObjectFieldsBinding.rmap {
        case List(
          F2DisperserBinding("disperser", rDisperser),
          F2FilterBinding.Option("filter", rFilter),
          F2FpuBinding("fpu", rFpu),
          F2ReadModeBinding.Option("explicitReadMode", rReadMode),
          F2ReadsBinding.Option("explicitReads", rReads),
          F2DeckerBinding.Option("explicitDecker", rDecker),
          F2ReadoutModeBinding.Option("explicitReadoutMode", rReadoutMode)
        ) => (
          rDisperser,
          rFilter,
          rFpu,
          rReadMode,
          rDecker,
          rReadoutMode,
          rReads
        ).parTupled
      }

    val Binding: Matcher[Create] =
      F2Data.rmap {
        case (
          disperser,
          filter,
          fpu,
          explicitReadMode,
          explicitDecker,
          explicitReadoutMode,
          explicitReads
      ) =>
        Result(Create(
          disperser,
          filter,
          fpu,
          explicitReadMode,
          explicitReads,
          explicitDecker,
          explicitReadoutMode
        ))
      }

  }

  case class Edit(
    grating: Option[F2Disperser],
    filter: Nullable[F2Filter],
    fpu: Option[F2Fpu],
    explicitReadMode: Nullable[F2ReadMode],
    explicitReads: Nullable[F2Reads],
    explicitDecker: Nullable[F2Decker],
    explicitReadoutMode: Nullable[F2ReadoutMode]
  ) {

    val observingModeType: ObservingModeType =
      ObservingModeType.Flamingos2LongSlit

    private def required[A](oa: Option[A], itemName: String): Result[A] =
      Result.fromOption(
        oa,
        Matcher.validationProblem(s"A $itemName is required in order to create a Flamingos 2 Long Slit observing mode.")
      )

    val toCreate: Result[Create] =
      for {
        g <- required(grating, "grating")
        u <- required(fpu, "fpu")
      } yield Create(
        g,
        filter.toOption,
        u,
        explicitReadMode.toOption
      )
  }

  object Edit {

    private val F2EditData: Matcher[(
      Option[F2Disperser],
      Nullable[F2Filter],
      Option[F2Fpu],
      Nullable[F2ReadMode],
      Nullable[F2Reads],
      Nullable[F2Decker],
      Nullable[F2ReadoutMode]
    )] =
      ObjectFieldsBinding.rmap {
        case List(
          F2DisperserBinding.Option("disperser", rDisperser),
          F2FilterBinding.Nullable("filter", rFilter),
          F2FpuBinding.Option("fpu", rFpu),
          F2ReadModeBinding.Nullable("explicitReadMode", rReadMode),
          F2ReadsBinding.Nullable("expliciReads", rReads),
          F2DeckerBinding.Nullable("explicitDecker", rDecker),
          F2ReadoutModeBinding.Nullable("explicitReadoutMode", rReadoutMode)
        ) => (
          rDisperser,
          rFilter,
          rFpu,
          rReadMode,
          rReads,
          rDecker,
          rReadoutMode
        ).parTupled
      }

    val Binding: Matcher[Edit] =
      F2EditData.rmap {
        case (
          grating,
          filter,
          fpu,
          explicitReadMode,
          explicitReads,
          explicitDecker,
          explicitReadoutMode
          ) =>
          Result(Edit(
            grating,
            filter,
            fpu,
            explicitReadMode,
            explicitReads,
            explicitDecker,
            explicitReadoutMode
          ))
      }

  }

}

