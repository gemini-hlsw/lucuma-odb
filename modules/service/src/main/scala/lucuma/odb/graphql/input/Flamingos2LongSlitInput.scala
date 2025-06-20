// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.Flamingos2Reads
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.Offset.Q
import lucuma.odb.data.Nullable
import lucuma.odb.format.spatialOffsets.*
import lucuma.odb.graphql.binding.*
import lucuma.odb.sequence.flamingos2.longslit.Config

object Flamingos2LongSlitInput {

  case class Create(
    disperser: Flamingos2Disperser,
    filter: Flamingos2Filter,
    fpu: Flamingos2Fpu,
    explicitReadMode: Option[Flamingos2ReadMode]       = None,
    explicitReads: Option[Flamingos2Reads]             = None,
    explicitDecker: Option[Flamingos2Decker]           = None,
    explicitReadoutMode: Option[Flamingos2ReadoutMode] = None,
    explicitSpatialOffsets: Option[List[Q]]            = None
  ) {
    def observingModeType: ObservingModeType =
      ObservingModeType.Flamingos2LongSlit

    // Formatted to store in a text column in the database with a regex constraint
    val formattedSpatialOffsets: Option[String] =
      explicitSpatialOffsets.map(OffsetsQFormat.reverseGet)

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
    private val Flamingos2Data: Matcher[(
      Flamingos2Disperser,
      Flamingos2Filter,
      Flamingos2Fpu,
      Option[Flamingos2ReadMode],
      Option[Flamingos2Decker],
      Option[Flamingos2ReadoutMode],
      Option[Flamingos2Reads],
      Option[List[Q]]
    )] =
      ObjectFieldsBinding.rmap {
        case List(
          Flamingos2DisperserBinding("disperser", rDisperser),
          Flamingos2FilterBinding("filter", rFilter),
          Flamingos2FpuBinding("fpu", rFpu),
          Flamingos2ReadModeBinding.Option("explicitReadMode", rReadMode),
          Flamingos2ReadsBinding.Option("explicitReads", rReads),
          Flamingos2DeckerBinding.Option("explicitDecker", rDecker),
          Flamingos2ReadoutModeBinding.Option("explicitReadoutMode", rReadoutMode),
          OffsetComponentInput.BindingQ.List.Option("explicitSpatialOffsets", rSpatialOffsets)
        ) => (
          rDisperser,
          rFilter,
          rFpu,
          rReadMode,
          rDecker,
          rReadoutMode,
          rReads,
          rSpatialOffsets
        ).parTupled
      }

    val Binding: Matcher[Create] =
      Flamingos2Data.rmap {
        case (
          disperser,
          filter,
          fpu,
          explicitReadMode,
          explicitDecker,
          explicitReadoutMode,
          explicitReads,
          explicitSpatialOffsets
      ) =>
        Result(Create(
          disperser,
          filter,
          fpu,
          explicitReadMode,
          explicitReads,
          explicitDecker,
          explicitReadoutMode,
          explicitSpatialOffsets
        ))
      }

  }

  case class Edit(
    disperser: Option[Flamingos2Disperser],
    filter: Option[Flamingos2Filter],
    fpu: Option[Flamingos2Fpu],
    explicitReadMode: Nullable[Flamingos2ReadMode],
    explicitReads: Nullable[Flamingos2Reads],
    explicitDecker: Nullable[Flamingos2Decker],
    explicitReadoutMode: Nullable[Flamingos2ReadoutMode],
    explicitSpatialOffsets: Nullable[List[Q]]
  ) {

    val observingModeType: ObservingModeType =
      ObservingModeType.Flamingos2LongSlit

    // Formatted to store in a text column in the database with a regex constraint
    val formattedSpatialOffsets: Nullable[String] =
      explicitSpatialOffsets.map(OffsetsQFormat.reverseGet)

    private def required[A](oa: Option[A], itemName: String): Result[A] =
      Result.fromOption(
        oa,
        Matcher.validationProblem(s"A $itemName is required in order to create a Flamingos 2 Long Slit observing mode.")
      )

    val toCreate: Result[Create] =
      for {
        g <- required(disperser, "disperser")
        f <- required(filter, "filter")
        u <- required(fpu, "fpu")
      } yield Create(
        g,
        f,
        u,
        explicitReadMode.toOption,
        explicitReads.toOption,
        explicitDecker.toOption,
        explicitReadoutMode.toOption,
        explicitSpatialOffsets.toOption
      )
  }

  object Edit {

    private val Flamingos2EditData: Matcher[(
      Option[Flamingos2Disperser],
      Option[Flamingos2Filter],
      Option[Flamingos2Fpu],
      Nullable[Flamingos2ReadMode],
      Nullable[Flamingos2Reads],
      Nullable[Flamingos2Decker],
      Nullable[Flamingos2ReadoutMode],
      Nullable[List[Q]]
    )] =
      ObjectFieldsBinding.rmap {
        case List(
          Flamingos2DisperserBinding.Option("disperser", rDisperser),
          Flamingos2FilterBinding.Option("filter", rFilter),
          Flamingos2FpuBinding.Option("fpu", rFpu),
          Flamingos2ReadModeBinding.Nullable("explicitReadMode", rReadMode),
          Flamingos2ReadsBinding.Nullable("explicitReads", rReads),
          Flamingos2DeckerBinding.Nullable("explicitDecker", rDecker),
          Flamingos2ReadoutModeBinding.Nullable("explicitReadoutMode", rReadoutMode),
          OffsetComponentInput.BindingQ.List.Nullable("explicitSpatialOffsets", rSpatialOffsets)
        ) => (
          rDisperser,
          rFilter,
          rFpu,
          rReadMode,
          rReads,
          rDecker,
          rReadoutMode,
          rSpatialOffsets
        ).parTupled
      }

    val Binding: Matcher[Edit] =
      Flamingos2EditData.rmap {
        case (
          grating,
          filter,
          fpu,
          explicitReadMode,
          explicitReads,
          explicitDecker,
          explicitReadoutMode,
          explicitSpatialOffsets
          ) =>
          Result(Edit(
            grating,
            filter,
            fpu,
            explicitReadMode,
            explicitReads,
            explicitDecker,
            explicitReadoutMode,
            explicitSpatialOffsets
          ))
      }

  }

}

