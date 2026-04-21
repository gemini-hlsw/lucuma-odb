// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.option.*
import cats.syntax.parallel.*
import eu.timepit.refined.types.numeric.PosInt
import grackle.Result
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.enums.ObservingModeType
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.binding.*

object GhostIfuInput:

  private val One: PosInt = PosInt.unsafeFrom(1)

  case class Create(
    stepCount:                 PosInt,
    resolutionMode:            GhostResolutionMode,
    red:                       Option[GhostDetectorConfigInput],
    blue:                      Option[GhostDetectorConfigInput],
    slitCameraExposureTime:    Option[TimeSpan],
    explicitIfu1FiberAgitator: Option[GhostIfu1FiberAgitator],
    explicitIfu2FiberAgitator: Option[GhostIfu2FiberAgitator]
  ):
    def observingModeType: ObservingModeType =
      ObservingModeType.GhostIfu

  object Create:
    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap:
        case List(
          PosIntBinding.Option("stepCount", rStepCount),
          GhostResolutionModeBinding("resolutionMode", rResMode),
          GhostDetectorConfigInput.Binding.Option("red", rRed),
          GhostDetectorConfigInput.Binding.Option("blue", rBlue),
          TimeSpanInput.Binding.Option("slitViewingCameraExposureTime", rSlit),
          GhostIfu1FiberAgitatorBinding.Option("explicitIfu1Agitator", rIfu1),
          GhostIfu2FiberAgitatorBinding.Option("explicitIfu2Agitator", rIfu2)
        ) => (rStepCount, rResMode, rRed, rBlue, rSlit, rIfu1, rIfu2).parMapN: (stepCount, resMode, red, blue, slit, ifu1, ifu2) =>
          Create(stepCount.getOrElse(One), resMode, red, blue, slit, ifu1, ifu2)

  case class Edit(
    stepCount:                 Option[PosInt],
    resolutionMode:            Option[GhostResolutionMode],
    red:                       Option[GhostDetectorConfigInput],
    blue:                      Option[GhostDetectorConfigInput],
    slitCameraExposureTime:    Nullable[TimeSpan],
    explicitIfu1FiberAgitator: Nullable[GhostIfu1FiberAgitator],
    explicitIfu2FiberAgitator: Nullable[GhostIfu2FiberAgitator]
  ):
    def observingModeType: ObservingModeType =
      ObservingModeType.GhostIfu

    def toCreate: Result[Create] =
      Result
        .fromOption(resolutionMode, OdbError.InvalidArgument("A resolution mode must be supplied for GHOST IFU observations".some).asProblem)
        .map(m => Create(stepCount.getOrElse(One), m, red, blue, slitCameraExposureTime.toOption, explicitIfu1FiberAgitator.toOption, explicitIfu2FiberAgitator.toOption))

  object Edit:
    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          PosIntBinding.Option("stepCount", rStepCount),
          GhostResolutionModeBinding.Option("resolutionMode", rResMode),
          GhostDetectorConfigInput.Binding.Option("red", rRed),
          GhostDetectorConfigInput.Binding.Option("blue", rBlue),
          TimeSpanInput.Binding.Nullable("slitViewingCameraExposureTime", rSlit),
          GhostIfu1FiberAgitatorBinding.Nullable("explicitIfu1Agitator", rIfu1),
          GhostIfu2FiberAgitatorBinding.Nullable("explicitIfu2Agitator", rIfu2)
        ) => (rStepCount, rResMode, rRed, rBlue, rSlit, rIfu1, rIfu2).parMapN: (stepCount, resMode, red, blue, slit, ifu1, ifu2) =>
          Edit(stepCount, resMode, red, blue, slit, ifu1, ifu2)