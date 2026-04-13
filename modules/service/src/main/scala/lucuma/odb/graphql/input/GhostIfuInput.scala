// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.option.*
import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.enums.ObservingModeType
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.binding.*

object GhostIfuInput:

  case class Create(
    resolutionMode:            GhostResolutionMode,
    red:                       Option[GhostArmInput],
    blue:                      Option[GhostArmInput],
    explicitIfu1FiberAgitator: Option[GhostIfu1FiberAgitator],
    explicitIfu2FiberAgitator: Option[GhostIfu2FiberAgitator]
  ):
    def observingModeType: ObservingModeType =
      ObservingModeType.GhostIfu

  object Create:
    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap:
        case List(
          GhostResolutionModeBinding("resolutionMode", rResMode),
          GhostArmInput.Binding.Option("red", rRed),
          GhostArmInput.Binding.Option("blue", rBlue),
          GhostIfu1FiberAgitatorBinding.Option("explicitIfu1Agitator", rIfu1),
          GhostIfu2FiberAgitatorBinding.Option("explicitIfu2Agitator", rIfu2)
        ) => (rResMode, rRed, rBlue, rIfu1, rIfu2).parMapN: (resMode, red, blue, ifu1, ifu2) =>
          Create(resMode, red, blue, ifu1, ifu2)

  case class Edit(
    resolutionMode:            Option[GhostResolutionMode],
    red:                       Option[GhostArmInput],
    blue:                      Option[GhostArmInput],
    explicitIfu1FiberAgitator: Nullable[GhostIfu1FiberAgitator],
    explicitIfu2FiberAgitator: Nullable[GhostIfu2FiberAgitator]
  ):
    def observingModeType: ObservingModeType =
      ObservingModeType.GhostIfu

    def toCreate: Result[Create] =
      Result
        .fromOption(resolutionMode, OdbError.InvalidArgument("A resolution mode must be supplied for GHOST IFU observations".some).asProblem)
        .map(m => Create(m, red, blue, explicitIfu1FiberAgitator.toOption, explicitIfu2FiberAgitator.toOption))

  object Edit:
    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          GhostResolutionModeBinding.Option("resolutionMode", rResMode),
          GhostArmInput.Binding.Option("red", rRed),
          GhostArmInput.Binding.Option("blue", rBlue),
          GhostIfu1FiberAgitatorBinding.Nullable("explicitIfu1Agitator", rIfu1),
          GhostIfu2FiberAgitatorBinding.Nullable("explicitIfu2Agitator", rIfu2)
        ) => (rResMode, rRed, rBlue, rIfu1, rIfu2).parMapN: (resMode, red, blue, ifu1, ifu2) =>
          Edit(resMode, red, blue, ifu1, ifu2)