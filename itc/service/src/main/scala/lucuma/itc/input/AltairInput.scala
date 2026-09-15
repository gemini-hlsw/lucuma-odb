// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.AltairMode
import lucuma.core.enums.FieldLens
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.itc.AltairParameters
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.AngleInput
import lucuma.odb.graphql.input.sourceprofile.BrightnessValueBinding

object AltairInput:

  val Binding: Matcher[AltairParameters] =
    ObjectFieldsBinding.rmap:
      case List(
            AltairModeBinding("mode", rMode),
            AngleInput.Binding.Option("guideStarSeparation", rSeparation),
            BrightnessValueBinding.Option("guideStarBrightness", rBrightness),
            FieldLensBinding.Option("fieldLens", rFieldLens)
          ) =>
        (rMode, rSeparation, rBrightness, rFieldLens).parTupled.flatMap(create)

  private def create(
    mode:       AltairMode,
    separation: Option[Angle],
    brightness: Option[BrightnessValue],
    fieldLens:  Option[FieldLens]
  ): Result[AltairParameters] =
    mode match
      case AltairMode.Ngs   =>
        (separation, brightness, fieldLens) match
          case (Some(s), Some(b), Some(f)) => Result(AltairParameters.Ngs(s, b, f))
          case _                           =>
            Result.failure(
              "Altair NGS requires guideStarSeparation, guideStarBrightness and fieldLens."
            )
      case AltairMode.Lgs   =>
        (separation, brightness, fieldLens) match
          case (Some(s), Some(b), None | Some(FieldLens.In)) => Result(AltairParameters.Lgs(s, b))
          case (Some(_), Some(_), Some(FieldLens.Out))       =>
            Result.failure(
              "Altair LGS always uses the field lens; fieldLens must be IN or omitted."
            )
          case _                                             =>
            Result.failure("Altair LGS requires guideStarSeparation and guideStarBrightness.")
      case AltairMode.LgsP1 =>
        if separation.isEmpty && brightness.isEmpty && fieldLens.isEmpty then
          Result(AltairParameters.LgsP1)
        else Result.failure("Altair LGS_P1 takes no guide star parameters.")
