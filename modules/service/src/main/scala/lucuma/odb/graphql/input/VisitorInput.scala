// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.VisitorObservingModeTypeBinding
import lucuma.odb.sequence.visitor.Config

object VisitorInput:

  type Create = Config
  val  Create = Config

  case class Edit(
    mode: Option[VisitorObservingModeType],
    centralWavelength: Option[Wavelength],
    guideStarMinSep: Option[Angle] 
  ):
    def toCreate: Result[Create] =
      Result.fromOption(
        (mode, centralWavelength, guideStarMinSep).mapN(Create.apply),
        "Cannot turn edit into create; all fields must be defined."
      )

  val CreateBinding: Matcher[Create] =
    ObjectFieldsBinding.rmap:
      case List(
        VisitorObservingModeTypeBinding("mode", rMode),
        WavelengthInput.Binding("centralWavelength", rWavelength),
        AngleInput.Binding("guideStarMinSep", rGuideStarMinSep)
      ) =>
        (rMode, rWavelength, rGuideStarMinSep).mapN(Create.apply)

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap:
      case List(
        VisitorObservingModeTypeBinding.Option("mode", rMode),
        WavelengthInput.Binding.Option("centralWavelength", rWavelength),
        AngleInput.Binding.Option("guideStarMinSep", rGuideStarMinSep)
      ) =>
        (rMode, rWavelength, rGuideStarMinSep).mapN(Edit.apply)
