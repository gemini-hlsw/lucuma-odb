// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.NonEmptyStringBinding
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.VisitorObservingModeTypeBinding
import lucuma.odb.sequence.visitor.Config

object VisitorInput:

  type Create = Config
  val  Create = Config

  case class Edit(
    mode: Option[VisitorObservingModeType],
    centralWavelength: Option[Wavelength],
    agsDiameter: Option[Angle],
    name: Option[NonEmptyString],
    totalRequestTime: Option[TimeSpan]
  ):
    def toCreate: Result[Create] =
      Result.fromOption(
        (mode, centralWavelength, agsDiameter).mapN((m, w, f) =>
          Create(m, w, f, name, totalRequestTime)
        ),
        "Cannot turn edit into create; all required fields must be defined."
      ).flatMap: c =>
        c.validate.fold(Result.failure, _ => Result.success(c))

  val CreateBinding: Matcher[Create] =
    ObjectFieldsBinding.rmap:
      case List(
        VisitorObservingModeTypeBinding("mode", rMode),
        WavelengthInput.Binding("centralWavelength", rWavelength),
        AngleInput.Binding("agsDiameter", rAgsDiameter),
        NonEmptyStringBinding.Option("name", rName),
        TimeSpanInput.Binding.Option("totalRequestTime", rTotalRequestTime)
      ) =>
        (rMode, rWavelength, rAgsDiameter, rName, rTotalRequestTime).mapN(Create.apply)

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap:
      case List(
        VisitorObservingModeTypeBinding.Option("mode", rMode),
        WavelengthInput.Binding.Option("centralWavelength", rWavelength),
        AngleInput.Binding.Option("agsDiameter", rAgsDiameter),
        NonEmptyStringBinding.Option("name", rName),
        TimeSpanInput.Binding.Option("totalRequestTime", rTotalRequestTime)
      ) =>
        (rMode, rWavelength, rAgsDiameter, rName, rTotalRequestTime).mapN(Edit.apply)
