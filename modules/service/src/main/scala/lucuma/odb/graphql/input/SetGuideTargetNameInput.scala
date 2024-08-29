// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.graphql.binding.*

final case class SetGuideTargetNameInput(
  programId:     Program.Id,
  observationId: Observation.Id,
  targetName:    Option[NonEmptyString]
)

object SetGuideTargetNameInput {
  val Binding: Matcher[SetGuideTargetNameInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding("programId", rPid),
        ObservationIdBinding("observationId", rObsId),
        NonEmptyStringBinding.Option("targetName", rName)
      ) => (rPid, rObsId, rName).parMapN(apply)
    }
}
