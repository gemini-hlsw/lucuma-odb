// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.data.NonEmptyList
import cats.syntax.all._
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.data.Nullable
import lucuma.odb.data.ProgramReference
import lucuma.odb.graphql.binding._

final case class CreateObservationInput(
  programId:        Option[Program.Id],
  programReference: Option[ProgramReference],
  SET:              Option[ObservationPropertiesInput.Create]
) {

  def asterism: Nullable[NonEmptyList[Target.Id]] =
    Nullable.orAbsent(SET).flatMap(_.asterism)

}

object CreateObservationInput {

  val Binding: Matcher[CreateObservationInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding.Option("programId", rPid),
        ProgramReferenceBinding.Option("programReference", rRef),
        ObservationPropertiesInput.Create.Binding.Option("SET", rSET),
      ) =>
        (rPid, rRef, rSET).parMapN(apply)
    }

}