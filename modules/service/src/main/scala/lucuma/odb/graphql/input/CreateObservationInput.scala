// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.data.NonEmptyList
import cats.syntax.all._
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.util.Bindings._

final case class CreateObservationInput(
  programId: Program.Id,
  SET:       Option[ObservationPropertiesInput]
) {

  def asterism: Nullable[NonEmptyList[Target.Id]] =
    Nullable.orAbsent(SET).flatMap(_.asterism)

}

object CreateObservationInput {

  val Binding: Matcher[CreateObservationInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding("programId", rProgramId),
        ObservationPropertiesInput.CreateBinding.Option("SET", rSET),
      ) =>
        (rProgramId, rSET).parMapN(apply)
    }

}