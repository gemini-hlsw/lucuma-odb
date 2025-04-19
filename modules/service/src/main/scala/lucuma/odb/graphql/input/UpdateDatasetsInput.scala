// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.Path
import grackle.Predicate
import lucuma.odb.graphql.binding.*

case class UpdateDatasetsInput(
  SET:   DatasetPropertiesInput,
  WHERE: Option[Predicate],
  LIMIT: Option[NonNegInt]
)

object UpdateDatasetsInput {

  def binding(path: Path) = {
    val WhereDatasetsBinding = WhereDataset.binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        DatasetPropertiesInput.Binding("SET", rSET),
        WhereDatasetsBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT)
      ) =>
        (rSET, rWHERE, rLIMIT).parMapN(apply)
    }
  }

}
