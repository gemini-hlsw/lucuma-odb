// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.data.NonEmptyList
import cats.syntax.all.*
import grackle.Result
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*

trait ImagingFilterCheck:

  def atLeastOne(instrumentName: String): OdbError =
    OdbError.InvalidArgument(s"At least one filter must be specified for $instrumentName imaging observations.".some)

  def notEmpty[L](instrumentName: String, filters: Result[List[L]]): Result[NonEmptyList[L]] =
    filters.flatMap: fs =>
      Result.fromOption(NonEmptyList.fromList(fs), atLeastOne(instrumentName).asProblem)

  def notEmptyIfPresent[L](instrumentName: String, filters: Result[Option[List[L]]]): Result[Option[NonEmptyList[L]]] =
    filters.flatMap(_.traverse(fs => Result.fromOption(NonEmptyList.fromList(fs), atLeastOne(instrumentName).asProblem)))
