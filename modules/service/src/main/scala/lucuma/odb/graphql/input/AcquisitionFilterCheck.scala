// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.data.NonEmptyList
import cats.syntax.all.*
import grackle.Result
import lucuma.core.syntax.string.*
import lucuma.core.util.Enumerated
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*

trait AcquisitionFilterCheck:

  def acquisitionFilter[L: Enumerated](
    allowed: NonEmptyList[L],
    filter:  Result[Nullable[L]]
  ): Result[Nullable[L]] =
    filter.flatMap:
      _.traverse: f =>
        if allowed.toList.contains(f) then Result(f)
        else
          val names = allowed.map(l => Enumerated[L].tag(l).toScreamingSnakeCase).mkString_(", ")
          OdbError.InvalidArgument(s"'explicitFilter' must contain one of: $names".some).asFailure
