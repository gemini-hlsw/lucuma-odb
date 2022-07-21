// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.util

import cats.syntax.all._
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Mapping
import edu.gemini.grackle.Query
import edu.gemini.grackle.Result

trait MutationCompanionOps[F[_]] extends Mapping[F] {

  // TODO: upstream this
  implicit class MutationCompanionOps(self: Mutation.type) {

    // A mutation that yields a single result and doesn't change the environment
    def simple(f: (Query, Cursor.Env) => F[Result[Query]]): Mutation =
      Mutation((q, e) => fs2.Stream.eval(f(q, e).map(_.tupleRight(e))))

    // A mutation that yields a stream of results and doesn't change the environment
    def simpleStream(f: (Query, Cursor.Env) => fs2.Stream[F, Result[Query]]): Mutation =
      Mutation((q, e) => f(q, e).map(_.tupleRight(e)))

  }

}