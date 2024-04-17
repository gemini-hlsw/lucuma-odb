// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.order.*
import cats.syntax.parallel.*
import grackle.Cursor
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import grackle.Result
import grackle.Term
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.graphql.binding.*


// TODO: DELETE ME?


object WhereTimestampInterval {

  case class Compare(x: Term[TimestampInterval])(f: TimestampInterval => Boolean) extends Predicate {
    def apply(c: Cursor): Result[Boolean] = x(c).map(f)
    def children = List(x)
  }

  def bindingStart(path: Path): Matcher[Predicate] =
    binding(path)(_.start)

  def bindingEnd(path: Path): Matcher[Predicate] =
    binding(path)(_.end)

  private def binding(path: Path)(f: TimestampInterval => Timestamp): Matcher[Predicate] =
    ObjectFieldsBinding.rmap {
      case List(
        TimestampBinding.Option("EQ", rEQ),
        TimestampBinding.Option("NEQ", rNEQ),
        TimestampBinding.List.Option("IN", rIN),
        TimestampBinding.List.Option("NIN", rNIN),
        TimestampBinding.Option("GT", rGT),
        TimestampBinding.Option("LT", rLT),
        TimestampBinding.Option("GTE", rGTE),
        TimestampBinding.Option("LTE", rLTE)
      ) =>
        (rEQ, rNEQ, rIN, rNIN, rGT, rLT, rGTE, rLTE).parMapN {
          (EQ, NEQ, IN, NIN, GT, LT, GTE, LTE) =>
            and(List(
              EQ.map(a => Compare(path)(f(_) === a)),
              NEQ.map(a => Compare(path)(f(_) =!= a)),
              IN.map(as => Compare(path)(ti => as.contains(f(ti)))),
              NIN.map(as => Compare(path)(ti => !as.contains(f(ti)))),
              GT.map(a => Compare(path)(f(_) > a)),
              LT.map(a => Compare(path)(f(_) < a)),
              GTE.map(a => Compare(path)(f(_) >= a)),
              LTE.map(a => Compare(path)(f(_) <= a))
            ).flatten)
        }
    }
}
