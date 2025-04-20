// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated


sealed abstract class Existence(val tag: String) extends Product with Serializable

object Existence {

  case object Present extends Existence("present")
  case object Deleted extends Existence("deleted")

  val Default = Present

  implicit val EnumeratedExistence: Enumerated[Existence] =
    new Enumerated[Existence] {
      def all: List[Existence] = List(Present, Deleted)
      def tag(a: Existence): String = a.tag
    }

}