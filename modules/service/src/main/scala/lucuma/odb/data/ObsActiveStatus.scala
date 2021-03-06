// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated


sealed abstract class ObsActiveStatus(val tag: String) extends Product with Serializable

object ObsActiveStatus {

  case object Active   extends ObsActiveStatus("active")
  case object Inactive extends ObsActiveStatus("inactive")

  val Default = Active

  implicit val EnumeratedExistence: Enumerated[ObsActiveStatus] =
    new Enumerated[ObsActiveStatus] {
      def all: List[ObsActiveStatus] = List(Active, Inactive)
      def tag(a: ObsActiveStatus): String = a.tag
    }

}