// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated


sealed abstract class ObsStatus(val tag: String) extends Product with Serializable

object ObsStatus {

  case object New       extends ObsStatus("new")
  case object Included  extends ObsStatus("included")
  case object Proposed  extends ObsStatus("proposed")
  case object Approved  extends ObsStatus("approved")
  case object ForReview extends ObsStatus("for_review")
  case object Ready     extends ObsStatus("ready")
  case object Ongoing   extends ObsStatus("ongoing")
  case object Observed  extends ObsStatus("observe")

  val Default = New

  implicit val EnumeratedExistence: Enumerated[ObsStatus] =
    new Enumerated[ObsStatus] {
      def all: List[ObsStatus] = List(
        New,
        Included,
        Proposed,
        Approved,
        ForReview,
        Ready,
        Ongoing,
        Observed,
      )
      def tag(a: ObsStatus): String = a.tag
    }

}