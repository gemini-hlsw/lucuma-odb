// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

enum CallForProposalsStatus(val tag: String) {
  case Open   extends CallForProposalsStatus("open")
  case Closed extends CallForProposalsStatus("closed")
}

object CallForProposalsStatus {

  given Enumerated[CallForProposalsStatus] = Enumerated.derived

}