// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

val TimingWindowsGraph: String = 
  s"""timingWindows {
      |  inclusion
      |  startUtc
      |  end {
      |    ... on TimingWindowEndAt {
      |      atUtc
      |    }
      |    ... on TimingWindowEndAfter {
      |      after {
      |        hours
      |      }         
      |      repeat {
      |        period {
      |          hours
      |        }
      |       times
      |      }
      |    }
      |  }
      |}""".stripMargin
