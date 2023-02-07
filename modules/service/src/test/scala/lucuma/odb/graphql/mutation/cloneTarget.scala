// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

class cloneTarget extends OdbSuite {
  import createTarget.FullTargetGraph

  val pi = TestUsers.Standard.pi(nextId, nextId)
  lazy val validUsers = List(pi)

  

}