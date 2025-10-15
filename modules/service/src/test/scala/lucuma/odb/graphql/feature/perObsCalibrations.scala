// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.feature

import lucuma.core.math.Wavelength
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.TestUsers
import lucuma.odb.graphql.query.ExecutionQuerySetupOperations
import lucuma.odb.graphql.subscription.SubscriptionUtils

class perObsCalibrations extends OdbSuite with SubscriptionUtils with ExecutionQuerySetupOperations {
  val pi       = TestUsers.Standard.pi(1, 101)
  val service  = TestUsers.service(3)

  val DefaultSnAt: Wavelength = Wavelength.fromIntNanometers(510).get

  override val validUsers = List(pi, service)
}
