// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

class configurationRequests_observingModeType extends OdbSuite with ObservingModeSetupOperations:

  val pi       = TestUsers.Standard.pi(1, 30)
  val admin    = TestUsers.Standard.admin(2, 31)
  val validUsers = List(pi, admin)

  // A configuration request inherits the observing mode of the observation it is
  // created from, and belongs to the program's proposal workflow, so the program
  // needs a proposal in a call for proposals before requests can be created.
  test("filter configuration requests on observingModeType"):
    for
      cfpid <- createGeminiCallForProposalsAs(admin)
      pid   <- createProgramAs(pi)
      _     <- addProposal(pi, pid, Some(cfpid), None)
      tid   <- createTargetWithProfileAs(pi, pid)
      // observations carrying distinct (complete) observing modes
      oid1  <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))   // GMOS_NORTH_LONG_SLIT
      oid2  <- createGmosSouthLongSlitObservationAs(pi, pid, List(tid))   // GMOS_SOUTH_LONG_SLIT
      oid3  <- createFlamingos2LongSlitObservationAs(pi, pid, List(tid))  // FLAMINGOS_2_LONG_SLIT
      // ...turned into configuration requests
      cid1  <- createConfigurationRequestAs(pi, oid1)
      cid2  <- createConfigurationRequestAs(pi, oid2)
      cid3  <- createConfigurationRequestAs(pi, oid3)

      eq      <- configurationRequestsWhere(pi, s"""program: { id: { EQ: "$pid" } }, observingModeType: { EQ: GMOS_NORTH_LONG_SLIT }""")
      in      <- configurationRequestsWhere(pi, s"""program: { id: { EQ: "$pid" } }, observingModeType: { IN: [ GMOS_NORTH_LONG_SLIT, GMOS_SOUTH_LONG_SLIT ] }""")
      neq     <- configurationRequestsWhere(pi, s"""program: { id: { EQ: "$pid" } }, observingModeType: { NEQ: GMOS_NORTH_LONG_SLIT }""")
      nin     <- configurationRequestsWhere(pi, s"""program: { id: { EQ: "$pid" } }, observingModeType: { NIN: [ GMOS_NORTH_LONG_SLIT, GMOS_SOUTH_LONG_SLIT ] }""")
      isNull  <- configurationRequestsWhere(pi, s"""program: { id: { EQ: "$pid" } }, observingModeType: { IS_NULL: true }""")
      notNull <- configurationRequestsWhere(pi, s"""program: { id: { EQ: "$pid" } }, observingModeType: { IS_NULL: false }""")
    yield
      assertEquals(eq,      List(cid1))
      assertEquals(in,      List(cid1, cid2))
      assertEquals(neq,     List(cid2, cid3))
      assertEquals(nin,     List(cid3))
      assertEquals(isNull,  Nil)
      assertEquals(notNull, List(cid1, cid2, cid3))
