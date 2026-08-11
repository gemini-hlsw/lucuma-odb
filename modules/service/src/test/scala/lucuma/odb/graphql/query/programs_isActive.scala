// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import io.circe.syntax.*

import java.time.LocalDate

class programs_isActive extends OdbSuite:

  val pi    = TestUsers.Standard.pi(1, 30)
  val staff = TestUsers.Standard.staff(2, 31)
  val validUsers = List(pi, staff)

  // `isActive` matches programs whose `[activeStart, activeEnd]` window contains
  // the current UTC date, computed by the `c_is_active` column on `v_program`.
  test("filter programs on isActive"):
    val today = LocalDate.now()
    for
      pidActive   <- createProgramAs(pi)
      _           <- setProgramActiveAs(staff, pidActive, today.minusDays(30), today.plusDays(30))
      pidInactive <- createProgramAs(pi)
      _           <- setProgramActiveAs(staff, pidInactive, today.plusDays(100), today.plusDays(200))
      idFilter     = s"id: { IN: [${pidActive.asJson}, ${pidInactive.asJson}] }"
      active      <- programsWhere(pi, s"$idFilter, isActive: true")
      inactive    <- programsWhere(pi, s"$idFilter, isActive: false")
      omit        <- programsWhere(pi, s"$idFilter") // no isActive filter: both match
    yield
      assertEquals(active, List(pidActive))
      assertEquals(inactive, List(pidInactive))
      assertEquals(omit.toSet, Set(pidActive, pidInactive))
