// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.graphql.query

import cats.effect.IO
import lucuma.resource.test.ResourceGraphQLSuite
import skunk.implicits.*

/**
 * Every text column below is read back through a codec that rejects an unparseable value, so one
 * bad row would fail each read of its table. These tests hold the write-time constraints that stop
 * such a row from being stored.
 */
class ColumnConstraintSuite extends ResourceGraphQLSuite:

  test("t_telescope_mode_block: a partner outside e_partner is rejected"):
    // 'US' is the GraphQL spelling; the stored label is the lucuma-core tag 'us'.
    expectDbRejection(
      exec(sql"""
        insert into t_telescope_mode_block (c_site, c_start, c_end, c_mode, c_program_references, c_partner) values
        ('gn', '2030-01-01 00:00:00', '2030-01-02 00:00:00', 'BlockScheduling', '{}', 'US')
      """.command)
    )

  test("t_telescope_mode_block: an unparseable program reference is rejected"):
    expectDbRejection(
      exec(sql"""
        insert into t_telescope_mode_block (c_site, c_start, c_end, c_mode, c_program_references) values
        ('gn', '2030-01-03 00:00:00', '2030-01-04 00:00:00', 'Classical', array['not a reference'])
      """.command)
    )

  test("t_published_semester: a semester that is not YYYYA or YYYYB is rejected"):
    expectDbRejection(
      exec(sql"""
        insert into t_published_semester (c_site, c_semester, c_title, c_nights_start, c_nights_end) values
        ('gn', '2026-A', 'Bad semester', '2026-02-01', '2026-07-31')
      """.command)
    )

  test("t_instrument_component: an empty alias is rejected"):
    expectDbRejection(
      exec(sql"""
        insert into t_instrument_component (c_instrument, c_component_type, c_code, c_name, c_aliases) values
        ('GMOS', 'FILTER', 'empty-alias', 'Empty alias', array[''])
      """.command)
    )

  test("a valid row of each shape is accepted"):
    val inserts: IO[Unit] =
      exec(sql"""
        insert into t_telescope_mode_block (c_site, c_start, c_end, c_mode, c_program_references, c_partner) values
        ('gn', '2030-02-01 00:00:00', '2030-02-02 00:00:00', 'BlockScheduling', array['G-2026B-1234-C'], 'us')
      """.command) >>
        exec(sql"""
          insert into t_published_semester (c_site, c_semester, c_title, c_nights_start, c_nights_end) values
          ('gn', '2026A', 'Good semester', '2026-02-01', '2026-07-31')
        """.command) >>
        exec(sql"""
          insert into t_instrument_component (c_instrument, c_component_type, c_code, c_name, c_aliases) values
          ('GMOS', 'FILTER', 'good-alias', 'Good alias', array['r','r_G0303'])
        """.command)

    inserts.attempt.map(result => assert(result.isRight, s"Expected success, got $result"))
