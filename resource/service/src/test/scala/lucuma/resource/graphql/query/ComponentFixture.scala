// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.graphql.query

import cats.effect.IO
import lucuma.resource.test.ResourceGraphQLSuite
import skunk.implicits.*

/** The instrument component catalog shared by the component query suites. */
trait ComponentFixture extends ResourceGraphQLSuite:

  // c-1, c-2 present at GN; c-3 (GNIRS disperser) deleted but present at GN;
  // c-4 has no blocks anywhere, so it never shows up in the catalog.
  protected def insertCatalog: IO[Unit] =
    exec(sql"""
      insert into t_instrument_component (c_id, c_instrument, c_component_type, c_code, c_name, c_barcode, c_aliases, c_existence) values
      ('c-1', 'GMOS', 'FILTER', 'G_PRIME', 'g''', null, array['g_G0301'], 'present'),
      ('c-2', 'GMOS', 'DISPERSER', 'R831_G5302', 'R831', null, '{}', 'present'),
      ('c-3', 'GNIRS', 'DISPERSER', 'B1200', 'B1200 old', null, '{}', 'deleted'),
      ('c-4', 'GHOST', 'OTHER', 'NO_BLOCKS', 'never placed', null, '{}', 'present')
    """.command) >>
      exec(sql"""
      insert into t_instrument_component_block (c_site, c_start, c_end, c_component_id, c_usage, c_location, c_note) values
      ('gn', '2026-08-01 18:00:00', '2026-08-02 06:00:00', 'c-1', 'SCIENCE', 'INSTALLED', null),
      ('gn', '2026-08-01 18:00:00', '2026-08-02 06:00:00', 'c-2', 'UNAVAILABLE', 'LAB', 'regrating'),
      ('gn', '2026-08-01 18:00:00', '2026-08-02 06:00:00', 'c-3', 'SCIENCE', 'INSTALLED', null)
    """.command)
