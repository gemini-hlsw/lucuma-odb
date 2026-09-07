-- Measuring scheduling availability from when it was declared.
--
-- V1310 measured an observation's scheduling availability across the whole active
-- period.  Two things went wrong with that, in opposite directions.
--
-- TIME ALREADY SPENT COUNTED AS AVAILABILITY
--
-- Late in the semester, a window opening back at its start and shutting in four
-- hours measured as almost six months.  It cleared any approved minimum, and yet
-- the only time it actually offered the scheduler was those four hours -- which
-- is precisely the short window the whole feature exists to police.
--
-- AN APPROVED MINIMUM COULD DEMAND CALENDAR THAT NO LONGER EXISTED
--
-- An observation added in April, open for every remaining minute of the
-- semester, measured 121 days against a 180 day minimum approved in February and
-- was rejected -- while the same observation saying nothing at all was accepted,
-- because silence was measured as the whole period.  Stating your availability
-- truthfully was penalised.
--
-- THE FIX
--
-- An observation records the moment its windows were last declared, and its
-- availability is measured from the later of that and the start of the
-- active period, through to the end of that period.  Taking the later of the two
-- matters for the ordinary case, where observations are designed before the
-- semester opens: those are measured over the whole of it.  Only a declaration
-- made once the period is under way is measured from itself.
--
--   c_availability_anchor                        the anchor
--   time open in [anchor, active end)               what it offers
--   length of  [anchor, active end)                 what it had to offer
--
-- The first figure kills the first problem: time gone before the PI wrote the
-- window down was never theirs to offer.  The second kills the second: an
-- observation is asked for the lesser of the approved minimum and everything it
-- had, so it can always satisfy a minimum by withholding nothing.
--
-- Neither figure moves as the semester goes on -- the anchor is frozen and the
-- active period only changes when staff change it -- so nothing falls out of
-- approval merely because time passed.

-------------------------------------------------------------------------------
-- The anchor.
-------------------------------------------------------------------------------

ALTER TABLE t_observation
  ADD COLUMN c_availability_anchor timestamp NOT NULL DEFAULT now();

COMMENT ON COLUMN t_observation.c_availability_anchor IS
  'When this observation''s timing windows were last declared.  Its scheduling '
  'window is measured from the later of this and the start of the program''s '
  'active period, so an observation designed before the semester opens is '
  'measured over the whole of it.  Re-stamped only when the set of windows '
  'actually changes, never merely because time passed: an observation that sits '
  'untouched keeps the availability it was approved for.';

-- Observations that predate this column were designed before the measurement
-- existed, so they are anchored at their program's active period rather than at
-- the migration.  Anchoring them at the migration instant would shrink the
-- window of every observation nobody had touched.
UPDATE t_observation o
   SET c_availability_anchor = p.c_active_start::timestamp
  FROM t_program p
 WHERE p.c_program_id = o.c_program_id;

-------------------------------------------------------------------------------
-- What the approved minimum was measured against.
-------------------------------------------------------------------------------

-- Carried beside the minimum so it can be read honestly: "6 hours" says nothing
-- on its own, where "6 hours out of the 152 days then remaining" is plainly a
-- tight window and "180 days out of 180" plainly is not.  Stored but neither
-- matched by SelectRequest nor part of the uniqueness key -- two requests that
-- differ only in the calendar they were measured against are the same request --
-- following the GMOS imaging filters.
--
-- Zero for rows predating this migration, which pairs with their zero minimum to
-- permit anything, so nothing already approved becomes unapproved.
DROP VIEW v_configuration_request;

ALTER TABLE t_configuration_request
  ADD COLUMN c_time_remaining_when_declared interval NOT NULL DEFAULT INTERVAL '0 seconds'
    CHECK (c_time_remaining_when_declared >= INTERVAL '0 seconds');

COMMENT ON COLUMN t_configuration_request.c_time_remaining_when_declared IS
  'How much of the active period the observation still had when its timing '
  'windows were last declared, as of the moment this request was made.  Note '
  'that this is measured from the declaration, not from the request: an '
  'observation designed months before it was requested carries the whole '
  'stretch.  Provenance for reading the minimum beside it; not part of the '
  'request''s identity.';

CREATE VIEW v_configuration_request AS
  SELECT
    *,
    CASE WHEN cr.c_reference_ra IS NOT NULL THEN cr.c_configuration_request_id END AS c_reference_id,
    CASE WHEN cr.c_region_ra_arc_type IS NOT NULL THEN cr.c_configuration_request_id END AS c_region_id,
    CASE WHEN cr.c_observing_mode_type = 'flamingos_2_long_slit' THEN cr.c_configuration_request_id END AS c_flamingos_2_longslit_id,
    CASE WHEN cr.c_observing_mode_type = 'flamingos_2_mos' THEN cr.c_configuration_request_id END AS c_flamingos_2_mos_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_north_imaging' THEN cr.c_configuration_request_id END AS c_gmos_north_imaging_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_south_imaging' THEN cr.c_configuration_request_id END AS c_gmos_south_imaging_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_north_long_slit' THEN cr.c_configuration_request_id END AS c_gmos_north_longslit_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_south_long_slit' THEN cr.c_configuration_request_id END AS c_gmos_south_longslit_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_north_mos' THEN cr.c_configuration_request_id END AS c_gmos_north_mos_id,
    CASE WHEN cr.c_observing_mode_type = 'gmos_south_mos' THEN cr.c_configuration_request_id END AS c_gmos_south_mos_id,
    CASE WHEN cr.c_observing_mode_type = 'igrins_2_long_slit' THEN cr.c_configuration_request_id END AS c_igrins_2_longslit_id,
    CASE WHEN cr.c_gmos_north_ifu_grating IS NOT NULL AND cr.c_gmos_north_ifu_fpu IS NOT NULL THEN cr.c_configuration_request_id END AS c_gmos_north_ifu_id,
    CASE WHEN cr.c_gmos_south_ifu_grating IS NOT NULL AND cr.c_gmos_south_ifu_fpu IS NOT NULL THEN cr.c_configuration_request_id END AS c_gmos_south_ifu_id,
    CASE WHEN cr.c_gnirs_longslit_grating IS NOT NULL AND cr.c_gnirs_longslit_camera IS NOT NULL AND cr.c_gnirs_longslit_prism IS NOT NULL THEN cr.c_configuration_request_id END AS c_gnirs_longslit_id,
    CASE WHEN cr.c_gnirs_ifu_grating IS NOT NULL AND cr.c_gnirs_ifu_fpu IS NOT NULL THEN cr.c_configuration_request_id END AS c_gnirs_ifu_id,
    CASE WHEN cr.c_visitor_radius IS NOT NULL THEN cr.c_configuration_request_id END AS c_visitor_id,
    CASE WHEN cr.c_visitor_radius IS NOT NULL THEN cr.c_observing_mode_type END AS c_visitor_mode,
    CASE WHEN cr.c_region_ra_arc_type = 'partial' THEN cr.c_configuration_request_id END AS c_partial_ra_region_id,
    CASE WHEN cr.c_region_dec_arc_type = 'partial' THEN cr.c_configuration_request_id END AS c_partial_dec_region_id
  FROM t_configuration_request cr
  ;

-------------------------------------------------------------------------------
-- Invalidation.
--
-- The anchor is stamped by the service that writes the windows, which already
-- invalidates obscalc through the timing window trigger from V1310.  A direct
-- change to the column still needs covering, since it moves the measurement.
-------------------------------------------------------------------------------

CREATE TRIGGER availability_anchor_obscalc_invalidate_trigger
  AFTER UPDATE OF c_availability_anchor ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION obsid_obscalc_invalidate();
