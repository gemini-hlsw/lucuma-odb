-- sc-10968: every telescope position has a purpose, on the object or on the
-- sky.  Only object positions count toward the exposures an observation
-- requests.  Until now "on the object" was approximated by "guided", which
-- fails for an unguided observation of the object (e.g., a sky calibration)
-- and for a guided sky position.

-- The imaging offset generators were already split by this distinction (object
-- offsets and sky offsets), so their role type becomes the purpose.
ALTER TYPE e_offset_generator_role RENAME TO e_offset_purpose;

-- Steps carry their resolved purpose.  Existing steps are given the purpose
-- the sequence generators now assign, as far as the stored columns can tell:
--
-- * Acquisition science steps away from the base position image the slit, the
--   field or the sky, never the target -- except IGRINS-2's slit-viewing
--   camera images, whose positions stay within its field.
-- * Other science steps take the purpose the cycle count assumed when they
--   were generated: unguided ones were sky positions.  Daytime calibrations
--   are never guided and have no target, so they stay object.
--
-- The rest (calibration steps, guided science steps) are object.  This is
-- necessarily approximate for history: e.g., an imaging object position the
-- user chose not to guide becomes sky.
ALTER TABLE t_step
  ADD COLUMN c_offset_purpose e_offset_purpose NOT NULL DEFAULT 'object';

UPDATE t_step
   SET c_offset_purpose = 'sky'
 WHERE c_step_type = 'science'
   AND (
         (    c_observe_class = 'acquisition'
          AND (c_offset_p <> 0 OR c_offset_q <> 0)
          AND c_instrument <> 'Igrins2'
         )
         OR
         (    c_observe_class NOT IN ('acquisition', 'dayCal')
          AND c_guide_state = 'disabled'
         )
       );

-- From V1363, adding the purpose.
CREATE OR REPLACE VIEW v_step_record AS
SELECT
  (s.c_atom_id::text || ':' || se.c_run_order::text) AS c_atom_run_id,
  se.c_step_id,
  s.c_atom_id,
  se.c_visit_id,
  se.c_execution_state,
  se.c_execution_order,
  se.c_run_order,
  se.c_first_event_time,
  se.c_last_event_time,
  s.c_instrument,
  s.c_step_type,
  s.c_observe_class,
  s.c_time_estimate,
  s.c_breakpoint,
  g.c_gcal_continuum,
  g.c_gcal_ar_arc,
  g.c_gcal_cuar_arc,
  g.c_gcal_thar_arc,
  g.c_gcal_xe_arc,
  g.c_gcal_filter,
  g.c_gcal_diffuser,
  g.c_gcal_shutter,
  m.c_smart_gcal_type,
  s.c_offset_p,
  s.c_offset_q,
  s.c_guide_state,
  (SELECT MAX(c_qa_state) FROM t_dataset d WHERE d.c_step_id = se.c_step_id) AS c_qa_state,
  s.c_offset_purpose
FROM
  t_step_execution se
JOIN t_step s ON s.c_step_id = se.c_step_id
JOIN t_atom a ON a.c_atom_id = s.c_atom_id
LEFT JOIN t_step_config_gcal g       ON g.c_step_id = se.c_step_id
LEFT JOIN t_step_config_smart_gcal m ON m.c_step_id = se.c_step_id;

-- Digest offsets carry their purposes alongside their guide states (V1065).  A
-- NULL purpose array is read as all object positions.
ALTER TABLE t_execution_digest
  ADD COLUMN c_acq_offset_purposes e_offset_purpose[],
  ADD COLUMN c_sci_offset_purposes e_offset_purpose[],
  ADD CONSTRAINT t_execution_digest_acq_offsets_purposes_length
    CHECK (c_acq_offset_purposes IS NULL OR array_length(c_acq_offsets, 1) IS NOT DISTINCT FROM array_length(c_acq_offset_purposes, 1)),
  ADD CONSTRAINT t_execution_digest_sci_offsets_purposes_length
    CHECK (c_sci_offset_purposes IS NULL OR array_length(c_sci_offsets, 1) IS NOT DISTINCT FROM array_length(c_sci_offset_purposes, 1));

ALTER TABLE t_obscalc
  ADD COLUMN c_acq_offset_purposes e_offset_purpose[],
  ADD COLUMN c_sci_offset_purposes e_offset_purpose[],
  ADD CONSTRAINT t_obscalc_acq_offsets_purposes_length
    CHECK (c_acq_offset_purposes IS NULL OR array_length(c_acq_offsets, 1) IS NOT DISTINCT FROM array_length(c_acq_offset_purposes, 1)),
  ADD CONSTRAINT t_obscalc_sci_offsets_purposes_length
    CHECK (c_sci_offset_purposes IS NULL OR array_length(c_sci_offsets, 1) IS NOT DISTINCT FROM array_length(c_sci_offset_purposes, 1));

-- Stored obscalc digests get purposes from their guide states, as the steps
-- did above (unguided is sky).  Observations recalculated below overwrite
-- them; the others (complete or deleted) keep these.
CREATE FUNCTION offset_purposes_from_guide_states(
  states e_guide_state[]
) RETURNS e_offset_purpose[] AS $$
  SELECT array_agg(
           CASE WHEN g = 'disabled' THEN 'sky' ELSE 'object' END::e_offset_purpose
           ORDER BY i
         )
    FROM unnest(states) WITH ORDINALITY AS t(g, i)
$$ LANGUAGE sql IMMUTABLE;

UPDATE t_obscalc
   SET c_acq_offset_purposes = offset_purposes_from_guide_states(c_acq_offset_guide_states),
       c_sci_offset_purposes = offset_purposes_from_guide_states(c_sci_offset_guide_states)
 WHERE c_acq_offset_guide_states IS NOT NULL
    OR c_sci_offset_guide_states IS NOT NULL;

DROP FUNCTION offset_purposes_from_guide_states(e_guide_state[]);

-- Cached digests have no purposes, and where positions now resolve from the
-- instrument geometry rather than guiding, the cycle counts change too.  The
-- digest cache is recomputed on demand.
DELETE FROM t_execution_digest;

-- Recalculate every present observation whose science is not yet complete.  A
-- deleted observation is recalculated if it is restored, and a complete one
-- has nothing left to generate.
DO $$
DECLARE
  obs_ids d_observation_id[];
BEGIN
  SELECT array_agg(o.c_observation_id) INTO obs_ids
    FROM t_observation o
    LEFT JOIN t_obscalc c ON c.c_observation_id = o.c_observation_id
   WHERE o.c_existence = 'present'
     AND COALESCE(c.c_sci_execution_state NOT IN ('completed', 'declared_complete'), true);

  IF obs_ids IS NOT NULL THEN
    CALL invalidate_obscalc_many(obs_ids);
  END IF;
END;
$$;
