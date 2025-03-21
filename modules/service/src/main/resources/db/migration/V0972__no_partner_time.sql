UPDATE t_visit
  SET c_raw_program_time   = c_raw_program_time   + c_raw_partner_time,
      c_final_program_time = c_final_program_time + c_final_partner_time;

ALTER TABLE t_visit
  DROP COLUMN c_raw_partner_time,
  DROP COLUMN c_final_partner_time;

UPDATE t_time_charge_discount
  SET c_program_discount = c_program_discount + c_partner_discount;

ALTER TABLE t_time_charge_discount
  DROP COLUMN c_partner_discount;

UPDATE t_execution_digest
  SET c_acq_program_time = c_acq_program_time + c_acq_partner_time,
      c_sci_program_time = c_sci_program_time + c_sci_partner_time;

ALTER TABLE t_execution_digest
  DROP COLUMN c_acq_partner_time,
  DROP COLUMN c_sci_partner_time;

-- Update the e_obs_class enumeration.  This is complicated because there is
-- no direct support for dropping enum values.

-- Drop this view because it references a column of type e_obs_class.  We'll
-- recreate it in the end.
DROP VIEW v_step_record;

-- Add the new `nightCal` value.  We'll change existing `partnerCal` and
-- `programCal` to `nightCal`.
ALTER TYPE e_obs_class ADD VALUE 'nightCal' BEFORE 'acquisition';

-- programCal -> nightCal

UPDATE t_execution_digest
   SET c_acq_obs_class = 'nightCal'::e_obs_class
 WHERE c_acq_obs_class = 'partnerCal'
    OR c_acq_obs_class = 'programCal';

UPDATE t_execution_digest
   SET c_sci_obs_class = 'nightCal'::e_obs_class
 WHERE c_sci_obs_class = 'partnerCal'
    OR c_sci_obs_class = 'programCal';

UPDATE t_step_record
   SET c_observe_class = 'nightCal'::e_obs_class
 WHERE c_observe_class = 'partnerCal'
    OR c_observe_class = 'programCal';

-- acquisitionCal -> acquisition

UPDATE t_execution_digest
   SET c_acq_obs_class = 'acquisition'::e_obs_class
 WHERE c_acq_obs_class = 'acquisitionCal';

UPDATE t_execution_digest
   SET c_sci_obs_class = 'acquisition'::e_obs_class
 WHERE c_sci_obs_class = 'acquisitionCal';

UPDATE t_step_record
   SET c_observe_class = 'acquisition'::e_obs_class
 WHERE c_observe_class = 'acquisitionCal';

-- The new enum values without the deleted values.

CREATE TYPE e_obs_class_new AS ENUM(
  'science',
  'nightCal',
  'acquisition',
  'dayCal'
);

-- Change the type of e_obs_class columns to e_obs_class_new

ALTER TABLE t_execution_digest
  ALTER COLUMN c_acq_obs_class SET DATA TYPE e_obs_class_new USING c_acq_obs_class::text::e_obs_class_new,
  ALTER COLUMN c_sci_obs_class SET DATA TYPE e_obs_class_new USING c_sci_obs_class::text::e_obs_class_new;

ALTER TABLE t_step_record
  ALTER COLUMN c_observe_class SET DATA TYPE e_obs_class_new USING c_observe_class::text::e_obs_class_new;

-- Rename e_obs_class_new to e_obs_class;

DROP TYPE e_obs_class;
ALTER TYPE e_obs_class_new RENAME TO e_obs_class;

-- Recreate the step record view

CREATE VIEW v_step_record AS
SELECT
  s.c_step_id,
  s.c_atom_id,
  a.c_visit_id,
  s.c_step_index,
  s.c_instrument,
  s.c_step_type,
  s.c_observe_class,
  s.c_created,
  s.c_execution_state,
  s.c_time_estimate,
  s.c_generated_id,
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
  (SELECT MAX(c_qa_state) FROM t_dataset d WHERE d.c_step_id = s.c_step_id) AS c_qa_state
FROM
  t_step_record s
LEFT JOIN t_step_config_gcal g
  ON g.c_step_id = s.c_step_id
LEFT JOIN t_step_config_smart_gcal m
  ON m.c_step_id = s.c_step_id
LEFT JOIN t_atom_record a
  ON a.c_atom_id = s.c_atom_id
ORDER BY
  s.c_step_id;