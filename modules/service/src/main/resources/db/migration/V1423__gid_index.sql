-- The number a GID encodes: 19004 for 'o-4a3c'.  A GID is a one letter tag, a dash and a
-- hexadecimal number, so GIDs compared as text stop following their numbers each time the
-- number gains a digit ('o-10000' sorts before 'o-4a3c').  Anything that orders or pages by
-- GID compares this instead.
CREATE FUNCTION gid_index(gid varchar)
  RETURNS bigint
  LANGUAGE sql
  IMMUTABLE
  STRICT
  PARALLEL SAFE
AS $$
  SELECT ('x' || lpad(substr(gid, 3), 16, '0'))::bit(64)::bigint
$$;

-- Lets a list paged by id read its rows in order, rather than sort all that match.
CREATE INDEX i_attachment_gid_index ON t_attachment (gid_index(c_attachment_id));
CREATE INDEX i_cfp_gid_index ON t_cfp (gid_index(c_cfp_id));
CREATE INDEX i_configuration_request_gid_index ON t_configuration_request (gid_index(c_configuration_request_id));
CREATE INDEX i_dataset_gid_index ON t_dataset (gid_index(c_dataset_id));
CREATE INDEX i_execution_event_gid_index ON t_execution_event (gid_index(c_execution_event_id));
CREATE INDEX i_group_gid_index ON t_group (gid_index(c_group_id));
CREATE INDEX i_observation_gid_index ON t_observation (gid_index(c_observation_id));
CREATE INDEX i_program_gid_index ON t_program (gid_index(c_program_id));
CREATE INDEX i_program_note_gid_index ON t_program_note (gid_index(c_program_note_id));
CREATE INDEX i_program_user_gid_index ON t_program_user (gid_index(c_program_user_id));
CREATE INDEX i_target_gid_index ON t_target (gid_index(c_target_id));
CREATE INDEX i_too_trigger_gid_index ON t_too_trigger (gid_index(c_too_trigger_id));
CREATE INDEX i_visit_gid_index ON t_visit (gid_index(c_visit_id));
