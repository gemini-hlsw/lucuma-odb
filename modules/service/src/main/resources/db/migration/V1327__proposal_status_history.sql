-- Proposal status changes are already recorded by the program chronicle
-- trigger.  Expose submissions and retractions as a per-program history.

CREATE INDEX i_chron_program_update_proposal_status
  ON t_chron_program_update (c_program_id, c_chron_id)
  WHERE c_mod_proposal_status;

-- Only updates count: the insert row records the initial 'not_submitted'
-- status, which is not a transition.  Staff decisions (accepted, not
-- accepted) are left out.
CREATE VIEW v_proposal_status_change AS
  SELECT
    c_chron_id,
    c_program_id,
    c_timestamp,
    c_new_proposal_status AS c_proposal_status
  FROM t_chron_program_update
  WHERE c_operation = 'UPDATE'
    AND c_mod_proposal_status
    AND c_new_proposal_status IN ('submitted', 'not_submitted');
