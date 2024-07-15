-- Proposals may reference existing CFPs.  They draw the semester from the CFP
-- and the type must agree as well.  This migration introduces an update
-- trigger on t_cfp such that when critical properties are modified, if there
-- are any existing, referencing proposals we prevent the change.

CREATE OR REPLACE FUNCTION validate_cfp_update()
  RETURNS TRIGGER AS $$
DECLARE
  program_ids text;
BEGIN
  IF ((NEW.c_existence <> OLD.c_existence) OR
      (NEW.c_semester  <> OLD.c_semester)  OR
      (NEW.c_type      <> OLD.c_type))     THEN

    -- Select program ids that use this CFP, if any.
    SELECT json_agg(c_program_id) INTO program_ids
    FROM t_proposal
    WHERE c_cfp_id = NEW.c_cfp_id;

    IF program_ids IS NOT NULL THEN
      RAISE EXCEPTION 'Cannot delete this CfP, or change its type or semester until dependent proposals no longer reference it.'
        USING ERRCODE = 'P0001', DETAIL =  program_ids::text;
    END IF;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER validate_cfp_update_trigger
BEFORE UPDATE OF c_existence, c_semester, c_type ON t_cfp
FOR EACH ROW
EXECUTE FUNCTION validate_cfp_update();