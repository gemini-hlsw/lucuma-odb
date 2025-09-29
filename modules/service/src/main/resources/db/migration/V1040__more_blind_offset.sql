
--------------------------------------------------------
-- updates this function from V0868 to exclude blind offsets from observation title
--------------------------------------------------------
CREATE OR REPLACE FUNCTION target_update()
  RETURNS trigger AS $$
DECLARE
  tid d_target_id;
  oid d_observation_id;
BEGIN
  tid := NEW.c_target_id;

  -- update the titles of observations that use this target
  if (OLD.c_name != NEW.c_name OR OLD.c_existence != NEW.c_existence) THEN
    -- If one of the above changes, it indirectly triggers ch_observation_edit by modifying the obs title
    update t_observation a 
    set c_title = (
      select array_to_string(
        coalesce(
            array_agg(coalesce(t.c_name, 'Unnamed') order by t.c_target_id), 
            array['Untargeted']
        ), 
        ', '
      )
      from t_asterism_target b
      join t_target t on b.c_target_id = t.c_target_id
      where a.c_observation_id = b.c_observation_id
      and t.c_existence = 'present'
      and (t.c_target_disposition = 'calibration'::e_target_disposition
       or t.c_target_disposition = 'science'::e_target_disposition)
    )
    where a.c_observation_id in (
      select c_observation_id
      from t_asterism_target
      where c_target_id = tid
    );
  ELSE
    -- We'll directly trigger ch_observation_edit
    FOR oid in 
      select c_observation_id
      from t_asterism_target
      where c_target_id = tid
    LOOP
      PERFORM pg_notify('ch_observation_edit', oid || ',' || NEW.c_program_id  || ',' || 'UPDATE');
    END LOOP;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

--------------------------------------------------------
-- Create a function to enforce immutability of target_disposition. This prevents a number of
-- model vialiations, including:
-- 1) A blind offset target being shared between observations
-- 2) Multiple blind offset targets in the same observation
-- 3) A calibration target being used in a science observation and vice versa
--------------------------------------------------------
CREATE OR REPLACE FUNCTION target_disposition_immutable()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_target_disposition != OLD.c_target_disposition 
  THEN
    RAISE EXCEPTION 'The disposition of a target cannot be changed.';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Create deferrable constraint trigger to enforce the constraint
CREATE CONSTRAINT TRIGGER trg_target_disposition_immutable
  AFTER INSERT OR UPDATE ON t_target
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW
  EXECUTE FUNCTION target_disposition_immutable();


--------------------------------------------------------
-- Create a function to enforce blind offset can only be used in one observation
--------------------------------------------------------
CREATE OR REPLACE FUNCTION check_blind_offset_not_shared()
RETURNS TRIGGER AS $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM t_target t
    WHERE t.c_target_id = NEW.c_target_id
    AND t.c_target_disposition = 'blind_offset'
  ) THEN
    IF EXISTS (
      SELECT 1 FROM t_asterism_target a
      JOIN t_target t ON a.c_target_id = t.c_target_id
      WHERE a.c_observation_id != NEW.c_observation_id
      AND a.c_target_id = NEW.c_target_id
    ) THEN
      RAISE EXCEPTION 'Blind offset targets cannot be sheared between observations';
    END IF;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Create deferrable constraint trigger to enforce the constraint
CREATE CONSTRAINT TRIGGER trg_blind_offset_not_shared
  AFTER INSERT OR UPDATE ON t_asterism_target
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW
  EXECUTE FUNCTION check_blind_offset_not_shared();

--------------------------------------------------------
-- Prevent a blind offset from existing without being assigned to an observation
--------------------------------------------------------
CREATE OR REPLACE FUNCTION delete_blind_offset_when_removed_from_asterism()
RETURNS TRIGGER AS $$
BEGIN
  DELETE FROM t_target WHERE c_target_id = OLD.c_target_id AND c_target_disposition = 'blind_offset';
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER trg_delete_blind_offset_when_removed_from_asterism
  AFTER DELETE ON t_asterism_target
  FOR EACH ROW
  EXECUTE FUNCTION delete_blind_offset_when_removed_from_asterism();

--------------------------------------------------------
-- If useBlindOffset is set to false, remove any blind offset target from the asterism
--------------------------------------------------------
CREATE OR REPLACE FUNCTION delete_move_blind_offset_when_use_flag_cleared()
RETURNS TRIGGER AS $$
BEGIN
  IF (NEW.c_use_blind_offset = false AND OLD.c_use_blind_offset = true) THEN
    DELETE FROM t_asterism_target
    WHERE c_observation_id = NEW.c_observation_id
      AND c_target_id IN (
	SELECT c_target_id FROM t_target WHERE c_target_disposition = 'blind_offset'
      );
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_delete_move_blind_offset_when_use_flag_cleared
  AFTER UPDATE OF c_use_blind_offset ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION delete_move_blind_offset_when_use_flag_cleared();

--------------------------------------------------------
-- If an observation 'existence' is changed, also change the existence of any blind offset target
--------------------------------------------------------
CREATE OR REPLACE FUNCTION blind_offset_existence_on_observation_existence()
RETURNS TRIGGER AS $$
BEGIN
  IF (NEW.c_existence != OLD.c_existence) THEN
    UPDATE t_target
    SET c_existence = NEW.c_existence
    WHERE c_target_disposition = 'blind_offset'
      AND c_target_id IN (
	SELECT c_target_id FROM t_asterism_target
	WHERE c_observation_id = NEW.c_observation_id
      );
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_blind_offset_existence_on_observation_existence
  AFTER UPDATE OF c_existence ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION blind_offset_existence_on_observation_existence();
