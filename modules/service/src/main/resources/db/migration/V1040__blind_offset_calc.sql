-- Add blind offset columns to existing t_obscalc table
ALTER TABLE t_obscalc ADD COLUMN c_blind_offset_state e_calculation_state DEFAULT 'pending';
ALTER TABLE t_obscalc ADD COLUMN c_blind_offset_manual_override boolean DEFAULT false;

-- Add trigger for c_use_blind_offset flag changes
CREATE OR REPLACE FUNCTION blind_offset_flag_invalidate()
  RETURNS TRIGGER AS $$
BEGIN
  IF (OLD.c_use_blind_offset IS DISTINCT FROM NEW.c_use_blind_offset) THEN
    UPDATE t_obscalc
    SET c_blind_offset_state = 'pending'::e_calculation_state,
        c_blind_offset_manual_override = CASE
          WHEN NEW.c_use_blind_offset = false THEN false
          ELSE c_blind_offset_manual_override
        END
    WHERE c_observation_id = NEW.c_observation_id;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER blind_offset_flag_invalidate_trigger
  AFTER UPDATE OF c_use_blind_offset ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION blind_offset_flag_invalidate();

-- Add trigger for observation time changes
CREATE OR REPLACE FUNCTION obs_time_blind_offset_invalidate()
  RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_use_blind_offset = true AND
     (OLD.c_observation_time IS DISTINCT FROM NEW.c_observation_time) THEN
    UPDATE t_obscalc
    SET c_blind_offset_state = 'pending'::e_calculation_state
    WHERE c_observation_id = NEW.c_observation_id
      AND c_blind_offset_manual_override = false;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER obs_time_blind_offset_invalidate_trigger
  AFTER UPDATE OF c_observation_time ON t_observation
  FOR EACH ROW
  EXECUTE FUNCTION obs_time_blind_offset_invalidate();

-- Add trigger for asterism changes (base position changes)
CREATE OR REPLACE FUNCTION asterism_blind_offset_invalidate()
  RETURNS TRIGGER AS $$
DECLARE
  uses_blind_offset boolean;
BEGIN
  SELECT c_use_blind_offset INTO uses_blind_offset
  FROM t_observation
  WHERE c_observation_id = COALESCE(NEW.c_observation_id, OLD.c_observation_id);

  IF uses_blind_offset = true THEN
    UPDATE t_obscalc
    SET c_blind_offset_state = 'pending'::e_calculation_state
    WHERE c_observation_id = COALESCE(NEW.c_observation_id, OLD.c_observation_id)
      AND c_blind_offset_manual_override = false;
  END IF;
  RETURN COALESCE(NEW, OLD);
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER asterism_blind_offset_invalidate_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_asterism_target
  FOR EACH ROW
  EXECUTE FUNCTION asterism_blind_offset_invalidate();

-- Extend existing target_invalidate function to also invalidate blind offset
CREATE OR REPLACE FUNCTION target_invalidate()
  RETURNS TRIGGER AS $$
DECLARE
  target record;
  obs_id d_observation_id;
  uses_blind_offset boolean;
BEGIN
  target := COALESCE(NEW, OLD);
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    FOR obs_id IN
      SELECT c_observation_id
        FROM t_asterism_target
       WHERE c_program_id = target.c_program_id
         AND c_target_id  = target.c_target_id
    LOOP
      CALL invalidate_obscalc(obs_id);

      SELECT c_use_blind_offset INTO uses_blind_offset
      FROM t_observation WHERE c_observation_id = obs_id;

      IF uses_blind_offset = true THEN
        UPDATE t_obscalc
        SET c_blind_offset_state = 'pending'::e_calculation_state
        WHERE c_observation_id = obs_id
          AND c_blind_offset_manual_override = false;
      END IF;
    END LOOP;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Initialize blind offset state for existing observations that use blind offset
UPDATE t_obscalc
SET c_blind_offset_state = 'pending'::e_calculation_state
FROM t_observation o
WHERE t_obscalc.c_observation_id = o.c_observation_id
  AND o.c_use_blind_offset = true;