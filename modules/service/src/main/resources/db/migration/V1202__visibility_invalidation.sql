-- Target visibility change tracking (sc-9274)
--
-- External consumers (scheduler / visibility service) need to know which
-- observations and targets may require a visibility recalculation.

-- triggers stamp a "last invalidation" timestamp on the observations and targets
-- whose visibility-relevant inputs changed

-- DEFAULT now() seeds every existing row at migration time.
ALTER TABLE t_observation
  ADD COLUMN c_last_visibility_invalidation timestamp NOT NULL DEFAULT now();
ALTER TABLE t_target
  ADD COLUMN c_last_visibility_invalidation timestamp NOT NULL DEFAULT now();

-- We index to speed up sorting by last invalidation time
CREATE INDEX observation_last_visibility_index ON t_observation (c_last_visibility_invalidation);
CREATE INDEX target_last_visibility_index      ON t_target      (c_last_visibility_invalidation);

-------------------------------------------------------
-- OBSERVATION constraintsinvalidations.
-------------------------------------------------------

-- A change to a visibility-relevant constraint field on an observation stamps the
-- observation.
-- BEFORE UPDATE so the stamp rides the same row write (no extra write)
-- the WHEN clause restricts firing to actual value changes.
CREATE OR REPLACE FUNCTION visibility_observation_stamp()
RETURNS TRIGGER AS $$
BEGIN
  NEW.c_last_visibility_invalidation := now();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER visibility_observation_trigger
  BEFORE UPDATE OF c_cloud_extinction, c_image_quality, c_sky_background, c_water_vapor,
                   c_air_mass_min, c_air_mass_max, c_hour_angle_min, c_hour_angle_max
  ON t_observation
  FOR EACH ROW
  WHEN (
       NEW.c_cloud_extinction IS DISTINCT FROM OLD.c_cloud_extinction
    OR NEW.c_image_quality    IS DISTINCT FROM OLD.c_image_quality
    OR NEW.c_sky_background   IS DISTINCT FROM OLD.c_sky_background
    OR NEW.c_water_vapor      IS DISTINCT FROM OLD.c_water_vapor
    OR NEW.c_air_mass_min     IS DISTINCT FROM OLD.c_air_mass_min
    OR NEW.c_air_mass_max     IS DISTINCT FROM OLD.c_air_mass_max
    OR NEW.c_hour_angle_min   IS DISTINCT FROM OLD.c_hour_angle_min
    OR NEW.c_hour_angle_max   IS DISTINCT FROM OLD.c_hour_angle_max
  )
  EXECUTE FUNCTION visibility_observation_stamp();

-------------------------------------------------------
-- OBSERVATION timing windows visibility invalidations.
-------------------------------------------------------

-- A change to any timing window stamps its observation.
CREATE OR REPLACE FUNCTION visibility_timing_window_stamp()
RETURNS TRIGGER AS $$
BEGIN
  UPDATE t_observation
  SET c_last_visibility_invalidation = now()
  WHERE c_observation_id = COALESCE(NEW.c_observation_id, OLD.c_observation_id);
  RETURN COALESCE(NEW, OLD);
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER visibility_timing_window_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_timing_window
  FOR EACH ROW
  EXECUTE FUNCTION visibility_timing_window_stamp();

-------------------------------------------------------
-- PROGRAM active peridod visibility invalidations.
-------------------------------------------------------
-- A change to a program's active period stamps every observation of that program.
CREATE OR REPLACE FUNCTION visibility_program_stamp()
RETURNS TRIGGER AS $$
BEGIN
  UPDATE t_observation
  SET c_last_visibility_invalidation = now()
  WHERE c_program_id = NEW.c_program_id;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER visibility_program_trigger
  AFTER UPDATE OF c_active_start, c_active_end ON t_program
  FOR EACH ROW
  WHEN (NEW.c_active_start IS DISTINCT FROM OLD.c_active_start
     OR NEW.c_active_end   IS DISTINCT FROM OLD.c_active_end)
  EXECUTE FUNCTION visibility_program_stamp();

----------------------------------
-- TARGET visibility invalidations.
----------------------------------

-- A change to a visibility-relevant target field stamps the target.  BEFORE
-- UPDATE, same reasoning as above.
CREATE OR REPLACE FUNCTION visibility_target_stamp()
RETURNS TRIGGER AS $$
BEGIN
  NEW.c_last_visibility_invalidation := now();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER visibility_target_trigger
  BEFORE UPDATE OF c_sid_ra, c_sid_dec, c_sid_pm_ra, c_sid_pm_dec, c_sid_epoch,
                   c_sid_rv, c_sid_parallax,
                   c_nsid_des, c_nsid_key_type, c_nsid_key
  ON t_target
  FOR EACH ROW
  WHEN (
       NEW.c_sid_ra        IS DISTINCT FROM OLD.c_sid_ra
    OR NEW.c_sid_dec       IS DISTINCT FROM OLD.c_sid_dec
    OR NEW.c_sid_pm_ra     IS DISTINCT FROM OLD.c_sid_pm_ra
    OR NEW.c_sid_pm_dec    IS DISTINCT FROM OLD.c_sid_pm_dec
    OR NEW.c_sid_epoch     IS DISTINCT FROM OLD.c_sid_epoch
    OR NEW.c_sid_rv        IS DISTINCT FROM OLD.c_sid_rv
    OR NEW.c_sid_parallax  IS DISTINCT FROM OLD.c_sid_parallax
    OR NEW.c_nsid_des      IS DISTINCT FROM OLD.c_nsid_des
    OR NEW.c_nsid_key_type IS DISTINCT FROM OLD.c_nsid_key_type
    OR NEW.c_nsid_key      IS DISTINCT FROM OLD.c_nsid_key
  )
  EXECUTE FUNCTION visibility_target_stamp();

