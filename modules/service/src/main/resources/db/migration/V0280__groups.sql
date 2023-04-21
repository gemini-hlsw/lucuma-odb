
create domain d_group_id as varchar; -- TODO format check
comment on domain d_group_id is 'GID type for observation group.';

create sequence s_group_id start with 256; -- three hex digits

create table t_group (

  -- every group has a unique id
  c_group_id          d_group_id        not null primary key default 'g-' || to_hex(nextval('s_group_id')),

  -- every group belongs to a program
  c_program_id        d_program_id      not null references t_program(c_program_id),

  -- a group *may* have a parent group, and always has an index (within group or at the root)
  c_parent_id         d_group_id        null,
  c_parent_index      int2              not null,

  -- parent group (if any) must be in the same program
  unique (c_program_id, c_group_id), -- obviously
  foreign key (c_program_id, c_parent_id) references t_group (c_program_id, c_group_id),

  -- optional properties
  c_name              text              null default null,
  c_description       text              null default null,
  c_min_required      int2              null default null,
  c_ordered           bool              not null default false,
  c_min_interval      interval          null default null,
  c_max_interval      interval          null default null,

  -- min < max if both are defined
  check (num_nulls(c_min_interval, c_max_interval) > 0 OR c_min_interval <= c_max_interval)

);

-- Add a group columns to observations.
alter table t_observation
  add column c_group_id d_group_id null,
  add c_group_index int2 null, -- we'll make this non-null after updating existing observations
  add foreign key (c_program_id, c_group_id) references t_group (c_program_id, c_group_id);

-- Number existing observations by obs id, starting at zero within each program
with helper as (
  select
    c_program_id p_id,
    c_observation_id o_id,
    row_number() over (partition by c_program_id order by c_observation_id) idx
  from t_observation
  order by c_program_id
)
update t_observation
set c_group_index = helper.idx
from helper
where c_program_id = helper.p_id and c_observation_id = helper.o_id;

-- Now make observation index non-null
alter table t_observation alter column c_group_index set not null;

-- Re-create this view to include the new columns.
drop view v_observation;
create view v_observation as
  select *,
  case when c_explicit_ra              is not null then c_observation_id end as c_explicit_base_id,
  case when c_air_mass_min             is not null then c_observation_id end as c_air_mass_id,
  case when c_hour_angle_min           is not null then c_observation_id end as c_hour_angle_id,
  case when c_observing_mode_type      is not null then c_observation_id end as c_observing_mode_id,
  case when c_spec_wavelength          is not null then c_observation_id end as c_spec_wavelength_id,          
  case when c_spec_signal_to_noise_at  is not null then c_observation_id end as c_spec_signal_to_noise_at_id,  
  case when c_spec_wavelength_coverage is not null then c_observation_id end as c_spec_wavelength_coverage_id, 
  case when c_spec_focal_plane_angle   is not null then c_observation_id end as c_spec_focal_plane_angle_id    
  from t_observation;

-- Raise an error if the specified program's group structure contains a cycle.
CREATE OR REPLACE PROCEDURE group_verify_acyclic(pid d_program_id) AS $$
DECLARE
  g0 d_group_id;
  g1 d_group_id;
BEGIN
  -- Compute the paths of all groups in the specified program and detect cycles, both via the 
  -- built-in CYCLE syntax. Select the endpoints of the first such cycle into g0 and g1.
  WITH RECURSIVE child AS (
    SELECT c_group_id, c_parent_id
    FROM   t_group
    WHERE  c_program_id = pid
    UNION
    SELECT g.c_group_id, g.c_parent_id
    FROM  t_group g      
    INNER JOIN child c ON g.c_group_id = c.c_parent_id
    WHERE g.c_program_id = pid
  ) CYCLE c_group_id SET c_cyclic USING c_path
  SELECT c_group_id, c_parent_id
  INTO g0, g1
  from child WHERE c_cyclic;
  -- If we found a cycle then raise an exception.
  IF FOUND THEN
    RAISE EXCEPTION 'Cycle detected in group structure between % and %.', g0, g1;
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Is the given array consecutive from zero?
CREATE OR REPLACE FUNCTION starts_at_zero_and_is_consecutive(arr int[]) RETURNS BOOLEAN AS $$
DECLARE
  curr_idx int := 0;
  i int;
BEGIN
  FOREACH i IN ARRAY arr LOOP
    IF i != curr_idx THEN
      RETURN FALSE;
    END IF;
    curr_idx := curr_idx + 1;
  END LOOP;
  RETURN TRUE;
END;
$$ LANGUAGE plpgsql;

-- Raise an error if the specified program contains a group with non-consecutive indices.
CREATE OR REPLACE PROCEDURE group_verify_indices(pid d_program_id) AS $$
DECLARE
  g d_group_id;
BEGIN
  WITH indices AS (
    SELECT c_parent_id AS c_group_id, c_parent_index as c_index
    FROM   t_group
    WHERE  c_program_id = pid
    UNION
    SELECT c_group_id, c_group_index
    FROM   t_observation
    WHERE  c_program_id = pid
  )
  SELECT
    c_group_id
  INTO g
  FROM
    indices
  GROUP BY
    c_group_id
  HAVING starts_at_zero_and_is_consecutive(array_agg(c_index order by c_index)) = false;
  IF FOUND THEN
    IF g IS NULL THEN
      RAISE EXCEPTION 'Index discontinuity detected in the top-level group.';
    ELSE
      RAISE EXCEPTION 'Index discontinuity detected in group %.', g;
    END IF;
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Raise an error if the specified program contains cyclic groups or a group with non-consecutive indices.
CREATE OR REPLACE PROCEDURE group_verify(pid d_program_id) AS $$
BEGIN
  CALL group_verify_acyclic(pid);
  CALL group_verify_indices(pid);
END;
$$ LANGUAGE plpgsql;

-- Create a trigger function to check group constraints for t_group
CREATE OR REPLACE FUNCTION t_group_trigger()
RETURNS TRIGGER AS $$
BEGIN
  -- If it's an update ...
  IF (TG_OP = 'UPDATE') THEN 
    IF (OLD.c_program_id is distinct from NEW.c_program_id) THEN
      -- should never happen, but check anyway
      CALL group_verify(OLD.c_program_id);
    END IF;
    CALL group_verify(NEW.c_program_id);
  ELSIF (TG_OP = 'DELETE') THEN CALL group_verify(OLD.c_program_id);
  ELSIF (TG_OP = 'INSERT') THEN CALL group_verify(NEW.c_program_id);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Call this trigger function on any group modification that can affect the structure.
CREATE CONSTRAINT TRIGGER group_trigger_groups
AFTER INSERT OR DELETE OR UPDATE OF c_program_id, c_parent_id, c_parent_index ON t_group
DEFERRABLE
FOR EACH ROW
EXECUTE FUNCTION t_group_trigger();

-- TODO: Call this trigger function on any obs modification that can affect the group structure.
CREATE CONSTRAINT TRIGGER group_trigger_observations
AFTER INSERT OR DELETE OR UPDATE OF c_program_id, c_grouo_id, c_group_index ON t_observation
DEFERRABLE
FOR EACH ROW
EXECUTE FUNCTION t_group_trigger();

-- Shuffle things forward to open a hole at the specified group+index. Used for insert and move.
-- Constraints must be deferred when calling.
CREATE OR REPLACE PROCEDURE group_open_hole(gid d_group_id, i int2) AS $$
BEGIN

  -- Shuffle groups forward as needed, keeping in mind that gid may be null
  UPDATE t_group
  SET c_parent_index = c_parent_index + 1
  WHERE c_parent_id IS NOT DISTINCT FROM gid 
  AND c_parent_index >= i;

  -- Shuffle observations forward as needed
  UPDATE t_observation
  SET c_group_index = c_group_index + 1
  WHERE c_group_id = gid
  AND c_group_index >= i;

END;
$$ LANGUAGE plpgsql;

-- Shuffle things back to close a hole at the specified group+index. Used for move and delete.
-- Constraints must be deferred when calling.
CREATE OR REPLACE PROCEDURE group_close_hole(gid d_group_id, i int2) AS $$
BEGIN

  -- Shuffle groups back as needed, keeping in mind that gid may be null
  UPDATE t_group
  SET c_parent_index = c_parent_index - 1
  WHERE c_parent_id IS NOT DISTINCT FROM gid 
  AND c_parent_index > i;

  -- Shuffle observations back as needed
  UPDATE t_observation
  SET c_group_index = c_group_index - 1
  WHERE c_group_id = gid
  AND c_group_index > i;

END;
$$ LANGUAGE plpgsql;

-- Move a group. Constraints must be deferred when calling.
CREATE OR REPLACE PROCEDURE group_move_group(gid d_group_id, dest d_group_id, dest_index int2) AS $$
DECLARE
  src d_group_id;
  src_index int2;
BEGIN

  -- Get the current location
  SELECT c_parent_id, c_parent_index
  INTO   src, src_index
  FROM   t_group
  WHERE  c_group_id = gid;

  -- Punt if not found
  IF NOT FOUND THEN
    RAISE EXCEPTION 'Group % was not found.', gid;
  END IF;
  
  -- Move it out of the way
  UPDATE t_group
  SET    c_parent_index = -1
  WHERE  c_group_id = gid;

  -- Close the hole where used to be
  CALL group_close_hole(src, src_index);

  -- Open a hole where we're going.
  CALL group_open_hole(dest, dest_index);

  -- And put the group in it.
  UPDATE t_group
  SET    c_parent_id = dest, c_parent_index = dest_index
  WHERE  c_group_id = gid;

END;
$$ LANGUAGE plpgsql;

-- Move an observation. Constraints must be deferred when calling.
CREATE OR REPLACE PROCEDURE group_move_observation(oid d_observation_id, dest d_group_id, dest_index int2) AS $$
DECLARE
  src d_group_id;
  src_index int2;
BEGIN

  -- Get the current location
  SELECT c_group_id, c_group_index
  INTO   src, src_index
  FROM   t_observation
  WHERE  c_observation_id = oid;

  -- Punt if not found
  IF NOT FOUND THEN
    RAISE EXCEPTION 'Observation % was not found.', gid;
  END IF;
  
  -- Clean up old group location, if any
  IF src IS NOT NULL THEN

    -- Move it out of the way
    UPDATE t_observation
    SET    c_group_id = null, c_group_index = -1
    WHERE  c_observation_id = oid;

    -- Close the hole where used to be
    CALL group_close_hole(src, src_index);

  END IF;

  -- Open a hole where we're going.
  CALL group_open_hole(dest, dest_index);

  -- And put the observation in it.
    UPDATE t_observation
    SET    c_group_id = dest, c_group_index = dest_index
    WHERE  c_observation_id = oid;

END;
$$ LANGUAGE plpgsql;

create view v_group as
  select *,
  case when c_min_interval is not null then c_group_id end as c_min_interval_id,
  case when c_max_interval is not null then c_group_id end as c_max_interval_id
  from t_group;

create view v_group_element as
  select 
    c_program_id,
    c_parent_id as c_group_id, 
    c_parent_index as c_index, 
    c_group_id as c_child_group_id,
    null::d_observation_id as c_child_observation_id
  from t_group
  union
  select c_program_id, c_group_id, c_group_index, null, c_observation_id 
  from t_observation
  where c_group_id is not null;

