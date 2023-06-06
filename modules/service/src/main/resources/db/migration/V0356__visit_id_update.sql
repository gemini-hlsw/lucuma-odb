--
-- Switch the Visit ID type to a Gid instead of UUID
--

-- 0) Truncate the (unused so far) tables that reference visit
TRUNCATE TABLE t_visit CASCADE;

-- 1) Drop foreign key references to t_visit(c_visit_id) and the columns themselves
ALTER TABLE t_gmos_north_static
  DROP CONSTRAINT t_gmos_north_static_c_observation_id_c_visit_id_key,
  DROP CONSTRAINT t_gmos_north_static_c_visit_id_fkey,
  DROP COLUMN c_visit_id;

ALTER TABLE t_gmos_south_static
  DROP CONSTRAINT t_gmos_south_static_c_observation_id_c_visit_id_key,
  DROP CONSTRAINT t_gmos_south_static_c_visit_id_fkey,
  DROP COLUMN c_visit_id;

 ALTER TABLE t_visit
  DROP COLUMN c_visit_id;

-- 2) Change the ID Type
ALTER DOMAIN d_visit_id
  DROP CONSTRAINT d_visit_id_check;

ALTER DOMAIN d_visit_id
  ADD CHECK (VALUE ~ '^v-[1-9a-f][0-9a-f]*$');

COMMENT ON DOMAIN d_visit_id IS 'ID for visits';

CREATE SEQUENCE s_visit_id START WITH 256;

-- 3) Add the c_visit_id column back.
ALTER TABLE t_visit
  ADD COLUMN c_visit_id d_visit_id PRIMARY KEY DEFAULT 'v-' || to_hex(nextval('s_visit_id'));

ALTER TABLE t_gmos_north_static
  ADD COLUMN c_visit_id d_visit_id NULL REFERENCES t_visit (c_visit_id),
  ADD UNIQUE NULLS NOT DISTINCT (c_observation_id, c_visit_id);

ALTER TABLE t_gmos_south_static
  ADD COLUMN c_visit_id d_visit_id NULL REFERENCES t_visit (c_visit_id),
  ADD UNIQUE NULLS NOT DISTINCT (c_observation_id, c_visit_id);