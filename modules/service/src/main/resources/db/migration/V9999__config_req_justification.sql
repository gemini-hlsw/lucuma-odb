
-- Configuration requests can have a justification
ALTER TABLE t_configuration_request
  ADD COLUMN c_justification text check (c_justification is null or length(c_justification) > 0);
 
-- re-create view
DROP VIEW v_configuration_request;
CREATE VIEW v_configuration_request AS
  SELECT 
    *,
    case when c_observing_mode_type = 'gmos_north_long_slit' then c_configuration_request_id end as c_gmos_north_longslit_id,
    case when c_observing_mode_type = 'gmos_south_long_slit' then c_configuration_request_id end as c_gmos_south_longslit_id
  FROM t_configuration_request;