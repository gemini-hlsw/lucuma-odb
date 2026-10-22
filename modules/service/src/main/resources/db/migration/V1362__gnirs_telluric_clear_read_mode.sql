-- sc-10966: GNIRS tellurics copied the science's explicit read mode, overriding the
-- one the ITC picks for the telluric's own exposure. The telluric reset now clears
-- it; this clears it on existing tellurics too. Visited tellurics keep the
-- configuration they were observed with, and user-defined ones may carry a read
-- mode the PI chose, so both are left alone.
UPDATE t_gnirs_spectroscopy g
   SET c_read_mode = NULL
  FROM t_observation o
 WHERE g.c_observation_id   = o.c_observation_id
   AND o.c_calibration_role = 'telluric'
   AND NOT o.c_is_user_defined_telluric
   AND g.c_read_mode IS NOT NULL
   AND NOT EXISTS (
     SELECT 1 FROM t_visit v WHERE v.c_observation_id = o.c_observation_id
   );
