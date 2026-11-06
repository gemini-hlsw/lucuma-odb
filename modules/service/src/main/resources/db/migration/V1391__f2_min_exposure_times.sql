-- Flamingos 2 minimum exposure times per read mode, as raised in OCS (REL-4503).
update t_f2_read_mode set c_min_exposure_time = '2 second'  where c_tag = 'Bright';
update t_f2_read_mode set c_min_exposure_time = '8 second'  where c_tag = 'Medium';
update t_f2_read_mode set c_min_exposure_time = '14 second' where c_tag = 'Faint';
