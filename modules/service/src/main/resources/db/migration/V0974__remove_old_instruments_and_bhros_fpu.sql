-- This migration removes old instruments and BHROS FPU

DO LANGUAGE plpgsql $$DECLARE to_delete d_tag[];    
BEGIN    
  to_delete := array['Bhros', 'Michelle', 'Nici', 'Nifs', 'Phoenix', 'Trecs'];    

  -- t_cfp_instrument has ON DELETE CASCADE
  -- t_gmos_north_longslit and t_gmos_south_longslit are constrained to their instruments

  UPDATE t_observation SET c_instrument = 'GmosNorth' WHERE c_instrument = any(to_delete);
  
  UPDATE t_program SET c_instrument = 'GmosNorth' WHERE c_instrument = any(to_delete);  

  DELETE FROM t_spectroscopy_config_option WHERE c_instrument = any(to_delete);
  DELETE FROM t_time_estimate WHERE c_instrument = any(to_delete);
  DELETE FROM t_gcal WHERE c_instrument = any(to_delete);
  DELETE FROM t_instrument WHERE c_tag = any(to_delete);    
    
END$$;

DELETE FROM t_gmos_south_dynamic WHERE c_fpu_builtin = 'Bhros';

UPDATE t_gmos_south_long_slit
  SET c_fpu = 'Ns1',
      c_initial_fpu = 'Ns1'
  WHERE c_fpu = 'Bhros'
    OR  c_initial_fpu = 'Bhros';

DELETE FROm t_smart_gmos_south WHERE c_fpu = 'Bhros';
DELETE FROM t_gmos_south_fpu WHERE c_tag = 'Bhros';
