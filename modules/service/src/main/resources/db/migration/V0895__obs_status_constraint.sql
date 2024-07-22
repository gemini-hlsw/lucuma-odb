-- Arbitrarily lower the observation status for all observations which do not
-- have a science band.
UPDATE t_observation
   SET c_status = 'for_review'::e_obs_status
 WHERE c_status >= 'ready'::e_obs_status AND c_science_band IS NULL;

-- An observation must be assigned a band if its status is ready or greater.
ALTER TABLE t_observation
  ADD CONSTRAINT obs_status_science_band CHECK (
    c_status < 'ready'::e_obs_status OR c_science_band IS NOT NULL
  );