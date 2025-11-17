-- Going forward, we want every imaging filter to be associated with a unique
-- science ETM.
ALTER TABLE t_gmos_north_imaging_filter
  ALTER COLUMN c_exposure_time_mode_id SET NOT NULL,
  ADD CONSTRAINT t_gmos_north_imaging_filter_unique_exposure_time_mode_id
    UNIQUE (c_exposure_time_mode_id);

ALTER TABLE t_gmos_south_imaging_filter
  ALTER COLUMN c_exposure_time_mode_id SET NOT NULL,
  ADD CONSTRAINT t_gmos_south_imaging_filter_unique_exposure_time_mode_id
    UNIQUE (c_exposure_time_mode_id);