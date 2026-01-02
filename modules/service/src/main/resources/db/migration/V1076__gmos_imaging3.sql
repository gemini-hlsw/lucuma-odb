
-- A composite type for the filter + exposure time mode fields.  This makes
-- aggregating over filters + etm easier.

CREATE TYPE s_filter_exposure_time_mode AS (
  c_filter               d_tag,
  c_exposure_time_mode   e_exp_time_mode,
  c_signal_to_noise_at   d_wavelength_pm,
  c_signal_to_noise      numeric(11, 3),
  c_exposure_time        interval,
  c_exposure_count       integer
);

-- We'll change the asterism result so any cached json values must be dropped.
TRUNCATE TABLE t_itc_result;