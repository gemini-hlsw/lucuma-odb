ALTER TYPE e_time_charge_discount_type
  ADD VALUE 'overlap' AFTER 'nodata';

CREATE TABLE t_time_charge_discount_overlap (
  c_discount_id    int8             NOT NULL REFERENCES t_time_charge_discount(c_id)    ON DELETE CASCADE,
  c_observation_id d_observation_id NOT NULL REFERENCES t_observation(c_observation_id) ON DELETE CASCADE,

  PRIMARY KEY (c_discount_id)
);