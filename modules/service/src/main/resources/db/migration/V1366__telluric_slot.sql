-- A telluric's place relative to its science, before or after, is its slot.
-- "Order" read as a sequence or a request; lucuma-core renamed the enum to
-- TelluricSlot and the database follows.

ALTER TYPE e_telluric_calibration_order RENAME TO e_telluric_slot;

ALTER TABLE t_telluric_resolution
  RENAME COLUMN c_calibration_order TO c_slot;
