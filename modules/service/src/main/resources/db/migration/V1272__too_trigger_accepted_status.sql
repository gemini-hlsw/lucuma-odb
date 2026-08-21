-- Adds the 'accepted' trigger status.
--
-- Part 1 of 2 (V1273 uses the status).

ALTER TYPE e_too_trigger_status ADD VALUE 'accepted' AFTER 'requested';