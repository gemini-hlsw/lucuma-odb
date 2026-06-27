-- Add the GNIRS IFU observing mode type. Isolated in its own migration because a new
-- enum value cannot be added and then used within the same transaction; the rest of the
-- GNIRS spectroscopy work (which references 'gnirs_ifu') follows in V1190.
ALTER TYPE e_observing_mode_type ADD VALUE 'gnirs_ifu';
