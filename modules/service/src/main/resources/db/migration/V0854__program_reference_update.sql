-- Add the new types to the program type enum
ALTER TYPE e_program_type ADD 'commissioning' BEFORE 'engineering';
ALTER TYPE e_program_type ADD 'monitoring'    BEFORE 'science';