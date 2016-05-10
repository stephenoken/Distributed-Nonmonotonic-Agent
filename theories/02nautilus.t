% Nautiluses.

    n.              % Nancy is a nautilus.
R1: {n} -> c.       % Nautiluses are cephalopods.
R2: {c} -> m.       % Cephalopods are molluscs.
R3: {n} => sh.      % Nautiluses usually have shells.
R4: {c} => ~sh.     % Cephalopods usually do not have shells.
R5: {m} => sh.      % Molluscs usually have shells.

R3 > R4.  % R3 is more specific than R4.
R4 > R5.  % R4 is more specific than R5.

% Nancy should have a shell, +d sh