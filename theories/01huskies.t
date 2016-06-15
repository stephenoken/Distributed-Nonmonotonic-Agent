% Siberian huskies.

     sh.              % Nanook is a Siberian husky.

R1:  {sh} -> d.       % Siberian huskies are dogs.
R2:  {sh} => ~b.      % Siberian huskies usually do not bark.
R3:  {d} => b.        % Dogs usually bark.

R2 > R3.              % R2 is more specific than R3.

% Defeasibly, Nanook should not bark. That is, +d ~b
