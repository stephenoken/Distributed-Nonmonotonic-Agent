% Genetically altered penguins.

     gap.             % We have a genetically altered penguin.

R1:  {p} -> b.        % Penguins are birds.
R2:  {gap} -> p.      % A genetically altered penguin is still a penguin.
R3:  {b} => f.        % Birds usually fly.
R4:  {p} => ~f.       % Penguins usually do not fly.
R5:  {gap} ~> f.      % Genetically altered penguins might fly.

R5 > R4.  % R5 is more specific than R4.
R4 > R3.  % R4 is more specific than R3.

% We don't know if genetically altered penguins fly or not.
% That is, -d f, and -d ~f