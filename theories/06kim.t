% Kim Bassinger.

     ms.                % Kim Bassinger is a movie star.
     s.                 % Kim Bassinger is a southerner.

R1:  {ms} => r.         % Movie stars are usually rich.
R2:  {r} => rep.        % Rich people are usually republicans.
R3:  {ms} => ~rep.      % Movie stars are usually nor republicans.
R4:  {s} => c.          % Southerners are usually conservative.
R5:  {c} => rep.        % Conservatives are usually republicans.
R6:  {s} => ~rep.       % Southerners are usually not republicans.

R3 > R2.  % R3 is more specific than R2.
R6 > R5.  % R6 is more specific than R5.

% Kim should not be a republican, +d ~rep