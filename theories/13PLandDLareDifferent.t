% PLandDLareDifferent.

R1: {p}  -> q.
R2: {~q} -> ~p.
R3: {}   => p.
R4: {}   => ~q.
R5: {}   => c.
R6: {p}  => ~c.
% > is empty.
% Plausible Logic can prove  -dp, -d~p, -dq, -d~q, -d~c, -dac, and +dc.
% Defeasible Logic can prove      -d~p, -dq,       -d~c,
% but cannot prove           -dp,            -d~q,          -dc or +dc.