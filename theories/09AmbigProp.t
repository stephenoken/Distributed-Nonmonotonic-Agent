% Ambiguity Propagation.

R1:  {} => a.
R2:  {} => b.
R3:  {} => c.
R4:  a => e.
R5:  b => d.
R6:  c => ~d.
R7:  d => ~e.
% > is empty.
% If +d e there is no ambiguity propagation.
% If +d e is not provable there is ambiguity propagation.