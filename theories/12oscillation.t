% Oscillation.

R1:  {} => a.
R2:  {a1} => ~a.
R3:  {} => a1.
R4:  {a2} => ~a1.
R5:  {} => a2.
R6:  {a3} => ~a2.
R7:  {} => a3.
R8:  {a4} => ~a3.
R9:  {} => a4.

R4 > R3.

% With  R8 > R7  "a" should be defeasibly provable.
% Without  R8 > R7  "a" should be defeasibly provable
%  with no ambiguity propagation.
% Without  R8 > R7  "a" should not be defeasibly provable
%  with ambiguity propagation.