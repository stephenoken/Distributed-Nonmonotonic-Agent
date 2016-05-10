% Nixon's political activity.

     qu.             % Nixon is a quaker.
     r.              % Nixon is a republican.

R3:  {qu} => d.      % Quakers are usually doves.
R4:  {r} => h.       % Republicans are usually hawks.
R5:  {d} => ~h.      % Doves are usually not hawks.
R6:  {h} => ~d.      % Hawks are usually not doves.
R7:  {d} => pa.      % Doves are usually politically active.
R8:  {h} => pa.      % Hawks are usually politically active.

% > is empty.
 
% Intuitions disagree about whether Nixon should be politically
% active, +d pa, or not. Defeasible logic says no. Plausible
% logic says yes.
