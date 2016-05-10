% Pennsylvania-Dutch.

    nspd.             % Hans is a native speaker of Pennsylvania-Dutch.

R1: {bp} -> busa.     % People born in Pennsylvania are born in the USA.
R2: {nspd} -> nsg.    % Native speakers of Pennsylvania-Dutch are native 
                      % speakers of German.
R3: {nspd} => bp.     % Native speakers of Pennsylvania-Dutch are usually born 
                      % in Pennsylvania.
R4: {nsg} => ~busa.   % Native speakers of German are usually not born in the 
                      % USA.

R1 > R4.  % If strict rules beat non-strict rules.

% Hans should be born in the USA, +d busa