% What colour is Fred

fr. % I am looking at Fred

{r} -> ~b.
{b} -> ~r.

R1: {fr} => r. % Fred appears to be red

% I look at Fred later on
R3: {later} => ~r.
R4: {later} ~> b.
R5: {fr} => later. % appears to be no longer red

% Meryl tells me that I am wearing I_am_wearing_blue_tinted_glasses
R6: {glasses} ~> ~later. % Fred might not be blue
R7: {fr} => glasses.
