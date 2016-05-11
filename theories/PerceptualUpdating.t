% What colour is Fred

fr. % I am looking at Fred

{r} -> ~b.
{b} -> ~r.

R1: {fr} => r. % Fred appears to be red

% I look at Fred later on
{later} => ~r.
{later} => b.
R2: {fr} => later. % appears to be no longer red

% Meryl tells me that I am wearing I_am_wearing_blue_tinted_glasses
{glasses} => ~later.
R3: {fr} => glasses.
