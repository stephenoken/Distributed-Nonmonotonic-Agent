% Rocks1.

R1: {p} -> q.   % p is a subclass of the class of rocks q.
R2: {} => p.    % Test1 indicates the sample of rock is a p-type rock.
R3: {} => ~q.   % Test2 indicates the sample of rock is not even a q-type rock.
% > is empty.
% We should have -d p, -d ~p, -d q, and -d ~q