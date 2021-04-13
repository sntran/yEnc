% Guard to check if a character, when encoded, becomes critical.
-define(critical(X),
        X =:= (256 - 42 + $\x00); % 214: NULL
        X =:= (256 - 42 + $\n); % 224: LF
        X =:= (256 - 42 + $\r); % 227: CR
        X =:= ($= - 42) % 19: =
).

% The generic formulas.
-define(ENCODE(X), (X + 42) rem 256).
-define(DECODE(X), X - 42 + 256).
