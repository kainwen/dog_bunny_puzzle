Definitions.

L = [a-zA-Z]
D = [0-9]

Rules.

{D}                      : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
[1-9]{D}*                : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.

{L}({L}|{D}|_)*          : {token, {identifier, TokenLine, list_to_atom(TokenChars)}}.


;                        : {token, {';', TokenLine}}.
:                        : {token, {':', TokenLine}}.
,                        : {token, {',', TokenLine}}.
-->                      : {token, {'-->',TokenLine}}.
~~                       : {token, {'~~',TokenLine}}.


Erlang code.
