Nonterminals path labels yeslabels nolabels edge.

Terminals integer identifier ';' ':' ',' '~~' '-->'.

Rootsymbol path.

path ->
    edge :
    {'$1', [], []}.

path ->
    edge ';' yeslabels :
    {'$1', '$3', []}.

path ->
    edge ';' yeslabels ':' nolabels :
   {'$1', '$3', '$5'}.

edge ->
    integer '-->' integer :
    {integer, _, From} = '$1',
    {integer, _, To} = '$3',
    {true, From, To}.

edge ->
    integer '~~' integer :
    {integer, _, From} = '$1',
    {integer, _, To} = '$3',
    {false, From, To}.

yeslabels ->
    labels :
    '$1'.

nolabels ->
    labels :
    '$1'.

labels ->
    '$empty' :
    [].    

labels ->
    identifier :
    {identifier, _, Label} = '$1',
    [Label].

labels ->
    identifier ',' labels :
    {identifier, _, Label} = '$1',
    [Label|'$3'].

Erlang code.
