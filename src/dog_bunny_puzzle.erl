%%%-----------------------------------------------------------------------------
%%% @doc The problem to solve the dog bunny puzzle: https://www.dogbunnypuzzle.com/.
%%%
%%% The algorithm is based on BFS on state transform graph. 
%%% @author Zhenghua Lyu
%%% @end
%%%-----------------------------------------------------------------------------
-module(dog_bunny_puzzle).

-include_lib("eunit/include/eunit.hrl").

-define(DOG, 0).
-define(BUNNY1, 1).
-define(BUNNY2, 2).

-export([solve/1]).

%% Part 1 types
-type graph()::digraph:graph().
-type raw_path():: {{boolean(), integer(), integer()}, [atom()], [atom()]}.
-type path()::{integer(), integer(), [atom()], [atom()]}.
%% Part 2 types
-type distribution()::array:array(integer()).
%% Part 3 types
-type state_trans_graph()::digraph:graph().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Part 1: the puzzle's graph model
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Interactively create the puzzle graph.
%% -spec load_puzzle() -> {graph(), integer(), integer()}.
%% load_puzzle() ->
%%     load_puzzle_common(standard_io).

-spec load_puzzle(string()) -> {graph(), integer(), integer()}.
load_puzzle(File) ->
    {ok, Device} = file:open(File, [read]),
    PG = load_puzzle_common(Device),
    ok = file:close(Device),
    PG.

%% Get all locations
-spec get_all_locations(graph()) -> [integer()].
get_all_locations(PuzzleGraph) ->
    digraph:vertices(PuzzleGraph).

%% Get location icon
-spec get_location_icon(graph(), integer()) -> [atom()].
get_location_icon(PuzzleGraph, Location) ->
    case digraph:vertex(PuzzleGraph, Location) of
	false ->
	    erlang:error({"location not exists", Location});
	{_, Icons} -> Icons
    end.

%% Get location's adj location
-spec get_adj_paths(graph(), integer()) -> [path()].
get_adj_paths(PuzzleGraph, Location) ->
    Es = digraph:out_edges(PuzzleGraph, Location),
    Es1 = [digraph:edge(PuzzleGraph, E) || E <- Es],
    [{From, To, YesLabels, NoLabels}
     || {_, From, To, {YesLabels, NoLabels}} <- Es1].

%% Internal help functions for load_puzzle
-spec load_puzzle_common(io:device()) -> {graph(), integer(), integer()}.
load_puzzle_common(Device) ->
    {ok, [NLocations]} = io:fread(Device, 
				  "Please enter the number of all locations: ", "~d"),
    PuzzleGraph = digraph:new([cyclic, private]),
    ok = load_locations(NLocations, PuzzleGraph, Device),
    {ok, [NPaths]} = io:fread(Device, "Please enter the number of all paths: ", "~d"),
    ok = load_paths(NPaths, PuzzleGraph, Device),
    {ok, Start} = io:fread(Device, "Please enter the start distribution (x,x,x): ", "~d,~d,~d"),
    {ok, End} = io:fread(Device, "Please enter the end distribution (x,x,x): ", "~d,~d,~d"),
    {PuzzleGraph, 
     get_state_id_from_distribution(array:from_list(Start), NLocations),
     get_state_id_from_distribution(array:from_list(End), NLocations)}.

%% Internal helper function for loading vertices of puzzle graph.
-spec load_locations(integer(), graph(), io:device()) -> ok.
load_locations(NLocations, PuzzleGraph, Device) ->
    load_locations(NLocations, PuzzleGraph, Device, 0).

-spec load_locations(integer(), graph(), io:device(), integer()) -> ok.
load_locations(NLocations, _PuzzleGraph, _Device, N) when N =:= NLocations -> ok;
load_locations(NLocations, PuzzleGraph, Device, N) ->
    {ok, [Icon]} = io:fread(Device, 
			    "enter the next location's icon (null for no icon): ", "~a"),
    case Icon of
	null ->
	    digraph:add_vertex(PuzzleGraph, N);
	_ ->
	    digraph:add_vertex(PuzzleGraph, N, Icon)
    end,
    load_locations(NLocations, PuzzleGraph, Device, N+1).

%% Internal helper function for loading paths(edges) of puzzle graph.
%% Here I use yecc and leex to parse the syntax.
-spec load_paths(integer(), graph(), io:device()) -> ok.
load_paths(Npaths, PuzzleGraph, Device) ->
    load_paths(Npaths, PuzzleGraph, Device, 0).

-spec load_paths(integer(), graph(), io:device(), integer()) -> ok.
load_paths(NPaths, _PuzzleGraph, _Device, N) when NPaths =:= N -> ok;
load_paths(NPaths, PuzzleGraph, Device, N) ->
    {ok, [PathStr]} = io:fread(Device, "enter the next path:", "~s"),
    {{IsDirected, From, To}, YesLabels, NoLabels} = parse_path(PathStr),
    case IsDirected of
	true ->
	    digraph:add_edge(PuzzleGraph, From, To, {YesLabels, NoLabels});
	false ->
	    digraph:add_edge(PuzzleGraph, From, To, {YesLabels, NoLabels}),
	    digraph:add_edge(PuzzleGraph, To, From, {YesLabels, NoLabels})
    end,
    load_paths(NPaths, PuzzleGraph, Device, N+1).

-spec parse_path(string) -> raw_path().
parse_path(PathStr) ->
    {ok, Toks, _} = path_tok:string(PathStr),
    {ok, Path} = path_grammar:parse(Toks),
    Path.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Part 2: state enconding
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_state_id_from_distribution(distribution(), integer()) -> integer().
get_state_id_from_distribution(Distribution, NLocs) ->
    DogLoc = array:get(?DOG, Distribution),
    StateId_dog = get_state_id_of_dog(DogLoc, NLocs),
    Bunny1Loc = array:get(?BUNNY1, Distribution),
    Bunny2Loc = array:get(?BUNNY2, Distribution),
    StateId_bunnies = get_state_id_of_bunnies(Bunny1Loc, Bunny2Loc, NLocs),
    StateId_dog * get_total_bunnies_state_num(NLocs) + StateId_bunnies.

-spec get_distribution_from_state_id(integer(), integer()) -> distribution().
get_distribution_from_state_id(StateId, NLocs) ->
    K = get_total_bunnies_state_num(NLocs),
    StateId_dog = StateId div K,
    DogLoc = get_dog_location_from_state_id(StateId_dog, NLocs),
    StateId_bunnies = StateId rem K,
    {Bunny1Loc, Bunny2Loc} = get_bunnies_locations_from_state_id(StateId_bunnies, NLocs),
    array:from_list([DogLoc, Bunny1Loc, Bunny2Loc]).

-spec get_total_state_num(integer()) -> integer().
get_total_state_num(NLocs) ->
    NLocs * get_total_bunnies_state_num(NLocs).

-spec get_total_bunnies_state_num(integer()) -> integer().
get_total_bunnies_state_num(NLocs) ->
    NLocs * (NLocs - 1) div 2  + NLocs.

-spec get_state_id_of_dog(integer(), integer()) -> integer().
get_state_id_of_dog(DogLoc, _NLocs) -> DogLoc.

-spec get_state_id_of_bunnies(integer(), integer(), integer()) -> integer().
get_state_id_of_bunnies(Bunny1Loc, Bunny2Loc, Nlocs) ->
    Diff = abs(Bunny1Loc - Bunny2Loc),
    I = Nlocs - Diff,
    J = min(Bunny1Loc, Bunny2Loc),
    ((I-1) * I) div 2 + J.

-spec get_dog_location_from_state_id(integer(), integer()) -> integer().
get_dog_location_from_state_id(StateId_dog, _Nlocs) -> StateId_dog.

-spec get_bunnies_locations_from_state_id(integer(), integer()) -> {integer(), integer()}.
get_bunnies_locations_from_state_id(StateId_bunnies, Nlocs) ->
    I = trunc(math:sqrt(2*StateId_bunnies + 0.25) + 0.5),
    J = StateId_bunnies - (((I-1) * I) div 2),
    Diff = Nlocs - I,
    Bunny1Loc = J,
    Bunny2Loc = J + Diff,
    {Bunny1Loc, Bunny2Loc}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Part 3: state transform graph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec build_state_trans_graph(graph()) -> state_trans_graph().
build_state_trans_graph(PuzzleGraph) ->
    NLocs = length(get_all_locations(PuzzleGraph)),
    NStates = get_total_state_num(NLocs),
    G = digraph:new([cyclic, private]),
    ok = lists:foreach(fun (S) ->
			       digraph:add_vertex(G, S)
		       end, lists:seq(0, NStates-1)),
    build_state_trans_graph(PuzzleGraph, NStates, NLocs, G, 0).

-spec build_state_trans_graph(graph(), integer(), integer(), state_trans_graph(), integer()) -> state_trans_graph().
build_state_trans_graph(_PuzzleGraph, NStates, _NLocs, G, CurrStateId) when CurrStateId =:= NStates -> G;
build_state_trans_graph(PuzzleGraph, NStates, NLocs, G, CurrStateId) -> 
    Dist = get_distribution_from_state_id(CurrStateId, NLocs),
    Indexes = [?DOG, ?BUNNY1, ?BUNNY2],
    Nexts = lists:flatmap(fun (I) ->
				  try_move_object(PuzzleGraph, Dist, I, NLocs)
			  end, Indexes),
    ok = lists:foreach(fun ({NextStateId, Action}) -> 
			       digraph:add_edge(G, CurrStateId, NextStateId, Action)
		       end, Nexts),
    build_state_trans_graph(PuzzleGraph, NStates, NLocs, G, CurrStateId + 1).

-spec try_move_object(graph(), distribution(), integer(), integer()) -> [{integer(), any()}].
try_move_object(PuzzleGraph, Dist, I, NLocs) ->
    Loc = array:get(I, Dist),
    Paths = get_adj_paths(PuzzleGraph, Loc),
    OtherAnimalIndexes = [Index || Index <- [?DOG, ?BUNNY1, ?BUNNY2], Index /= I],
    OtherAnimalIcons = sets:from_list(lists:filter(fun (X) -> X /= [] end,
						   [get_location_icon(PuzzleGraph, array:get(Index, Dist))
						    || Index <- OtherAnimalIndexes])),
    [{get_state_id_from_distribution(array:set(I, To, Dist), NLocs),
      {move, get_animal_name_from_index(I), from, Loc, to, To}}
     || {_From, To, YesLabels, NoLabels} <- Paths, 
	sets:is_subset(sets:from_list(YesLabels), OtherAnimalIcons),
	sets:size(sets:intersection(sets:from_list(NoLabels), OtherAnimalIcons)) == 0
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Part 4: Solve the game.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec solve(string()) -> [any()].
solve(File) ->
    {PuzzleGraph, StartStateId, EndStateId} = load_puzzle(File),
    StateTransGraph = build_state_trans_graph(PuzzleGraph),
    Vertices = digraph:get_short_path(StateTransGraph, StartStateId, EndStateId),
    fetch_actions(Vertices, StateTransGraph).

-spec get_animal_name_from_index(integer()) -> atom().
get_animal_name_from_index(0) -> dog;
get_animal_name_from_index(1) -> bunny;
get_animal_name_from_index(2) -> bunny.

-spec fetch_actions([integer()], state_trans_graph()) -> [any()].
fetch_actions(Vs, StateTransGraph) ->
    fetch_actions(Vs, StateTransGraph, []).

-spec fetch_actions([integer()], state_trans_graph(), [any()]) -> [any()].
fetch_actions([_V], _StateTransGraph, Actions) -> lists:reverse(Actions);
fetch_actions([V1, V2|Vs], StateTransGraph, Actions) ->
    Es = [digraph:edge(StateTransGraph, E) || E <- digraph:out_edges(StateTransGraph, V1)],
    {_, V1, V2, Action} = lists:keyfind(V2, 3, Es),
    fetch_actions([V2|Vs], StateTransGraph, [Action|Actions]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Unit Test Part
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(EUNIT).
parse_path_test() ->
    ?assert(parse_path("1~~2;a,c:d") =:= {{false, 1, 2}, [a,c], [d]}),
    ?assert(parse_path("1-->2") =:= {{true, 1, 2}, [], []}),
    ?assert(parse_path("1-->2;a") =:= {{true, 1, 2}, [a], []}).

puzzle_graph_test() ->
    {G, _, _} = load_puzzle("src/demo1224.txt"),
    ?assert(lists:sort(get_all_locations(G)) =:= lists:seq(0, 6)),
    ?assert(get_location_icon(G, 3) =:= []),
    ?assert(get_location_icon(G, 6) =:= bone),
    Es1 = get_adj_paths(G, 5),
    ?assert(lists:sort(fun (E1, E2) ->
			       element(2, E1) < element(2, E2)
		       end, Es1) =:= [{5,3,[],[]}, {5,4,[],[]}]),
    Es2 = get_adj_paths(G, 3),
    ?assert(lists:sort(fun (E1, E2) ->
			       element(2, E1) < element(2, E2)
		       end, Es2) =:= [{3,2,[house],[]}, {3,4,[house],[]},
				      {3,6,[],[]}]).

state_test() ->
    %% Nlocs = 2, two locations 0, 1
    %% all possible states are (listed in nature number order) 6 in total
    %% (0). 0:{dog, bunny}, 1:{bunny}
    %% (1). 0:{dog, bunny, bunny}, 1:{}
    %% (2). 0:{dog}, 1:{bunny, bunny}
    %% (3). 0:{bunny}, 1:{dog, bunny}
    %% (4). 0:{bunny, bunny}, 1:{dog}
    %% (5). 0:{}, 1:{dog, bunny, bunny}
    Dist1 = array:from_list([0, 0, 1]),
    ?assert(get_state_id_from_distribution(Dist1, 2) =:= 0),
    Dist2 = array:from_list([1, 0, 1]),
    ?assert(get_state_id_from_distribution(Dist2, 2) =:= 3),
    ?assert(array:to_orddict(get_distribution_from_state_id(0, 2)) =:= array:to_orddict(Dist1)),
    ?assert(array:to_orddict(get_distribution_from_state_id(3, 2)) =:= array:to_orddict(Dist2)).

state_trans_graph_test() ->
    %% Nlocs = 2, two locations 0, 1
    %% 0(carrot) ~~ 1(bone)
    %% all possible states are (listed in nature number order) 6 in total
    %% (0). 0:{dog, bunny}, 1:{bunny}
    %% (1). 0:{dog, bunny, bunny}, 1:{}
    %% (2). 0:{dog}, 1:{bunny, bunny}
    %% (3). 0:{bunny}, 1:{dog, bunny}
    %% (4). 0:{bunny, bunny}, 1:{dog}
    %% (5). 0:{}, 1:{dog, bunny, bunny}
    {PuzzleGraph, _, _} = load_puzzle("src/demo1.txt"),
    G = build_state_trans_graph(PuzzleGraph),
    ?assert(lists:sort(digraph:vertices(G)) =:= lists:seq(0, 5)),
    Es = [digraph:edge(G, E) || E <- digraph:out_edges(G, 0)],
    ?assert(lists:sort([{From, To, Action} || {_, From, To, Action} <- Es]) =:=
		[{0,1,{move,bunny,from,1,to,0}},
		 {0,2,{move,bunny,from,0,to,1}},
		 {0,3,{move,dog,from,0,to,1}}]).
-endif.
