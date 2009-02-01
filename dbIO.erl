-module(dbIO).
-export([open/2]).
-include_lib("eunit/include/eunit.hrl").

%this process is responsible for reading and writting a database record from a database file
open(FileName, Type) -> 
	F = fun() -> 
			Open = myIO:open(FileName),
			Dict = get_dict(Type),
			loop(Open, Dict) end,
	spawn(F).

%This could be made into its own independent process, allowing sharing of this dictionary 
%across all processes, save memory.
get_basic(Type) ->
	case Type of 
		movie -> dict:from_list([{type,movie}, {seq, [{movie_id,2},{year,1}]}]);
		qualifying -> dict:from_list([{type,qualifying}, {seq, [{movie_id,2},{user,3},{date,2}]}]);
		rating -> dict:from_list([{type,rating},{seq, [{stars,1},{movie_id,2},{user,3},{date,2}]}]);
		_ -> {error, "I don't recognize this type"} end.

get_dict(Type) ->
	B = get_basic(Type),
	B_seq = dict:fetch(seq, B), 
	%Calculate the Sum of this record
	Sum = find_length(B_seq),
	B1 = dict:store(length, Sum, B),
	%create the simple integer list of this record
	Int_Seq = lists:map(fun({_, Y}) -> Y end, B_seq),
	dict:store(int_seq, Int_Seq, B1).

%This finds the length of a record in bytes based on the seq attribute
find_length(List) -> find_length(List, 0).
find_length([{_, L}|T], Acc) -> find_length(T, Acc + L);
find_length([], Acc) -> Acc.

%This sums a list of numbers
sum(L) -> sum(L, 0). 
sum([], N) -> N; 
sum([H|T], N) -> sum(T, H+N). 

%converts a list of tuples into a single contiguous binary
tuples_to_binary(Tuples, Seq) ->
	%msg({tuples, Tuples, seq, Seq}),
	L = lists:map(fun(X) -> tuple_to_binary(X, Seq) end, Tuples),
	list_to_binary(L).
%converts a tuple into a binary
tuple_to_binary(Tuple, Seq) ->
	TL = tuple_to_list(Tuple),
	tup_to_bin(TL, Seq, []).
tup_to_bin([],[],Rest) ->
	list_to_binary(lists:reverse(Rest));
tup_to_bin([H1|T1],[H2|T2], Rest) ->
	tup_to_bin(T1,T2,[int_to_bin(H1, H2)|Rest]).

%converts either a list of binaries or a single binary into a list of tuples
binary_to_tuples(Binary, Seq) when is_binary(Binary) ->
	Size = sum(Seq),
	Binaries = binary_to_binaries(Binary, Size),
	binary_to_tuples(Binaries, Seq);
binary_to_tuples(Binaries, Seq) when is_list(Binaries) ->
	lists:map(fun(X) -> binary_to_tuple(X, Seq) end, Binaries).

%helper function that splits a single binary into multiple binaries based on length
binary_to_binaries(Binary, Size) ->
	binary_to_binaries(Binary, Size, []).
binary_to_binaries(<<>>, _, Result) ->
	lists:reverse(Result);
binary_to_binaries(Binary, Size, Result) ->
	{Bin, Rest} = split_binary(Binary, Size),
	binary_to_binaries(Rest, Size, [Bin|Result]).

%converts a single binary into a tuple according to Seq
binary_to_tuple(Binary, Seq) ->
	bin_to_tup(Binary, Seq, []).
bin_to_tup(<<>>, [], Rest) ->
	list_to_tuple(lists:reverse(Rest));
bin_to_tup(Binary, [H|T], Rest) ->
	L = H * 8,
	<<Bin:L/integer, Next/binary>> = Binary,
	bin_to_tup(Next, T, [Bin|Rest]).
	
%This converts a integer into a binary, length of binary is in bytes
int_to_bin(Integer, Length) ->
	L = Length * 8,
	<<Integer:L>>.

%This converts a binary into a integer
bin_to_int(Binary) ->
	L = size(Binary) * 8,
	<<I:L/integer>> = Binary,
	I.

loop(MyIO, Dict) ->
	RecordLength = dict:fetch(length, Dict),
	receive
		{From, {read, Start, Num}} ->
			{read, ReadReturnTotal} = rpc(MyIO, {read, Start * RecordLength, RecordLength * Num}),
			Ret = binary_to_tuples(ReadReturnTotal, dict:fetch(int_seq, Dict)),
			From ! {self(), {read, Ret}},
			loop(MyIO, Dict);
		{From, {read, At}} 	->
			{read, Ret} = rpc(MyIO, {read, At * RecordLength, RecordLength}),
			[FormatedRet] = binary_to_tuples(Ret, dict:fetch(int_seq, Dict)),
			From ! {self(), {read, FormatedRet}},
			loop(MyIO, Dict);
		{From, {write, Position, Write}} ->
			Binary = if
				is_binary(Write) -> 
					Write; 
				true -> 
					tuples_to_binary(Write, dict:fetch(int_seq, Dict)) end,
			{write, Ret} = rpc(MyIO, {write, Position * RecordLength, Binary}),
			From ! {self(), {write, Ret}},
			loop(MyIO, Dict);
		{From, close} -> 
			{close, "Process was destroyed"} = rpc(MyIO, close),
			From ! {self(), {close, "Process was destroyed"}};
		Other ->
			throw({error, "unkown message", Other}),
			loop(MyIO, Dict)
	end.

rpc(Pid, Request) -> 
	Pid ! {self(), Request}, 
	receive 
		{Pid, Response} ->
			Response
	end.
	
unit_test_() ->
	Movie = get_dict(movie),
	Qualifying = get_dict(qualifying),
	Rating = get_dict(rating),
	Seq = [1,2,3,4],
	%check that all the items are returning the proper type
   [?_assertEqual(movie, dict:fetch(type, Movie)),
	?_assertEqual(qualifying, dict:fetch(type, Qualifying)),
	?_assertEqual(rating, dict:fetch(type, Rating)),
	%check that all the items have the right sequence
	?_assertEqual([{movie_id,2},{year,1}], dict:fetch(seq, Movie)),
	?_assertEqual([{movie_id,2},{user,3},{date,2}], dict:fetch(seq, Qualifying)),
	?_assertEqual([{stars,1},{movie_id,2},{user,3},{date,2}], dict:fetch(seq, Rating)),
	%check that all the items are returning the proper length (in bytes)
	?_assertEqual(3, dict:fetch(length, Movie)),
	?_assertEqual(7, dict:fetch(length, Qualifying)),
	?_assertEqual(8, dict:fetch(length, Rating)),
	%check the functionality of int_to_bin and bin_to_int
	?_assertEqual(<<4, 22>>, int_to_bin(1046, 2)),
	?_assertEqual(1046, bin_to_int(<<4, 22>>)),
	?_assertEqual(127, bin_to_int(int_to_bin(127, 4))),
	%checks the functionality of tuple_to_binary and binary_to_tuple
	?_assertEqual(<<1:8, 2:16, 4:24, 8:32>>,tuple_to_binary({1,2,4,8}, Seq)),
	?_assertEqual({1,2,4,8}, binary_to_tuple(<<1:8,2:16,4:24,8:32>>,Seq)),
	?_assertEqual({1,2,3}, binary_to_tuple(tuple_to_binary({1,2,3}, [1,2,4]),[1,2,4])),
	%checks the functionality of binary_to_binaries
	%POTENTIAL BUG WHEN SIZE == 0,
	%I don't think this is, it seems to duplicate regardless of whether you pink
	%int = 0 or 1.
	?_assertEqual([<<"abcd">>,<<"efgh">>],binary_to_binaries(<<"abcdefgh">>,4)),
	?_assertEqual([],binary_to_binaries(<<>>, 4)),
	%checks the functionality of tuples_to_binary
	?_assertEqual(<<1:8, 2:16, 4:8, 8:16>>, tuples_to_binary([{1,2},{4,8}], [1,2])),
	%checks the functionality of binary_to_tuples
	?_assertEqual([{1,2},{4,8}], binary_to_tuples(<<1:8, 2:16, 4:8, 8:16>>, [1,2]))
	].

io_test_() ->
	DB = open("testing_dbIO.txt", qualifying),
	{write, ok} = rpc(DB, {write, 1, [{1,2,3},{4,5,6},{7,8,9},{10,11,12},{13,14,15}]}),
	[	?_assertMatch({read,{1,2,3}}, rpc(DB, {read, 1})),
		?_assertMatch({read,[{4,5,6},{7,8,9},{10,11,12}]}, rpc(DB, {read,2,3})),
		?_assertMatch({close, "Process was destroyed"}, rpc(DB, close))].