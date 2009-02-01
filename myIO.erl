-module(myIO).
-export([open/1, close/1, file_size/1, read/3, write/3]).
-include_lib("eunit/include/eunit.hrl").

%Buffer is used to do buffered reading and writting from a random access file. 
%Buffer is the "workhorse" that actually does the reading and writting from the file.
%Buffer takes and returns binary data.
rpc(Pid, Request) -> 
	Pid ! {self(), Request}, 
	receive 
		{Pid, Response} ->
			Response
	end.

open(FileName) -> 
	F = fun() ->
		case file:open(FileName, [read,write,raw,binary]) of
		{ok, IODevice} ->
			loop(FileName, IODevice);
		{error, Reason} ->
			{error, Reason} end end,
	T = spawn(F),
	io:format("Pid of myIO is ~p~n", [T]),
	T.

close(Pid) ->
	rpc(Pid, close).

read(Pid, Position, Length) ->
	rpc(Pid, {read, Position, Length}).

write(Pid, Position, Buffer) ->
	rpc(Pid, {write, Position, Buffer}).

file_size(Pid) ->
	rpc(Pid, size).

loop(FileName, Fd) ->
	receive
		{From, {read, Position, Length}} 	-> 
			{ok, Position} = file:position(Fd, Position),
			{ok, Read} = file:read(Fd, Length),
			From ! {self(), {read, Read}},
			loop(FileName, Fd);
		{From, {write, Position, Write}} ->
			From ! {self(), {write, file:pwrite(Fd, Position, Write)}},
			loop(FileName, Fd);
		{From, size} ->
			{ok, FileInfo} = file:read_file_info(FileName),
			From ! {self(), {size, element(2, FileInfo)}},
			loop(FileName, Fd);
		{From, close} 				->
			file:close(Fd),
			From ! {self(), {close, "Process was destroyed"}};
		Other ->
			throw({error, "unkown message", Other}),
			loop(FileName, Fd)
	end.

unit_test_() ->
	Filename = "testing_myIO.txt",
	{setup,
	fun() -> 
		file:write_file(Filename, "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
		open(Filename) end,
	fun(Pid) ->
		close(Pid),
		file:delete(Filename) end,
	fun(Pid) ->
		{inorder,
		[	
			?_assert(is_pid(Pid)),
			?_assertEqual({read, <<"ABC">>},read(Pid, 0, 3)),
			?_assertEqual({write, ok}, write(Pid, 0, <<"123">>)),
			?_assertEqual({read, <<"3DE">>}, read(Pid, 2, 3)),
			?_assertEqual({write, ok}, write(Pid, 3, <<"45">>)),
			?_assertEqual({size, 26}, file_size(Pid)),
			?_assertEqual({write, ok}, write(Pid, 26, <<"123456789">>)),
			?_assertEqual({read, <<"Z123">>}, read(Pid, 25, 4)),
			?_assertEqual({size, 35}, file_size(Pid))
		]}
	end}.
