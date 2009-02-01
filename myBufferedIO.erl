-module(myBufferedIO).
-export([open/2, close/1, file_size/1, t/0]).
-include_lib("eunit/include/eunit.hrl").

%myBufferedIO is equivalent in functionality to myIO. It is in fact just a wrapper to myIO. 
%myBufferedIO simply adds simple file buffering to a opened file. This can result in 
%improved performance.

%the buffering is really simple. The assumption is made that this myBufferedIO process is the only
%owner of this file (no external modification). When myBufferedIO receives a request to read at a 
%particular point, it basically reads (within the file) 1/2 buffer size from the selected record
%to 1/2 of the buffer beyond the record. This represents the buffer that is "checked out" by any
%particular myBufferdIO process. Any request to read or write within the buffer will result in 
%modifications only to the memory buffer. When a request is made to read or write outside of the 
%"checked out" buffer, or this process is closed, the current buffer will first be written to 
%the file. 

%This implementation technically means that in a file with a linear sequence, most data ends up
%being read twice as often as strictly neccesary, this can be fixed, but is probably not worth 
%the added time.

close(Pid) ->
	rpc(Pid, close).
	
file_size(Pid) ->
	rpc(Pid, size).

rpc(Pid, Request) -> 
	io:format("sending message ~p to ~p from ~p~n", [Request, Pid, self()]),
	Pid ! {self(), Request}, 
	receive 
		{Pid, Response} ->
			io:format("received message ~p from ~p in rpc process ~p~n", [Response, Pid, self()]),
			Response
	end.

unit_test_() ->
	FileName = "testing_myBufferedIO.txt",
	{setup, 
	fun() -> 
		file:write_file(FileName, "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
		open(FileName, 2) end,
	fun(MyBIO) ->
		rpc(MyBIO, close),
		file:delete(FileName) end,
	fun unit_tests/1}.

unit_tests(MyBIO) ->
	{inorder,
	[	?_assert(is_pid(MyBIO))
		%?_assertEqual({size, 26}, rpc(MyBIO, size)),
		%?_assertEqual(<<"C">>, rpc(MyBIO, {read, 2, 1}))%,
		%?_assertEqual(<<"DEF">>, rpc(MyBIO, {read, 3, 3})),
		%?_assertEqual({write, ok}, rpc(MyBIO, {write, 3, <<"123">>})),
		%?_assertEqual({write, ok}, rpc(MyBIO, {write, 26, <<"!@#">>})),
		%?_assertEqual(<<"C123G">>, rpc(MyBIO, {read, 2, 5})),
		%?_assertEqual(<<"Z!@#">>, rpc(MyBIO, {read, 25, 4})),
		%?_assertEqual({close, "Process was destroyed"}, rpc(MyBIO, close))
	]
	}.

t() ->
	FileName = "testing_myBufferedIO.txt",
	file:write_file(FileName, "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
	Pid = open(FileName, 2),
	io:format("I have a Pid of ~p~nmyBufferedIO has a Pid of ~p~n", [self(), Pid]),
	rpc(Pid, size).

%Specify the file to open, and the size of the buffer
%open(string(), int())
open(FileName, BufferSize) -> 
	F = fun() ->
		MyIO = myIO:open(FileName),
		{size, Size} = rpc(MyIO, size),
		Buffer_Size = if
			Size < BufferSize -> Size;
			true -> BufferSize
		end,
		{read, Buffer} = rpc(MyIO, {read, 0, Buffer_Size}),
		loop(MyIO, Buffer_Size, {0, Buffer, false}) 
end,
	spawn(F).

%specify the PID of a myIO process as the first argument. And the size of the buffer as the 
%second argument. The third argument concerns the buffer itself. The arguments in the tuple 
%are 1) the index of the first record int the buffer (base 0) 2) the index of the last record
%in the buffer (base 0) 3) the buffer itself 4) whether the buffer has been changed from disk.
%When the myBufferedIO is created, its current buffer is instantiated as the buffer starting
%at index 0 and going for BufferSize bytes, or the end of the file, whichever is smaller.
%loop(pid(), int(), {int(), binary(), boolean()})
loop(MyIO, Size, Buffer) ->
	%io:format("~p:~p:~p~n", [MyIO, Size, Buffer]),
	receive
		%{From, {read, Position, Length}} 	-> 
		%	NewBuffer = read(MyIO, Size, Buffer, Position, Length),
		%	loop(MyIO, Size, NewBuffer);
		%{From, {write, Position, Write}} ->
		%	NewBuffer = write(MyIO, Size, Buffer, Position, Write),
		%	loop(MyIO, Size, NewBuffer);
		{From, size} ->
			From ! {self(), rpc(MyIO, size)},
			io:format("size ~p~p~p~n", [MyIO, Size, Buffer]),
			loop(MyIO, Size, Buffer);
		{From, close} ->
			io:format("close is being called"),
			Result = rpc(MyIO, close),
			io:format("I was closed succesfully ~p", Result),
			From ! {close, "Process was destroyed"};
		Other ->
			throw({error, "unkown message", Other}),
			loop(MyIO, Size, Buffer)
	end.
	
%read(MyIO, _, {Start, Buffer, _}, Position, Length) ->
%	case in_buffer(Buffer, Start, Position, Length) of
%		true -> 
%			{_, B } = split_binary(Position - Start, Buffer),
%			{_, B2 } = split_binary(Length, B);
%		false -> 
%			write_out_buffer(MyIO, Start, Buffer),
%			read_in_buffer(MyIO, Start, Length)%,
%			%From ! {self(), {read, }
%		end.
		
%write(MyIO, Size, {Start, Buffer, Changed}, Position, Write) -> done.
	%case in_buffer(Buffer, Start, Position, Length) of
	%	true -> ;
	%	false -> end

read_from_and_write_to_buffer_test_() ->
	[	?_assertEqual(<<"ABC">>, read_from_buffer(<<"ABCDEFGHIJ">>, 0, 0, 3)),
		?_assertEqual(<<"CDE">>, read_from_buffer(<<"ABCDEFGHI">>, 0, 2, 3)),
		?_assertEqual(<<"CDE">>, read_from_buffer(<<"ABCDEFGHI">>, 2, 4, 3)),
		?_assertEqual(<<"123DEFGHI">>, write_to_buffer(<<"ABCDEFGHI">>, 0, 0, <<"123">>)),
		?_assertEqual(<<"A12DEFGHI">>, write_to_buffer(<<"ABCDEFGHI">>, 2, 3, <<"12">>))].
	

%This reads from the passed Buffer binary starting at Start for Length bytes. It modifies
%how it reads from Buffer based upon its knowledge of where in the "master" buffer this
%buffer begins at. 
%read_from_buffer(binary(), int(), int(), int())
read_from_buffer(Buffer, BufferStart, Position, Length) -> 
	{_, B } = split_binary(Buffer, Position - BufferStart),
	{B2, _ } = split_binary(B, Length),
	B2.

%This writes to the passed Buffer binary at Position the content of the Write binary
%write_to_buffer(binary(), int(), int(), binary())
write_to_buffer(Buffer, BufferStart, Position, Write) ->
	Split = Position - BufferStart,
	io:format("!!!!!~p~n", [Split]),
	{Before, Rest} = split_binary(Buffer, Split),
	{_, After} = split_binary(Rest, size(Write)),
	list_to_binary([Before, Write, After]).

read_and_write_buffer_test_() ->
	FileName = "testing_myBufferedIO.txt",
	{setup,
	fun() ->
		file:write_file(FileName, "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
		myIO:open(FileName) end,
	fun(MyIO) ->
		rpc(MyIO, close),
		file:delete(FileName) end,
	fun read_and_write_buffer_tests/1}.

read_and_write_buffer_tests(MyIO) ->
	{inorder,
	[	?_assertEqual(<<"ABC">>, read_in_buffer(MyIO, 0, 3)),
		?_assertEqual(<<"FGHIJ">>, read_in_buffer(MyIO, 5, 5)),
		?_assertEqual({write, ok}, write_out_buffer(MyIO, 2, <<"123">>)),
		?_assertEqual(<<"AB123F">>, read_in_buffer(MyIO, 0, 6))]
	}.

%returns the new Length sized binary object read from a myIO descriptor at its Start position.
%read_in_buffer(Pid(), int(), int())
read_in_buffer(MyIO, Start, Length) ->
	{read, Read} = rpc(MyIO, {read, Start, Length}),
	Read.

%writes the content of the buffer object to the MyIO descriptor at position Start
write_out_buffer(MyIO, Start, Buffer) ->
	{write, ok} = rpc(MyIO, {write, Start, Buffer}).
		
%does the Buffer Binary which came from absolute position Start containt the Length 
%sized binary starting at absolute position Position.
%in_buffer(binary(), int(), int(), int())
in_buffer(Buffer, BufferStart, Position, Length) ->
	if
		BufferStart > Position -> false;
		BufferStart + size(Buffer) < Position + Length -> false;
		true -> true
	end.
	
in_buffer_test_() ->
	[	?_assertEqual(true, in_buffer(<<"1234567890">>, 0, 0, 3)),
		?_assertEqual(true, in_buffer(<<"12345">>, 0, 0, 5)),
		?_assertEqual(true, in_buffer(<<"123">>, 0, 1,2)),
		?_assertEqual(false, in_buffer(<<"123">>, 1, 0, 2)),
		?_assertEqual(false, in_buffer(<<"123">>, 0, 1, 3)),
		?_assertEqual(false, in_buffer(<<"123">>, 4, 8, 1)),
		?_assertEqual(false, in_buffer(<<"123">>, 8, 4, 1)),
		?_assertEqual(false, in_buffer(<<"1">>, 3, 2, 3))].