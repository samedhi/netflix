-module(testing_processes).
-export([t/0]).
-include_lib("eunit/include/eunit.hrl").

rpc(Pid, Request) -> 
	io:format("sending message ~p to ~p from ~p~n", [Request, Pid, self()]),
	Pid ! {self(), Request}, 
	receive 
		{Pid, Response} ->
			io:format("received message ~p from ~p in rpc process ~p~n", [Response, Pid, self()]),
			Response;
		{O, Response} ->
			io:format("ERROR: rpc received message ~p from ~p in rpc process ~p~n", [Response, O, self()]),
			Response
	end.
	
open() ->
	F = fun() -> loop() end,
	spawn(F).
	
t() ->
	T = open(),
	rpc(T, 3),
	rpc(T, 15),
	rpc(T, 10).
	
loop() ->
	receive
		{From, Int} ->
			From ! {self(), Int * Int},
			loop()
	end.
	
open_test_() ->
	T = open(),
	[	?_assertEqual(9, rpc(T, 3)),
		?_assertEqual(25, rpc(T, 5)),
		?_assertEqual(100, rpc(T, 10))].
		
%Have to isollate what exactly is causing this doubling bullshit.