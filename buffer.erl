-module[buffer].
-export[create/1].
%buffer is a process that uses a myIO proccess to read a file in a buffered fashion. 
%The only real knowledge that buffer has that myIO does not is
%1) a knowledge of the desired size that the buffer should be
%2) a messaging process between itself and all other buffer proccesses. This allows for 
%intelligent buffering of the file between all buffer procceses.

%Simply explained, buffer proccesses receive request to read and write data from anywhere
%within a particular file (no appends).
%If the data is within the buffer belonging to this buffer proccess, it is retrieved/modified
%and a success is sent back
%If the data was not within the buffer belonging to this buffer proccess, this process sends a 
%message to all other buffer processes asking them if they have the data.
% __ If any other process does contain the data, this process returns the following
% __ {switch_to_this_buffer, PID} where PID is the process id of the buffer that claims to contain
% __ that data.
% __ 
% __ If no other buffer contains the requested data, this buffer will then write itself out (only
% __ in the event that any changes were made to its buffer data). It will then read a appropriately
% __ sized amount of data around the area the data that is being asked for (the buffer) and it will
% __ then perform whatever operations was originally requested of it.

%Because "misses" request to a buffer result in a request to all other buffers (a broadcast) as
%well as a second message being sent to the buffer that does hopefully contain the data, it is a
%comparitavly expensive operation. So it would be best if locales always use the suggested buffers
%first, and then go back to using their "own" buffer. If at any time a locale is not able to 
%receive data from the suggested buffer, it will then go through all suggested buffers, and then 
%lasly try its "own" buffer. If its "own" buffer does not contain the data, it will as usual
%write itself out and fetch the data itself.

%Before a buffer can modify itself, it (should) write out changes to io and (must) read new data
%tfrom io. Io is under the domain of the bufferSync process. All io is done through the bufferSync
%proc. Because there is (hopefully) only one bufferSync proc for this file, all io is 
%effectivly synchronized. bufferSync keeps any two buffers from accidently buffering the same 
%segment of a file. If a request for segment data for a buffer would result in overlaping buffers, 
%the binary returned by bufferSync will be just small enough to avoid the overlap.
% check out from bufferSync {checked_out, PID, Position, Binary}.
% return to bufferSync      {check_in, PID, Position, Binary}.

%If two locales decide to go from sharing a buffer to each using their own. The locale making the
create(Size, Sync) ->  spawn(fun() -> loop(Size, Sync)).

loop(Size, Sync, Binary, BinarySize, BinaryStart) ->
	receive
		{read, Position, Length, From} ->
			From ! {read, self(), pread(Fd, Position, Length)},
			loop(Size, Sync);
		{write, Position, Binary, From} ->
			From ! {write, self(), pwrite(Fd, Position, Binary)},
			loop(Size, Sync);
		{size, From} -> 
			From ! {size, self(), Size},
			loop(Size, Sync);
		{change_size, NewSize, From } ->
			From ! {change_size, self(), "changed size from ~p to ~p~n", [Size, NewSize]},
			loop(NewSize, Sync);
		{close, From} ->
			From ! {close, self, "Buffer was destroyed\n"}];
		Other ->
			io:format({error, self, "I don't know what to do with message \"~p~n\""}, [Other])
			loop(Size, Sync)
	end.

