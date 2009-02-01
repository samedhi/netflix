-module(myBufferedRandomIO).
-export([]).
-include_lib("eunit/include/eunit.hrl").
%myBufferedIO will work optimally if a file is read or written to sequentially, or as long as the 
%io access ussually occurs within the domain of the checked out buffer. However, when this is
%not the case myBufferedRandomIO will perform much better. myBufferedRandomIO (henceforth called
%myBRIO) basically creates multiple myBufferedIO proccesses, and manages them to allow for 2 or more
%points of sequential access. Without getting into the details, if you have a usage case that 
%involves io access to N different points in a file, that are far enough away that a single buffer
%would be impracticle to span them, then you should use myBufferedRandomIO with a Locale count of N.

%Locale basically specifies the total number of different places that are going to be "concurrently"
%written to at the same time. No need to worry about how exactly this works, just think about how
%the io algorithm you are using works, and consider the total number of locales that this algorithm
%needs. If uncertain, just pick a large integer for local, and see what happens. If Locale is N, 
%myBufferedRandomIO should use roughly N times as much memory as myBufferedIO uses, but performance
%may be much better. A Locale of zero is stupid.

%Specify the file to open, the size of the buffer, and the number of locales
%open(string(), int(), int())
open(FileName, BufferSize, Locale) -> fail.