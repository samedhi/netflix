-module(database).
-export([:all]).

%The functionality of this proccess acts as a file object. This process is
%created around a individual database file. It allows read only access to 
%the records within said file.

%I don't really know how to structure this part of the code. Do I make it 
%a proccess that takes a function. Do I make it a function that takes a 
%proccess. Is there some way to tie a function to a process? I just don't
%know. Also, how do you do fileIO in erlang? I am guessing that open and 
%pread will be used a lot?

%read_records(integer(), integer()) -> list() | nil()
%returns count records in this database starting at index. returns 
%nil if something is wrong (index is negative or index + count >
%than number of records in this database)  
read_records(Index, Count) -> "implement".

%maybe just ignore the linked list and use a "splay-tree" data structure
%evidently this allows for quicker access of commonly used elements.
%actually, even the splay-tree would require a linked list to maitain
%history information. This is a buffer, it will fill itself considering
%the number of records we are talking about.

%See "Purely functional data structures"

%add_record_time(record()) -> new_time_record().
%adds the following record to the time buffer. The time buffer is a
%buffer that contains a discreet number of records. When the limit 
%on the number of records in the buffer is reached the record that 
%is the oldest record in this structure will be removed.
add_record_time(Record) -> "implements".

%get_record_time(Index) -> record() | nil
%gets the record with index i if it exist, if it does not exist 
%it return nil.
get_record_time(Index, Count) -> "implement".

%add_record_space(Record) -> record()
%add record to the space buffer. The space buffer is a buffer 
%that "resist" holding records that are contiguous. It will 
%make an attempt to only hold records that are distinct from
%one another. Or put another way, it will never hold two 
%records that have the exact same index string.
add_record_space(Record) -> "implement".

%get_record_space(record()) -> record()
%returns a record that has the same index range as the record
%Record. If no record is found, it will return nil. This is 
%looking within the record space buffer. 
get_record_space(Record) -> "implement".