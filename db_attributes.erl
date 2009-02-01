-module(db_attributes).
-export([:all]).

%get_attributes(string()) -> list
%reads the description on the file and returns a list of all the
%attributes the file has. This is based exclusively on the file
%name of the file. containing directories in the string are 
%ignored, only the name of the file is analyzed.
%Example
%get_attributes("./a/lot/of/directories/title_id:16_year:8.txt") ->
%	[["id", 16], ["year", 8]]
get_attributes(FileName) -> "implement".

%num_records(string()) -> integer()
%returns the number of records this database file has. The database
%file in question must be a file that can be evaluated by 
%get_attributes 
num_records(FileName) -> "implement".

%remove_dir(string()) -> "implements"
%removes the directory part of a string, best shown by a example
%remove_dir("./a/lot/of/directories/file.txt") -> "file.txt"
remove_dir(FileName) -> "implements".

%record_size(string()) -> integer()
%returns the size of each record in this database in bytes. Note
%that the record_size is larger than the file description because
%each record also includes several indexes.
record_size(FileName) -> "implements".