-module(createDB).
-export([]).

%creates a new database for a specific flat file. Does so by creating a "database" that basically 
%represents every permutation possible for said file, in a sorted format.

%The Input is a myIO (or its descendants) responsive proccess. The string is the directory
%in which all the permuations of this file will be written.
%
%Format is a tuple of integers. The tuple represents the format of each record in the existant
%flattened file that is referenced by Input. The result of the file referenced mod (the sum of 
%the integers in Format) should be zero.
%
%A directory named OutputDir will be created. Within it there will be one file for each possible
%permuation according to Format. The file will be specified as "#{OutputDir}[_record():int()]+.db"
%where each the _record():int() comes directly from the Format attribute, and is permuted every 
%possible way. As might be guessed, each of these files will contain the same records, only they
%will be sorted in ascending order according to the permutation they represent. 
%create(pid(), string(), list({record(),int()})
create(Input, OutputDir, Format) -> fails.
