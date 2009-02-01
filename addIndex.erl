-module(addIndex).
-export([]).

%This adds a index to each record for the specified file. It does so according to the format of 
%record, which must be specific to the file. It does not read the content of the file to 
%determine the format (although in this project it could).

%The index basically allows us to know how many records "match" this current record. If a file 
%contains records that have 3 attributes, there would be a total of 6 additional fields added to
%it that would be "index" fields. So for each attribute in the record, this function adds to 
%additional fields that allow us to know how many records are similar to this one. These additional
%attributes are always "to the right" and adjacent to the object they represent. Example:

%Original Record:
% [Attribute1, Attribute2, Attribute3]

%New Record created from running original record's DB through addIndex.
% [Attribute1, IncIndex1, DecIndex1, Attribute2, IncIndex2, DecIndex2, Attribute3, IncIndex3, DecIndex3]

%The IncIndexN contains the count of this record in releation to all the records that have a identical
%AttributeN Value. Similarily, the DecIndexN contains this records count in relation to all records that
%have a identical AttributeN value. So if a attribute of a record had a 4 as a IndIndex and a 7 as a 
%DecIndex, that would means there are a total of 10 records with a identical AttributeN to this record
%and there are 3 that occur "before" this record, and 6 that occur "after".

%The index attributes size would always need to be at most large enough to contain a integer of the
%total number of records. Therefore the size of the index attributes is equal to 2logN where N is 
%the total number of records. It is then rounded up to the nearest whole byte.

%Get and Set are functions that are used to read from the existant database and write to the new
%indexed database, respectively. 

%Format specifies a list of integers that specify the form of the records received from get.
%addIndex(fun(int()), fun(int()), [int()], int())
