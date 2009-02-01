-module(retriever).
-export(open/1).

%A very high level abstraction that allows you to retrieve from this custom database I have written
%a the result of the query to said database. 

%open has two arguments. The first is a Directory string that specifies the directory that this
%database is stored within. It is expected that in Directory there will be indexed directory files
%for each possible permutation of a record. The files will be named according to a format specified 
%in createDb.erl but with the string "_indexed" inserted before the "." of the filename.

%This process basically minimizes the number of operations neccesary by always looking for the 
%record(s) within its collection that will satisfy a query in the minimum number of steps. It has a 
%few strategies that allow it to do this, they are:
%1) It contains a N! number of files, where N is the number of (non-indexed) attributes that belong to
%the records in this database. So each file represents one sorted permutation of the records. This 
%proccess, armed with the knowledge of this, will attempt to read from a file that has the records 
%searched for in contiguous order. Ties are broken by always picking the file that contains the 
%smallest number of records.
%2) Each record contains a index, the index allows us to know how many records match what we are 
%looking for, this allows us to zoom pretty fast down into the record we want. Once a record with
%all the correct attributes is found, the index allows us to specify exactly all the records that 
%should be read in order to get the entire collection.
%3) It contains its own static sprinkling of records for each file in the database that it always 
%contains in memory. These records are a breadth first drop into each file. The nature of how these
%records are chosen guarantees a safe sprinkling of records across many unique buckets of records.
%Anyway, when determine where it should start looking when attempting to find a match from the 
%file, it will actually first consult these memory resident things to determine if any of them 
%can help it zoom on its desired record faster 
%Finally, each Database contains its own queue of records of size StackSize. Any record that is 
%needed will first be searched for in here.

%So when a call is made, it first checks its stack to determine if the record exist, it then attempts
%to zoom in as best as possible to the location of the record based upon the "index sprinkles". If the
%index sprinkle happened to contain the record, it simply returns that. Finally, it searches the 
%correct file for the record it is looking for, zooming quickly down using the index information
%in each record it reads.

%GetFile is a fun that takes a tuple of integers and returns either {error, no_file_found} or a 
%myIO (or its descendents) object that points to a io stream that matches the specified tuple.
%open(fun(), tuple())
open(GetFile, Format) -> .

%You can only send {pid(), string()} and record() to this process. 
%Pid is the process id that the response should be sent to, string is the query string.
%to end, you should send the record 'close' to this process.

%The query string:
%The Query string allows you to specify in english what you are looking for:
%The smallest thing you can write are equality operators (=, <, <=, >, >=, !=). Equality 
%operators are infix operators. The thing to the left of it must be a attribute name, and the 
%thing to the right of it must be a value. You can link equality operators by either specifying 
%"and" or "or" between them. The same attribute can be specified twice, for instance you can specify
%"id = 6 and id != 6". No logical checks are done to "reduce" these logical statements, so be a little
%carefull in specifying them.

%Query returns:
%The query will return a fun() or {error, Message}. If a error tuple is received, then consult.
%Otherwise the fun takes one argument. The first argument is the record 'size'. This specifies the
%total number of records contained in this return. The second argument is a (base 1) index that allows
%you to retrieve the record at any index contained in this return value. The reason a fun is used 
%rather than a list is that sometimes the number of records returned may be quiet large. If this is the
%case than the fun will only contain so many records, and when you ask for a record that it does not 
%contain, it will behind the scene retrieve the next batch of them and return them. However, this fun
%should attempt to be used as a list (that is to say, don't use anything except the head of a list). 