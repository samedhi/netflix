-module(reader).
-export().
-import(db_attributes, [:all]).

%read(string()) -> [record()]
%takes a string description of the desired records. The description is a 
%string that describes the constraints that must be true of all records 
%returned by this function. Conceptually, this function reads through
%all records checking whether each record implements the string description,
%all records that match the string description are put in a list, the list
%is returned. The value of Description can include Boolean symbols (&&, ||, 
%!, == , ()) as well as comparison operators. Since all record attributes 
%are integers, every attribute constraint should be comparing to a integer.
%
%Also note, you need to be careful to specify a reasonable constraint. Be as
%specific as possible, something too broad will easily exhaust available memory.
%
%Example   "titles where (id > 100 && id <= 300) || year == 1968"
%The previous description would return all title records as a list that had 
%id's greater than one hundred and less than or equal to 300 and all title 
%records that had a year value of 1968. The returned list is a set, so there
%is no duplication.

%neat idea, what if I returned a list of list, where each list started with the attributes
%that make up the list, and then you can see the differences within that. Do this instead
%of returning a flat list of all records. I am not sure, might be overly complicated, but
%something to consider.

%Hey, remember QLC, that was pretty neat stuff, could we use that and avoid writting all 
%this logic that we would need otherwise.

%Red-black tree vs AVL tree seems vs B-Tree like both solutions offer log n performance. 
%What should be used. Implement these tree data structures as their own resources. Check 
%that they don't exist in erlang. 
%B-tree is good for asynchronous opertations.
%T-Tree good for in memory balanced databases.
read(Description) -> "implement".

