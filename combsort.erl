-module(combsort).
-export([]).
-include_lib("eunit/include/eunit.hrl").

%I am trying to stretch my metaprogramming chops on this one. Therefore I am going to program 
%combsort, which is a fairly high performance in place memory constant sorting algorithm, in a
%generic fashion. I am going to do this by specifying functions that must be passed to combsort,
%rather than specifying any particular thing. combsort will use these functions to implement the
%combsort algorithm. Lets see how it goes.

%Get is a fun that takes a integer index argument (base 1) and returns the element at that position.
% Get = fun(int())
%
%Set is a fun that takes a integer index argument (base 1) and sets the element at that position
%to Elem.
% Set = fun(int(), Elem)
%
%Comp is a fun that compares two elements. It return 1 if the first is larger, 0 if they are 
%equal, and -1 if the second argument is larger.
%Comp = fun(Elem1, Elem2)
%
%Size returns the number of elements we are sorting.
%
%Gap is the gap size in the combsort. It changes only on every full iteration through all elements.
%
%Current is the index of the compared item (compared to Current + Gap). It increments per 
%recursive call to sort, except when the "end" is reached, it is reset to 0.
%sort will sort the elements according to the specified functions. It returns when done.
%sort({fun(), fun(), fun(), int()}, int(), int())
sort({Get, Set, Comp, Size}, Gap, Current) -> fails.