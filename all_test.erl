-module(all_test).
-include_lib("eunit/include/eunit.hrl").

all_test_() ->
	[	{module, myIO},
		{module, dbIO},
		{module, myBufferedIO},
		{module, myBufferedRandomIO},
		{module, addIndex},
		{module, combsort},
		{module, createDB}].