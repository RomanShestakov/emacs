-module(myapp_make).
-export([all/1]).
-compile(export_all).

all(Path) ->
case filename:extension(Path) of
    ".erl" -> 
	{ok, OldDir} = file:get_cwd(),
	NewDir = my_find_emakefile_dir(Path),
	ok = file:set_cwd(my_find_emakefile_dir(Path)),
	%%Result = make:all([load]),
	io:format("compile test"),
	ok = file:set_cwd(OldDir);
    ".cfg" ->
	Ebin = filename:join([my_find_emakefile_dir(Path), "ebin", filename:basename(Path)]),
	io:format("coping ~p to ~p ~n", [Path, Ebin]),
	{ok, _BytesCopied} = file:copy(Path, Ebin);
    Ext ->
	maybe_my_custom_stuff
end.

%my_find_emakefile_dir() ->
%    {file, Ebin} = code:is_loaded(?MODULE),
%    filename:join([filename:dirname(filename:dirname(Ebin))]);
%my_find_emakefile_dir(Path) ->
%    filename:join([filename:dirname(filename:dirname(Path))]).

my_find_emakefile_dir(Path) ->
    case filename:basename(Path) of
	"src" ->
	    filename:dirname(Path);
	_ ->
	    my_find_emakefile_dir(filename:dirname(Path))
    end.

		       
