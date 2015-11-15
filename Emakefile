% -*- mode: erlang -*-
%% 编译user_default
{[	
	"src/util/user_default.erl"
 ], 
 [	
	{i, "."},
 	{i, "include"},
 	{i, "./src"},
  	{outdir, "."},
	debug_info,
	report,
	warnings_as_errors,
	verbose
 ]
}.
{[	
	"src/mod/gen_mod.erl",
	"src/map/gen_map_ai.erl",
	"src/map/gen_mon_ai.erl"
 ], 
 [	
	{i, "."},
 	{i, "include"},
 	{i, "./src"},
  	{outdir, "ebin/game"},
	debug_info,
	report,
	warnings_as_errors,
	verbose
 ]
}.
%% 主体代码
{[	
	"src/*" ,
	"src/*/*" ,
	"src/*/*/*",
	"src/map/ai/*/*",
	"data/*"
 ], 
 [	
	{i, "."},
 	{i, "include"},
 	{i, "src/map"},
  	{outdir, "ebin/game"},
	{inline_size, 30},
	%'S',
	report,
	warnings_as_errors,
	verbose
 ]
}.
