%% Copyright (c) 2011-2015, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

{application, cat, [
	{description, "cat game server"},
	{vsn, "0.0.1"},
	{id, "55ab9d2-dirty"},
	{modules, ['anti_cheat','chat_log','db_sup','erl_top','essdb','essdb_client','essdb_parser','game','game_app','game_config','game_ctl','game_formula','game_log','game_path','game_send','game_sup','gen_mod','gm_command','hash_set','listen_sup','log_init','mod_heart','mod_role','net_encrypt','pack','pmod_pt','proc_list','proc_timer','role_counter','role_server','role_sup','serv_gm_ctrl','serv_listen','serv_misc','serv_role_cache','serv_role_mgr','serv_timer_engine','temp_cd','util','web_handler','web_server_sup']},
	{applications, [
		kernel,
		stdlib,
		cowboy
	]},
	{mod, {cat_app, []}},
	{env, []}
]}.
