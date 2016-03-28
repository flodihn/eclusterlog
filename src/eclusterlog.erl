-module(eclusterlog).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    init/0,
    create_db/0,
    join_db/1,
    fetch_logs/0,
    fetch_logs/1,
    clear_logs/0]).

%% This function is supposed to crash and can be used to quickly test
%% if the error logging works.
-export([
    make_crash/0
    ]).

-include("eclusterlog.hrl").
-define(TABLE, eclusterlog).

%% ------------------------------------------------------------------
%% High level functions
%% ------------------------------------------------------------------
init() ->
    application:load(eclusterlog),
    mnesia:start(),
    check_config(),
    case table_exists() of
        true ->
            pass;
        false ->
            case get_join_db() of
                {ok, Node} ->
                    join_db(Node);
                 undefined ->
                    create_db()
            end
    end,           
    error_logger:add_report_handler(eclusterlog_handler).

create_db() ->
    mnesia:start(),
    mnesia:change_config(extra_db_nodes, [node()]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    mnesia:create_table(?TABLE, [
        {disc_copies, [node()]},
        {type, ordered_set},
        {attributes, record_info(fields, ?TABLE)}]).

join_db(Node) ->
    mnesia:start(),
    mnesia:change_config(extra_db_nodes, [Node]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    rpc:call(Node, mnesia, add_table_copy, [?TABLE,
        node(), disc_copies]).

fetch_logs() ->
    fetch_logs(100).

fetch_logs(NumLogs) ->
    %% This works because we use integers as keys.
    LastKey = mnesia:table_info(?TABLE, size) - 1,
    case LastKey >= 0 of
        false -> [];
        true -> fetch_logs(LastKey, [], NumLogs)
    end.

clear_logs() ->
    mnesia:clear_table(?TABLE).

%% ------------------------------------------------------------------
%% Mid level functions
%% ------------------------------------------------------------------
check_config() ->
    case {get_join_db(), table_exists()} of
        {{ok, JoinDb}, false} ->
            error_logger:info_msg("eclusterlog: Found env var join_db, joining existing mnesia cluster on node: ~p~n", [JoinDb]);
        {undefined, false} ->
            error_logger:warning_msg("eclusterlog: No join_db env var found, will create new mnesia cluster.~n");
        {_, true} ->
            pass     
    end.

%% ------------------------------------------------------------------
%% Low level functions
%% ------------------------------------------------------------------
get_join_db() ->
    application:get_env(?TABLE, join_db).

table_exists() ->
    lists:member(?TABLE, mnesia:system_info(tables)).

fetch_logs('$end_of_table', Logs, _NumLogs) ->
    lists:reverse(Logs);

fetch_logs(_Key, Logs, 0) ->
    lists:reverse(Logs);

fetch_logs(Key, Logs, NumLogs) ->
    NextKey = mnesia:dirty_prev(?TABLE, Key),
    case mnesia:dirty_read({?TABLE, Key}) of
        [Record] ->
            fetch_logs(NextKey, [Record | Logs], NumLogs - 1);
        [] ->
            fetch_logs(NextKey, Logs, NumLogs - 1)
    end.

%% ------------------------------------------------------------------
%% Utility functions
%% ------------------------------------------------------------------
make_crash() ->
    F = fun() ->
        Foo = 3,
        Foo = 4
    end,
    spawn(F).
