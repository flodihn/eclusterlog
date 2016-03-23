-module(eclusterlog).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    init/0,
    create_db/0,
    join_db/1]).

-export([
    test/0
    ]).

-include("eclusterlog.hrl").

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
    mnesia:create_table(eclusterlog, [
        {disc_copies, [node()]},
        {type, set},
        {attributes, record_info(fields, eclusterlog)}]).

join_db(Node) ->
    mnesia:start(),
    mnesia:change_config(extra_db_nodes, [Node]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    rpc:call(Node, mnesia, add_table_copy, [eclusterlog,
        node(), disc_copies]).

check_config() ->
    case {get_join_db(), table_exists()} of
        {{ok, JoinDb}, false} ->
            error_logger:info_msg("eclusterlog: Found env var join_db, joining existing mnesia cluster on node: ~p~n", [JoinDb]);
        {undefined, false} ->
            error_logger:warning_msg("eclusterlog: No join_db env var found, will create new mnesia cluster.~n");
        {_, true} ->
            pass     
    end.

get_join_db() ->
    application:get_env(eclusterlog, join_db).

table_exists() ->
    lists:member(eclusterlog, mnesia:system_info(tables)).

test() ->
    F = fun() ->
        Foo = 3,
        Foo = 4
    end,
    spawn(F).


