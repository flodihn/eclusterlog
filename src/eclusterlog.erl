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
    mnesia:start(),
    error_logger:add_report_handler(eclusterlog_handler).

create_db() ->
    mnesia:start(),
    mnesia:change_config(extra_db_nodes, [node()]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    case lists:member(eclusterlog, mnesia:system_info(tables)) of
        true ->
            pass;
        false ->
            mnesia:create_table(eclusterlog, [
                {disc_copies, [node()]},
                {type, set},
                {attributes, record_info(fields, eclusterlog)}])
    end.

join_db(Node) ->
    mnesia:change_config(extra_db_nodes, [Node]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    rpc:call(Node, mnesia, add_table_copy, [eclusterlog,
        node(), disc_copies]).

test() ->
    F = fun() ->
        Foo = 3,
        Foo = 4
    end,
    spawn(F).


