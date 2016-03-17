-module(eclusterlog_handler).
-behaviour(gen_event).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         add_handler/2]).

%% ------------------------------------------------------------------
%% gen_event Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).
-include("eclusterlog.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?MODULE, Handler, Args).

%% ------------------------------------------------------------------
%% gen_event Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_event({error, _Leader, Error}, State) ->
    log_error(Error),
    {ok, State};

handle_event({error_report, _Leader, Error}, State) ->
    log_error(Error),
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

log_error({_Pid, _Format, Data}) ->
    TransFun = fun() ->
        Id = mnesia:table_info(eclusterlog, size),
        [_ErrorPid, Node, {{ErrorType, _}, ErrLine}] = Data,
        Time = erlang:timestamp(),
        Record = #eclusterlog{id=Id, node=Node, type=ErrorType, msg=Data, time=Time},
        mnesia:write(Record)
    end,
    case mnesia:transaction(TransFun) of
        {atomic, _} ->
            ok;
        {aborted, Reason} ->
            error_logger:warning_report({?MODULE,
                {failed_writing_log, Reason}})
    end.
 
