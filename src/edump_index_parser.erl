-module(edump_index_parser).

-export([init/1,
         handle_header_start/3,
         handle_header_end/3,
         handle_token/3,
         terminate/2]).

-record(state, {tid, header, offset, data=[]}).

init(0) ->
    Table = ets:new(edump_index, [set]),
    {ok, #state{ tid = Table }}.

handle_header_start(<<"end">>=Tag, Offset, State) ->
    S = State#state{ header=Tag, offset=Offset, data=[] },
    insert_tag(Tag, byte_size(Tag) + Offset, S),
    S;
handle_header_start(Tag, Offset, State0) ->
    io:format("header_start: Tag=~p, Offset=~p, State=~p~n", [Tag, Offset, State0]),
    State0#state{ header=Tag, offset=Offset, data=[] }.

handle_header_end(Tag, EndPos, S) ->
    io:format("header_end: Tag=~p, Offset=~p, State=~p~n", [Tag, EndPos, S]),
    insert_tag(Tag, EndPos, S),
    S.

handle_token(Token, _Offset, #state{ header={proc,_} }=State) ->
    io:format("token: Token=~p, State=~p~n", [Token, State]),
    pad_proc_header(Token, State);
handle_token(_Token, _Offset, State) ->
    State.

terminate(_Offset, #state{ tid=Table }) ->
    {edump_index, Table}.

pad_proc_header({<<"Message queue length">>, Len}, #state{ data=Data }=S) ->
    S#state{ data=[{msg_queue_len, Len} | Data] };
pad_proc_header({<<"Reductions">>, Amt}, #state{ data=Data }=S) ->
    S#state{ data=[{reductions, Amt} | Data] };
pad_proc_header({<<"Stack+heap">>, Size}, #state{ data=Data }=S) ->
    S#state{ data=[{stack_heap, Size} | Data] }.

insert_tag({Tag, Id}, EndPos, #state{ tid=Tid, offset=StartPos, data=Data }) ->
    ets:insert(Tid, {{Tag, StartPos, EndPos}, Id, Data});
insert_tag(Tag, EndPos, S) ->
    insert_tag({Tag, undefined}, EndPos, S).

