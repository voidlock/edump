-module(edump_line_parser).

-export([start/2]).

start(GenStream, TokenParser) ->
    Pos = get_pos(GenStream),
    case get_chunk(GenStream) of
        eof ->
            throw({error, no_data});
        Bin ->
            {ok, State0} = TokenParser:init(Pos),
            parse_chunk(Bin, Pos, {edump_new_stream,0}, State0, GenStream, TokenParser)
    end.

parse_chunk(eof, Pos, _LastInfo, State0, _S, Mod) ->
    Mod:terminate(Pos, State0);
parse_chunk(Bin, Pos, LastInfo0, State0, S, Mod) ->
    case binary:split(Bin, <<"\n">>, [trim]) of
        [Line] ->
            case get_chunk(S) of
                eof ->
                    {State1, LastInfo1} = parse_line(Line, Pos, LastInfo0, State0, Mod),
                    NextPos = Pos + byte_size(Line),
                    parse_chunk(eof, NextPos, LastInfo1, State1, S, Mod);
                Rest ->
                    NextBin = <<Line/binary, Rest/binary>>,
                    parse_chunk(NextBin, Pos, LastInfo0, State0, S, Mod)
            end;
        [Line, Rest] ->
            {State1, LastInfo1} = parse_line(Line, Pos, LastInfo0, State0, Mod),
            NextPos = Pos + byte_size(Line),
            parse_chunk(Rest, NextPos, LastInfo1, State1, S, Mod)
    end.

parse_line(<<$=:8, TagAndRest/binary>>, Pos, {LastTag, LastPos}, State0, Mod) ->
    case split_header(TagAndRest) of
        Tag when Tag =/= LastTag ->
            State1 = case LastTag of
                edump_new_stream -> State0;
                _ ->
                    Mod:handle_header_end(LastTag, LastPos, State0)
            end,
            State2 = Mod:handle_header_start(Tag, Pos, State1),
            {State2, {Tag, Pos}};
        Tag ->
            {State0, {Tag, Pos}}
    end;
parse_line(Line, Pos, LastInfo, State0, Mod) ->
    State1 = Mod:handle_token(split_token(Line), Pos, State0),
    {State1, LastInfo}.

get_pos(S) ->
    {stream_pos, Pos} = gen_stream:stream_pos(S),
    Pos.

get_chunk(S) ->
    case gen_stream:next_chunk(S) of
        {next_chunk, end_of_stream} -> eof;
        {next_chunk, Binary} -> Binary
    end.

split_header(Header) ->
    case binary:split(Header, <<":">>, [trim]) of
        [Tag] -> Tag;
        [Tag, Id] -> {Tag, Id}
    end.

split_token(Line) ->
    case binary:split(Line, <<": ">>, [trim]) of
        [Token] -> Token;
        [Token, Val] -> {Token, Val}
    end.
