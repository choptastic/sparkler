%% vim: ts=4 sw=4 sts=4 et

-module(sparkler).
-behaviour(gen_server).

%-compile(export_all).

-define(TIMEOUT,get_env(timeout, 300)).
-define(API_KEY, get_env(api_key, "")).
-define(API_PREFIX,get_env(api_prefix, "https://api.sparkpost.com/api/v1/")).
-define(HTTP_OPTIONS, []).
-define(OPTIONS, [{full_result,false}]).

-export([
    start_link/0,
    init/1,
    stop/0,
    reset/0,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    status/0,
    code_change/3
]).

-export([
    send_text/4,
    send_text/5,
    send_html/4,
    send_html/5,
    send/4,
    send/5,
    send/6
]).

-export([fix_floating_linefeeds/1]).

-record(mail,{from,to,subject,text,html,headers}).
-record(data,{queue}).

get_env(Key, Default) ->
    case application:get_env(sparkler, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

send_text(From, To, Subject, Text) ->
    send_text(From, To, Subject, Text, []).

send_text(From, To, Subject, Text, Headers) ->
    send(From, To, Subject, Text, "", Headers).

send_html(From, To, Subject, Html) ->
    send_html(From, To, Subject, Html, []).

send_html(From, To, Subject, Html, Headers) ->
    send(From, To, Subject, "", Html, Headers).

send(From,To,Subject,Text,Html,Headers) ->
    Mail = #mail{
        to=To,
        from=From,
        subject=Subject,
        text=Text,
        html=Html,
        headers=Headers
    },
    gen_server:call(?MODULE,{queue,Mail}).

%% kept for backwards compatibitlity only
send(From,To,Subject,Text) ->
    send(From,To,Subject,Text,[],[]).


send(From,To,Subject,Text,Html) ->
    send(From,To,Subject,Text,Html,[]).

start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE,#data{},[]).

stop() ->
    gen_server:call(?MODULE,stop).

reset() ->
    gen_server:call(?MODULE,reset).

status() ->
    gen_server:call(?MODULE,status).

terminate(_Reason,_Data) ->
    ok.

init(Data) ->
    {ok,Data#data{queue=queue:new()},?TIMEOUT}.

handle_call({queue,Mail},_From,#data{queue=Queue} = Data) ->
    NewQueue = queue:in(Mail,Queue),
    NewLen = queue:len(NewQueue),
    {reply,{queued,NewLen},Data#data{queue=NewQueue},?TIMEOUT};
handle_call(reset,_From,Data) ->
    {reply,ok,Data#data{queue=queue:new()},?TIMEOUT};
handle_call(status,_From,#data{queue=Queue} = Data) ->
    Len = queue:len(Queue),
    Status = [
        {queue_size,Len}
    ],
    {reply,Status,Data,?TIMEOUT};
handle_call(stop,_From,Data) ->
    {stop,stopped,Data}.

handle_info(timeout,#data{queue=Queue} = Data) ->
    NewQueue = case queue:out(Queue) of
        {{value,Mail},NQ} ->
            #mail{
                from=From,
                to=To,
                subject=Subject,
                text=Text,
                html=Html,
                headers=Headers
            } = Mail,
            spawn(fun() ->
                try
                    int_send(?API_KEY,?API_PREFIX,From,To,Subject,Text,Html,Headers)
                catch E:T:S ->
                    error_logger:error_msg("~p:~p~n~p~n",[E,T,S])
                end
            end),
            NQ;
        {empty,Queue} ->
            Queue
    end,
    {noreply,Data#data{queue=NewQueue},?TIMEOUT}.

handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Headers is a proplist of {header,value}
int_send(0,APIKey,Prefix,From,To,Sub,_Text,_Html,_Headers) ->
    error_logger:error_report([
        failed_send,
        {api_key,APIKey},
        {prefix,Prefix},
        {from,From},
        {to,To},
        {subject,Sub}
    ]);
int_send(TryNum,_APIKey,Prefix,From,To,Subject,Text,Html,Headers) when TryNum > 0 ->
    URL = Prefix ++ "transmissions",
    EncodedJson = make_json(From, To, Subject, Text, Html, Headers),
    Body = iolist_to_binary(EncodedJson),
    error_logger:info_msg("Sending: ~nAPI KEY: ~s~nURL: ~s~nMessage: ~s~n",[?API_KEY, URL, Body]),
    case ibrowse:send_req(URL,[{authorization, ?API_KEY}],post,Body,[{content_type,"application/json"}]) of
        {ok, _, _, Result} ->
            error_logger:info_msg("Sent: ~p~n",[Result]),
            do_nothing;
        {error, Reason} ->
            error_logger:info_msg("Error In Send: ~p~n",[Reason])
    end.

int_send(Server,Port,From,To,Subject,Text,Html,Headers) ->
    int_send(5,Server,Port,From,To,Subject,Text,Html,Headers).

make_json(From, To, Subject, Text, Html, Headers) ->
    {_Headers2, ReplyTo} = extract_reply_to(Headers),
    ReplyToOption = case ReplyTo of
        undefined -> [];
        _ -> [{reply_to, i2b(ReplyTo)}]
    end,
    HtmlOption = case Html of
        [] -> [];
        _ -> [{html, i2b(Html)}]
    end,
    {FromName, FromEmail} = normalize_email_name(From),
    {ToName, ToEmail} = normalize_email_name(To),
    Proplist = [
        {content, [
            {text, i2b(Text)},
            {subject, i2b(Subject)},
            {from, [{email, i2b(FromEmail)}, {name, i2b(FromName)}]}
        ] ++ ReplyToOption ++ HtmlOption},
        {recipients,[
            [{address, [{email,i2b(ToEmail)},{name,i2b(ToName)}]}]
        ]},
        {options, [{click_tracking, false}]}
    ],
    jsx:encode(Proplist).

extract_reply_to(Headers) ->
    lists:foldl(fun({K,V}, {HeaderAcc, ReplyTo}) ->
        case normalized_header(K) == "replyto" of
            true -> {HeaderAcc, V};
            false -> {[{K,V}|HeaderAcc], ReplyTo}
        end
    end, {[], undefined}, Headers).


normalized_header(H) when is_atom(H) ->
    normalized_header(atom_to_list(H));
normalized_header(H) when is_list(H) ->
    H2 = re:replace(H, "[^a-zA-Z0-9]", "", [{return, list}]),
    string:to_lower(H2).

normalize_email_name({FromName, FromEmail}) ->
    {FromName, FromEmail};
normalize_email_name(EmailString) ->
    case re:run(EmailString, "^\"(.*)\"\s*<(.*@.*)>$", [{capture, all_but_first, list}]) of
        {match, [Name, Email]} -> {Email, Name};
        nomatch -> {EmailString, EmailString}
    end.

fix_floating_linefeeds([]) ->
    [];
fix_floating_linefeeds([13,10 | Text]) ->
    [13,10 | fix_floating_linefeeds(Text)];
fix_floating_linefeeds([13 | Text]) ->
    [13,10 | fix_floating_linefeeds(Text)];
fix_floating_linefeeds([10 | Text]) ->
    [13,10 | fix_floating_linefeeds(Text)];
fix_floating_linefeeds([H | Text]) ->
    [H | fix_floating_linefeeds(Text)].

i2b(IO) ->
    unicode:characters_to_binary(IO).
