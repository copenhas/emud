-module(emud_http_websocket).

-include_lib("emud/include/emud.hrl").

-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3]).

-record(state, {
        sessid,
        sess
    }).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, SessId, Sess} = emud_srv:connect(),
    {ok, Req, #state{sessid = SessId, sess = Sess}}.

websocket_handle({text, Msg}, Req, #state{sessid=SessId, sess=Sess} = State) ->
    case emud_http_data:decode_cmd({text, Msg}) of
        Cmd when is_record(Cmd, cmd) ->
            emud_sess:handle_cmd(Sess, Cmd#cmd{sessid=SessId}),
            {ok, Req, State};
        Error when is_atom(Error) -> 
            {reply, senderror(Error), Req, State}
    end.

websocket_info({emud_msg, _Ref, Msg}, Req, State) ->
    Json = emud_http_data:encode_msg(Msg),
    {reply, {text, Json}, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

senderror(Error) when is_atom(Error) ->
    ErrorMsg = #msg { type = Error },
    {text, emud_http_data:encode_msg(ErrorMsg)}.
