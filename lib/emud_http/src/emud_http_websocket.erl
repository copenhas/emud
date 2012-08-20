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
    Cmd = #cmd{
        type = list_to_atom(binary_to_list(Msg)), % totally unsafe!!!
        sessid = SessId
    },
    emud_sess:handle_cmd(Sess, Cmd),
    {ok, Req, State}.

websocket_info({emud_msg, _Ref, Msg}, Req, State) ->
    {reply, {text, Msg#msg.text}, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
