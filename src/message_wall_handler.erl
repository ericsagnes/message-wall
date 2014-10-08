-module(message_wall_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

% プロセスステートにIPアドレスとユーザエージェントを保存
-record(state, {ip, ua}).

% テーブル名をマクロで定義
-define(TABLE, message_wall).

% ========================
%  初期化
% ========================

init(_, _, _) ->
  % 接続をWebSocketにアップグレード
  {upgrade, protocol, cowboy_websocket}.

% websocket_init はwebsocket接続が開始された時に実行されます
websocket_init(_, Req, _Opts) ->
  % プロセスをgproc pubsubに登録する
  io:format("~p~n", [self()]),
  gproc_ps:subscribe(l, new_message),
  % stateを設定する
  Ip = get_ip(Req),
  {UserAgent, _Req} = cowboy_req:header(<<"user-agent">>, Req),
  State = #state{ip=Ip, ua=UserAgent},
  % WebSocketリクエストは長くなる可能性があるため
  % 不要なデータをReqから削除
  Req2 = cowboy_req:compact(Req),
  % 自動切断を10分に設定する（60万ミリ秒）
  {ok, Req2, State, 600000, hibernate}.


% ========================
%  メッセージハンドリング
% ========================

% get_listメッセージの場合はメッセージのリストを返します
websocket_handle({text, <<"get_list">>}, Req, State) ->
  % 最新のメッセージを取得する
  RawMessages = get_recent_messages(10),
  % メッセージをjiffyが変換できる形式に変更
  Messages = format_messages(RawMessages),
  % jiffyでJsonレスポンスを生成
  JsonResponse = jiffy:encode(#{
    <<"type">> => <<"message_list">>,
    <<"messages">> => Messages
  }),
  % JSONを返す
  {reply, {text, JsonResponse}, Req, State};

% get_list以外のメッセージは保存する
websocket_handle({text, Text}, Req, #state{ua=Ua, ip=Ip} = State) ->
  Time = now(),
  Message = {Time, Text, Ip, Ua},
  save_message(Message),
  % gprocにイベントを公開し、
  % 全ての接続クライアントにwebsocket_info({gproc_ps_event, new_message, Time}, Req, State)を呼び出します
  gproc_ps:publish(l, new_message, Time),
  {ok, Req, State};

websocket_handle(_Frame, Req, State) ->
  {ok, Req, State}.

% websocket_info は本プロセスにErlangメッセージが届いた時に実行されます
% gprocからnew_messageメッセージの場合はそのメッセージをWebSocketに送信します
websocket_info({gproc_ps_event, new_message, Key}, Req, State) ->
  RawMessage = ets:lookup(?TABLE, Key),
  % ETSエントリーをマップに変換
  Message = format_message(RawMessage),
  JsonResponse = jiffy:encode(#{
    <<"type">> => <<"new_message">>,
    <<"message">> => Message
  }),
  {reply, {text, JsonResponse}, Req, State};

websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.


% ========================
%  メッセージ関連
% ========================

% ETSにメッセージを保存する
save_message({Time, Text, Ip, Ua}) ->
  ets:insert(?TABLE, {Time, unicode:characters_to_binary(Text), Ip, Ua}).

% 最新のNumberメッセージを取得する
get_recent_messages(Number) ->
  case ets:last(?TABLE) of
    '$end_of_table' -> [];
    Key -> get_recent_messages(Key, Number, [])
  end.

get_recent_messages(_Key, 0, Messages) -> lists:reverse(Messages);
get_recent_messages('$end_of_table', _Number, Messages) -> lists:reverse(Messages);
get_recent_messages(Key, Number, Messages) ->
  Message = ets:lookup(?TABLE, Key),
  PreviousKey = ets:prev(?TABLE, Key),
  get_recent_messages(PreviousKey, Number-1, [Message|Messages]).

% IP取得
get_ip(Req) ->
  % プロキシ経由
  case cowboy_req:header(<<"x-real-ip">>, Req) of
    {undefined, _Req} ->
      {{Ip, _Port}, _Req} = cowboy_req:peer(Req),
      Ip;
    {Ip, _Req} -> Ip
  end.

% ETS結果メッセージをjiffyが変換できる形式に変更
format_messages(RawMessages) ->
  lists:map(fun(Message) -> format_message(Message) end, RawMessages).

% ETS結果メッセージをjiffyが変換できる形式に変更
format_message([{Time, Message, Ip, Ua}]) ->
  #{
    <<"date">> => unicode:characters_to_binary(iso8601(Time)),
    <<"ip">>   => unicode:characters_to_binary(format_ip(Ip)),
    <<"text">> => Message,
    <<"ua">>   => Ua
  }.

% IPタプルを文字列に変換
format_ip({I1,I2,I3,I4}) ->
  io_lib:format("~w.~w.~w.~w",[I1,I2,I3,I4]);
format_ip(Ip) -> Ip.

% erlangのdatetimeをISO8601形式に変換
iso8601(Time) ->
  {{Year, Month, Day},{Hour, Minut, Second}} = calendar:now_to_universal_time(Time),
  io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Year, Month, Day, Hour, Minut, Second]). 
