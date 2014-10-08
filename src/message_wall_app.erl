-module(message_wall_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  % ETSテーブルを初期化
  ets:new(message_wall, [ordered_set, named_table, public]),
  % ルート宣言
  Dispatch = cowboy_router:compile([
    {'_', [
       % cowboy_staticはパスマッチに対して、静的ファイルを読み込む
       % index.htmlを読み込む
       {"/", cowboy_static, {priv_file, message_wall, "index.html"}},
       % /websocketのリクエストをws_handlerに渡す
       {"/websocket", message_wall_handler, []}
      ]}
   ]),
  % Cowboyを起動
  {ok, _} = cowboy:start_http(http, 100, 
      [{port, 8001}],
      [
       {env, [{dispatch, Dispatch}]}
      ]),
  message_wall_sup:start_link().

stop(_State) ->
  ok.
