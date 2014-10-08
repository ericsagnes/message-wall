# メッセージウォール

[Cowboy](https://github.com/ninenines/cowboy)を利用したWebSocketサンプルアプリケーション。

他のクライアントがメッセージを投稿すれば、WebSocket経由でメッセージは追加されます（ページリロード不要）。

[解説](http://clina.jp/blog/cowboy-websocket.html)

## テスト方法

```
make
./_rel/message_wall_release/bin/message_wall_release console
```
