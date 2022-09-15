#!/usr/bin/env bash
REMOTE_SERVER_PORT=18800
python3 -m http.server --directory resources/test/mock_server $REMOTE_SERVER_PORT &
server_pid=$!

curl -i -d source=http://localhost:$REMOTE_SERVER_PORT/article.html -d target=https://astrid.tech/foo/bar http://localhost:8000/api/webmention

sleep 1

curl -X POST http://localhost:8000/api/rpc/processWebmentions

sleep 4

kill $server_pid

