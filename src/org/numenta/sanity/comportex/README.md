# Sanity Server

The server runs a "simulation" and maintains a "journal".

The server listens to two channels, one for the simulation and one for the journal. The client sends messages into these channels to control the simulation and to request / subscribe to content. The server responds by putting values on client-provided channels. To put a value v on a channel remotely, send

~~~clojure
[target-id :put! v]
~~~

into the websocket. This `target-id` is provided by the client. The client sends similar messages to the server, e.g.

~~~clojure
;; 24601 and 24602 are target-ids
[:into-journal :put! [:subscribe 40 24601 24602]]
~~~

This server doesn't serve HTM models directly to the client. It serves a description of "what to draw". It serves content. In other words, it's a webserver, not a database. The format of these "what to draw" descriptions is different for each command.
