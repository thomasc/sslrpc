sslrpc
======

sslrpc allows ssl encrypted RPC between otherwise unconnected Erlang nodes.


Security
--------

* All communication between nodes is ssl encrypted
* Configurable server certificate and key
* Simple authentication based on the erlang cookie (overridable)


Usage
-----

The application can be configured to run as a client, a server or both.

``` erlang
% add a remote host
sslrpc:add_host("example.com").
% use a different port than the default (8443) 
sslrpc:add_host("example.com", [{port, 8080}]).
% manually set the cookie
sslrpc:add_host("example.com", [{auth_token, secret}]).
% RPC
sslrpc:call("example.com", erlang, nodes, []).
```

