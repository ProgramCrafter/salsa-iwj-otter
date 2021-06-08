Installing
==========

This is not very polished or well-documented yet.

The `Makefile` has a `deploy` target which shows how things work on
chiark.

You will need to write a `server-config.toml`.  There is not currently
any documentation for that.  But there are some examples.  Consider
looking at `server-test.toml` in the toplevel, or one of the config
files made by the tests, in `tmp/*/server-config.toml`.

The config is a TOML file, which is read using Rust Serde.  You can
get at least a list of fields by looking at the Rustdoc-generated "API
doc" for the type `otter::config::ServerConfigSpec`, which is probably
in `target/doc/otter/config/struct.ServerConfigSpec.html`.

Here is a copy of the config file from chiark.  As you can see I am
running Otter as an "application server".  My usual Apache
configuration handles the TLS.  (Rocket's TLS is not advertised as
being suitable for production use.)

```
http_port = 1323
public_url = "https://otter.chiark.net"
sse_wildcard_url = "https://*.sse.otter.chiark.net"

base_dir = "/volatile/Otter"

save_dir = "/home/Otter/save"
command_socket = "/volatile/Otter/var/command.socket"

ssh_proxy_command = "/volatile/Otter/bin/otter"

[log]
global_level = 'debug'

[log.modules]
'hyper::server' = 'info'
rocket = 'error'
_ = "error" # rocket
"game::debugreader" = 'info'
```

The `*.sse` wildcard domain is to work around a bug in the web
facility "server-sent events".  You will probably want one of those
too.  The SSE bug is documented on MDN
<https://developer.mozilla.org/en-US/docs/Web/API/EventSource>
(as referenced by the docs for `rocket::response::Stream::chunked`
which is the underlying facility used by Otter).


Final weirdness
---------------

For running on chiark I build with the Rust target
`x86_64-unknown-linux-musl` which on my system is configured to
produce a completely statically linked binary.  I have this in my
`~/.cargo/config` (in the lesser privsep account):

```
[target.x86_64-unknown-linux-musl]
rustflags = ["-C", "target-feature=+crt-static"]
# ^ from https://stackoverflow.com/questions/31770604/how-to-generate-statically-linked-executables
```
