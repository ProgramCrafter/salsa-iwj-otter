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
configuration handles the TLS.

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
"game::debugreader" = 'info'
```

The `*.sse` wildcard domain is to work around a bug in the web
facility "server-sent events".  You will probably want one of those
too.  The SSE bug is documented on MDN
<https://developer.mozilla.org/en-US/docs/Web/API/EventSource>.


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


Stability and semver
--------------------

The following interfaces are covered by semantic versioning:

 * Save game compatibility (new servers loading old games)
 * Command line (new client invoked in old ways)
 * Game and table specifications (new software, old specs)
 * Library bundles (new servers, old bundles)

Additionally,
we will try to support old library bundles indefinitely
(and to break them as little as possible).

Of course when a new server is handling a game that uses new features,
old software (eg old management clients) may not cope.

The following are **NOT** covered by semantic versioning:

 * All Rust library APIs.
 * The web templates, AJAX protocol between frontend and backend,
   and JavaScript internals.
 * Server-client game management protocol (you may need to run the
   same version of otter on the client as the server is running)

This means that the Otter crates do not offer a public stable API.
You may of course use them outside of the Otter project,
but we have no compuction about breaking such use.
