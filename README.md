[atdjs](https://github.com/barko/atdjs) is a tool for generating
serializers and unserializers from
[atd](https://github.com/MyLifeLabs/atd) type definition.  Like
[atdgen](https://github.com/MyLifeLabs/atdgen/),
[atdjs](https://github.com/barko/atdjs) generates OCaml source code.
However, unlike [atdgen](http://oss.wink.com/atdgen/), the generated
code is intended for compilation by the
[js_of_ocaml](https://ocsigen.org/js_of_ocaml) compiler, an OCaml
compiler with a Javascript backend.

[atdjs](https://github.com/barko/atdjs) simplifies the construction of
browser-based client apps (written in OCaml and compiled with
[js_of_ocaml](https://ocsigen.org/js_of_ocaml) which send/receive
typed messages via Javascript's XHR.

Plain OCaml and [atd]https://github.com/MyLifeLabs/atd/) are required
for building [atdjs](https://github.com/barko/atdjs).
[js_of_ocaml](https://ocsigen.org/js_of_ocaml) and
[jsonoj](https://github.com/barko/jsonoj) are required for building
the code which [atdjs](https://github.com/barko/atdjs) generates.
