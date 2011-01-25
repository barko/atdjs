[atdjs](http://github.com/barko/atdjs) is a tool for generating
serializers and unserializers from [atd](http://oss.wink.com/atd/)
type definition.  Like [atdgen](http://oss.wink.com/atdgen/),
[atdjs](http://github.com/barko/atdjs) generates OCaml source code.
However, unlike [atdgen](http://oss.wink.com/atdgen/), the generated
code is intended for compilation by the
[ocamljs](http://github.com/jaked/ocamljs) compiler, an OCaml compiler
with a Javascript backend.  

[atdjs](http://github.com/barko/atdjs) simplifies the construction of
browser-based apps (written in OCaml and compiled with
[ocamljs](http://github.com/jaked/ocamljs) which send/receive typed
messages via Javascript's XHR.
