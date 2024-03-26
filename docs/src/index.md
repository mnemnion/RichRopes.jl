# RichRopes

```@meta
CurrentModule = RichRopes
```

[RichRopes.jl](https://github.com/mnemnion/RichRopes.jl) is a package implementing a
[rope data structure](https://www.wikiwand.com/en/Rope_(data_structure)).  This is
one of the classic string data structures used in text editing, with good performance
for insertion, deletion, and concatenation.

RichRopes enrich the nodes of the rope by including counts of `Char`s, lines, and
graphemes.  They are immutable, returning a new rope for every operation.  This
allows for shared structure, and easy-to-implement undo operations: simply return a
previous version of the rope.

RichRopes are natively indexed by `Char`, not bytes, meaning `rope[5:10]` will return
the fifth through tenth character.  By using
[`StringUnits`](https://mnemnion.github.io/StringUnits.jl/), codeunits and graphemes
may also be used to index and slice efficiently.  Textwidth is not stored in nodes,
because this metric is mainly useful for reasonably-short spans such as a single line,
but `tw` units from `StringUnits` are also supported for `RichRope` operations.

Support for the `AbstractString` interface is complete as to the basics.  Currently
absent are any searching operations, such as `findfirst|last` or `occursin`.  We
intend to support these for literal strings, unfortunately, regex is currently
impractical, since PCRE operates on the C interface, not natively Julian data
structures.

```@index
```

```@autodocs
Modules = [RichRopes]
```
