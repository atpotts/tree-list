# Tree Drawing Tool

Small experiment.

The standard linux `tree` tool does it's own directory walking. It has the
ability to express patterns end excludes, but is still limited to walking
the filesystem itself.

This tool draws trees for arbitrary delimited strings. As standard, it will
split up directories by a `/` delimiter, but other delimiters can be specified
to enable other use cases.

The usual use case will be to use a seperate directory listing programme,
which may allow more elaborate filtering.

e.g.

```bash
> find . | tree-list
```

```
> git ls-files | tree-list
```

It has a couple of flags affecting the order of the output and the format of
the tree that is printed.

Ordinary view
```bash
> find examples | tac | tree-list
examples
 ├─y
 │  └─a
 │     ├─z
 │     └─a
 ├─x
 │  ├─a
 │  │  └─b
 │  │     ├─b
 │  │     │  └─c
 │  │     └─a
 │  └─c
 │     └─d
 └─c
    └─a
       └─a
```

Sorted:
```
> find examples | tac | tree-list -s
examples
 ├─c
 │  └─a
 │     └─a
 ├─x
 │  ├─a
 │  │  └─b
 │  │     ├─a
 │  │     └─b
 │  │        └─c
 │  └─c
 │     └─d
 └─y
    └─a
       ├─a
       └─z
```

Compact
```
> find examples | tac | tree-list -c
examples─┬─y───a─┬─z
         │       └─a
         ├─x─┬─a───b─┬─b───c
         │   │       └─a
         │   └─c───d
         └─c───a───a
```

