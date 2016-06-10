## TypeTL

A library containing currently implemented functions for binaries and ETS tables
designed to be equivalent in function and form to functions acting over lists in
`erlang` and `lists` modules. Accompanies paper entitled *Semi-Automatic
Data-Type Translation for Parallelism in Erlang* submitted for consideration to
the
[Fifteenth ACM SIGPLAN Erlang Workshop in Nara, Japan](http://erlang.org/workshop/2016/).

The library is set up using Rebar3, with ETS-equivalent functions in the
`etslists` module, and binary-equivalent functions in the `binarylists` module.
We include a suite of simple EUnit tests for demonstration. These tests can be
invoked by running `make test`.

## Primitive and Library List Function Classifications

Below we include full listings of classifications for each primitive and library
function. These classifications are used as part of the decision process to
bound the refactoring.

| Inspection/Single-Element-Lookup   | Addition    | Removal     | Creation    | Map          | Fold                    |
| ---------------------------------  | --------    | -------     | --------    | ---          | ----                    |
| `hd`                               | `append`    | `tl`        | `duplicate` | `foreach`    | `foldl`                 | 
| `length`                           | `concat`    | `delete`    | `seq`       | `keymap`     | `foldr`                 |
| `all`                              | `flatmap`   | `droplast`  |             | `keyreplace` | `mapfoldl`              |
| `any`                              | `flatten`   | `dropwhile` |             | `keysort`    | `mapfoldr`              |
| `flatlength`                       | `keymerge`  | `filter`    |             | `keystore`   | `partition`             |
| `keyfind`                          | `merge`     | `filtermap` |             | `map`        | `split`                 |
| `keymember`                        | `ukeymerge` | `keydelete` |             | `reverse`    | `splitwith`             |
| `keysearch`                        | `umerge`    | `keytake`   |             | `sort`       | `list_to_tuple`         |
| `last`                             |             | `nthtail`   |             | `ukeysort`   | `list_to_binary`        |
| `max`                              |             | `sublist`   |             | `zip`        | `list_to_pid`           |
| `member`                           |             | `subtract`  |             | `zipwith`    | `list_to_atom`          |
| `min`                              |             | `takewhile` |             |              | `list_to_float`         |
| `nth`                              |             |             |             |              | `list_to_integer`       |
| `prefix`                           |             |             |             |              | `list_to_bitstring`     |
| `suffix`                           |             |             |             |              | `list_to_existing_atom` |
| `sum`                              |             |             |             |              | `iolist_size`           |
|                                    |             |             |             |              | `iolist_to_binary`      |
