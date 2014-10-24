---
layout: post
title: A Python DSL
excerpt: This post is about writing the Python script that creates s-expressions representing the Python program.
---


## Creating a DSL to express Python code in Lisp

As I stated in the first post, the first step in our workflow will be
to translate Python code into an abstract syntax tree (AST), using
Python itself, and then dump the tree into a Lisp form.

The [Python docs](https://docs.python.org/3.4/library/ast.html) aren't
great, but
[Green Tree Snakes](http://greentreesnakes.readthedocs.org/en/latest/nodes.html)
has a great reference with a list of all the nodes in Python's
abstract grammar and a description of what they're for.

Another way of obtaining a list of all the node types is as follows:

```py3
>>> import ast
>>> import inspect
>>> for name, c in inspect.getmembers(ast, inspect.isclass):
>>>     if issubclass(c, ast.AST):
>>>         print(name)
Add
And
Assert
# <snip>
unaryop
withitem
```


## Dumping the AST to a Lisp string

There are a lot of nodes, so at this stage rather than manually decide
how to represent every node, we'll dump the AST tree into generic
s-expression with minimal Python-side processing.

The first thing we'll want to define is a target format. Since we want
to do minimal Python processing, we'll use generic trees with branches
(e.g., `BinOp` or `withitem`) following the pattern `(node-name
sub-node-1 sub-node-2 ... sub-node-n)` and leaves that are either
`node-name` (e.g., `Ellipsis`) or `literal-value` (e.g., `3`).

For example, we'll want to say

```py3
translate(ast.parse("def f(arg1):\n  pass")))
```

and have it return a string containing the following form:

```lisp
(py-module
 ((py-functiondef "f"
                  (py-arguments ((py-arg "arg1" |None|)) |None| () () |None| ())
                  (py-pass)
                  ()
                  |None|)))
```

(of course, we won't expect the translator to pretty print the form :)

We can use a simple recursive function to handle most cases:

```py3
def translate(node_or_literal):
    """Convert an AST node or Python literal into a Lisp form (recursively)."""
    if isinstance(node_or_literal, ast.AST):
        symbol = "|py-%s|" % node_or_literal.__class__.__name__
        if not node_or_literal._fields:  # this is a leaf node
            return symbol
        args = " ".join(translate(sub_nl)
                        for _, sub_nl in ast.iter_fields(node_or_literal))
        return "(%s %s)" % (symbol, args)
    return translate_literal(node_or_literal)
```

We're punting on literal translation for a minute to momentarily savor
this quick success.

## Translating AST literals into Lisp

So far we've handled two of the three cases we'd described: branch
nodes and leaf nodes. Now we are going to translate Python
literals. Per the official docs, the Python abstract grammar has six
builtin types which can end up becoming literals in the AST:

| Grammar type | `AST` attribute      | Lisp type |
|--------------+----------------------+-----------|
| `identifier` | `str`                | `string`
| `int`        | `int`                |
| `string`     | `str`                |
| `bytes`      | `bytes`              |
| `object`     | Used for `ast.Num`, where it's used to store a number |
| `singleton`  | Used for `ast.NameConstant`, holds `True`, `False`,  or `None` |
| `*`          | There's an implied seventh type `list`, which is indicated in the grammar by the use of `*` |

Of all the attributes possible, only `None` is not built into Lisp as
a literal (we can express `bytes` as a literal vector). We can leave
it as a symbol for now and define it as an object later on. These
choices for literals don't have to correspond to our run-time
representations for objects. The real decision point will be when we
implement the `Str` node from our Pythonic DSL.

Our translation code is therefore

```py3
def translate_literal(literal):
    "Translate a Python literal into a Lisp literal."
    if isinstance(literal, str):
        return '"%s"' % literal
    elif isinstance(literal, bytes):
        return "#(%s)" % " ".join(int.from_bytes(b, "big")
                                       for b in literal)
    elif literal is None:
        return "|None|"
    elif isinstance(literal, bool):
        return "t" if literal else "nil"
    elif isinstance(literal, list):
        return "(%s)" % " ".join(map(translate, literal))
    else:  # Should be a number
        return str(literal)
```


## Where to go from here?

Our implementation of the translation code has determined a first
version of our DSL. From here, we'll begin implementing macros and
functions so our translated code can run! I expect there will be some
node types we'll go back and special-case in Python where different
forms may be easier to work with in Lisp. Of course, we'll have to
design our (first version of the) data model to fully implement many
of the nodes here.
