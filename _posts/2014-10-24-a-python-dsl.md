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

~~~ python
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
~~~


## Dumping the AST to a Lisp string

There are a lot of nodes, so at this stage rather than manually decide
how to represent every node, we'll dump the AST tree into generic
s-expression with minimal Python-side processing.

The first thing we'll want to define is a target format. Since we want
to do minimal Python processing, we'll use simple cons trees: Nodes
will follow the pattern `(node-name sub-node-1 sub-node-2
... sub-node-n)` and their literal attributes will be represented by
Lisp literal values.

For example, we'll want to say

~~~ python
translate(ast.parse("def f(arg1):\n  pass"))
~~~

and have it return a string containing the following form:

~~~ lisp
(|py-Module|
 ((|py-FunctionDef| "f"
                    (|py-arguments| ((|py-arg| "arg1" |None|))
                                    |None|
                                    ()
                                    ()
                                    |None|
                                    ())
                    ((|py-Pass|))
                    ()
                    |None|)))
~~~

(of course, we won't expect the translator to pretty print the form :)

We can use a simple recursive function to handle most cases:

~~~ python
def translate(node_or_literal):
    """Convert an AST node or Python literal into a Lisp form (recursively)."""
    if isinstance(node_or_literal, ast.AST):
        symbol = "|py-%s|" % node_or_literal.__class__.__name__
        if not node_or_literal._fields:  # this is a leaf node
            return "(%s)" % symbol
        args = " ".join(translate(sub_nl)
                        for _, sub_nl in ast.iter_fields(node_or_literal))
        return "(%s %s)" % (symbol, args)
    return translate_literal(node_or_literal)
~~~

We're punting on literal translation for a minute to momentarily savor
this quick success.

## Translating AST literals into Lisp

So far we've handled half of the translation we'd described; now we
are going to translate Python literals. Per the official docs, the
Python abstract grammar has six builtin types which can end up
becoming literals in the AST:

| Grammar type | `AST` attribute      | Lisp type |
|:-------------|:---------------------|:----------|
| `identifier` | `str`                | `simple-vector`
| `int`        | `int`                | `integer`
| `string`     | `str`                | `simple-vector`
| `bytes`      | `bytes`              | `simple-vector`
| `object`     | Used for `ast.Num`, where it's used to store a number | `number`
| `singleton`  | Used for `ast.NameConstant`, holds `True`, `False`,  or `None` | `t`, `nil`, or `|None|`
| `*`          | There's an implied seventh type `list`, which is indicated in the grammar by the use of `*` | `list`


Of all the attributes possible, only `None` is not built into Lisp as
a literal; we can leave it as a symbol for now and define it as an
object later on. We can represent `bytes` as literal vectors of
`fixnum`s using the `#()` notation.

It's worth noting that these choices for literals don't have to
correspond to our run-time (or even compiled) representations for
objects. We can make forms like `|Bytes|` or `|Str|` generate a better
represenation in Lisp, and keep our Python code simple for now.

### Unicode and Python strings

It may seem strange to not use Lisp `string`s to represent Python
`str` objects. Although Lisp has a `string` type, unicode support
across implementations is not great (and not guaranteed by the
standard), so we need a simple, portable represenation for the
translated source.

One such representation for `identifier` and `string` literals is to
encode them as big-endian `UTF-32` and then represent them like we
represent `bytes`. Because the grammar doesn't contain any places that
accept more than one of `string`, `identifier`, or `bytes`, we'll be
able to convert the `#()` vector into its run-time represenation once
we decide what that is and how to implement our DSL.

### Representing floats properly

[The Python data model](https://docs.python.org/3/reference/datamodel.html#the-standard-type-hierarchy)
explicitly states that `numbers.Real` leave you "at the mercy of the
underlying machine architecture for the accepted range and handling of
overflow." The `float` class (which is a different class altogether),
[states](https://docs.python.org/3.4/library/stdtypes.html#typesnumeric)
that they "are usually implemented using `double` in C."

Unfortunately,
[the Common Lisp spec](http://www.lispworks.com/documentation/HyperSpec/Body/t_short_.htm#double-float)
does not specify standard float sizes (though it does state
minimums), so we are not necessarily able to represent the same set of
floats in Python and Lisp. The best we can do is output float literals
using `repr`, which ensures that `float(repr(x)) == x` for all floats
`x` other than `inf`, `-inf`, and `NaN`.

Another issue we'll have to deal with later is representing the `IEEE
754` special values `inf`, `-inf`, `NaN`, which Common Lisp does not
require, and properly handling overflow calculations. Since Python
[doesn't have `inf` and `nan` literals](http://legacy.python.org/dev/peps/pep-0754/),
we don't have to worry about these problems (yet).

Our translation code for literals can therefore be specified as:

~~~ python
def translate_literal(literal):
    "Translate a Python literal into a Lisp literal."
    if isinstance(literal, str):
        return translate_literal(literal.encode("utf-32-be"))
    elif isinstance(literal, bytes):
        return "#(%s)" % " ".join(map(str, literal))
    elif literal is None:
        return "|None|"
    elif isinstance(literal, bool):
        return "t" if literal else "nil"
    elif isinstance(literal, complex):
        return "#C(%r %r)" % (literal.real, literal.imag)
    else:  # Should be an integer or float
        return repr(literal)
~~~


## Where to go from here?

Our implementation of the translation code has determined a first
version of our DSL. From here, we'll begin implementing macros and
functions so our translated code can run! I expect there will be some
node types we'll go back and special-case in Python, where different
forms may be easier to work with in Lisp. Of course, we'll also have
to design our (first version of the) data model to fully implement
many of the nodes here.
