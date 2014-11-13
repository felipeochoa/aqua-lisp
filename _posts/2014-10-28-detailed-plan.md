---
layout: post
title: First draft of the Data Model
---

## Function calls

I think the most efficient way of handling function calls in Python
will be to translate them into function calls in Lisp. The main
difficulties will be:

* **Argument parsing**
  [This PEP](http://legacy.python.org/dev/peps/pep-3102/) describes
  how Python parses function calls and binds arguments to their
  values. I think with some care, much of this functionality can be
  replicated using macros.
* **Scope** [This PEP](http://legacy.python.org/dev/peps/pep-0227/)
  describes how Python creates scope. Since Python uses lexical
  scoping, we can translate these rules efficiently into Lisp `let`s
  (though likely using `setq` more often than in traditional Lisp
  code). Importantly, we can determine at compile-time which names are
  bound in which blocks.
* **Returning values** Translated code will likely use Lisp's
  `return-from` operator much more than idiomatic Lisp, and will
  similarly have to take care to return `None` when the Python
  function doesn't provide a return value

## Exception handling

Python's `try: ... except:...` blocks should be readily translatable
into signals and handlers in Lisp. Since we're implementing Python
functions as Lisp functions, in principle the Python stack will be (a
subsegment of) the Lisp stack. However, to allow introspection and
keep track of the Python-specific
[stack frames](https://docs.python.org/3.4/library/inspect.html#the-interpreter-stack),
it will likely be necessary to keep a parallel stack, which will
likely take the form of a dynamic variable, `*current-frame*`. We will
also be able to create `traceback` objects from this parallel stack.

## Classes and types

Python classes will be translated into CLOS classes. While I don't
expect this translation will be an easy task, I think CLOS + MOP is
sufficiently flexible to allow an efficient translation to be
generated. Initially though, classes will be implemented as CLOS
objects (of a particular a Lisp class `py-class`), and instances will
similarly be implemented as CLOS instances (of `py-object` super-class
of `py-class`). Class features (attribute resolution, metaclasses,
etc.) would be implemented manually (i.e., outside the MOP
scheme). Practically speaking, a Python `class` statement would be
translated into a `(make-instance 'py-class)` Lisp form, with some
initializing of the class.

Longer-term, a more efficient translation would likely implement
Python classes as different CLOS classes, using MOP machinery to
ensure attribute resolution, metaclasses, etc. work the same way as in
Python. I.e., translating a Python `class` statement into a Lisp
`defclass` form.

## Built-in types

Many of the built-in types that get used a lot in Python code will be
implemented with the Lisp built-in types, and thus will no not inherit
from `py-object`.  We'll therefore have to special-case some of the
syntax-related functions (like `getattr`) to deal with these objects,
but it will make it easier for Lisp code to call Python code. I think
that added simplicity for client code is worth the complexity in the
translation code.

### Numeric types

Python numbers have easy analogues in Lisp types:

| Python type | Lisp type      |
+-------------+----------------+
| `int`       | `integer`      |
| `float`     | `double-float` |
| `complex`   | `complex`      |


### Strings, bytes, bytearray, memoryview

Similar to what we did when defining the Python DSL, we'll represent
 `bytes` and `bytearray` in our runtime as vectors, only for
 efficiency we'll use the type `(unsigned-byte 8)`. We'll have to
 manually impose the immutability of `bytes`.

Python `str` objects are a little tricky to implement since Common
Lisp does not require supporting any characters beyond the
[96 standard characters](http://www.lispworks.com/documentation/HyperSpec/Body/02_ac.htm). To
use native functions wherever possible, we'll likely have to use a
structure similar to
[the one CPython uses](http://legacy.python.org/dev/peps/pep-0393/).
Strings whose characters all fall within the implementation's
definition of `character` are then represented with native strings,
and all other strings are represented with vectors of type
`(unsigned-byte x)` for `x` in `{8, 16, 32}`.

As for `memoryview`, the whole issue of the C API is something I want
to punt on for now, but if the need arose I could see implementing
this class for the built-in types.

### List

Per
[the docs](https://docs.python.org/3.4/faq/design.html#how-are-lists-implemented)
Python's `list`s are implemented as variable-length arrays, so we will
do the same. We'll leave the "cleverness" behind the resizing of the
array to Lisp for now though.

### Tuple

Tuples will also be arrays, with the immutability imposed by our code.

### Dict

Per
[the same docs](https://docs.python.org/3.4/faq/design.html#how-are-lists-implemented),
Python's `dict`s are implemented as hash tables, which we will also
do. In the future, it might make sense to mimic Python's
implementation of the hashing algorithms and hash-table access.

#### dict_keys, dict_items, dict_values

These will likely be full-blown objects to allow for their "live" state.

### Generators

I think the most logical way to implement these is as wrappers to
continuations. I'll have to explore the different continuations
libraries (or go look at
[Paul Graham's _On Lisp_](http://www.paulgraham.com/onlisp.html) for
his "fake" implementation).

### File objects

These will be wrappers around Lisp file streams, with Python's `open`
getting translated into an appropriate call to Lisp's `open`.

### Other

#### Sets and frozensets

Initially, as Python did, these could be hashtables with dummy
values. I imagine there are small-enough implementations of these out
there that including (via copy-paste-adapt) one in `py-lisp` would not
be too difficult.

#### Range

These will be full-blown objects
