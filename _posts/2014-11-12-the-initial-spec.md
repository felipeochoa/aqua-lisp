---
layout: post
title: The Initial Spec
---

As I started to implement some of the nodes in our Python DSL, I
realized there were a few elementary operations like attribute access
and assignment, exception raising, function calling and object
creation that I kept needing to perform at the Python level. Since
these operations are core to the language, they are highly
interdependent, and we can't just implement them sequentially. For
example, to implement function calling, we need to be able to raise
exceptions for invalid arguments; to raise exceptions we need to be
able to create them; and to create them we need to be able to call
their class.

To get around this circularity, we will first specify the interface
that these operations will obey and then we can attack these in any
order we like.

## Attribute access

Attribute access will be provided through a single function:

~~~ lisp
(defun getattr (obj &rest attrs))
~~~

We allow multiple attributes to be given in case of nested attribute
access. Thus `o.attr` in Python translates to `(getattr o attr)` and
`o.attr1.attr2` translates into `(getattr o attr1 attr2)`.

`getattr` will define a generalized variable so that assignment and
deletion work transparently (they way it does in Python).

## Assignment

Contrary to Lisp, Python variables default to the innermost lexical
scope, unless they are declared `nonlocal` or `global`. Managing this
difference will be the responsibility of our `assign` macro:

~~~ lisp
(defmacro assign (target value))
~~~

Since Python has it's own generalized variables (limited to attribute
and subscript access), assign will play a role similar to `setf` in
Lisp. Note that for simplicity `assign` will not destructure `target`
if it is a Python tuple or list, and will instead raise an error. If
it becomes necessary, we can provide a `py-destructuring-bind` macro
on top of assign.

## New functions

In Python, we can create functions using either the `def` statement or
the `lambda` expression. Since `def` creates function bindings in the
local scope (contrary to Lisp's `defun`), `def` can be thought of as a
multi-expression `lambda` together with an assignment. It will be
convenient for us to separate the function creation from the function
assignment (for decorators, binding methods, and possibly more). Our
primary means of creating Python functions will be using
`pylambda`:

~~~ lisp
(defmacro pylambda ((&optional args starargs kwargs starkwargs)
                    &body body))
~~~

The slightly strange signature is designed to allow replicating
Python's signature unambiguously.

~~~ lisp
(pylambda ((x y (z 3))
           args
           (kw1 (kw2 2))
           kwargs))
~~~

is equivalent to

~~~ python
lambda x, y, z=3, *args, kw1, kw2=2, **kwargs: None
~~~

Unlike `lambda` in Python, however, `pylambda` is not restricted to a
single Python expression. Thus `def` can be translated into a
`pylambda` combined with an assignment, which we wrap as:

~~~ lisp
(defmacro pydef (name
                 (&optional (&rest args) starargs (&rest kwargs) starkwargs)
                 &body body))
~~~

## Calling callables

Until we decide on the exact structure of Python objects in general,
and functions in particular, it will be good to have an easily
available hook we can use to wrap/unwrap objects, rearrange arguments,
etc. To provide this hook to our program, we'll use a function to call
our Python callable objects:

~~~ lisp
(defun pycall (func &optional args starargs kwargs starkwargs))
~~~

`pycall` is equivalent to the pseudo-Python:

~~~ python
func(args[0], args[1], ... args[n],
     *starargs,
     kw.keys[0]=kw.vals[0], kw.keys[1]=kw.vals[1], ... kw.keys[m]=kw.vals[m],
     **starkwargs)
~~~

In other words, positional arguments are passed as a (Lisp) list in
`args` and keyword arguments are in `kwargs` (as an alist), while
`starargs` and `starkwargs` handle the `*` and `**` arguments.

## Exceptions

For the core language features, we'll only need to be able to raise
built-in exceptions. For now we'll only define a function for doing so
easily:

~~~ lisp
(defun raise-builtin (class-name &key args kwargs))
~~~

`raise-builtin` accepts a class name, given as a (case-sensitive)
keyword (e.g., `:|TypeError|`), a (Lisp) list of positional arguments,
and an alist of keyword arguments. The arguments are passed to the
class constructor to create the instance that will be raised.

## New Python classes

Creating new Python classes does not require any new code, since we
can use the three argument form of `type` to create the class and
`assign` to place it in the local namespace. It will be convenient,
however, to provide a shortcut that takes Lisp objects for its
arguments:

~~~ lisp
(defmacro pyclass (name (&rest bases) &optional dict))
~~~

`pyclass` creates a new Python class named `name` with base-classes
given by `bases` (a Lisp list). If the optional argument `dict` (a
`hash-table` or `alist`) is given, it will fill the class
namespace. This function is roughly equivalent to `name = type(name,
bases, dict)`
