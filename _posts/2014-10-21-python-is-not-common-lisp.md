---
layout: post
title: Python is not Common Lisp
---

## Project aim

Simply put, I want to bring the vast number of Python libraries to the
Common Lisp world. I want to give a better answer to
[this Stack Overflow question](http://stackoverflow.com/questions/5174199/).


The process I envision would operate as follows:

1. Python community develops awesome library `pyX` to do `X`.
2. Avid Lisper downloads `pyX` and runs the source through a Python
   script that generates Lisp code.
3. Said Lisper writes a thin wrapper around the generated code,
   packages the code as library `CLX` and puts it up on Quicklisp or
   similar.
4. A different (possibly less avid) Lisper without access to a Python
   interpreter and needing to do `X` downloads `CLX` along with its
   dependency `pylisp`. She happily peforms `X` in Lisp, no Python
   necessary.

## Why this project?

In my experience, Python has many more and better libraries than
Common Lisp. I have many times wanted to work on certain projects that
I think would be well suited for CL, only to discover that there's no
robust (for example) PDF-parsing library.

## Why this structure?

The way I see it, there are fundamentally three ways of bridging the
Python-Lisp gap:

### 1. Embed Common Lisp in Python

The example I first came across is
[Peter Norvig's](http://norvig.com/lispy2.html), but the closest thing
I've found is [`Hy`](https://github.com/paultag/hy), which is more of
a Python dialect written in S-expressions (and freaking awesome,
too!).

I suspect that doing this properly would be a _massive_ undertaking,
amounting to a full re-write of CL in Python.


### 2. Embed Python in Lisp

There is [`cl-python`](https://github.com/metawilm/cl-python), which
targets compatibility with Python 2.7, but it is unfortunately no
longer maintained. This seems to me like the closest anyone has come
to bridging the two languages. It installs with quicklisp, but the
test suite does not pass on my machine. (There are a bunch of
"Expected test failure for ... did not occur."; I may dig into the
source to investigate what is going on.) At any rate, this project
seems more ambitious than my own, since it provides a means of calling
Lisp from Python as well.

This is my favored approach. The Python `ast` has relatively few
nodes, most of which should be relatively straightforward to
implement. I expect that classes may require some MOP trickery to get
right and function calls/proper scoping of variables may require
special care. Some of the introspection machinery in particular seems
difficult to reproduce. The oft-pointed to
[similarity between Python and Lisp](http://norvig.com/python-lisp.html)
is, I think, quite-apparent at this level.

### 3. Provide an FFI

I'm not sure, but I think
[`burgled-batteries`](http://www2s.biglobe.ne.jp/~niitsuma/pythononlispex.html)
is the top choice here, though it's been 3 years since the last
commit. They list [`Pyffi`](http://www.cliki.net/Pyffi) (appears to
only support Python up to 2.5), `cl-python`, and
[`Python-on-lisp`](http://common-lisp.net/project/python-on-lisp/)
(dead since 06).

I think this is a tremendously elegant solution which basically
reduces your requirements down to a DLL. Despite having written
several Python C-extensions, I still find its C API a bit daunting.


## Project plan

1. Initially, I will rely on a Python interpreter to translate Python
   source, into an
   [`AST`](http://greentreesnakes.readthedocs.org/en/latest/), and
   then into an S-expression representation of the language. (Perhaps
   longer-term, the project may warrant having its own lexer/parser to
   generate ASTs.) I expect to create a Python script to perform this
   rough translation first.
2. The first real order of business will be to implement all the AST
   nodes as Lisp in a require-able runtime. Many of the nodes will be
   straightforward to write as macros, but I expect that `ClassDef`
   will require some careful work with MOP and
   `Assign`/`Delete`/`Name` will require some overhead to track
   scope. Some of the other nodes I'll likely just punt to the next
   step. (E.g., `Import`)
3. The next step will be creating the
   [built-in types and functions](https://docs.python.org/3.4/library/functions.html)
   (including the aforementioned `__import__`) that most Python
   programs rely on.
4. Once we can run basic (`import`-less) programs, we'll need to
   re-implement the part of the standard library that does not have a
   pure-Python equivalent. I imagine some of these will take longer
   (e.g. `_socket`) than others (e.g. `math`).
5. Figure out how to use C modules


Throughout all of this, I hope to learn all the nooks and crannies
of Python and understand their design decisions with the language,
while increasing my familiarity with CL.
