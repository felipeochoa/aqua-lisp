---
layout: post
title: Generators
---

I initially thought of implementing generators using either
[`cl-cont`](http://quickdocs.org/cl-cont/) or
[Paul Graham's continuation-passing macros](http://stackoverflow.com/questions/24721676/continuation-in-common-lisp-by-macros-regarding-an-implemetation-in-onlisp),
but I no longer think this strategy is necessary or appropriate. Using
continuations to implement generators is unnecessary because
generators do not need to store the entire call chain. Using
continuations is inappropriate because both implementations transform
code into continuation-passing style, which can cause problems with
implementations that don't properly optimize away tail calls.[^TCO]

Before describing a different implementation of Python generators,
I'll attempt to describe the main problems we need to solve:

1. Generators need to store the values of local variables when `yield`
   causes them to suspend.
2. Generators must be able to resume execution at an arbitrary point
   in the code body. This requirement includes jumping into loops and
   if branches
3. Generators need to store intermediate calculation values when
   `yield` is used as a sub-expression
4. Generators must support the two coroutine methods, `throw` and
   `send`.
5. Generators must allow yielding from `with` and `try`
   statements. Generators must ensure proper release of resources when
   garbage collected.

## 1. Local variables

The first problem is similar to the upward funarg problem in
implementing closures. In fact, since Python's local variables are
statically scoped[^static-scope], we can "lift" these variables out of
the generator scope and simply let Lisp handle the "local" variables
for us. For example, say we wanted to implement the following Python
generator:

~~~ python
def fib_gen():
    fib = 1
    prev = 0
    n = 0
    while True:
        fib, prev = fib + prev, fib
        n += 1
        yield (fib, n)
        # Yes, we're skipping the zero-th Fibonacci number
~~~

We could implement a similar concept in Lisp using
[let over lambda](http://letoverlambda.com/) as follows:

~~~ lisp
(defun fib-gen ()
  (let ((fib 1) (prev 0) (n 0))
    (lambda ()
      (psetq fib (+ fib prev)
             prev fib)
      (incf n)
      (values fib n))))
~~~

Instead of a generator with a `next()` method, our lisp code returns a
closure, but its behavior is otherwise identical.


## 2. Resuming execution

Since we're only implementing generators for our Python DSL, we only
need to worry about [a few control-flow statements](
http://greentreesnakes.readthedocs.org/en/latest/nodes.html#control-flow). With
the exception of `try`/`finally` and the `with` statement, all the
statements (`if`, `for`, `while`, `break`) can be compiled into a
`tagbody` and `go` forms (yes, I'm going there...). We can therefore
wrap the generator body in a `tagbody` and expand the control-flow
statements into appropriate tags and `go` calls. By appropriately
tagging `yield` statements, we can simply `go` to the right place in
the generator body when we need to resume.

Let's translate another Python generator into a Lisp closure:

~~~ python
def xrange(end):
    i = 0
    while i < end:
        yield i
        i += 1
~~~

In Lisp, this could be:

~~~ lisp
(defun xrange (end)
  (let ((i 0) (status 'not-started))
    (lambda ()
      (block gen
        (tagbody
         (case status
           ('not-started nil)
           ('started (go yield-restart))
           ('ended (go while-end)))
         (setq status 'started)
         while-start
         (unless (< i end) (go while-end))
         (return-from gen i)
         yield-restart
         (incf i)
         (go while-start)
         while-end
         (setq status 'ended)
         (error "generator ended"))))))
~~~

Though the tagbody results in ugly, difficult to read code, it gives
us the requisite flexibility to jump nearly anywhere in the generator
body. By extending `status` and the case statement as necessary, we
can support multiple `yield` expressions in the body of a generator.

## 3. Intermediate calculations

`tagbody` doesn't quite let us jump anywhere in the body of a
generator. For example, we can't pause the evaluation of arguments to
a function before calling the function. E.g., in `f(expr(),
(yield))`, the generator must store the value of `expr()` before
suspending, and must use the value to call `f` when it resumes.

The technique to deal with this case is to convert the intermediate
calculations into explicit (gensym-ed) variables, inserting the label
between the hidden assignment and the function call.[^evalorder] Let's
see an example:

~~~ python
def update_items(owner):
    for item in get_items_from_db(owner):
        update(item.get_name(),
               (yield item),  # Allow caller to modify item
               now())
~~~

In Lisp this generator could be

~~~ lisp
(defun update-items (owner)
  (let (item temp-1 temp-2 (status 'not-started))
    (lambda (val)
      (block gen
        (tagbody
         (case status
           ('not-started nil)
           ('started (go yield-restart))
           ('ended (go end)))
         (setq status 'started)
         (setq temp-1 (get-items-from-db owner))
         for-begin
         (handler-case
          (setq item (funcall temp-1))  ; item = next(temp_1)
          (error () (go for-end)))
         (setq temp-2 (get-name item))
         (return-from gen item)
         yield-restart
         (update temp-2 val (now))  ; val has the value sent in
         (go for-begin)
         for-end
         (setq status 'ended)
         end
         (error "Generator finished"))))))
~~~

## 4. Throw and send

Our last example implemented a `send` method. A full generator
implementation could use a message passing mechanism to signal the
closure whether the call was a `next`, `send`, or `throw` and pass in
the value or exception, if any.

## 5. Release of resources

> **Edit**: The original version of this post glossed over the  `next`,
> `send`, and `throw` implementations. I will expand this section over
> the next couple of days to address this non-trivial point.

Unlike `for` and `while`, `try...finally` (and its sibling, `with`),
cannot be emptied out into an enclosing `tagbody`, since in Lisp they
ultimately need to be wrapped in an `unwind-protect`.

The Python documentation specifies that the `__del__` method of the
generator calls its `close` method, ensuring that any `finally`
clauses and `with` blocks get resolved properly. Implementing the
`close` method as a wrapper around `throw` is straightforward; the
difficulty lies in ensuring the `__del__` method gets called. This
difficulty extends beyond just generators to any object that defines
custom finalization code and will be addressed separately.

## Footnotes

[^TCO]:
    I think most implementations support this optimization. See
    [this post by Marc Simpson](http://0branch.com/notes/tco-cl.html)
    for more details. Nonetheless, it seems dangerous to depend on such
    a feature for proper support.

[^static-scope]:
    By static I mean that local variables are determined
    at compile time rather than run time, and result in the use of the
    `LOAD_FAST` opcode.

[^evalorder]:
    [Lambda: The Ultimate Imperative](http://library.readscheme.org/page1.html),
    by Steele and Sussman, has a good discussion of evaluation order
    (see the endnote "evalorder"). Technically, we only need to
    temporarily store expressions that don't evaluate trivially,
    though in this case not all variables evaluate trivially (e.g.,
    module-level variables can change values between yielding and
    resuming).
