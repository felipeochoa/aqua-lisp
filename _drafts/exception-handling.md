---
layout: post
title: Exception Handling
---

As we begin implementing the DSL, one thing we'll find is that raising
exceptions is an activity we'll have to do frequently. We'll therefore
begin our implementation of `py-lisp` with the `raise`, `try`,
`except`, and `finally` statements.

## Exceptions as signals

Lisp already provides signals, which are more general than exceptions,
so we may as well translate exception raising into error signaling. We
can then translate `try:...except:...` statements can into calls to
`handler-case` and `try:...finally:...` statements into calls to
`unwind-protect`. (`try` statements with both `except` clauses and a
`finally` clause can be translated into a `handler-case` inside an
`unwind-protect`).

Determining which handlers are compatible with which signals can turn
into a source of friction if we try and map every Python exception
into a different Lisp error. Instead, we'll use the same error type
`py-exception` and "attach" the Python exception object to the signal.

~~~ lisp
(define-condition py-exception (error)
  ; This is the error used to signal Python exceptions
  ((exception :initarg :exception :reader get-exception)))
~~~

Finally, because of the nature of exception handling, we'll need a way
of extracting the current Python stack into a `traceback`
object. We'll implement this functionality when we implement function
calls, so for now we'll just specify the function we'll use to obtain
the current traceback:

~~~ lisp
(defun get-current-traceback ())
~~~


## Raising exceptions

The Python `raise` statement has three primary means of operation:

1. **Called with no arguments**: re-raises the exception being handled, or
   `RuntimeError` if there is no exception being handled
2. **Called with a class argument**: creates an exception instance
   using the given class and raises it. If the class doesn't derive
   from `BaseException`, raises `TypeError`
3. **Called with an instance argument**:  raises the given exception
   instance, or `TypeError` if it isn't a `BaseException` object

Ultimately, all three forms of the statement raise an exception
instance, so we can start with that one. There are 5 things that
`raise` must do when given an exception instance:

1. Store the current `traceback` as `exception.__traceback__`.
   (described in
   [PEP 3134](http://legacy.python.org/dev/peps/pep-3134/))
2. If there's an exception currently being handled, store it under
   `exception.__context__` (also described in PEP 3134).
3. If the statement has a `from` clause, store its value under
   `exception.__cause__` (or raise `TypeError` if the given value is
   not an exception)
4. Create a Lisp `error` with the Python `exception` in one of its
   slots
5. Signal the error to calling code

~~~ lisp
(defun raise-exception (exception &optional from)
  "Raise an exception in the current context."
  (unless (isinstance exception builtins:|BaseException|)
    (raise-builtin :|TypeError| ("exceptions must derive from BaseException")))
  (setattr exception "__traceback__" (get-current-traceback))
  (when *current-exception*
    (setattr exception "__context__" *current-exception*))
  (when from
    (if (isinstance from builtins:|BaseException|)
        (setattr exception "__cause__" from)
        (raise-builtin :|TypeError| ("exception causes must derive from BaseException"))))
  (setf *current-exception* exception)
  (error (make-instance 'py-exception :exception exception)))
~~~

I'm using the `isinstance` and `setattr` forms, which I haven't
mentioned but sit in *py-user.lisp*. There's also `|BaseException|`,
which we'll discuss below. Also worth noting is that `raise-builtin`
will call `raise-exception` again, but with an actual exception
instance and no from clause, so there will be at most one recursive
call. Implementing the other two cases of the `raise` statement gives
us:

~~~ lisp
(defun raise-exception-class (cls &optional from)
  "Make an exception and raise it in the current context."
  (raise-exception (pycall cls) from))

(defun raise-statement (&optional cls-or-instance from)
  (unless cls-or-instance
    (if *current-exception*
        (error (make-instance 'py-exception :exception *current-exception*))
        (raise-builtin :|RuntimeError| '("No active exception to reraise"))))
  (if (classp cls-or-instance)
      (raise-exception-class cls-or-instance from)
      (raise-exception cls-or-instance from)))
~~~

Now I'm using `classp`, which is a simple predicate test for whether
an object is a Python class.


## The built-in exceptions

Python defines
[61 built-in exceptions](https://docs.python.org/3.4/library/exceptions.html#exception-hierarchy)
that we may want to raise from our Lisp code. (Other exceptions could
get defined and raised in client code.) For ease of access from our
Lisp code, we'll define a hash-table `*builtin-exceptions*`mapping
symbols to exception classes, which will allow us to say

~~~ lisp
(raise-exception (gethash '|TypeError| *builtin-exceptions*)
                 "int() takes at most 2 arguments (3 given)")
~~~
