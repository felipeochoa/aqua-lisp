(defpackage :py-user
  (:use :common-lisp))

(in-package :py-user)

(defun getattr (obj &rest attrs)
  "Attribute accessor for `obj`.
`obj` should be a `py-object` and `attrs` should be `lisp-string`s"
  (declare (ignore obj attrs)))

(defmacro assign (target value)
  "Assign value to `target`.
`target` should be a symbol or `getattr` or `getitem` form.
`value` is the value to assign to target"
  (declare (ignore target value)))

(defun setattr (obj attr value)
  "Set the object's attribute to the given value."
  `(assign (getattr ,obj ,attr) ,value))

(defmacro pylambda ((&optional args starargs
                               kwargs starkwargs)
                    &body body)
  "Create a (nameless) function object."
  (declare (ignore args starargs kwargs starkwargs body)))

(defmacro pydef (name
                 (&optional args starargs kwargs starkwargs)
                 &body body)
  "Create a named function, binding it in the appropriate scope."
  `(assign ,name (pylambda (,args ,starargs ,kwargs ,starkwargs) ,@body)))

(defun pycall (func &optional args starargs kwargs starkwargs)
  "Call a Python callable, `func`.
`args` is a Lisp list with all positional arguments.
`starargs` is the object to pass using *starargs.
`kwargs` is an alist with keyword arguments (though symbols should not be keywords themselves).
`starkwargs` is the object to pass using **starkwargs."
  (declare (ignore func args starargs kwargs starkwargs)))

(defun raise-builtin (class-name &key args kwargs)
  "Raise one of the builtin exceptions.
`class-name` should be a a (case-sensitive) keyword.
`args` should be a Lisp list of positional arguments to pass to the constructor.
`kwargs` should be an alist of keyword arguments to pass to the constructor."
  (declare (ignore class-name args kwargs)))

(defun pyclass (name bases &optional dict)
  "Create a new Python class and bind it to `name` in the appropriate scope.
`name` is the name to give the class.
`bases` should be Python `class` objects to use as the baseclasses for this class
`dict` should be an alist or `hash-table` that will fill the class namespace."
  (declare (ignore name bases dict)))
