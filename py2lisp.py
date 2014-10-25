#!python

"""
This module translates Python source into Lisp forms.

The generated Lisp is little more than a Python-heavy DSL which depends on a
Lisp runtime for all the macro/function definitions.

"""

import ast


def translate(node_or_literal):
    "Convert an AST node or Python literal into a Lisp form (recursively)."
    if isinstance(node_or_literal, ast.AST):
        symbol = "|py-%s|" % node_or_literal.__class__.__name__
        if not node_or_literal._fields:  # this is a leaf node
            return "(%s)" % symbol
        args = " ".join(translate(sub_nl)
                        for _, sub_nl in ast.iter_fields(node_or_literal))
        return "(%s %s)" % (symbol, args)
    return translate_literal(node_or_literal)


def translate_literal(literal):
    "Translate a Python literal into a Lisp literal."
    if isinstance(literal, str):
        return translate_literal(literal.encode("utf-8"))
    elif isinstance(literal, bytes):
        return "#(%s)" % " ".join(int.from_bytes(b, "big")
                                       for b in literal)
    elif literal is None:
        return "|None|"
    elif isinstance(literal, bool):
        return "t" if literal else "nil"
    elif isinstance(literal, list):
        return "(%s)" % " ".join(map(translate, literal))
    elif isinstance(literal, complex):
        return "#C(%s %s)" % (literal.real, literal.imag)
    else:  # Should be an integer or float
        return str(literal)
