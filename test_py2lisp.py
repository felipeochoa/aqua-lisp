#!python

import unittest
import ast
from py2lisp import translate, translate_literal

class TestLiterals(unittest.TestCase):

    function = staticmethod(translate_literal)

    def test_string(self):
        self.assertEqual("#()", self.function(""))
        self.assertEqual("#(0 0 0 97)", self.function("a"))
        self.assertEqual("#(0 0 0 97 0 0 0 98)", self.function("ab"))
        self.assertEqual("#(0 0 0 97 0 0 0 98 0 0 0 99)",
                          self.function("abc"))
        self.assertEqual("#(0 16 1 0)", self.function("\U00100100"))

    def test_int(self):
        self.assertEqual("-23", self.function(-23))
        self.assertEqual("0", self.function(0))
        self.assertEqual("5", self.function(5))
        self.assertEqual("99991152921504606846975",
                         self.function(99991152921504606846975))
        self.assertEqual("-99991152921504606846975",
                         self.function(-99991152921504606846975))

    def test_bytes(self):
        self.assertEqual("#()", self.function(b""))
        self.assertEqual("#(97)", self.function(b'\x61'))
        self.assertEqual("#(97 98)", self.function(b'\x61\x62'))
        self.assertEqual("#(97 98 99)", self.function(b'\x61\x62\x63'))

    def test_float(self):
        # Example from http://stackoverflow.com/questions/3481289/
        f = 0.38288746115497402
        self.assertEqual(f, float(self.function(f)))

    def test_complex(self):
        import re
        float_pattern = "([0-9]*(?:\\.[0-9]+)?)"
        complex_re = re.compile("^#C\\(%s %s\\)$" %
                                (float_pattern, float_pattern))
        f = 0.38288746115497402
        def test_complex(complex_):
            match = complex_re.match(self.function(complex_))
            self.assertTrue(match)
            self.assertEqual(float(match.group(1)), complex_.real)
            self.assertEqual(float(match.group(2)), complex_.imag)
        test_complex(f + 0j)
        test_complex(f * 1j)
        test_complex(f + f * 1j)

    def test_None(self):
        self.assertEqual("|None|", self.function(None))

    def test_bool(self):
        self.assertEqual("t", self.function(True))
        self.assertEqual("nil", self.function(False))

    def test_list(self):
        self.assertEqual("()", self.function([]))
        self.assertEqual("(1 2 3)", self.function([1, 2, 3]))
        self.assertEqual("(#(97) 1 #(0 0 0 98) |None| t nil)",
                         self.function([b'\x61', 1, 'b',
                                        None, True, False]))


class TestTranslate(TestLiterals):

    function = staticmethod(translate)

    def test_leaf_node(self):
        self.assertEqual("(|py-Gt|)", self.function(ast.Gt()))
        self.assertEqual("(|py-Load|)", self.function(ast.Load()))

    def test_tree_node(self):
        self.assertEqual("(|py-Bytes| #(97 98))",
                         self.function(ast.Bytes(b'\x61\x62')))
        self.assertEqual("(|py-Str| #(0 0 0 97))",
                          self.function(ast.Str('a')))
        self.assertEqual("(|py-Num| 97)", self.function(ast.Num(97)))
        self.assertEqual("(|py-NameConstant| t)",
                         self.function(ast.NameConstant(True)))
        self.assertEqual("(|py-NameConstant| nil)",
                         self.function(ast.NameConstant(False)))
        self.assertEqual("(|py-NameConstant| |None|)",
                         self.function(ast.NameConstant(None)))
        self.assertEqual("(|py-Module| " +
                         "((|py-Expr| (|py-NameConstant| |None|))" +
                         " (|py-Expr| (|py-NameConstant| |None|))" +
                         " (|py-Expr| (|py-NameConstant| |None|))))",
                         self.function(ast.Module(
                             [ast.Expr(ast.NameConstant(None))] * 3)))

    def test_parsing(self):
        [("None", "(|py-Module| ((|py-Expr| (|py-NameConstant| |None|))))"),
         ("3", "(|py-Module| ((|py-Expr| (|py-Num| 3))))"),
         ("'abc'", "(|py-Module| ((|py-Expr| (|py-Str| "+
          "#(0 0 0 97 0 0 0 98 0 0 0 99)))))"),
         ("if a:\n  return b\nelse:\n  continue", (
             "(|py-Module|" +
             " (|py-If| (|py-Name| #(0 0 0 97) (|py-Load|))" +
              " ((|py-Return| (|py-Name| #(0 0 0 98) (|py-Load|))))" +
              " ((|py-Continue|))))"))
        ]


if __name__ == "__main__":
    unittest.main()
