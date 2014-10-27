#!python

import unittest
from py2lisp import translate_literal

class TestLiterals(unittest.TestCase):

    def test_string(self):
        self.assertEqual("#()", translate_literal(""))
        self.assertEqual("#(0 0 0 97)", translate_literal("a"))
        self.assertEqual("#(0 0 0 97 0 0 0 98)", translate_literal("ab"))
        self.assertEqual("#(0 0 0 97 0 0 0 98 0 0 0 99)",
                          translate_literal("abc"))
        self.assertEqual("#(0 16 1 0)", translate_literal("\U00100100"))

    def test_int(self):
        self.assertEqual("-23", translate_literal(-23))
        self.assertEqual("0", translate_literal(0))
        self.assertEqual("5", translate_literal(5))
        self.assertEqual("99991152921504606846975",
                         translate_literal(99991152921504606846975))
        self.assertEqual("-99991152921504606846975",
                         translate_literal(-99991152921504606846975))

    def test_bytes(self):
        self.assertEqual("#()", translate_literal(b""))
        self.assertEqual("#(97)", translate_literal(b'\x61'))
        self.assertEqual("#(97 98)", translate_literal(b'\x61\x62'))
        self.assertEqual("#(97 98 99)", translate_literal(b'\x61\x62\x63'))

    def test_float(self):
        # Example from http://stackoverflow.com/questions/3481289/
        f = 0.38288746115497402
        self.assertEqual(f, float(translate_literal(f)))

    def test_complex(self):
        import re
        float_pattern = "([0-9]*(?:\\.[0-9]+)?)"
        complex_re = re.compile("^#C\\(%s %s\\)$" %
                                (float_pattern, float_pattern))
        f = 0.38288746115497402
        def test_complex(complex_):
            match = complex_re.match(translate_literal(complex_))
            self.assertTrue(match)
            self.assertEqual(float(match.group(1)), complex_.real)
            self.assertEqual(float(match.group(2)), complex_.imag)
        test_complex(f + 0j)
        test_complex(f * 1j)
        test_complex(f + f * 1j)

    def test_None(self):
        self.assertEqual("|None|", translate_literal(None))

    def test_bool(self):
        self.assertEqual("t", translate_literal(True))
        self.assertEqual("nil", translate_literal(False))

    def test_list(self):
        self.assertEqual("()", translate_literal([]))
        self.assertEqual("(1 2 3)", translate_literal([1, 2, 3]))
        self.assertEqual("(#(97) 1 #(0 0 0 98) |None| t nil)",
                         translate_literal([b'\x61', 1, 'b',
                                            None, True, False]))

if __name__ == "__main__":
    unittest.main()
