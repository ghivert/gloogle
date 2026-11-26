import gleam/dict
import gleam/dynamic/decode
import gleam/result
import gleam/time/calendar
import gleam/time/duration
import gleam/time/timestamp
import gleeunit
import tom

pub fn main() {
  gleeunit.main()
}

pub fn parse_empty_test() {
  assert tom.parse("") == Ok(dict.from_list([]))
}

pub fn parse_spaces_test() {
  assert tom.parse(" ") == Ok(dict.from_list([]))
}

pub fn parse_newline_test() {
  assert tom.parse("\n") == Ok(dict.from_list([]))
}

pub fn parse_crlf_test() {
  assert tom.parse("\r\n") == Ok(dict.from_list([]))
}

pub fn parse_quoted_key_test() {
  let expected = dict.from_list([#(" ", tom.Bool(True))])
  assert tom.parse("\" \" = true\n") == Ok(expected)
}

pub fn parse_single_key_test() {
  let expected = dict.from_list([#("", tom.Bool(True))])
  assert tom.parse("'' = true\n") == Ok(expected)
}

pub fn parse_true_test() {
  let expected = dict.from_list([#("cool", tom.Bool(True))])
  assert tom.parse("cool = true\n") == Ok(expected)
}

pub fn parse_false_test() {
  let expected = dict.from_list([#("cool", tom.Bool(False))])
  assert tom.parse("cool = false\n") == Ok(expected)
}

pub fn parse_unicode_key_test() {
  let expected = dict.from_list([#("பெண்", tom.Bool(False))])
  assert tom.parse("பெண் = false\n") == Ok(expected)
}

pub fn parse_int_test() {
  let expected = dict.from_list([#("it", tom.Int(1))])
  assert tom.parse("it = 1\n") == Ok(expected)
}

pub fn parse_int_underscored_test() {
  let expected = dict.from_list([#("it", tom.Int(1_000_009))])
  assert tom.parse("it = 1_000_0__0_9\n") == Ok(expected)
}

pub fn parse_int_positive_test() {
  let expected = dict.from_list([#("it", tom.Int(234))])
  assert tom.parse("it = +234\n") == Ok(expected)
}

pub fn parse_int_negative_test() {
  let expected = dict.from_list([#("it", tom.Int(-234))])
  assert tom.parse("it = -234\n") == Ok(expected)
}

pub fn parse_string_test() {
  let expected = dict.from_list([#("hello", tom.String("Joe"))])
  assert tom.parse("hello = \"Joe\"\n") == Ok(expected)
}

pub fn parse_string_escaped_quote_test() {
  let expected = dict.from_list([#("hello", tom.String("\""))])
  assert tom.parse("hello = \"\\\"\"\n") == Ok(expected)
}

pub fn parse_string_tab_test() {
  let expected = dict.from_list([#("hello", tom.String("\t"))])
  assert tom.parse("hello = \"\\t\"\n") == Ok(expected)
}

pub fn parse_string_newline_test() {
  let expected = dict.from_list([#("hello", tom.String("\n"))])
  assert tom.parse("hello = \"\\n\"\n") == Ok(expected)
}

pub fn parse_string_linefeed_test() {
  let expected = dict.from_list([#("hello", tom.String("\r"))])
  assert tom.parse("hello = \"\\r\"\n") == Ok(expected)
}

pub fn parse_escaped_slash_test() {
  let expected = dict.from_list([#("hello", tom.String("\\"))])
  assert tom.parse("hello = \"\\\\\"\n") == Ok(expected)
}

pub fn parse_float_test() {
  let expected = dict.from_list([#("it", tom.Float(1.0))])
  assert tom.parse("it = 1.0\n") == Ok(expected)
}

pub fn parse_bigger_float_test() {
  let expected = dict.from_list([#("it", tom.Float(123_456_789.9876))])
  assert tom.parse("it = 123456789.9876\n") == Ok(expected)
}

pub fn parse_multi_segment_key_test() {
  let expected =
    dict.from_list([
      #(
        "one",
        tom.Table(
          dict.from_list([
            #("two", tom.Table(dict.from_list([#("three", tom.Bool(True))]))),
          ]),
        ),
      ),
    ])
  assert tom.parse("one.two.three = true\n") == Ok(expected)
}

pub fn parse_multi_segment_key_with_spaeces_test() {
  let expected =
    dict.from_list([
      #(
        "one",
        tom.Table(
          dict.from_list([
            #("two", tom.Table(dict.from_list([#("three", tom.Bool(True))]))),
          ]),
        ),
      ),
    ])
  assert tom.parse("one  . two   .   three = true\n") == Ok(expected)
}

pub fn parse_multi_segment_key_quotes_test() {
  let expected =
    dict.from_list([
      #(
        "1",
        tom.Table(
          dict.from_list([
            #("two", tom.Table(dict.from_list([#("3", tom.Bool(True))]))),
          ]),
        ),
      ),
    ])
  assert tom.parse("\"1\".two.\"3\" = true\n") == Ok(expected)
}

pub fn parse_multiple_keys_test() {
  let expected = dict.from_list([#("a", tom.Int(1)), #("b", tom.Int(2))])
  assert tom.parse("a = 1\nb = 2\n") == Ok(expected)
}

pub fn parse_duplicate_key_test() {
  assert tom.parse("a = 1\na = 2\n") == Error(tom.KeyAlreadyInUse(["a"]))
}

pub fn parse_conflicting_keys_test() {
  assert tom.parse("a = 1\na.b = 2\n") == Error(tom.KeyAlreadyInUse(["a"]))
}

pub fn parse_empty_array_test() {
  let expected = dict.from_list([#("a", tom.Array([]))])
  assert tom.parse("a = []\n") == Ok(expected)
}

pub fn parse_array_test() {
  let expected = dict.from_list([#("a", tom.Array([tom.Int(1), tom.Int(2)]))])
  assert tom.parse("a = [1, 2]\n") == Ok(expected)
}

pub fn parse_multi_line_array_test() {
  let expected = dict.from_list([#("a", tom.Array([tom.Int(1), tom.Int(2)]))])
  assert tom.parse("a = [\n  1 \n ,\n  2,\n]\n") == Ok(expected)
}

pub fn parse_table_test() {
  let expected = dict.from_list([#("a", tom.Table(dict.from_list([])))])
  assert tom.parse("[a]\n") == Ok(expected)
}

pub fn parse_table_with_values_test() {
  let expected =
    dict.from_list([
      #(
        "a",
        tom.Table(
          dict.from_list([
            #("a", tom.Int(1)),
            #("b", tom.Table(dict.from_list([#("c", tom.Int(2))]))),
          ]),
        ),
      ),
    ])
  assert tom.parse(
      "[a]
a = 1
b.c = 2
",
    )
    == Ok(expected)
}

pub fn parse_table_with_values_before_test() {
  let expected =
    dict.from_list([
      #("name", tom.String("Joe")),
      #("size", tom.Int(123)),
      #(
        "a",
        tom.Table(
          dict.from_list([
            #("a", tom.Int(1)),
            #("b", tom.Table(dict.from_list([#("c", tom.Int(2))]))),
          ]),
        ),
      ),
    ])
  assert tom.parse(
      "name = \"Joe\"
size = 123

[a]
a = 1
b.c = 2
",
    )
    == Ok(expected)
}

pub fn parse_multiple_tables_test() {
  let expected =
    dict.from_list([
      #("name", tom.String("Joe")),
      #("size", tom.Int(123)),
      #(
        "a",
        tom.Table(
          dict.from_list([
            #("a", tom.Int(1)),
            #("b", tom.Table(dict.from_list([#("c", tom.Int(2))]))),
          ]),
        ),
      ),
      #("b", tom.Table(dict.from_list([#("a", tom.Int(1))]))),
    ])
  assert tom.parse(
      "name = \"Joe\"
size = 123

[a]
a = 1
b.c = 2

[b]
a = 1
",
    )
    == Ok(expected)
}

pub fn parse_inline_table_empty_test() {
  let expected = dict.from_list([#("a", tom.InlineTable(dict.from_list([])))])
  assert tom.parse("a = {}\n") == Ok(expected)
}

pub fn parse_inline_table_test() {
  let expected =
    dict.from_list([
      #(
        "a",
        tom.InlineTable(
          dict.from_list([
            #("a", tom.Int(1)),
            #("b", tom.Table(dict.from_list([#("c", tom.Int(2))]))),
          ]),
        ),
      ),
    ])
  assert tom.parse(
      "a = {
  a = 1,
  b.c = 2
}
",
    )
    == Ok(expected)
}

pub fn parse_inline_trailing_comma_table_test() {
  let expected =
    dict.from_list([
      #(
        "a",
        tom.InlineTable(
          dict.from_list([
            #("a", tom.Int(1)),
            #("b", tom.Table(dict.from_list([#("c", tom.Int(2))]))),
          ]),
        ),
      ),
    ])
  assert tom.parse(
      "a = {
  a = 1,
  b.c = 2,
}
",
    )
    == Ok(expected)
}

pub fn parse_invalid_newline_in_string_test() {
  assert tom.parse("a = \"\n\"") == Error(tom.Unexpected("\n", "\""))
}

pub fn parse_invalid_newline_windows_in_string_test() {
  assert tom.parse("a = \"\r\n\"") == Error(tom.Unexpected("\r\n", "\""))
}

pub fn parse_array_of_tables_empty_test() {
  let expected =
    dict.from_list([
      #(
        "a",
        tom.ArrayOfTables([
          dict.from_list([]),
          dict.from_list([]),
          dict.from_list([]),
        ]),
      ),
    ])
  assert tom.parse(
      "[[a]]
[[a]]
[[a]]
",
    )
    == Ok(expected)
}

pub fn parse_array_of_tables_nonempty_test() {
  let expected =
    dict.from_list([
      #(
        "a",
        tom.ArrayOfTables([
          dict.from_list([#("a", tom.Int(1))]),
          dict.from_list([#("a", tom.Int(2))]),
          dict.from_list([#("a", tom.Int(3))]),
        ]),
      ),
    ])
  assert tom.parse(
      "[[a]]
a = 1

[[a]]
a = 2

[[a]]
a = 3
",
    )
    == Ok(expected)
}

pub fn parse_array_of_tables_with_subtable_test() {
  let expected =
    dict.from_list([
      #(
        "fruits",
        tom.ArrayOfTables([
          dict.from_list([]),
          dict.from_list([
            #("name", tom.String("apple")),
            #(
              "physical",
              tom.Table(
                dict.from_list([
                  #("color", tom.String("red")),
                  #("shape", tom.String("round")),
                ]),
              ),
            ),
          ]),
        ]),
      ),
    ])
  assert tom.parse(
      "[[fruits]]

[[fruits]]
name = \"apple\"

[fruits.physical]  # subtable
color = \"red\"
shape = \"round\"
",
    )
    == Ok(expected)
}

pub fn parse_single_quote_string_test() {
  let expected = dict.from_list([#("a", tom.String("\\n"))])
  assert tom.parse("a = '\\n'\n") == Ok(expected)
}

pub fn parse_multi_line_string_test() {
  let expected = dict.from_list([#("a", tom.String("hello\nworld"))])
  assert tom.parse(
      "a = \"\"\"
hello
world\"\"\"
",
    )
    == Ok(expected)
}

pub fn parse_multi_line_single_quote_string_test() {
  let expected = dict.from_list([#("a", tom.String("hello\\n\nworld"))])
  assert tom.parse(
      "a = '''
hello\\n
world'''
",
    )
    == Ok(expected)
}

pub fn parse_multi_line_single_quote_string_too_many_quotes_test() {
  assert tom.parse(
      "a = '''
''''
'''
",
    )
    == Error(tom.Unexpected("''''", "'''"))
}

pub fn parse_multi_line_literal_string_with_hash_test() {
  let expected =
    dict.from_list([
      #(
        "a",
        tom.String(
          "This string contains a #hash character\nand more text after it\n",
        ),
      ),
    ])
  assert tom.parse(
      "a = '''
This string contains a #hash character
and more text after it
'''
",
    )
    == Ok(expected)
}

pub fn parse_multi_line_string_escape_newline_test() {
  let expected =
    dict.from_list([
      #("a", tom.String("The quick brown fox jumps over the lazy dog.")),
    ])
  assert tom.parse(
      "a = \"\"\"
The quick brown \\


  fox jumps over \\
    the lazy dog.\"\"\"
",
    )
    == Ok(expected)
}

pub fn parse_multi_line_string_escape_newline_windows_test() {
  let expected =
    dict.from_list([
      #("a", tom.String("The quick brown fox jumps over the lazy dog.")),
    ])
  assert tom.parse(
      "a = \"\"\"
The quick brown \\\r\n


  fox jumps over \\\r\n
    the lazy dog.\"\"\"
",
    )
    == Ok(expected)
}

pub fn parse_nan_test() {
  let expected = dict.from_list([#("a", tom.Nan(tom.Positive))])
  assert tom.parse("a = nan\n") == Ok(expected)
}

pub fn parse_positive_nan_test() {
  let expected = dict.from_list([#("a", tom.Nan(tom.Positive))])
  assert tom.parse("a = +nan\n") == Ok(expected)
}

pub fn parse_negative_nan_test() {
  let expected = dict.from_list([#("a", tom.Nan(tom.Negative))])
  assert tom.parse("a = -nan\n") == Ok(expected)
}

pub fn parse_infinity_test() {
  let expected = dict.from_list([#("a", tom.Infinity(tom.Positive))])
  assert tom.parse("a = inf\n") == Ok(expected)
}

pub fn parse_positive_infinity_test() {
  let expected = dict.from_list([#("a", tom.Infinity(tom.Positive))])
  assert tom.parse("a = +inf\n") == Ok(expected)
}

pub fn parse_negative_infinity_test() {
  let expected = dict.from_list([#("a", tom.Infinity(tom.Negative))])
  assert tom.parse("a = -inf\n") == Ok(expected)
}

pub fn parse_write_to_key_that_does_not_exist_test() {
  let expected =
    dict.from_list([
      #("apple", tom.Table(dict.from_list([#("smooth", tom.Bool(True))]))),
    ])
  assert tom.parse("apple.smooth = true\n") == Ok(expected)
}

pub fn parse_binary_test() {
  let expected = dict.from_list([#("a", tom.Int(0b101010))])
  assert tom.parse("a = 0b101010\n") == Ok(expected)
}

pub fn parse_binary_positive_test() {
  let expected = dict.from_list([#("a", tom.Int(0b101010))])
  assert tom.parse("a = +0b101010\n") == Ok(expected)
}

pub fn parse_binary_negative_test() {
  let expected = dict.from_list([#("a", tom.Int(0b101010 * -1))])
  assert tom.parse("a = -0b101010\n") == Ok(expected)
}

pub fn parse_binary_underscores_test() {
  let expected = dict.from_list([#("a", tom.Int(0b101010))])
  assert tom.parse("a = 0b1__010___1_0\n") == Ok(expected)
}

pub fn parse_octal_test() {
  let expected = dict.from_list([#("a", tom.Int(0o1234567))])
  assert tom.parse("a = 0o1234567\n") == Ok(expected)
}

pub fn parse_octal_positive_test() {
  let expected = dict.from_list([#("a", tom.Int(0o1234567))])
  assert tom.parse("a = +0o1234567\n") == Ok(expected)
}

pub fn parse_octal_negative_test() {
  let expected = dict.from_list([#("a", tom.Int(0o1234567 * -1))])
  assert tom.parse("a = -0o1234567\n") == Ok(expected)
}

pub fn parse_octal_underscores_test() {
  let expected = dict.from_list([#("a", tom.Int(0o1234567))])
  assert tom.parse("a = 0o1_23_45__6_7\n") == Ok(expected)
}

pub fn parse_hex_test() {
  let expected = dict.from_list([#("a", tom.Int(0xdeadbeef))])
  assert tom.parse("a = 0xdeadbeef\n") == Ok(expected)
}

pub fn parse_hex_positive_test() {
  let expected = dict.from_list([#("a", tom.Int(0xdeadbeef))])
  assert tom.parse("a = +0xdeadbeef\n") == Ok(expected)
}

pub fn parse_hex_negative_test() {
  let expected = dict.from_list([#("a", tom.Int(0xdeadbeef * -1))])
  assert tom.parse("a = -0xdeadbeef\n") == Ok(expected)
}

pub fn parse_hex_underscores_test() {
  let expected = dict.from_list([#("a", tom.Int(0xdeadbeef))])
  assert tom.parse("a = 0xd_e_a_d__b___e____e______f\n") == Ok(expected)
}

pub fn parse_hex_uppercase_test() {
  let expected = dict.from_list([#("a", tom.Int(0xdeadbeef))])
  assert tom.parse("a = +0xDEADBEEF\n") == Ok(expected)
}

pub fn parse_float_exponent_test() {
  let expected = dict.from_list([#("a", tom.Float(1.0e6))])
  assert tom.parse("a = 1e6\n") == Ok(expected)
}

pub fn parse_float_exponent_uppercase_test() {
  let expected = dict.from_list([#("a", tom.Float(1.0e6))])
  assert tom.parse("a = 1E6\n") == Ok(expected)
}

pub fn parse_float_exponent_postive_test() {
  let expected = dict.from_list([#("a", tom.Float(5.0e22))])
  assert tom.parse("a = 5e+22\n") == Ok(expected)
}

pub fn parse_float_exponent_negative_test() {
  let expected = dict.from_list([#("a", tom.Float(-2.0e-22))])
  assert tom.parse("a = -2e-22\n") == Ok(expected)
}

pub fn parse_float_decimal_and_exponent_test() {
  let expected = dict.from_list([#("a", tom.Float(6.626e25))])
  assert tom.parse("a = 6.626e25\n") == Ok(expected)
}

pub fn parse_float_decimal_and_exponent_positive_test() {
  let expected = dict.from_list([#("a", tom.Float(6.626e25))])
  assert tom.parse("a = 6.626e+25\n") == Ok(expected)
}

pub fn parse_float_decimal_and_exponent_negative_test() {
  let expected = dict.from_list([#("a", tom.Float(6.626e-25))])
  assert tom.parse("a = 6.626e-25\n") == Ok(expected)
}

pub fn parse_date_test() {
  let expected =
    dict.from_list([#("a", tom.Date(calendar.Date(1979, calendar.May, 27)))])
  assert tom.parse("a = 1979-05-27\n") == Ok(expected)
}

pub fn parse_time_test() {
  let expected =
    dict.from_list([#("a", tom.Time(calendar.TimeOfDay(7, 32, 1, 0)))])
  assert tom.parse("a = 07:32:01\n") == Ok(expected)
}

pub fn parse_time_zero_minute_test() {
  let expected =
    dict.from_list([#("a", tom.Time(calendar.TimeOfDay(7, 0, 1, 0)))])
  assert tom.parse("a = 07:00:01\n") == Ok(expected)
}

pub fn parse_time_nanoseconds_999999_test() {
  let expected =
    dict.from_list([#("a", tom.Time(calendar.TimeOfDay(7, 32, 1, 999_999_000)))])
  assert tom.parse("a = 07:32:01.999999\n") == Ok(expected)
}

pub fn parse_time_nanoseconds_09179_test() {
  let expected =
    dict.from_list([#("a", tom.Time(calendar.TimeOfDay(7, 32, 1, 91_790_000)))])
  assert tom.parse("a = 07:32:01.09179\n") == Ok(expected)
}

pub fn parse_time_nanoseconds_123456789_test() {
  let expected =
    dict.from_list([#("a", tom.Time(calendar.TimeOfDay(7, 32, 1, 123_456_789)))])
  assert tom.parse("a = 07:32:01.123456789\n") == Ok(expected)
}

pub fn parse_time_nanoseconds_1_test() {
  let expected =
    dict.from_list([#("a", tom.Time(calendar.TimeOfDay(7, 32, 1, 100_000_000)))])
  assert tom.parse("a = 07:32:01.1\n") == Ok(expected)
}

pub fn parse_time_nanoseconds_001_test() {
  let expected =
    dict.from_list([#("a", tom.Time(calendar.TimeOfDay(7, 32, 1, 1_000_000)))])
  assert tom.parse("a = 07:32:01.001\n") == Ok(expected)
}

pub fn parse_time_nanoseconds_000000789_test() {
  let expected =
    dict.from_list([#("a", tom.Time(calendar.TimeOfDay(7, 32, 1, 789)))])
  assert tom.parse("a = 07:32:01.000000789\n") == Ok(expected)
}

pub fn parse_time_no_seconds_test() {
  let expected =
    dict.from_list([#("a", tom.Time(calendar.TimeOfDay(7, 32, 0, 0)))])
  assert tom.parse("a = 07:32\n") == Ok(expected)
}

pub fn parse_date_time_test() {
  let expected =
    dict.from_list([
      #(
        "a",
        tom.DateTime(
          calendar.Date(1979, calendar.May, 27),
          calendar.TimeOfDay(7, 32, 0, 0),
          offset: tom.Local,
        ),
      ),
    ])
  assert tom.parse("a = 1979-05-27T07:32:00\n") == Ok(expected)
}

pub fn parse_date_time_space_test() {
  let expected =
    dict.from_list([
      #(
        "a",
        tom.DateTime(
          calendar.Date(1979, calendar.May, 27),
          calendar.TimeOfDay(7, 0, 1, 0),
          offset: tom.Local,
        ),
      ),
    ])
  assert tom.parse("a = 1979-05-27 07:00:01\n") == Ok(expected)
}

pub fn parse_offset_z_date_time_test() {
  let expected =
    dict.from_list([
      #(
        "a",
        tom.DateTime(
          calendar.Date(1979, calendar.May, 27),
          calendar.TimeOfDay(7, 32, 0, 0),
          offset: tom.Offset(calendar.utc_offset),
        ),
      ),
    ])
  assert tom.parse("a = 1979-05-27T07:32:00Z\n") == Ok(expected)
}

pub fn parse_offset_z_date_time_space_test() {
  let expected =
    dict.from_list([
      #(
        "a",
        tom.DateTime(
          calendar.Date(1979, calendar.May, 27),
          calendar.TimeOfDay(7, 0, 1, 0),
          offset: tom.Offset(calendar.utc_offset),
        ),
      ),
    ])
  assert tom.parse("a = 1979-05-27 07:00:01Z\n") == Ok(expected)
}

pub fn parse_offset_positive_date_time_space_test() {
  let expected =
    dict.from_list([
      #(
        "a",
        tom.DateTime(
          calendar.Date(1979, calendar.May, 27),
          calendar.TimeOfDay(7, 0, 1, 0),
          offset: tom.Offset(duration.add(
            duration.hours(7),
            duration.minutes(40),
          )),
        ),
      ),
    ])
  assert tom.parse("a = 1979-05-27 07:00:01+07:40\n") == Ok(expected)
}

pub fn parse_offset_negative_date_time_space_test() {
  let expected =
    dict.from_list([
      #(
        "a",
        tom.DateTime(
          calendar.Date(1979, calendar.May, 27),
          calendar.TimeOfDay(7, 0, 1, 0),
          offset: tom.Offset(duration.add(
            duration.hours(-7),
            duration.minutes(-1),
          )),
        ),
      ),
    ])
  assert tom.parse("a = 1979-05-27 07:00:01-07:01\n") == Ok(expected)
}

pub fn parse_no_trailing_newline_test() {
  let expected = dict.from_list([#("a", tom.Int(1))])
  assert tom.parse("a = 1") == Ok(expected)
}

pub fn parse_trailing_whitespace_test() {
  let expected = dict.from_list([#("a", tom.Int(1))])
  assert tom.parse("a = 1 ") == Ok(expected)
}

pub fn parse_trailing_other_test() {
  assert tom.parse("a = 1 b") == Error(tom.Unexpected("b", "\n"))
}

pub fn parse_sequence_e_test() {
  assert tom.parse("a = \"\\e\"")
    == Ok(dict.from_list([#("a", tom.String("\u{001b}"))]))
}

pub fn parse_sequence_f_test() {
  assert tom.parse("a = \"\\f\"")
    == Ok(dict.from_list([#("a", tom.String("\f"))]))
}

pub fn parse_sequence_b_test() {
  assert tom.parse("a = \"\\b\"")
    == Ok(dict.from_list([#("a", tom.String("\u{0008}"))]))
}

pub fn parse_ignore_comments_test() {
  let expected = dict.from_list([#("field", tom.String("#"))])
  assert tom.parse(
      "# This should be ignored
field = \"#\"",
    )
    == Ok(expected)
}

pub fn parse_not_remove_hash_in_string_test() {
  let content = tom.Table(dict.from_list([#("field", tom.String("#"))]))
  let expected = dict.from_list([#("section", content)])
  assert tom.parse(
      "[section]
field = \"#\"",
    )
    == Ok(expected)
}

pub fn parse_multi_line_string_newline_test() {
  let expected =
    dict.from_list([
      #("contents", tom.String("Hello, Joe!\n")),
    ])
  assert tom.parse(
      "contents = \"\"\"
Hello, Joe!
\"\"\"
",
    )
    == Ok(expected)
}

pub fn parse_single_string_test() {
  let expected =
    dict.from_list([
      #("contents", tom.String("Hello, Joe!")),
    ])
  assert tom.parse(
      "contents = 'Hello, Joe!'
",
    )
    == Ok(expected)
}

pub fn parse_multi_line_single_string_newline_test() {
  let expected =
    dict.from_list([
      #("contents", tom.String("Hello, Joe!\n")),
    ])
  assert tom.parse(
      "contents = '''
Hello, Joe!
'''
",
    )
    == Ok(expected)
}

pub fn parse_multi_line_string_with_hash_test() {
  let expected =
    dict.from_list([
      #(
        "contents",
        tom.String(
          "document.querySelector(\"#app\").textContent = \"hello\";\n",
        ),
      ),
    ])
  assert tom.parse(
      "contents = \"\"\"
document.querySelector(\"#app\").textContent = \"hello\";
\"\"\"
",
    )
    == Ok(expected)
}

pub fn get_data_in_table_and_inline_table_test() {
  let toml =
    "section = { field = \"data\" }
another_section = { another_field = \"another_data\", int_field = 2 }
[still_a_section]
still_a_field = 1"
    |> tom.parse
  assert result.is_ok(toml) == True
  use toml <- result.map(toml)

  assert tom.get(toml, ["section", "field"]) == Ok(tom.String("data"))

  assert tom.get(toml, ["another_section", "another_field"])
    == Ok(tom.String("another_data"))

  assert tom.get(toml, ["another_section", "int_field"]) == Ok(tom.Int(2))

  assert tom.get(toml, ["still_a_section", "still_a_field"]) == Ok(tom.Int(1))
}

pub fn tom_as_int_test() {
  assert tom.as_int(tom.Int(1)) == Ok(1)

  assert tom.as_int(tom.Float(1.5)) == Error(tom.WrongType([], "Int", "Float"))
}

pub fn tom_as_float_test() {
  assert tom.as_float(tom.Float(1.5)) == Ok(1.5)

  assert tom.as_float(tom.Int(1)) == Error(tom.WrongType([], "Float", "Int"))
}

pub fn tom_as_bool_test() {
  assert tom.as_bool(tom.Bool(True)) == Ok(True)

  assert tom.as_bool(tom.Int(1)) == Error(tom.WrongType([], "Bool", "Int"))
}

pub fn tom_as_string_test() {
  assert tom.as_string(tom.String("hello")) == Ok("hello")

  assert tom.as_string(tom.Int(1)) == Error(tom.WrongType([], "String", "Int"))
}

pub fn tom_as_date_test() {
  let date = calendar.Date(2023, calendar.September, 23)

  assert tom.as_date(tom.Date(date)) == Ok(date)

  assert tom.as_date(tom.Int(1)) == Error(tom.WrongType([], "Date", "Int"))
}

pub fn tom_as_time_of_day_test() {
  let time = calendar.TimeOfDay(12, 30, 0, 4_000_000)

  assert tom.as_time_of_day(tom.Time(time)) == Ok(time)

  assert tom.as_time_of_day(tom.Int(1))
    == Error(tom.WrongType([], "Time", "Int"))
}

pub fn tom_as_calendar_time_test() {
  let date = calendar.Date(2023, calendar.September, 23)
  let time_of_day = calendar.TimeOfDay(10, 30, 00, 00)
  let offset = tom.Local

  assert tom.as_calendar_time(tom.DateTime(date, time_of_day, offset))
    == Ok(#(date, time_of_day, offset))

  assert tom.as_calendar_time(tom.Int(1))
    == Error(tom.WrongType([], "DateTime", "Int"))
}

pub fn tom_as_timestamp_test() {
  let date = calendar.Date(1970, calendar.January, 1)
  let time_of_day = calendar.TimeOfDay(0, 0, 00, 00)

  assert tom.as_timestamp(tom.Int(1))
    == Error(tom.WrongType([], "DateTime with offset", "Int"))

  assert tom.as_timestamp(tom.DateTime(date, time_of_day, tom.Local))
    == Error(tom.WrongType([], "DateTime with offset", "DateTime"))

  assert tom.as_timestamp(tom.DateTime(
      date,
      time_of_day,
      tom.Offset(duration.seconds(0)),
    ))
    == Ok(timestamp.from_unix_seconds(0))
}

pub fn tom_as_array_test() {
  let array = [tom.Int(1), tom.Int(2), tom.Int(3)]

  assert tom.as_array(tom.Array(array)) == Ok(array)

  assert tom.as_array(tom.Int(1)) == Error(tom.WrongType([], "Array", "Int"))
}

pub fn tom_as_table_test() {
  let dict = dict.new()

  assert tom.as_table(tom.Table(dict)) == Ok(dict)

  assert tom.as_table(tom.InlineTable(dict)) == Ok(dict)

  assert tom.as_table(tom.Int(1)) == Error(tom.WrongType([], "Table", "Int"))
}

pub fn tom_as_number_test() {
  assert tom.as_number(tom.Int(1)) == Ok(tom.NumberInt(1))

  assert tom.as_number(tom.Float(1.5)) == Ok(tom.NumberFloat(1.5))

  assert tom.as_number(tom.Bool(True))
    == Error(tom.WrongType([], "Number", "Bool"))
}

pub fn get_date_test() {
  let assert Ok(parsed) = tom.parse("a.b.c = 1979-05-27")

  assert tom.get_date(parsed, ["a", "b", "c"])
    == Ok(calendar.Date(1979, calendar.May, 27))

  assert tom.get_time_of_day(parsed, ["a", "b", "c"])
    == Error(tom.WrongType(["a", "b", "c"], "Time", "Date"))
}

pub fn get_time_of_day_test() {
  let assert Ok(parsed) = tom.parse("a.b.c = 07:32:00")

  assert tom.get_time_of_day(parsed, ["a", "b", "c"])
    == Ok(calendar.TimeOfDay(7, 32, 0, 0))

  assert tom.get_time_of_day(parsed, ["foo"]) == Error(tom.NotFound(["foo"]))
}

pub fn get_calendar_time_test() {
  let assert Ok(parsed) = tom.parse("a.b.c = 1979-05-27T07:32:00Z")
  let expected = #(
    calendar.Date(1979, calendar.May, 27),
    calendar.TimeOfDay(7, 32, 0, 0),
    tom.Offset(calendar.utc_offset),
  )

  assert tom.get_calendar_time(parsed, ["a", "b", "c"]) == Ok(expected)
}

pub fn get_timestamp_test() {
  let assert Ok(parsed) = tom.parse("a.b.c = 1970-01-01T00:00:00Z")
  assert tom.get_timestamp(parsed, ["a", "b", "c"])
    == Ok(timestamp.from_unix_seconds(0))

  let assert Ok(parsed) = tom.parse("a.b.c = 1970-01-01T00:00:00")
  assert tom.get_timestamp(parsed, ["a", "b", "c"])
    == Error(tom.WrongType(["a", "b", "c"], "DateTime with offset", "DateTime"))

  let assert Ok(parsed) = tom.parse("a = 1")
  assert tom.get_timestamp(parsed, ["a", "b", "c"])
    == Error(tom.WrongType(["a"], "Table", "Int"))
}

pub fn to_dynamic_integer_test() {
  let assert Ok(parsed) = tom.parse("a = 5")
  let dynamic = tom.to_dynamic(parsed)
  let decoder = {
    use a_field <- decode.field("a", decode.int)
    decode.success(a_field)
  }
  assert decode.run(dynamic, decoder) == Ok(5)
}

pub fn to_dynamic_nan_test() {
  let assert Ok(parsed) = tom.parse("a = nan")
  let dynamic = tom.to_dynamic(parsed)
  let decoder = {
    use a <- decode.field("a", tom.number_decoder())
    decode.success(a)
  }
  assert decode.run(dynamic, decoder) == Ok(tom.NumberNan(tom.Positive))
}

pub fn to_dynamic_nan_positive_test() {
  let assert Ok(parsed) = tom.parse("a = +nan")
  let dynamic = tom.to_dynamic(parsed)
  let decoder = {
    use a <- decode.field("a", tom.number_decoder())
    decode.success(a)
  }
  assert decode.run(dynamic, decoder) == Ok(tom.NumberNan(tom.Positive))
}

pub fn to_dynamic_nan_negative_test() {
  let assert Ok(parsed) = tom.parse("a = -nan")
  let dynamic = tom.to_dynamic(parsed)
  let decoder = {
    use a <- decode.field("a", tom.number_decoder())
    decode.success(a)
  }
  assert decode.run(dynamic, decoder) == Ok(tom.NumberNan(tom.Negative))
}

pub fn to_dynamic_infinity_test() {
  let assert Ok(parsed) = tom.parse("a = inf")
  let dynamic = tom.to_dynamic(parsed)
  let decoder = {
    use a <- decode.field("a", tom.number_decoder())
    decode.success(a)
  }
  assert decode.run(dynamic, decoder) == Ok(tom.NumberInfinity(tom.Positive))
}

pub fn to_dynamic_infinity_positive_test() {
  let assert Ok(parsed) = tom.parse("a = +inf")
  let dynamic = tom.to_dynamic(parsed)
  let decoder = {
    use a <- decode.field("a", tom.number_decoder())
    decode.success(a)
  }
  assert decode.run(dynamic, decoder) == Ok(tom.NumberInfinity(tom.Positive))
}

pub fn to_dynamic_infinity_negative_test() {
  let assert Ok(parsed) = tom.parse("a = -inf")
  let dynamic = tom.to_dynamic(parsed)
  let decoder = {
    use a <- decode.field("a", tom.number_decoder())
    decode.success(a)
  }
  assert decode.run(dynamic, decoder) == Ok(tom.NumberInfinity(tom.Negative))
}

pub fn to_dynamic_string_test() {
  let assert Ok(parsed) = tom.parse("a = \"Hello, Joe\"")
  let dynamic = tom.to_dynamic(parsed)
  let decoder = {
    use a_field <- decode.field("a", decode.string)
    decode.success(a_field)
  }

  assert decode.run(dynamic, decoder) == Ok("Hello, Joe")
}

pub fn to_dynamic_bool_test() {
  let assert Ok(parsed) = tom.parse("a = false")
  let dynamic = tom.to_dynamic(parsed)
  let decoder = {
    use a_field <- decode.field("a", decode.bool)
    decode.success(a_field)
  }

  assert decode.run(dynamic, decoder) == Ok(False)
}

pub fn to_dynamic_array_test() {
  let assert Ok(parsed) = tom.parse("a = [1, 2, 3]")
  let dynamic = tom.to_dynamic(parsed)
  let decoder = {
    use a_field <- decode.field("a", decode.list(decode.int))
    decode.success(a_field)
  }

  assert decode.run(dynamic, decoder) == Ok([1, 2, 3])
}

pub fn to_dynamic_table_test() {
  let assert Ok(parsed) =
    tom.parse(
      "
  [a]
  a = 1
  b = 2
  c = 3
  ",
    )
  let dynamic = tom.to_dynamic(parsed)
  let decoder = {
    use a_table_field <- decode.field(
      "a",
      decode.dict(decode.string, decode.int),
    )

    decode.success(a_table_field)
  }

  let decoded = decode.run(dynamic, decoder)

  assert decoded == Ok(dict.from_list([#("a", 1), #("b", 2), #("c", 3)]))
}

pub fn to_dynamic_inline_table_test() {
  let assert Ok(parsed) = tom.parse("a = {a = 1, b = 2, c = 3}")
  let dynamic = tom.to_dynamic(parsed)
  let decoder = {
    use a_table_field <- decode.field(
      "a",
      decode.dict(decode.string, decode.int),
    )

    decode.success(a_table_field)
  }

  let decoded = decode.run(dynamic, decoder)

  assert decoded == Ok(dict.from_list([#("a", 1), #("b", 2), #("c", 3)]))
}

pub fn to_dynamic_array_of_tables_test() {
  let assert Ok(parsed) =
    tom.parse(
      "[[a]]
    a = 1
    b = 2
    c = 3
    [[a]]
    a = 4
    b = 5
    c = 6
    [[a]]
    a = 7
    b = 8
    c = 9
  ",
    )

  let dynamic = tom.to_dynamic(parsed)

  let decode = {
    let decode_table = decode.dict(decode.string, decode.int)
    use a_field <- decode.field("a", decode.list(decode_table))
    decode.success(a_field)
  }

  let decoded = decode.run(dynamic, decode)

  assert decoded
    == Ok([
      dict.from_list([#("a", 1), #("b", 2), #("c", 3)]),
      dict.from_list([#("a", 4), #("b", 5), #("c", 6)]),
      dict.from_list([#("a", 7), #("b", 8), #("c", 9)]),
    ])
}

pub fn to_dynamic_date_test() {
  let assert Ok(parsed) = tom.parse("a = 1979-05-27")
  let dynamic = tom.to_dynamic(parsed)
  let decoder = {
    use a_field <- decode.field("a", tom.date_decoder())
    decode.success(a_field)
  }

  assert decode.run(dynamic, decoder)
    == Ok(calendar.Date(year: 1979, month: calendar.May, day: 27))
}

pub fn to_dynamic_time_test() {
  let assert Ok(parsed) = tom.parse("a = 07:32:01")
  let dynamic = tom.to_dynamic(parsed)
  let decoder = {
    use a_field <- decode.field("a", tom.time_of_day_decoder())
    decode.success(a_field)
  }

  assert decode.run(dynamic, decoder)
    == Ok(calendar.TimeOfDay(hours: 7, minutes: 32, seconds: 1, nanoseconds: 0))
}

pub fn to_dynamic_datetime_test() {
  let assert Ok(parsed) = tom.parse("a = 1979-05-27T07:32:00Z")
  let dynamic = tom.to_dynamic(parsed)
  let decoder = {
    use a_field <- decode.field("a", tom.calendar_date_time_of_day_decoder())
    decode.success(a_field)
  }

  assert decode.run(dynamic, decoder)
    == Ok(#(
      calendar.Date(year: 1979, month: calendar.May, day: 27),
      calendar.TimeOfDay(hours: 7, minutes: 32, seconds: 0, nanoseconds: 0),
      tom.Offset(calendar.utc_offset),
    ))
}

pub fn to_dynamic_datetime_local_offset_test() {
  let assert Ok(parsed) = tom.parse("a = 1979-05-27T07:32:00")
  let dynamic = tom.to_dynamic(parsed)
  let decoder = {
    use a_field <- decode.field("a", tom.calendar_date_time_of_day_decoder())
    decode.success(a_field)
  }

  assert decode.run(dynamic, decoder)
    == Ok(#(
      calendar.Date(year: 1979, month: calendar.May, day: 27),
      calendar.TimeOfDay(hours: 7, minutes: 32, seconds: 0, nanoseconds: 0),
      tom.Local,
    ))
}

pub fn to_dynamic_datetime_numeric_offset_offset_test() {
  let assert Ok(parsed) = tom.parse("a = 1979-05-27T07:32:00-05:00")
  let dynamic = tom.to_dynamic(parsed)
  let decoder = {
    use a_field <- decode.field("a", tom.calendar_date_time_of_day_decoder())
    decode.success(a_field)
  }

  assert decode.run(dynamic, decoder)
    == Ok(#(
      calendar.Date(year: 1979, month: calendar.May, day: 27),
      calendar.TimeOfDay(hours: 7, minutes: 32, seconds: 0, nanoseconds: 0),
      tom.Offset(duration.hours(-5)),
    ))
}

pub fn to_dynamic_timestamp_utc_test() {
  let assert Ok(parsed) = tom.parse("a = 1970-01-01T00:00:00Z")
  let dynamic = tom.to_dynamic(parsed)
  let decoder = {
    use a_field <- decode.field("a", tom.timestamp_decoder())
    decode.success(a_field)
  }

  assert decode.run(dynamic, decoder) == Ok(timestamp.unix_epoch)
}

pub fn to_dynamic_timestamp_with_offset_test() {
  let assert Ok(parsed) = tom.parse("a = 1970-01-01T01:00:00+01:00")
  let dynamic = tom.to_dynamic(parsed)
  let decoder = {
    use a_field <- decode.field("a", tom.timestamp_decoder())
    decode.success(a_field)
  }

  assert decode.run(dynamic, decoder) == Ok(timestamp.unix_epoch)
}

pub fn to_dynamic_timestamp_local_fails_test() {
  let assert Ok(parsed) = tom.parse("a = 1970-01-01T00:00:00")
  let dynamic = tom.to_dynamic(parsed)
  let decoder = {
    use a_field <- decode.field("a", tom.timestamp_decoder())
    decode.success(a_field)
  }

  assert decode.run(dynamic, decoder)
    == Error([decode.DecodeError("DateTime with offset", "Dict", ["a"])])
}

// Gloogle patch. TOML allows a super-table to be defined after one of its
// sub-tables: `[a.b]` creates `a` implicitly, `[a]` may then define it once.
// Seen in the wild in lumenmail 1.1.2's gleam.toml.
pub fn parse_super_table_after_sub_table_test() {
  let expected =
    dict.from_list([
      #(
        "a",
        tom.Table(
          dict.from_list([
            #("b", tom.Table(dict.from_list([#("x", tom.Int(1))]))),
            #("y", tom.Int(2)),
          ]),
        ),
      ),
    ])
  assert tom.parse("[a.b]\nx = 1\n\n[a]\ny = 2\n") == Ok(expected)
}

pub fn parse_lumenmail_gleam_toml_test() {
  let toml =
    "name = \"lumenmail\"

[dependencies]
gleam_stdlib = \">= 0.62.0 and < 2.0.0\"

[dev-dependencies.envoy]
version = \">= 1.0.2 and < 2.0.0\"

[dev-dependencies]
gleeunit = \">= 1.0.0 and < 2.0.0\"
"
  let assert Ok(parsed) = tom.parse(toml)
  assert tom.get_string(parsed, ["dev-dependencies", "gleeunit"])
    == Ok(">= 1.0.0 and < 2.0.0")
  assert tom.get_string(parsed, ["dev-dependencies", "envoy", "version"])
    == Ok(">= 1.0.2 and < 2.0.0")
}

// Still errors: the same header twice, explicit or after its sub-table.
pub fn parse_duplicate_table_header_test() {
  assert tom.parse("[a]\nx = 1\n[a]\ny = 2\n")
    == Error(tom.KeyAlreadyInUse(["a"]))
  assert tom.parse("[a.b]\n[a]\n[a]\n") == Error(tom.KeyAlreadyInUse(["a"]))
  assert tom.parse("[a.b]\n[a.b]\n") == Error(tom.KeyAlreadyInUse(["a", "b"]))
}

// Still errors: the super-table redefining a key its sub-table created.
pub fn parse_super_table_conflicting_key_test() {
  assert tom.parse("[a.b]\nx = 1\n[a]\nb = 2\n")
    == Error(tom.KeyAlreadyInUse(["a", "b"]))
}

// Array-of-tables headers keep appending, as before.
pub fn parse_super_table_after_array_of_tables_test() {
  let assert Ok(parsed) = tom.parse("[[a.b]]\nx = 1\n[a]\ny = 2\n")
  assert tom.get_int(parsed, ["a", "y"]) == Ok(2)
}
