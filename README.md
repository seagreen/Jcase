# Intro

A dead-simple standard for language agnostic test cases and suites.

# Version

Unreleased

# Components

## Jcase

A generic test case description. Totally program and language agnostic.

Example:

```
{
    "description": "Basic FooBar test case",
    "input": [
        1,
        2,
        3
    ],
    "output": [
        "1",
        "2",
        "foo"
    ]
}
```

## Jsuite

A specific way of collecting Jcases into a test suite.

Example:

```
{
    "description": "FooBar -- Accept a list of integers, convert them to strings, and return them. Exceptions are if the integer is divisible by 3 (in which case return \"foo\"), 5 (return \"bar\") or both (return \"foobar\").",
    "cases": [
        {
            "description": "Basic FooBar test case",
            "input": [
                1,
                2,
                3
            ],
            "output": [
                "1",
                "2",
                "foo"
            ]
        },
        {
            "description": "Tricky FooBar test case -- handle negative ints",
            "input": [
                -1,
                -2
            ],
            "output": [
                "-1",
                "-2"
            ]
        }
    ]
}
```

# Potential Uses

## Code Katas

Write katas as Jsuites. Solve them in any language.

An example solution to FooBar:

```
import jsuitekata # As yet unbuilt

def foobar(num):
    if num % 3 == 0 and num % 5 == 0:
        return "foobar"
    elif num % 3 == 0:
        return "foo"
    elif num % 5 == 0:
        return "bar"
    else:
        return str(num)

jsuitekata.answer_with(foobar)
```

Which when ran might print something like:

```
$ cat foobar.json | foobar.py
    ✔ "handles positive numbers correctly"
    ✔ "handles negative numbers correctly"
    ✘ "handles zero correctly"
```

## Code testing

Self-explanatory.

## Test creation tools

Standardized test formats make it easy to create GUIs or other tools for working with tests.
