# CLIPS

A Tool for Building Expert Systems

This repository is a mirror of the [CLIPS](http://www.clipsrules.net/) SVN repository.

This branch refers to CLIPS 6.30.

## Features

The following features have been added to CLIPS.

### Regular Expressions

PERL regular expressions support via the PCRE library.

---

```
(regex-match pattern subject)
```

The `regex-match` function allows to match a pattern to a given subject. Both the pattern and the subject can be either a `SYMBOL` or a `STRING`.

The `regex-match` function returns a `multifield` containing at its first position the full match followed by the capturing groups if any.

```
CLIPS> (regex-match foo "foobar")
("foo")
```

Using capturing groups.

```
CLIPS> (regex-match "(www.)(.*)(.com|.net)" "http://www.clipsrules.net/")
("www.clipsrules.net" "www." "clipsrules" ".net")
```

---

```
(regex-imatch pattern subject)
```

Case insensitive version of the `regex-match` function.

```
CLIPS> (regex-imatch foo "FOObar")
("FOO")
```

#### Performance considerations

The regular expression patterns are compiled lazily when the related functions get called for the first time.

Each pattern gets compiled into bytecode and JIT compiled if the underlying platform supports it. This leads to significant performance improvements at the cost of longer compilation time.

To reduce the compiling cost, each pattern gets cached once compiled. Multiple matches against the same pattern will result in a single compilation.

The User can control the cache size via the `REGEXP_CACHE_SIZE` constant in `core/regfun.h`. Setting the value to 0 disables the caching mechanism.

The User can also disable the JIT compilation by setting the `REGEX_JIT_COMPILE` constant in `core/regfun.h` to 0.

#### Building CLIPS with PCRE

Build dependencies:

* [PCRE2](http://www.pcre.org/) headers
* [uthash](https://troydhanson.github.io/uthash/)

Dependencies:

* [PCRE2](http://www.pcre.org/) 8 bit runtime library

Please refer to the attached `makefiles`.

#### Files

* core/regfun.h
* core/regfun.c
* core/userfunctions.c
* test_suite/rgxfun.tst
* test_suite/rgxfun.bat
* test_suite/Expected/rgxfun.out

#### Tests

Launch clips within the `test_suite` folder and execute:

```
(batch rgxfun.tst)
```
