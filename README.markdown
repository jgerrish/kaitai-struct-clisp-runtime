# kaitai-struct-clisp-runtime

## Overview

This provides a Kaitai-struct runtime for Common LISP.  It was built
and tested with SBCL, but should work on any Common LISP
implementation.  Testing with other systems is appreciated.

This project tries to follow the kaitai project's recommendations for
runtime structure and services.  At the same time, there are several
features built in to build a more functional compiler and generated
parser.

The goal here is to get a prototype out the door, to help other
software engineers benefit from the network effects of the Kaitai
community, where the true value lies.

All contributions are welcome.  Rebuilding the compiler as a
parser-combinator implementation may be possible.

See the section Development below for some functions that may help in
a refactor.


## Usage

This runtime should be used with the CommonLISP kaitai-struct
compiler.  The compiler uses the kaitai-struct Scala compiler project
to generate classes and instructions specific to your file format.

The generated parser includes :use expressions to pull in this
runtime.  You may need to include (ql:quickload
"kaitai-struct-clisp-runtime") if you use quicklisp or make it
available somewhere your Common LISP system can find it.

For quicklisp, cloning kaitai-struct-clisp-runtime into
~/quicklisp/local-projects makes it discoverable by ql:quickload.


## Installation

This uses the ASDF (Another System Definition Facility) system for
Common LISP to manage dependencies and libraries.

The directory uses hyphens.  This is following the target language
recommendations.  The Kaitai project runtimes mostly use underscores.
Most Common LISP projects and ASDF systems use hyphens for directory
names.  Adding the directory to your source-registry should enable the
asd file to be found:

[https://asdf.common-lisp.dev/asdf/Configuring-ASDF-to-find-your-systems.html](Configuring-ASDF-to-find-your-systems "Configuring ASDF to find your systems")


The system was created with the following commands:

(load "quicklisp/quicklisp.lisp")
(ql:quickload "cl-project")
(cl-project:make-project #P"quicklisp/local-projects/kaitai-struct-clisp-runtime")

Users who are familiar with ASDF should know how to use it in their
projects.

To enter the system:

(in-package :kaitai-struct-clisp-runtime)


## Development

The runtime includes some functions that could help with building a
more functional implementation.

Each kaitai-struct object includes a slot-readers slot that holds a
mapping between slot names and reader functions.

register-attribute registers a reader for a slot.

peek-read is a macro that wraps custom save and restore functions and
executes a read function in that saved context.  It can be used to
push and pop stream positions before and after a series of reads or
writes.

reader-builder uses peek-read and builds a parameter-less reader that
can be passed to registered attributes.

read-attribute is used to call a reader for a slot, setting the slot.
It also returns the new value of the slot, keeping with the
EveryReadIsExpression compiler interface.  So expressions can be built
up from slot reads into object reads and higher.  The current Kaitai
compiler interface doesn't treat _read methods as expressions.

with-cached-slot provides a custom context to test if a computed slot
has been computed and return the computed slot or compute it and
return it.


### Testing

To run tests:

(asdf:test-system :kaitai-struct-clisp-runtime)

## Links

[Kaitai Struct](https://kaitai.io/) Kaitai Struct
