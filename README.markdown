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


To enter the system:

(in-package :kaitai-struct-clisp-runtime)


## Example

This section runs through creating a simple hello world application.

Copy the following content into a file called hello_world.ksy:


meta:
  id: hello_world
  application: A sample hello world binary file for testing
  endian: le
doc: |
  A sample hello world binary file for testing.
  Right now it only consists of a single byte followed by a little-endian short.
doc-ref: https://github.com/jgerrish/kaitai_struct_compiler.git
seq:
  - id: one
    type: u1
  - id: two
    type: u2

Create a simple example file: hello_world_data.bin

You can evalute the following in Emacs with C-c C-c, it will create a
file called hello_world_data.bin in your current directory.

#+BEGIN_SRC sh :exports :results value file :file hello_world_data.bin :shebang #!/bin/sh
  echo -en "\x00\x01\x02\x04"
#+END_SRC

#+RESULTS:
[[file:hello_world_data.bin]]


Generate runtime code:

In the kaitai_struct_compiler directory, run the Scala Build Tool (sbt):

$ sbt

Then after it loads, run the following command:

fgRun --target clisp "hello_world.ksy"

This will generate a file called hello-world.lisp

A quick note about underscores, hyphens and dashes and naming
conventions.  Up until now we've been using underscores because that's
what the Kaitai project standardized on.  But LISP uses hyphens, so
the filename for the LISP code will use a hyphen and the code
generated will use hyphens.


Then start up your LISP system and load in the runtime and generated
code:

If you're using Quicklisp, SLIME and SBCL:

CL-USER> (ql:quickload :kaitai-struct-clisp-runtime)

Then load the hello-world.lisp code:

(load "hello-world.lisp")

You may need to add a relative or absolute path to the file.

The example runtime runner below uses inline data instead of the file
generated earlier.


#+NAME: run-it-byte-stream
#+BEGIN_SRC common-lisp :tangle run.lisp
  (defun run-it-byte-stream ()
    (let* ((kbs (make-instance 'kaitai-stream::kaitai-byte-stream
			       :data #(#X00 #X01 #X02 #X04)))
	   (hw (make-instance 'hello-world :stream kbs)))
      hw))
#+END_SRC

Running that code block should parse the binary file and return the
value 513.

Here is the same code but using the external file:

#+NAME: run-it-file-stream
#+BEGIN_SRC common-lisp :tangle run-it-file-stream.lisp
  (defun run-it-file-stream ()
    (let* ((filename "test.bin")
	   (in (open filename :direction :input :element-type '(unsigned-byte 8)))
	   (kfs (make-instance 'kaitai-stream::kaitai-file-stream :io in))
	   (hw (make-instance 'hello-world :stream kfs)))
      hw))
#+END_SRC


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


## Original Steps to Create Runtime Project Skeleton

The system was created with cl-project and the following commands:

(load "quicklisp/quicklisp.lisp")
(ql:quickload "cl-project")
(cl-project:make-project #P"quicklisp/local-projects/kaitai-struct-clisp-runtime")

Users who are familiar with ASDF should know how to use it in their
projects.


## Links

[Kaitai Struct](https://kaitai.io/) Kaitai Struct
