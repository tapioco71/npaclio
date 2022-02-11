.. comment: -*- mode:rst; coding:utf-8; electric-indent-mode:nil; tab-always-indent:t -*-

   
Introduction
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

This tutorial loosely follows the exposition of network programming in "UNIX
Network Programming, Networking APIs: Sockets and XTI 2nd Edition" by W.
Richard Stevens ([STEVENS1997]_). Many examples are derived from the source codes in that book.
Major deviations from the C sources include converting the concurrent examples
which use fork() into threaded examples which use the portable Bordeaux Threads
package, more structured implementations of certain concepts such as data
buffers and error handling, and general movement of coding style towards a
Common Lisp viewpoint.

The scope of this version of the tutorial is:

    0. Exposition suitable for programmers unfamiliar with ANSI Common Lisp

    1. IPV4 TCP

    2. Client/Server architecture

    3. Iterative vs Concurrent (via threading) vs Multiplexed Server Design

    4. Blocking and nonblocking I/O

It is intended, however, that this tutorial grows to contain the entirety of
IOLib's API as detailed in the Future Directions section of this tutorial. As
newer revisions of this tutorial are released, those gaps will be filled until
the whole of the IOLib API has been discussed.

Finally, the example code in this tutorial is algorithmically cut from the
actual example programs and inserted into the tutorial via a template
generation method. The example codes have embedded in them a tiny markup
language which facilitates this in the form (on a single line) of ';; ex-NNNb'
to begin an example section, and ';; ex-NNNe' to end an example section--NNN
stands for an enumeration integer for which each section's begin and end must
match.


Acknowledgements
===============================================================================

I would like to greatly thank Stelian Ionescu, the author of IOLib
for his exposition of the various features of IOLib and his patience
in our sometimes long conversations.


Supporting Code
===============================================================================

The file package.lisp contains a small library of codes used widely in the
examples. The supporting code implements:

    0. The package containing the examples, called :iolib.examples.

    1. The variables \*host\* and \*port\*, set to "localhost" and 9999
       respectively. This is the default name and port to which
       client connect and servers listen. Servers usually bind
       to 0.0.0.0, however.

    2. A small, but efficient, queue implementation, from "ANSI Common Lisp"
       by Paul Graham. The interface calls are:

       .. code::
          
          (make-queue)
          (enqueue obj q)
          (dequeue q)
          (empty-queue q)

    3. \:iolib.examples currently depends upon IOLib alone and uses 
       packages \:common-lisp, \:iolib, and \:bordeaux-threads.

       
Running the Examples
===============================================================================

These examples were developed and tested on SBCL 1.0.33.30 running on an x86
Ubuntu 8.10 machine. They were ran with two sessions of SBCL running, one
acting as a client, and the other as a server.

Supposing we'd like to start up the first example of the daytime server and
connect to it with the first daytime client example. Initially, the server will
bind to \*host\* and \*port\* and wait for the client to connect. We connect with
the client to \*host\* and \*port\*, get the time, and exit.

First we'll start up a server:

.. code::
   
   Linux black > sbcl
   This is SBCL 2.1.1.debian, an implementation of ANSI Common Lisp.
   More information about SBCL is available at <http://www.sbcl.org/>.

   SBCL is free software, provided as is, with absolutely no warranty.
   It is mostly in the public domain; some portions are provided under
   BSD-style licenses.  See the CREDITS and COPYING files in the
   distribution for more information.
   * (require :iolib.examples) ; much output!
   * (in-package :iolib.examples)

   #
   * (run-ex1-server)
   Created socket: #[fd=5]
   Bound socket: #
   Listening on socket bound to: 0.0.0.0:9999
   Waiting to accept a connection...
   [ server is waiting for the below client to connect! ]
   Got a connection from 127.0.0.1:34794!
   Sending the time...Sent!
   T
   *

Now we'll start up the client which connected to the above server:

.. code::
   
   Linux black > sbcl
   This is SBCL 2.1.1.debian, an implementation of ANSI Common Lisp.
   More information about SBCL is available at <http://www.sbcl.org/>.

   SBCL is free software, provided as is, with absolutely no warranty.
   It is mostly in the public domain; some portions are provided under
   BSD-style licenses.  See the CREDITS and COPYING files in the
   distribution for more information.
   * (require :iolib.examples) ; much output!
   * (in-package :iolib.examples)

   #
   * (run-ex1-client)
   Connected to server 127.0.0.1:9999 via my local connection at 127.0.0.1:34794!
   2/27/2010 13:51:48
   T
   *

In each client example, one can specify which host or port to which it should
connect:

.. code::
   
   * (run-ex1-client :host "localhost" :port 9999)
   Connected to server 127.0.0.1:9999 via my local connection at 127.0.0.1:34798!
   2/27/2010 13:53:7
   T
   *

The servers can be told a port they should listen upon and in this tutorial,
unless otherwise specified, will always bind to 0.0.0.0:9999 which means across
all interfaces on the machine and on port 9999.


.. comment: end of file
