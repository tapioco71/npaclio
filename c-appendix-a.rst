.. comment: -*- mode: rst; coding: utf-8; electric-indent-mode: nil; tab-always-indent: t -*-


Appendix A
===============================================================================

This holds a rough approximation between the sources in this tutorial and the
original sources in the network programming book by Stevens mentioned in the
beginning of the tutorial. Aspects about the implementation of each client or
server are summarized here.


The Clients
-------------------------------------------------------------------------------

Figure 1.5, page 6

- ex1-client: Blocking I/O, daytime client, C Style

Figure 1.5, page 6

- ex2-client: Blocking I/O, daytime client, Lisp Style

Figure 1.5, page 6

- ex3-client: ex2-client, but with much more error handling

Figure 5.4, 5.5, page 114, 115

- ex4-client: Blocking I/O, line oriented

Figure 6.9, page 157

- ex5a-client: I/O multiplexing with iolib, line oriented, blocking I/O

- note: since this is still blocking I/O, I'm using \*standard-input\*
  and friends. Also note, with batch input, it will close the socket with
  in-flight data still present which is incorrect.

Figure 6.13, page 162

- ex5b-client: Same as ex5a-client EXCEPT shutdown is called when the input
  reaches end-of-file as to prevent in flight data from being destroyed
  on the way to the server.


The servers
-------------------------------------------------------------------------------

Figure 4.11, page 101

- ex1-server: Iterative, blocking I/O daytime server, C Style, no
  error handling, one shot, line oriented

Figure 4.11, page 101

- ex2-server: Iterative, blocking I/O daytime server, Lisp Style,
  no error handling, loop forever, line oriented

Figure 4.11, page 101

- ex3-server: daytime server, ex2-server, but with error handling, line oriented

Figure 4.13, page 105

- ex4-server: daytime server, concurrent, blocking I/O, line oriented

Figure 5.2, 5.3, page 113, 114

- ex5-server: Concurrent, blocking I/O, echo server, line oriented

Figure 6.21,6.22 page 165,166

- ex6-server: I/O multiplexing of clients with iolib, line oriented,
  blocking I/O

Figure 6.21, 6.22 page 165,166

- ex7-server, ex7-buffer: individual I/O handlers for read/write,
  I/O multiplexing of clients with iolib, line oriented, blocking I/O,
  has problem with denial of service, page 167.

Figure 15.3, 15.4, 15.5 page 400-403
- ex8-server, ex8-buffer: nonblocking I/O, event-dispatch, send-to, receive-from

.. comment: end of file
