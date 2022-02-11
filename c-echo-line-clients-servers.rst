.. comment: -*- mode:rst; coding:utf-8; electric-indent-mode:nil; tab-always-indent:t -*-


Echo Line Clients and Servers
===============================================================================

These next examples focus on the echo protocol. This is simply a server that
sends back to the client whatever the client wrote to it.  A client can request
to quit talking to a server (except ex8-server, where this feature isn't
implemented) by sending the word "quit", on a line by itself.  This tells the
server to close the connection to the client once it has finished echoing the
line.  The closing of the client's read socket lets the client know the
connection to the server went away and that it is time to exit.  We also
introduce the socket multiplexer interface which allows concurrent processing
of socket connections. This is similar to how UNIX's select(), epoll(), or
kqueue() works. Due to portability concerns on doing nonblocking operations on
\*standard-input\* and \*standard-output\* (we can't easily do it) we are beholden
to some form of blocking I/O in our clients because they interact with a human.
We will explore true non-blocking I/O in the ex8-server example since that
server only has to converse with connected clients.


.. comment: end of file
