.. comment: -*- mode: rst; coding: utf-8; electric-indent-mode: nil; tab-always-indent: t -*-


Overview of Examples
===============================================================================

The examples consist of a collection of clients and servers. They are split
into two groups: a set of daytime clients and server, and echo clients and
servers. In some of the examples, a certain network protocol, suppose
end-of-file handling, must be matched between client and server causing further
delineation.

Client protocols are matched to server protocols thusly:

Clients: ex1-client, ex2-client, ex3-client, can work
with servers: ex1-server, ex2-server, ex3-server, ex4-server.

Clients: ex4-client, ex5a-client, can work with servers: ex5-server,
ex6-server.

Clients: ex5b-client, can work with servers: ex7-server, ex8-server

Some clients and servers use the "daytime" series of protocols, those
are ex1-client, ex2-client, ex3-client, and ex1-server, ex2-server,
ex3-server, and ex4-server.

Some clients and servers use the "echo a line" series of protocols,
those are ex4-client, ex5a-client, ex5b-client, and ex5-server,
ex6-server, ex7-server, and ex8-server.

Even though much of the example source is included in the tutorial, it is
recommended that the example sources be carefully read and understood in order
to gain the most benefit from the tutorial.

.. comment: end of file
