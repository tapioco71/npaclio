.. comment: -*- mode:rst; coding:utf-8; electric-indent-mode:nil; tab-always-indent:t -*-

   
Daytime Clients
===============================================================================

In this section we show the evolution of a client which connects to a server
and gets the time of day. Each example shows some kind of an incremental
improvement to the previous one.


Daytime Client IVP4/TCP: ex1-client.lisp
-------------------------------------------------------------------------------

This example is a very simple daytime client program which contacts a server,
by default at \*host\* and \*port\*, returns a single line of text that is the
current date and time, and then exits. It is written in more of a C style just
to make it easy to compare with similar simple examples in other languages. It
uses blocking, line oriented I/O.

The steps this program performs are:

0. The ex1-client.lisp entrance call:

   .. code::

      (defun run-ex1-client (&key (host *host*) (port *port*))


1. Create an active TCP socket:

   The socket creation function (MAKE-SOCKET ...) is the method by which one
   creates a socket in IOLib.  It is very versatile and can be used to both
   create and initialize the socket in a single call.

   In this case, we use it simply and create an active IPV4 Internet stream
   socket which can read or write utf8 text and that understands a particular
   newline convention in the underlying data.

   One small, but important, deviation of IOLib sockets from Berkeley sockets
   is that when a socket is created, it is predestined to forever and
   unalterably be either an active or passive socket. Active sockets are used
   to connect to a server and passive sockets are used for a server's
   listening socket.

   .. code::
           
      ;; Create a internet TCP socket under IPV4
      (let ((socket (make-socket :connect :active
                                 :address-family :internet
                                 :type :stream
                                 :external-format '(:utf-8 :eol-style :crlf)
                                 :ipv6 nil)))


2. Specify the Server's IP address and port and establish a connection 
   with the server:

   This bit of code contains many calls into IOLib and we shall examine each
   of them. 

   The function LOOKUP-HOSTNAME takes as a string the DNS name
   for a machine and returns 4 values:
    
   A. an address
       
   B. a list of additional addresses(if existent)
       
   C. the canonical name of the host
       
   D. an alist of all the host's names with their respective addresses 

   We use only the first return value, the address component, to pass to the
   function CONNECT.

   The function CONNECT will connect the socket to the address, but to a
   random port if the :port keyword argument is not specified. The average
   client codes usually use :wait t to block until the connect can resolve
   with a connected fd or an error. The exception to always using :wait t is
   if the client needs to connect to many servers at once, suppose a web
   client, or if a server is also a client in other contexts and wishes not to
   block.

   The functions REMOTE-HOST and REMOTE-PORT return the ip address and port of
   the remote connection associated with the connected socket. LOCAL-HOST and
   LOCAL-PORT return the information of the client's end of the connected
   socket. Analogous calls REMOTE-NAME and LOCAL-NAME each return two values
   where the first value is the equivalent of \*-host and the second value is
   the equivalent of \*-port.

   .. code::

      ;; do a blocking connect to the daytime server on the port.
      (connect socket (lookup-hostname host) :port port :wait t)
      (format t "Connected to server ~A:~A via my local connection at ~A:~A!~%"
              (remote-host socket) (remote-port socket)
              (local-host socket) (local-port socket))


3. Read and display the server's reply:

   Now that the socket has been connected to the server, the server will send
   a line of text to the client. The client uses the standard Common Lisp
   function READ-LINE to read the information from the socket. The function
   READ-LINE blocks and will only return when an *entire line* is read. Once
   read, the line is emitted to \*standard-output\* via the function call
   FORMAT.

   .. code::
           
      ;; read the one line of information I need from the daytime
      ;; server.  I can use read-line here because this is a TCP socket.
      (let ((line (read-line socket)))
        (format t "~A" line))


4. End program:

   We close the socket with the standard function CLOSE and return true so the
   return value of this example is t.

   .. code::
           
      ;; all done
      (close socket)
      t))


While this program works, it has some major flaws in it. First and foremost is
that it doesn't handle any conditions that IOLib signals in common use cases.
An example would be to run the ex1-client.lisp example without a daytime server
running. In most, if not all, Common Lisp toplevels, you'll be dropped into the
debugger on an unhandled SOCKET-CONNECTION-REFUSED-ERROR condition. Secondly,
it isn't written in the Common Lisp style.


Daytime Client IVP4/TCP: ex2-client.lisp
-------------------------------------------------------------------------------

In this example, we simply tackle the fact ex1-server.lisp can be shortened
with an IOLib form to something where the application writer has less to do
concerning cleaning up the socket object. It also uses line oriented blocking
I/O.

The introduced macro WITH-OPEN-SOCKET calls MAKE-SOCKET with the arguments in
question and binds the socket to the variable 'socket'. When this form returns,
it will automatically close the socket.

This shortens the program so much, that the example can be included in its 
entirety:

.. code::

   (defun run-ex2-client (&key (host *host*) (port *port*))

     ;; We introduce with-open-socket here as a means to easily wrap
     ;; usually synchronous and blocking communication with a form that
     ;; ensures the socket is closed no matter how we exit it.
     (with-open-socket (socket :connect :active
                               :address-family :internet
                               :type :stream
                               :external-format '(:utf-8 :eol-style :crlf)
                               :ipv6 nil)

       ;; Do a blocking connect to the daytime server on the port.  We
       ;; also introduce lookup-hostname, which converts a hostname to an
       ;; 4 values, but in our case we only want the first, which is an
       ;; address.
       (connect socket (lookup-hostname host) :port port :wait t)
       (format t "Connected to server ~A:~A from my local connection at ~A:~A!~%"
               (remote-name socket) (remote-port socket)
               (local-name socket) (local-port socket))

       ;; read the one line of information I need from the daytime
       ;; server.  I can use read-line here because this is a TCP
       ;; socket. It will block until the whole line is read.
       (let ((line (read-line socket)))
         (format t "~A" line)
         t)))
   

This shorthand can go even further, if we add this to the WITH-OPEN-SOCKET
flags

.. code::
   
    :remote-host (lookup-hostname host)
    :remote-port port

    
then the underlying MAKE-SOCKET call will in fact connect the socket directly
to the server before it is available for the body of the macro allowing us to
remove the connect call entirely! In the early examples, however, we don't
utilize IOLib's shorthand notations to this degree in order to make apparent
how the library maps into traditional socket concepts. After one gains
familiarity with the IOLib API, the situations where application of the
shortcuts are useful become much easier to see.


Daytime Client IVP4/TCP: ex3-client.lisp
-------------------------------------------------------------------------------

Now we come to condition handling, which can moderately affect the layout of
your IOLib program. Any real program using IOLib must handle IOLib's signaled
conditions which are common to the boundary cases of network programming.
We've already seen one of these boundary cases when we tried to connect a
daytime client to a server that wasn't running.  The condition signaled in that
case was: SOCKET-CONNECTION-REFUSED-ERROR.  The stream interface has a set of
conditions which IOLib will signal, and another lower level IOLib layer--which
we'll come to in the nonblocking I/O examples have another set of conditions.
There is some intersection between them and we will explore that later. For
now, we'll just use the conditions associated with a stream.

Our rewrite of ex2-client.lisp into ex3-client.lisp (continuing to use line
oriented blocking I/O) proceeds thusly:

0. We create a helper function which connects to the server and reads the
   daytime line:

   Notice the HANDLER-CASE macro around the portion of the function which
   reads the date from the server. In looking at the boundary conditions from
   the server given this protocol, we can receive an END-OF-FILE condition if
   the client connected, but before the server could respond it exited,
   closing the connection. Since in this case we're inside of a
   WITH-OPEN-SOCKET form, we simply note that we got an END-OF-FILE and let
   the cleanup forms of WITH-OPEN-SOCKET close the connection. If we don't
   catch this condition, then the program will break into the debugger and
   that isn't useful.  It is usually debatable as to where one should handle
   conditions: either near to or far away from the generating calls. In these
   simple examples, no choice has any significant pros or cons. As your IOLib
   programs become more and more complex, however, it becomes more obvious at
   what abstraction level to handle signaled conditions.

   .. code::
           
      (defun run-ex3-client-helper (host port)

        ;; Create a internet TCP socket under IPV4
        (with-open-socket
          (socket :connect :active
                  :address-family :internet
                  :type :stream
                  :external-format '(:utf-8 :eol-style :crlf)
                  :ipv6 nil)

          ;; do a blocking connect to the daytime server on the port.
          (connect socket (lookup-hostname host) :port port :wait t)
          (format t "Connected to server ~A:~A from my local connection at ~A:~A!~%"
                  (remote-name socket) (remote-port socket)
                  (local-name socket) (local-port socket))

          (handler-case
              ;; read the one line of information I need from the daytime
              ;; server.  I can use read-line here because this is a TCP
              ;; socket. It will block until the whole line is read.
              (let ((line (read-line socket)))
                (format t "~A" line)
                t)

            ;; However, let's notice the signaled condition if the server
            ;; went away prematurely...
            (end-of-file ()
              (format t "Got end-of-file. Server closed connection!")))))


1. Some conditions which are complete show-stoppers to the functioning of the
   code are caught at a higher level:

   Notice we catch the possible SOCKET-CONNECTION-REFUSED-ERROR from the
   connect inside of the function run-ex3-client-helper.

   .. code::

      ;; The main entry point into ex3-client
      (defun run-ex3-client (&key (host *host*) (port *port*))
        (handler-case

            (run-ex3-client-helper host port)

          ;; handle a commonly signaled error...
          (socket-connection-refused-error ()
            (format t "Connection refused to ~A:~A. Maybe the server isn't running?~%"
                    (lookup-hostname host) port))))


Here are some common conditions in IOLib (some from ANSI Common Lisp too) and
under what situations they are signaled.  In any IOLib program, *at least*
these conditions should be handled where appropriate.

END-OF-FILE:
    When a stream function such as READ, READ-LINE, etc...(but not
    RECEIVE-FROM), reads from a socket where the other end has been closed.

HANGUP:
    When writing to a socket with a stream function such as WRITE,
    FORMAT, etc...(but not SEND-TO), if the socket is closed then this
    condition is signaled.

SOCKET-CONNECTION-RESET-ERROR:
    When doing I/O on a socket and the other side of the socket sent a
    RST packet, this condition is signaled.  It can also happen with
    the IOLIb function ACCEPT and similar.

SOCKET-CONNECTION-REFUSED-ERROR:
    Signaled by connect if there is no server waiting to accept the incoming
    connection.

.. comment: end of file
   
