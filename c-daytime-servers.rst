.. comment: -*- mode: rst; coding: utf-8; electric-indent-mode: nil; tab-always-indent: t -*-


Daytime Servers
===============================================================================

Now that we have completed the evolution of the daytime client, let's look at
the daytime servers.

The exposition of the servers follows in style of the clients.


Daytime Server IVP4/TCP: ex1-server.lisp
-------------------------------------------------------------------------------

This first example is an iterative server which handles a single client and
then exits. The I/O is blocking and no error handling is performed.  This is
similar in scope to the ex1-client.lisp example.

0. Create the server socket:

   We see that the socket is \:passive. Every socket in IOLib is predestined to
   be either an active or passive socket and since this is a server socket, it
   is passive. Also here we see that we can ask for the underlying fd of the
   socket with the function SOCKET-OS-FD.

   .. code:: lisp

      (defun run-ex1-server (&key (port *port*))
        ;; Create a passive (server) TCP socket under IPV4 Sockets meant to
        ;; be listened upon *must* be created passively. This is a minor
        ;; deviation from the Berkeley socket interface.
        (let ((socket (make-socket :connect :passive
                                   :address-family :internet
                                   :type :stream
                                   :external-format '(:utf-8 :eol-style :crlf)
                                   :ipv6 nil)))
          (format t "Created socket: ~A[fd=~A]~%" socket (socket-os-fd socket))


1. Bind the socket

   Binding a socket is what gives it an endpoint to which clients can connect.
   The IOLib constant \+IPV4-UNSPECIFIED\+ represents 0.0.0.0 and means if a
   connection arrives on any interface, it will be accepted if it comes to the
   \:port specified. The :reuse-addr keyword represents the socket option
   SO_REUSEADDR and states (among other things) that if the socket is in the
   TIME_WAIT state it can be reused immediately.  It is recommended that all
   servers use \:reuse-addr on their listening socket.

   .. code:: lisp

      ;; Bind the socket to all interfaces with specified port.
      (bind-address socket
                    +ipv4-unspecified+ ; which means INADDR_ANY or 0.0.0.0
                    :port port
                    :reuse-addr t)
      (format t "Bound socket: ~A~%" socket)


2. Listen on the socket

   Listening on a socket allows clients to connect. In this example, we've
   specified that 5 pending connection can be queued up in the kernel before
   being accepted by the process.

   .. code:: lisp

      ;; Convert the sockxet to a listening socket
      (listen-on socket :backlog 5)
      (format t "Listening on socket bound to: ~A:~A~%"
              (local-host socket) (local-port socket))


3. Accept the client connection.

   Here we finally call the IOLib function ACCEPT-CONNECTION. We would like it
   to block, so we pass it :wait t. When ACCEPT-CONNECTION returns it will
   return a new socket which represents the connection to the client.
   ACCEPT-CONNECTION can return nil under some situations, such as on a slow
   server when the client sent a TCP RST packet in between the time the kernel
   sees the connection attempt and ACCEPT-CONNECTION is actually called.  We
   also opt to use the function REMOTE-NAME, which returns two values, the ip
   address and port of the remote side of the socket.

   .. code:: lisp

      ;; Block on accepting a connection
      (format t "Waiting to accept a connection...~%")
      (let ((client (accept-connection socket :wait t)))
        (when client
          ;; When we get a new connection, show who it is from.
          (multiple-value-bind (who rport)
              (remote-name client)
            (format t "Got a connection from ~A:~A!~%" who rport))


4. Write the time to the client.

   Here we've figured out the time string and wrote it to the client.  Notice
   we call the function FINISH-OUTPUT. This ensures that all output is written
   to the client socket. For streams using blocking I/O, it is recommended
   that every write to a blocking socket be followed up with a call to
   FINISH-OUTPUT.

   .. code:: lisp

      ;; Since we're using a internet TCP stream, we can use format
      ;; with it. However, we should be sure to call finish-output on
      ;; the socket in order that all the data is sent. Also, this is
      ;; a blocking write.
      (multiple-value-bind (s m h d mon y)
          (get-decoded-time)
        (format t "Sending the time...")
        (format client "~A/~A/~A ~A:~A:~A~%" mon d y h m s)
        (finish-output client))


5. Close the connection to the client.

    We're done writing to the client, so close the connection so the client
    knows it got everything.

    .. code:: lisp

       ;; We're done talking to the client.
       (close client)
       (format t "Sent!~%"))


6. Close the server's socket.

   Since this is a one shot server, we close the listening socket and exit. In
   this and all other servers we call FINISH-OUTPUT to flush all pending
   message to \*standard-output\*, if any.

   .. code:: lisp

      ;; We're done with the server socket too.
      (close socket)
      (finish-output)
      t)))


The above code is the basic idea for how a very simple TCP blocking I/O server
functions. Like ex1-client, this server suffers from the inability to handle
common signaled conditions such as a HANGUP from the client--which means the
client went away before the server could write the time to it.

However, one major, and subtle, problem of this particular example is that the
socket to the client is *not immediately closed* if the server happens to exit,
say by going through the debugger back to toplevel--or a signaled condition,
before writing the date to the client. If this happens, it can take a VERY long
time for the socket to be garbage collected and closed. In this scenario, the
client will hang around waiting for data which will never come until the Lisp
implementation closes the socket when it gets around to collecting it. Garbage
collection is an extremely nice feature of Common Lisp, but non-memory OS
resources in general should be eagerly cleaned up.  Clients can suffer from
this problem too, leaving open, but unmanipulable, sockets to servers.

All clients or servers written against IOLib should either use some IOLib
specific macros to handle closing of socket, Common Lisp's condition system
like handler-case to catch the signaled conditions, or some other manual
solution.


Daytime Server IVP4/TCP: ex2-server.lisp
-------------------------------------------------------------------------------

Similarly to ex2-client, this server uses the macro WITH-OPEN-SOCKET to open
the server socket. We introduce WITH-ACCEPT-CONNECTION to accept the client and
convert this server from a single shot server to an iterative server which can
handle, in a serial fashion only, multiple clients.

0. Serially accept and process clients:

   This portion of ex2-server shows the infinite loop around the accepting of
   the connection.  The macro WITH-ACCEPT-CONNECTION takes the server socket
   and introduces a new binding: client, which is the accepted connection. We
   ensure to tell the accept we'd like to be blocking. If for whatever reason
   we exit the body, it'll clean up the client socket automatically.

   .. code:: lisp

      ;; Keep accepting connections forever.
      (loop
          (format t "Waiting to accept a connection...~%")

          ;; Using with-accept-connection, when this form returns it will
          ;; automatically close the client connection.
          (with-accept-connection (client server :wait t)
            ;; When we get a new connection, show who it is from.
            (multiple-value-bind (who rport)
                (remote-name client)
              (format t "Got a connnection from ~A:~A!~%" who rport))

              ;; Since we're using a internet TCP stream, we can use format
              ;; with it. However, we should be sure to finish-output in
              ;; order that all the data is sent.
              (multiple-value-bind (s m h d mon y)
                  (get-decoded-time)
                (format t "Sending the time...")
                (format client "~A/~A/~A ~A:~A:~A~%" mon d y h m s)
                (finish-output client)
                (format t "Sent!~%")
                (finish-output)
                t)))))


For very simple blocking I/O servers like this one, serially accepting and
handling client connections isn't so much of a problem, but if the server does
anything which takes a lot of time or has to send lots of data back and forth
to many persistent clients, then this is a poor design. The means by which you
exit this server is by breaking evaluation and returning to the toplevel. When
this happens, the WITH-\* forms automatically close the connection to the
client.


Daytime Server IVP4/TCP: ex3-server.lisp
-------------------------------------------------------------------------------

In this iterative and blocking I/O server example, we add the handling of the
usual signaled conditions in network boundary cases often found with sockets.
Like the earlier client where we introduced HANDLER-CASE, this involves a
little bit of restructuring of the codes.

0. A helper function which opens a passive socket, binds it, and
   listens on it:

   There is nothing new in this portion of the code. We've seen this pattern
   before. In production code, we could probably shorten this further by
   having WITH-OPEN-SOCKET do the binding and connecting with appropriate
   keyword arguments.

   .. code:: lisp

      (defun run-ex3-server-helper (port)
        (with-open-socket
          (server :connect :passive
                  :address-family :internet
                  :type :stream
                  :ipv6 nil
                  :external-format '(:utf-8 :eol-style :crlf))

          (format t "Created socket: ~A[fd=~A]~%" server (socket-os-fd server))

          ;; Bind the socket to all interfaces with specified port.
          (bind-address server +ipv4-unspecified+ :port port :reuse-addr t)
          (format t "Bound socket: ~A~%" server)

          ;; start listening on the server socket
          (listen-on server :backlog 5)
          (format t "Listening on socket bound to: ~A:~A~%"
                  (local-host server)
                  (local-port server))


1. Repeatedly handle clients in a serial fashion:

   The new material in this function is the HANDLER-CASE around sending the
   client the time information. The boundary conditions when writing to a
   client include the server getting a reset (RST) from the client or
   discovering the client had gone away and there is no-one to which to write.
   Since the write is contained within the WITH-ACCEPT-CONNECTION form, if any
   of these conditions happen, we simply notice that they happened and let the
   form clean up the socket when it exits.  If we didn't catch the conditions,
   however, we'd break into the debugger.

   One might ask what the value of catching these conditions here is at all
   since we don't actually do anything with them--other than printing a
   message and preventing the code from breaking into the debugger. For the
   purposes of the tutorial, it is intended that the reader induce the
   boundary cases manually and see the flow of the code and to understand
   exactly what conditions may be signaled under what conditions and how to
   structure code to deal with them. In production code where the author might
   not care about these conditions at all, one might simply ignore all the
   signaled conditions that writing to the client might cause.

   Of course, the appropriateness of ignoring network boundary conditions is
   best determined by context.

   .. code:: lisp

      ;; keep accepting connections forever.
      (loop
          (format t "Waiting to accept a connection...~%")

          ;; Here we see with-accept-connection which simplifies closing
          ;; the client socket when are done with it.
          (with-accept-connection (client server :wait t)
            ;; When we get a new connection, show who it
            ;; is from.
            (multiple-value-bind (who rport)
                (remote-name client)
              (format t "Got a connnection from ~A:~A!~%" who rport))

              ;; Since we're using an internet TCP stream, we can use format
              ;; with it. However, we should be sure to finish-output in
              ;; order that all the data is sent.
              (multiple-value-bind (s m h d mon y)
                  (get-decoded-time)
                (format t "Sending the time...")

                ;; Catch the condition of the client closing the connection.
                ;; Since we exist inside a with-accept-connection, the
                ;; socket will be automatically closed.
                (handler-case
                    (progn
                      (format client "~A/~A/~A ~A:~A:~A~%" mon d y h m s)
                      (finish-output client))

                  (socket-connection-reset-error ()
                    (format t "Client reset connection!~%"))

                  (hangup ()
                    (format t "Client closed conection!~%")))

                (format t "Sent!~%"))))


2. End of the helper function, returns T to whomever called it:

   .. code:: lisp

      t))


3. The entry point into this example:

   We handle the condition SOCKET-ADDRESS-IN-USE-ERROR which is most commonly
   signaled when we try to bind a socket to address which already has a server
   running on it or when the address is in the TIME_WAIT state. The latter
   situation is so common--usually caused by a server just having exited and
   another one starting up to replace it, that when binding addresses, one
   should supply the keyword argument :reuse-addr with a true value to
   BIND-ADDRESS to allow binding a socket to an address in TIME_WAIT state.

   .. code:: lisp

      ;; This is the main entry point into the example 3 server.
      (defun run-ex3-server (&key (port *port*))
        (handler-case

            (run-ex3-server-helper port)

          (socket-address-in-use-error ()
            ;; Here we catch a condition which represents trying to bind to
            ;; the same port before the first one has been released by the
            ;; kernel.  Generally this means you forgot to put ':reuse-addr
            ;; t' as an argument to bind address.
            (format t "Bind: Address already in use, forget :reuse-addr t?")))

        (finish-output))


Daytime Server IVP4/TCP: ex4-server.lisp
-------------------------------------------------------------------------------

This is the first of our concurrent servers and the last of our daytime
protocol servers. Usually concurrency is introduced (in the UNIX environment)
with the fork() library call which creates an entirely new process with
copy-on-write semantics to handle the connection to the client. In this
tutorial environment, we've chosen to render this idea with the portable
threading library Bordeaux Threads.  The I/O is still line oriented and
blocking, however, when a thread blocks another can run giving the illusion of
a server handling multiple clients in a non-blocking fashion.

We also introduce UNWIND-PROTECT ensures that various sockets are closed under
various boundary conditions in the execution of the server.  An UNWIND-PROTECT
executes a single form, and after the evaluation, or interruption, of that
form, evaluates a special cleanup form. The cleanup form is *always* evaluated
and we use this to cleanup non-memory system resources like sockets.

Threads present their own special problems in the design of a server. Two
important problems are: data races and thread termination. The tutorial tries
very hard to avoid any data races in the examples and this problem is
ultimately solvable using Bordeaux-Threads mutexes or condition variables.  Our
simple examples do not need mutexes as they do not share any data between
themselves.

The harder problem is thread termination. Since the tutorial encourages
experimentation with the clients and servers in a REPL, threads may leak when
the server process' initial thread stops execution and goes back to the REPL.
We use three API calls from the Bordeaux Threads: THREAD-ALIVE-P, ALL-THREADS,
and DESTROY-THREAD--which are not to be used in normal thread programming.  We
do this here in order to try and clean up leaked threads so the clients know
immediately when the server process stopped and we don't pollute the REPL with
an ever increasing number of executing threads. The employed method of
destroying the threads, on SBCL specifically, allows the invocation of the
thread's UNWIND-PROTECT's cleanup form, which closes the socket to the client
before destroying the thread.  On other implementations of Common Lisp, we are
not guaranteed that the thread's UNWIND-PROTECT cleanup form will be evaluated
when we destroy it.

This method is also extremely heavy handed in that it uses the function
IGNORE-ERRORS to ignore any condition that Bordeaux Thread's DESTROY-THREAD may
have signaled, including important conditions like HEAP-EXHAUSTED-ERROR, an
SBCL specific condition. In a real threaded server, the exiting of the initial
thread (which means exiting of the runtime and termination of the entire Lisp
process) will destroy all other threads as the process tears itself down and
exits. This is the recommended way a threaded server should exit.

Since threading is implementation dependent for what guarantees are provided,
any non-toy threaded network server will probably use the native implementation
of threads for a specific Common Lisp implementation.  An example difficult
situation would be trying to terminate a thread which is blocked on I/O.
Different implementations would handle this in different ways.

The two provided examples, ex4-server and ex5-server, provide a general idea
for the structuring of the code to utilize threads.

Here is the dissection of ex4-server:

0. A special variable which will allow the initial thread to pass a client
   socket to a thread handling said client:

   .. code:: lisp

      ;; This variable is the means by which we transmit the client socket from
      ;; the initial thread to the particular thread which will handle that client.
      (defvar *ex4-tls-client* nil)


1. A helper function which begins with the usual recipe for a server:

   .. code:: lisp

      (defun run-ex4-server-helper (port)
        (with-open-socket
          (server :connect :passive
                  :address-family :internet
                  :type :stream
                  :ipv6 nil
                  :external-format '(:utf-8 :eol-style :crlf))

          (format t "Created socket: ~A[fd=~A]~%" server (socket-os-fd server))

          ;; Bind the socket to all interfaces with specified port.
          (bind-address server +ipv4-unspecified+ :port port :reuse-addr t)
          (format t "Bound socket: ~A~%" server)

          ;; start listening on the server socket
          (listen-on server :backlog 5)
          (format t "Listening on socket bound to: ~A:~A~%"
                  (local-host server)
                  (local-port server))


2. Forever more, accept a client connection on the listening socket
   and start a thread which handles it:

   There is a lot going on in this piece of code. The first thing to notice is
   the UNWIND-PROTECT and its cleanup form. The form which UNWIND-PROTECT is
   guarding is an infinite loop which does a blocking accept to get a client
   socket, rebinds \*default-special-bindings\* adding to its assoc list the
   binding for \*ex4-tls-client\*, and creates a thread which handles the
   client.

   The cleanup form walks all of the active client threads and destroys them,
   ignoring any conditions that may have arose while doing so. Destroying the
   threads prevents them from piling up and eventually causing havoc if many
   servers start and exit over time. In addition, it forces an eager close on
   the client sockets allowing any clients to know the server went away
   immediately.

   .. code:: lisp

      ;; Here we introduce unwind-protect to ensure we properly clean up
      ;; any leftover threads when the server exits for whatever reason.
      ;; keep accepting connections forever, but if this exits for
      ;; whatever reason ensure to destroy any remaining running
      ;; threads.
      (unwind-protect
          (loop                         ; keep accepting connections...
              (format t "Waiting to accept a connection...~%")
              (finish-output)
              (let* ((client (accept-connection server :wait t))
                     ;; set up the special variable according to the
                     ;; needs of the Bordeaux Threads package to pass in
                     ;; the client socket we accepted to the about to be
                     ;; created thread.  *default-special-bindings* must
                     ;; not be modified, so here we just push a new scope
                     ;; onto it.
                     (*default-special-bindings* (acons '*ex4-tls-client* client
                                                         *default-special-bindings*)))

                ;; ...and handle the connection!
                (when client
                  (make-thread #'process-ex4-client-thread
                               :name 'process-ex4-client-thread))))

            ;; Clean up form for uw-p.
            ;; Clean up all of the client threads when done.
            ;; This code is here for the benefit of the REPL because it is
            ;; intended that this tutorial be worked interactively. In a real
            ;; threaded server, the server would just exit--destroying the
            ;; server process, and causing all threads to exit which then notifies
            ;; the clients.
            (format t "Destroying any active client threads....~%")
            (mapc #'(lambda (thr)
                      (when (and (thread-alive-p thr)
                        (string-equal "process-ex4-client-thread"
                                      (thread-name thr)))
                        (format t "Destroying: ~A~%" thr)
                        ;; Ignore any conditions which might arise if a
                        ;; thread happened to finish in the race between
                        ;; liveness testing and destroying.
                        (ignore-errors (destroy-thread thr))))
                  (all-threads)))))


3. The beginning of the thread handling the client:

   When the thread is born, the aforementioned explicit binding of the client
   socket to \*ex4-tls-client\* takes effect via the \*default-special-bindings\*
   mechanism. By declaring \*ex4-tls-client\* ignorable, we inform the compiler
   that this variable is set "elsewhere" and no warning should be emitted
   about its possibly undefined value. In our case, this will always be
   defined at runtime in this server.

   .. code:: lisp

      ;;; The thread which handles the client connection.
      (defun process-ex4-client-thread ()
        ;; This variable is set outside of the context of this thread.
        (declare (ignorable *ex4-tls-client*))


4. Send the time to the socket:

   The UNWIND-PROTECT in this form handles every possible case of leaving the
   evaluable function such as it completing normally, a condition being
   signaled, or by thread destruction--on SBCL! In all cases, the socket to
   the client is closed which cleans up OS resources and lets the client know
   right away the server has closed the connection. The HANDLER-CASE form here
   just informs us which of the common IOLib conditions may have been signaled
   while writing the time to the client.

   .. code:: lisp

      ;; We ensure the client socket is always closed!
      (unwind-protect
          (multiple-value-bind (who port)
              (remote-name *ex4-tls-client*)
            (format t "A thread is handling the connection from ~A:~A!~%"
                    who port)

            ;; Prepare the time and send it to the client.
            (multiple-value-bind (s m h d mon y)
                (get-decoded-time)
              (handler-case
                  (progn
                    (format t "Sending the time to ~A:~A..." who port)
                    (format *ex4-tls-client*
                            "~A/~A/~A ~A:~A:~A~%"
                            mon d y h m s)
                    (finish-output *ex4-tls-client*)
                    (format t "Sent!~%"))

                (socket-connection-reset-error ()
                  (format t "Client ~A:~A reset the connection!~%" who port))

                (hangup ()
                  (format t "Client ~A:~A closed connection.~%" who port)))))

        ;; Cleanup form for uw-p.
        (format t "Closing connection to ~A:~A!~%"
                (remote-host *ex4-tls-client*) (remote-port *ex4-tls-client*))
        (close *ex4-tls-client*)))


   It is a bit tricky to robustly handle closing of the client socket in the
   thread. For example, if we bound the special variable \*ex4-tls-client\* to a
   lexically scoped variable and then did the UNWIND-PROTECT form to close the
   lexically scoped variable, then if this thread wakes up and gets destroyed
   after the lexical binding, but before the UNWIND-PROTECT, we'd lose a
   socket to a client into the garbage collector.

    Such incorrect code would look like:

    .. code:: lisp

       ;; This code is incorrect!
       (defun process-ex4-client-thread ()
         (declare (ignorable *ex4-tls-client*))
         (let ((client *ex4-tls-thread*))
           ;; thread gets destroyed right here! client socket is left open!
           (unwind-protect
               ( [evaluable form] )
             (close client))))


5. The entry point into this example:

   Like earlier servers, we call the helper function and catch what happens if
   \:reuse-addr wasn't true in the BIND-ADDRESS function call.

   .. code:: lisp

      ;; The entry point into this example.
      (defun run-ex4-server (&key (port *port*))
        (handler-case

            (run-ex4-server-helper port)

          ;; handle some common signals
          (socket-address-in-use-error ()
            (format t "Bind: Address already in use, forget :reuse-addr t?")))

        (finish-output))



Daytime Client/Server Commentary
-------------------------------------------------------------------------------

This concludes the examples using the daytime protocol. We've seen patterns
emerge in how the simplest of clients and servers are built and began to reason
about how to handle common signaled conditions. Threading, of course, increases
the care one must have in order to ensure that data access and control flow is
kept consistent.

.. comment: end of file
