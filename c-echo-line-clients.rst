.. comment: -*- mode: rst; coding: utf-8; electric-indent-mode: nil; tab-always-indent: t -*-


Echo Clients
===============================================================================

The echo clients are a group of programs which read a line from
\*standard-input\*, write it to the server, read back the response from the
server, and emit the result to \*standard-output\*.  While there is a portable
method to read "however much is available" from \*standard-input\*, there isn't
the symmetrical method to write "whatever I'm able" to \*standard-output\*.  For
our client design, this means that all of these clients are line oriented and
do blocking I/O when reading from \*standard-input\* and writing to
\*standard-output\*.


Echo Client IPV4/TCP: ex4-client.lisp
-------------------------------------------------------------------------------

This is a very basic echo client program that handles the usual conditions
while talking to the server:

0. Connect to the server and start echoing lines:

   Here we use WITH-OPEN-SOCKET to create an active socket that we then use to
   connect to the server. We handle HANGUP, for when the server went away
   before the client could write to it, and END-OF-FILE, for when the server
   closes down the connection.

   Notice we call the function ex4-str-cli inside of a HANDLER-CASE macro.
   This allows us to not check for any signaled conditions in ex4-str-cli and
   greatly simplifies its implementation.

   In this specific example, we don't do anything other than notify that the
   condition happened since after that the socket gets closed via
   WITH-OPEN-SOCKET.

   .. code:: lisp

      (defun run-ex4-client-helper (host port)

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
                  (remote-host socket) (remote-port socket)
                  (local-host socket) (local-port socket))

          (handler-case
              (ex4-str-cli socket)

            (socket-connection-reset-error ()
              (format t "Got connection reset. Server went away!"))

            (hangup ()
              (format t "Got hangup. Server closed connection on write!~%"))

            (end-of-file ()
              (format t "Got end-of-file. Server closed connection on read!~%")))))


1. Echo lines to the server:

   Until the user inputs "quit" on a line by itself, we read a line, send it
   to the server, read it back, and emit it to stdout. If any of the usual
   conditions are signaled here, the handler-case in the Step 0 code fires and
   we deal with it there.

   When "quit" is entered, the line is sent on the round trip to the server
   like usual, but this time the server closes the connection to the client.
   Unfortunately, since the client is doing blocking I/O, we must read another
   line from \*standard-input\* before we get any signaled condition when IOLib
   discovers the socket has been closed by the server.

   In practice, this means after the server closed the connection, the user
   must hit  in order to drive the I/O loop enough to get the signaled
   condition.

   .. code:: lisp

      ;; read a line from stdin, write it to the server, read the response, write
      ;; it to stdout. If we read 'quit' then echo it to the server which will
      ;; echo it back to us and then close its connection to us.
      (defun ex4-str-cli (socket)
        (loop
            (let ((line (read-line)))
              ;; send it to the server, get the response.
              (format socket "~A~%" line)
              (finish-output socket)
              (format t "~A~%" (read-line socket)))))


2. Entry point into the example:

   We handle the usual connection refused condition, but otherwise this step
   is unremarkable.

   .. code:: lisp

      ;; This is the entry point into this example
      (defun run-ex4-client (&key (host *host*) (port *port*))
        (unwind-protect
            (handler-case

                (run-ex4-client-helper host port)

              ;; handle a commonly signaled error...
              (socket-connection-refused-error ()
                (format t "Connection refused to ~A:~A. Maybe the server isn't running?~%"
                        (lookup-hostname host) port)))

          ;; Cleanup form
          (format t "Client Exited.~%")))


Echo Client IPV4/TCP: ex5a-client.lisp
-------------------------------------------------------------------------------

This is the first client to use the socket multiplexer to notice when the
socket to the server is ready for reading or writing. While the multiplexer is
often used in single threaded servers it can be used for clients--especially
clients which may talk to multiple servers like web clients.  Use of the
multiplexer API will require a significant change in how the code is
structured. It is not recommended that the multiplexer and threads be used
simultaneously to handle network connections.

Keeping in mind the fact that we ALWAYS could block while reading from
\*standard-input\* or writing to \*standard-output\*, we only attempt to read/write
to the standard streams when the multiplexer thinks it can read/write to the
server without blocking. This is a change from the traditional examples of how
to do this in C because in C one can determine if STDIN or STDOUT are ready in
the same manner as a network file descriptor.

The first big change from our previous examples is that we stop using
WITH-OPEN-SOCKET since now we must manually control when the socket to the
server must be closed. This is especially important for clients who use active
sockets. The second change is how we do the creation and registering of the
handlers for reading and writing to the server socket.  The third change is how
to unregister a handler and close the socket associated with it under the right
conditions. Other changes will be explained as we meet them.

The main functions of the multiplexer API are:

.. code:: lisp

   (make-instance 'iomux:event-base ....)

Create an instance of the event-base, and associate some properties
with it, such as event-dispatch should return if the multiplexer
does not have any sockets it is managing.
Passed an:
\:exit-when-empty - when no handlers are registered, event-dispatch
will return.

.. code:: lisp

   (event-dispatch ...)


By default, sit in the multiplexer loop forever and handle I/O
requests. It is passed the event-base binding and in addition:
\:once-only - run the ready handlers once then return.
\:timeout - when there is no I/O for a certain amount of time return.

.. code:: lisp

   (set-io-handler ...)

Associates a handler with a state to be called with a specific socket.
Passed an:

- event-base binding

- \:read or \:write or \:error keyword

- the handler closure

.. code:: lisp

   (remove-fd-handlers ...)

Removes a handler for a specific state with a specific socket.
Passed an:

- event-base binding

- an fd

- one or more of \:read t, \:write t, \:error t

Here is the example using this API.

0. The event base:

   The event-base is the object which holds the state of the multiplexer.  It
   must be initialized and torn down as we'll see in the entry function to
   this example.

   .. code:: lisp

      ;; This will be an instance of the multiplexer.
      (defvar *ex5a-event-base*)


1. A helper function in which we create the active socket:

   Instead of using WITH-OPEN-SOCKET, we manually create the socket. We do
   this to better control how to close the socket. WITH-OPEN-SOCKET will try
   to FINISH-OUTPUT on the socket before closing it. This is bad if the socket
   had been previously closed or signaled a condition like HANGUP. Trying to
   write more data to an already hung up socket will simply signal another
   condition. To prevent layers of condition handling code, we explicitly
   handle closing of the socket ourselves.

   .. code:: lisp

      (defun run-ex5a-client-helper (host port)
        ;; Create a internet TCP socket under IPV4
        ;; We specifically do not use with-open-socket here since that form is
        ;; more suited for synchronous i/o on one socket. Since we do not use that
        ;; form, it is up to the handlers to decide to remove and close the socket
        ;; when the connection to the server should be closed.
        (let ((socket (make-socket :connect :active
                                   :address-family :internet
                                   :type :stream
                                   :external-format '(:utf-8 :eol-style :crlf)
                                   :ipv6 nil)))


2. Connect to the server, register the socket handlers:

   We protect the closing of the socket via UNWIND-PROTECT. We will talk about
   the ramifications of this decision in the next step which describes the
   UNWIND-PROTECT's cleanup form. In this section of code, we set up a read
   and write handler for the socket, and invoke the dispatch function, which
   will continue calling the handlers associated with the socket until the
   socket gets closed and the handlers unregistered. When this happens (see
   the entrance function step for why), EVENT-DISPATCH returns and we continue
   on to the cleanup form for the UNWIND-PROTECT.

   Setting up a handler in the multiplexer requires several arguments to
   the function set-io-handler. Here are what the arguments to that function
   are:

   a. .. code:: lisp

        *ex5a-event-base*

      This is the instance of the multiplexer for which we are setting
      up the handler.

   b. .. code:: lisp

        (socket-os-fd socket)

      This call returns the underlying operating system's file
      descriptor associated with the socket.

   c. .. code:: lisp

        :read

      This keyword states that we'd like to call the handler when the
      socket is ready to read. There is also :write and :error.

   d. .. code:: lisp

        (make-ex5a-str-cli-read socket (make-ex5a-client-disconnector socket))

      The make-ex5a-str-cli-read function returns a closure over the
      socket and another closure returned by the
      make-ex5a-client-disconnector function. This function is what will
      be called when the socket is ready for reading. We will shortly
      explain the signature of this function and what gets passed to it
      by the multiplexer. The disconnector function will be called by the
      returned reader function if the reader function thinks that it
      needs to close the socket to the server.

   .. code:: lisp

      (unwind-protect
          (progn
            ;; do a blocking connect to the echo server on the port.
            (connect socket (lookup-hostname host) :port port :wait t)

            (format t "Connected to server ~A:~A from my local connection at ~A:~A!~%"
                    (remote-host socket) (remote-port socket)
                    (local-host socket) (local-port socket))

            ;; set up the handlers for read and write
            (set-io-handler *ex5a-event-base*
                            (socket-os-fd socket)
                            :read (make-ex5a-str-cli-read socket
                                                          (make-ex5a-client-disconnector socket)))

            (set-io-handler *ex5a-event-base*
                            (socket-os-fd socket)
                            :write (make-ex5a-str-cli-write socket
                                                            (make-ex5a-client-disconnector socket)))

            (handler-case
                ;; keep processing input and output on the fd by
                ;; calling the relevant handlers as the socket becomes
                ;; ready. The relevant handlers will take care of
                ;; closing the socket at appropriate times.
                (event-dispatch *ex5a-event-base*)

              ;; We'll notify the user of the client if a handler missed
              ;; catching common conditions.
              (hangup ()
                (format t "Uncaught hangup. Server closed connection on write!%"))
              (end-of-file ()
                (format t "Uncaught end-of-file. Server closed connection on read!%"))))


3. Cleanup form for UNWIND-PROTECT:

   In the cleanup form, we always close the socket and we pass the function
   close \:abort t to try and close the socket in any way possible. If we just
   tried closing the socket, then we might cause another condition to be
   signaled if a previous condition, like HANGUP, had already affected the
   socket. \:abort t avoids that case. If the socket is already closed by a
   handler by the time we get here, closing it again hurts nothing.

   .. code:: lisp

      ;; Cleanup expression for uw-p.
      ;; Try to clean up if the client aborted badly and left the socket open.
      ;; It is safe to call close mutiple times on a socket.
      ;; However, we don't want to finish-output on the socket since that
      ;; might signal another condition since the io handler already closed
      ;; the socket.
      (format t "Client safely closing open socket to server.~%")
      (close socket :abort t))))


4. Make the writer function for when the socket is ready to write:

   This function returns a closure which is called by the multiplexer when it
   is ready to read something from the server. The arguments to the closure
   are fd, the underlying file descriptor for the ready socket, event, which
   could be \:read, \:write, or :error if the handler was registered multiple
   times, and exception, which is nil under normal conditions, :error under an
   error with the socket, or \:timeout, if we were using timeout operations
   when dealing with the socket.

   The closure will read a line with the function READ-LINE and write it to
   the server. The read will be blocking, but hopefully the write won't be
   since the multiplexer told us we could perform the write and not block.
   Obviously, is we write an enormous line, then we might block again, and in
   this case the FINISH-OUTPUT on the socket will push the data in a blocking
   I/O fashion until it is done and we return from the handler. So while this
   closure for the most part writes when ready, there are cases under which
   it'll still block.

   In this handler, if there is a signaled condition either reading from
   \*standard-input\* (the END-OF-FILE condition) or writing to the server
   socket (the HANGUP condition), we invoke the disconnector closure and pass
   it \:close. When we get to the description of the disconnector function,
   you'll see what that means.

   Once the disconnector closure is invoked, the handler will have been
   removed and the socket closed. This will make EVENT-DISPATCH return since
   the only socket it was multiplexing for was closed--because we've told the
   multiplexer to do so when it was made!

   .. code:: lisp

      (defun make-ex5a-str-cli-write (socket disconnector)
        ;; When this next function gets called it is because the event dispatcher
        ;; knows the socket to the server is writable.
        (lambda (fd event exception)
          ;; Get a line from stdin, and send it to the server
          (handler-case
              (let ((line (read-line)))
                (format socket "~A~%" line)
                (finish-output socket))

            (end-of-file ()
              (format t "make-ex5a-str-cli-write: User performed end-of-file!~%")
              (funcall disconnector :close))

            (hangup ()
              (format t
                      "make-ex5a-str-cli-write: server closed connection on write!~%")
              (funcall disconnector :close)))))


5. Make the reader function for when the socket is ready to read:

   This piece of code is very similar to the previous step's code, we just
   handle the appropriate conditions and after reading the line from the
   server emit it to \*standard-output\*. Again, even though we are told we can
   read from the server without blocking, if the read is large enough we will
   continue to block until read-line reads the all the data and the newline.

   .. code:: lisp

      (defun make-ex5a-str-cli-read (socket disconnector)
        ;; When this next function gets called it is because the event dispatcher
        ;; knows the socket from the server is readable.
        (lambda (fd event exception)
          ;; get a line from the server, and send it to *standard-output*
          (handler-case
              ;; If we send "quit" to the server, it will close its connection to
              ;; us and we'll notice that with an end-of-file.
              (let ((line (read-line socket)))
                (format t "~A~%" line)
                (finish-output))

            (end-of-file ()
              (format t "make-ex5a-str-cli-read: server closed connection on read!~%")
              (funcall disconnector :close)))))


6. The disconnector function:

   This function returns a closure which takes an arbitrary number of
   arguments. If the arguments to the invoked closure contain \:read, \:write,
   or \:error, the respective handler on the associated socket is removed. If
   none of those three are supplied, then all handlers for that socket are
   removed.  Additionally if \:close is specified, the socket is closed.  While
   not all features of this function is used in this example, this function
   (or a similar one using the correct event-base special variable) is used
   whenever we use the multiplexer in an example.

   The closure is called whenever a handler believes it should unregister
   itself or another handler, or close the socket. Because we will often close
   the socket in the disconnector closure, we can't use WITH-OPEN-SOCKET to
   automatically close the socket because WITH-OPEN-SOCKET may try to flush
   data on the socket, signaling another condition.

   .. code:: lisp

      (defun make-ex5a-client-disconnector (socket)
        ;; When this function is called, it can be told which callback to remove, if
        ;; no callbacks are specified, all of them are removed! The socket can be
        ;; additionally told to be closed.
        (lambda (&rest events)
          (format t "Disconnecting socket: ~A~%" socket)
          (let ((fd (socket-os-fd socket)))
            (if (not (intersection '(:read :write :error) events))
                (remove-fd-handlers *ex5a-event-base* fd :read t :write t :error t)
                (progn
                  (when (member :read events)
                    (remove-fd-handlers *ex5a-event-base* fd :read t))
                  (when (member :write events)
                    (remove-fd-handlers *ex5a-event-base* fd :write t))
                  (when (member :error events)
                    (remove-fd-handlers *ex5a-event-base* fd :error t)))))
          ;; and finally if were asked to close the socket, we do so here
          (when (member :close events)
            (close socket :abort t))))


7. The entry point for this example and setting up the event-base:

   This function is much more complex than in examples that do not use the
   multiplexer. Protected by an UNWIND-PROTECT, we first initialize the event
   base my calling make-instance 'iomux:event-base.  Here is where we pass the
   keyword argument \:exit-when-empty t which states that the event-dispatch
   function should return when there are no more registered handlers. Once
   that is done, we call the helper, catching a common condition and waiting
   until we return.

   .. code:: lisp

      ;; This is the entry point for this example.
      (defun run-ex5a-client (&key (host *host*) (port *port*))
        (let ((*ex5a-event-base* nil))
          (unwind-protect
              (progn
                ;; When the connection gets closed, either intentionally in the client
                ;; or because the server went away, we want to leave the multiplexer
                ;; event loop. So, when making the event-base, we explicitly state
                ;; that we'd like that behavior.
                (setf *ex5a-event-base*
                      (make-instance 'iomux:event-base :exit-when-empty t))
                (handler-case
                    (run-ex5a-client-helper host port)

                  ;; handle a commonly signaled error...
                  (socket-connection-refused-error ()
                    (format t "Connection refused to ~A:~A. Maybe the server isn't running?~%"
                            (lookup-hostname host) port))))


8. The cleanup form for UNWIND-PROTECT:

   This cleanup form closes the \*ex5a-event-base\* instance. IOLib defines a
   method for the generic function CLOSE which accepts an event-base and
   performs the necessary work to shut it down.

   .. code:: lisp

      ;; Cleanup form for uw-p
      ;; ensure we clean up the event base regardless of how we left the client
      ;; algorithm
      (when *ex5a-event-base*
        (close *ex5a-event-base*))
      (format t "Client Exited.~%")
      (finish-output))))


While this program works just fine with human input, it has a failure when
reading batch input. The failure is that when we get the END-OF-FILE condition
when \*standard-input\* closes, we _immediately_ unregister the read/write
handlers to the server, close the socket and exit the program. This destroys
any in-flight data to/from the server and lines being echoed may be lost.


Echo Client IPV4/TCP: ex5b-client.lisp
-------------------------------------------------------------------------------

In order to fix the batch input problem of ex5a-client, we will use the
shutdown function which allows us to inform the server we are done writing
data, but leave the socket open so we can read the rest of the responses from
the server. This effectively closes only one-half of the TCP connection. The
server has to be made aware of this kind of protocol so it doesn't assume the
client completely exited when it gets an END-OF-FILE from the client and shuts
down the whole connection throwing away any queued data for the client.

This client is nearly identical to ex5a-client except we shut down the write
end of the connection to the server when we get END-OF-FILE from
\*standard-input\* and wait until we get all of the data back from the server.
The server signifies to us that it has sent all of the pending data by closing
the write end of its connection. The client sees the closing of the server's
write end as an END-OF-FILE on the socket connected to the server.

We show this example as a difference to ex5aq-client.

0. Shutdown the write end of the socket to the server:

   Here we use the expanded functionality of the disconnector closure.  After
   we shut down the write end of our TCP connection, we call (funcall
   disconnector \:write) which states only to remove the write (to the server)
   handler, but leave the connection open. After this happens, there is no way
   we can read from \*standard-input\* again.  Once the server sends the final
   data and the closes its connection to this client, we remove the read
   handler, which removes the last handler, and causes the EVENT-DISPATCH
   function to return, which ends the client computation.

   .. code:: lisp

      (defun make-ex5b-str-cli-write (socket disconnector)
        ;; When this next function gets called it is because the event dispatcher
        ;; knows the socket to the server is writable.
        (lambda (fd event exception)
          ;; Get a line from stdin, and send it to the server
          (handler-case
              (let ((line (read-line)))
                (format socket "~A~%" line)
                (finish-output socket))

            (end-of-file ()
              (format t
                      "make-ex5b-str-cli-write: User performed end-of-file!~%")
              ;; Shutdown the write end of my pipe to give the inflight data the
              ;; ability to reach the server!
              (format t
                      "make-ex5b-str-cli-write: Shutting down write end of socket!~%")
              (shutdown socket :write t)
              ;; since we've shut down the write end of the pipe, remove this handler
              ;; so we can't read more data from *standard-input* and try to write it
              ;; it to the server.
              (funcall disconnector :write))

            (hangup ()
              (format t
                      "make-ex5b-str-cli-write: server closed connection on write!~%")
              (funcall disconnector :close)))))


Be aware that even if both directions on one end of a connection are shutdown,
close still must be called upon the socket in order to release resources held
by the operating system.


.. comment: end of file
