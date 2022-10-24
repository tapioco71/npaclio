.. comment: -*- mode: rst; coding: utf-8; electric-indent-mode: nil; tab-always-indent: t -*-


Echo Servers
===============================================================================

The echo servers, paired to clients as per the beginning of this tutorial,
further evolve to using the multiplexer and becoming more fine grained with
respect to when I/O is done until we reach the ability to perform nonblocking
I/O of arbitrary read/write sizes.


Echo Server IPV4/TCP: ex5-server.lisp
-------------------------------------------------------------------------------

This threaded server is very similar to ex4-server, but instead of sending only
the time, each thread handles an echo protocol to a client.  While this is
still a blocking I/O server, only a single thread talking to a client gets
blocked, not the whole server.  Other than the server not honoring batch input
from the client correctly, this is a common model for a class of servers due to
its nonblocking behavior.

0. The special variable used to communicate the client socket to the thread:

   .. code:: lisp

      ;; The special variable used to hold the client socket for the thread
      ;; managing it.
      (defvar *ex5-tls-client* nil)


1. The usual prologue to a server:

   .. code:: lisp

      (defun run-ex5-server-helper (port)
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


2. First half of creating the client threads:

   .. code:: lisp

      ;; keep accepting connections forever, but if this exits for whatever
      ;; reason ensure to destroy any remaining running threads.
      (unwind-protect
          (loop
              (format t "Waiting to accept a connection...~%")
              (finish-output)
              (let* ((client (accept-connection server :wait t))
                ;; set up the special variable to store the client
                ;; we accepted...
                     (*default-special-bindings*
                       (acons '*ex5-tls-client* client
                              *default-special-bindings*)))

                ;; ...and handle the connection!
                (when client
                  (make-thread #'process-ex5-client-thread
                               :name 'process-ex5-client-thread))))


3. Second half, the cleanup form for the UNWIND-PROTECT:

   We make sure to clean up only the client threads!

   .. code:: lisp

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
                           (string-equal "process-ex5-client-thread"
                                         (thread-name thr)))
                  (format t "Destroying: ~A~%" thr)
                  ;; Ignore any conditions which might arise if a
                  ;; thread happened to finish in the race between
                  ;; liveness testing and destroying.
                  (ignore-errors (destroy-thread thr))))
            (all-threads)))))


4. Handle the client and deal with signaled conditions:

   In this function, we ensure that under all conditions of the execution of
   this function, if something goes wrong, we eagerly close the socket to the
   client so it is not leaked into the garbage collector.  We also handle
   numerous conditions the the client could generate while talking to it in
   the function str-ex5-echo.

   .. code:: lisp

      ;; The thread which handles the client connection.
      (defun process-ex5-client-thread ()
        ;; declared ignorable because this dynamic variable is bound outside
        ;; of the context of this function.
        (declare (ignorable *ex5-tls-client*))
        ;; no matter how we get out of the client processing loop, we always
        ;; close the connection.
        (unwind-protect
            (multiple-value-bind (who port)
                (remote-name *ex5-tls-client*)
              (format t "A thread is handling the connection from ~A:~A!~%"
                      who port)

              (handler-case
                  ;;  perform the actual echoing algorithm
                  (str-ex5-echo *ex5-tls-client* who port)

                (socket-connection-reset-error ()
                  (format t "Client ~A:~A: connection reset by peer.~%"
                          who port))

                (end-of-file ()
                  (format t "Client ~A:~A closed connection for a read.~%"
                          who port)
                  t)

                (hangup ()
                  (format t "Client ~A:~A closed connection for a write.~%"
                          who port)
                  t)))

          ;; cleanup form of the unwind-protect
          ;; We always close the connection to the client, even if this
          ;; thread gets destroyed (at least in SBCL this cleanup form gets
          ;; run when this thread is destroyed).
          (format t "Closing connection to ~A:~A!~%"
                  (remote-host *ex5-tls-client*) (remote-port *ex5-tls-client*))
          (close *ex5-tls-client*)
          t))


5. Actually perform the echo protocol to the client:

   Read lines from the client and echo them back. All of this I/O is blocking.
   If we see "quit" from the client, then exit the loop, which causes the
   UNWIND-PROTECT cleanup form in step 4 to fire and close the connection to
   the client.

   .. code:: lisp

      ;; The actual function which speaks to the client.
      (defun str-ex5-echo (client who port)
        ;; here we let signaled conditions on the boundary conditions of the
        ;; client (meaning it closes its connection to us on either a read or
        ;; a write) bail us out of the infinite loop
        (let ((done nil))
          (loop until done
              do
                (let ((line (read-line client)))
                  (format t "Read line from ~A:~A: ~A~%" who port line)
                  (format client "~A~%" line)
                  (finish-output client)
                  (format t "Wrote line to ~A:~A: ~A~%" who port line)

                  ;; Exit the thread when the user requests it with 'quit'.
                  ;; This forces a close to the client socket.
                  (when (string= line "quit")
                    (setf done t))
                  t))))

6. The entrance function into this example:

   .. code:: lisp

      ;; This just checks for some error conditions so we can print out a nice
      ;; message about it.
      (defun run-ex5-server (&key (port *port*))
        (handler-case

            (run-ex5-server-helper port)

          ;; handle some common conditions
          (socket-address-in-use-error ()
            (format t "Bind: Address already in use, forget :reuse-addr t?")))

        (finish-output))


Echo Server IPV4/TCP: ex6-server.lisp
-------------------------------------------------------------------------------

This is the first of the echo servers which use the multiplexer to handle
multiple clients concurrently. It is a single threaded program. As mentioned
before, one shouldn't mix the multiplexer and threads together to handle
network connections.

We explore a new concept with the multiplexer in that the listening server
socket is itself registered with the multiplexer. The read handler (called the
listener handler in this context) associated with this socket becomes ready
when a client has connected to the server address. Thus, once the listening
socket is ready the listener handler accepts the client and associates the line
echo protocol callback with the client's socket in the multiplexer.

The I/O design of this server is such that if the client connection is ready to
read, we read a line, then immediately write the line back to the client in the
same function without waiting to see if it is ready for writing. Since we are
still using blocking I/O, this is ok.  The reason for this example's design was
to minimize the complexity of using the multiplexer in order to introduce the
listener handler. Later examples become much more complex as we push the
multiplexer API farther.

0. The variable which holds the multiplexer instance:

   .. code:: lisp

      ;; This variable represents the multiplexer state.
      (defvar *ex6-server-event-base*)


1. A hash table of client connections:

   We record each client that connects to the server into a hash table socket
   keyed by the list (ip address port) and associate with it a value of the
   client's socket. This is so that under any conditions of the server exiting
   we can eagerly close any open connections to clients in a cleanup form.

   .. code:: lisp

      ;; This holds any open connections to clients as keys in the table. The values
      ;; is a list containing the host and port of the connection. We use this to
      ;; close all connections to the clients, if any, when the server exits.  This
      ;; allows all clients to notice the server had gone away.
      (defvar *ex6-server-open-connections*)


2. Create and bind the server socket:

   We protect how we manipulate the server socket with an UNWIND-PROTECT so we
   ensure to close the socket at the end of the server's computation or if
   something went wrong.

   .. code:: lisp

      ;; Set up the server and server clients with the multiplexer
      (defun run-ex6-server-helper (port)

        ;; We don't use with-open-socket here since we may need to have a
        ;; finer control over when we close the server socket.
        (let ((server (make-socket :connect :passive
                                   :address-family :internet
                                   :type :stream
                                   :ipv6 nil
                                   :external-format '(:utf-8 :eol-style :crlf))))
          (unwind-protect
              (progn
                (format t "Created socket: ~A[fd=~A]~%" server (socket-os-fd server))
                ;; Bind the socket to all interfaces with specified port.
                (bind-address server +ipv4-unspecified+ :port port :reuse-addr t)
                (format t "Bound socket: ~A~%" server)

                ;; start listening on the server socket
                (listen-on server :backlog 5)
                (format t "Listening on socket bound to: ~A:~A~%"
                        (local-host server)
                        (local-port server))


3. Register a listener handler on the server socket and start dispatching
   events with the multiplexer:

   .. code:: lisp

      ;; Set up the initial listener handler for any incoming clients
      (set-io-handler *ex6-server-event-base*
                      (socket-os-fd server)
                      :read
                        (make-ex6-server-listener-handler server))

      ;; keep accepting connections forever.
      (handler-case
          (event-dispatch *ex6-server-event-base*)

        ;; Just in case any handler misses these conditions, we
        ;; catch them here.
        (socket-connection-reset-error ()
          (format t "~A~A~%"
                  "Caught unexpected reset by peer! "
                  "Client connection reset by peer!"))
        (hangup ()
          (format t "~A~A~%"
                  "Caught unexpected hangup! "
                  "Client closed connection on write!"))
        (end-of-file ()
          (format t "~A~A~%"
                  "Caught unexpected end-of-file! "
                  "Client closed connection on read!"))))


4. When the server stops handling clients, we close the server socket:

   .. code:: lisp

      ;; Cleanup expression for uw-p.
      ;; Ensure the server socket is closed, regardless of how we left
      ;; the server.
      (close server))))


5. The listener handler:

   Once the returned closure from this function is called by the multiplexer
   on the ready server socket, we accept the client with a blocking accept.
   We then save the client connection in our table and register the line echo
   closure with the socket.  The line echo closure will also contain a
   disconnector function as in previous usages of the multiplexer.

   .. code:: lisp

      ;; When the multiplexer states the server socket is ready for reading
      ;; it means that we have a client ready to accept. So we accept it and
      ;; then register the accepted client socket back into the multiplexer
      ;; with the appropritate echo protocol function.
      (defun make-ex6-server-listener-handler (socket)
        (lambda (fd event exception)

          ;; do a blocking accept, returning nil if no socket
          (let* ((client (accept-connection socket :wait t)))
            (when client
              (multiple-value-bind (who port)
                  (remote-name client)
                (format t "Accepted a client from ~A:~A~%" who port)

                ;; save the client connection in case we need to close it later
                ;; when the server exits.
                (setf (gethash `(,who ,port) *ex6-server-open-connections*) client)

                ;; set up an line echo function for the client socket.
                (set-io-handler *ex6-server-event-base*
                                (socket-os-fd client)
                                :read (make-ex6-server-line-echoer client
                                                                   who
                                                                   port
                                                                   (make-ex6-server-disconnector client))))))))


6. The line echo closure generator:

   This function returns a closure which is then bound to a client socket in
   the multiplexer. When the socket is ready, we read a line form the client
   and write it back to the client immediately. Since this is blocking I/O the
   whole server will wait until this transaction is complete.  This means that
   a client which sends one byte of ASCII that is not a newline can cause the
   whole server to block for all clients. This serious defect is remedied with
   non-blocking I/O, which we show in a later example.

   .. code:: lisp

      ;; This function returns a function that reads a line, then
      ;; echoes it right back onto the socket it came from. This is blocking
      ;; i/o.  This code can suffer denial of service attacks like on page
      ;; 167 of "Unix Network Programming 2nd Edition: Sockets and XTI", by
      ;; Richard Stevens.
      (defun make-ex6-server-line-echoer (socket who port disconnector)
        (format t "Creating line-echoer for ~A:~A~%" who port)
        (lambda (fd event exception)
          (handler-case
              (let ((line (read-line socket))) ;; read a line from the client
                (format t "Read ~A:~A: ~A~%" who port line)
                (format socket "~A~%" line) ;; write it the client
                (finish-output socket)
                (format t "Wrote ~A:~A: ~A~%" who port line)

                ;; close the connection to the client if it asked to quit
                (when (string= line "quit")
                  (format t "Client requested quit!~%")
                  (funcall disconnector who port)))

            (socket-connection-reset-error ()
              ;; Handle the usual and common conditions we'll see while
              ;; talking to a client
              (format t "Client's connection was reset by peer.~%")
              (funcall disconnector who port))

            (hangup ()
              (format t "Client went away on a write.~%")
              (funcall disconnector who port))

            (end-of-file ()
              (format t "Client went away on a read.~%")
              (funcall disconnector who port)))))


7. The disconnector closure generator:

   This function returns a closure that removes all the handlers from the
   socket in question and then closes it. Notice that this means this server
   is not capable of handling batch input from a client, since when it
   receives the END-OF-FILE on the read from a client, will immediately tear
   down the connection destroying any in flight data. After closing the
   socket, we also remove it from our table of open connections.

   .. code:: lisp

      ;; If we decide we need to disconnect ourselves from the client, this will
      ;; remove all the handlers and remove the record of our connection from
      ;; *ex6-server-open-connections*.
      (defun make-ex6-server-disconnector (socket)
        (lambda (who port)
          (format t "Closing connection to ~A:~A~%" who port)
          (remove-fd-handlers *ex6-server-event-base* (socket-os-fd socket))
          (close socket)
          (remhash `(,who ,port) *ex6-server-open-connections*)))


8. Initialize the event-base, the connection table, and start the server:

   This code is the beginning of the UNWIND-PROTECT form which protects the
   server's socket resources.

   .. code:: lisp

      ;; This is the entrance function into this example.
      (defun run-ex6-server (&key (port *port*))
        (let ((*ex6-server-open-connections* nil)
              (*ex6-server-event-base* nil))
          (unwind-protect
              (handler-case
                  (progn
                    ;; Clear the open connection table and init the event base
                    (setf *ex6-server-open-connections*
                          (make-hash-table :test #'equalp)

                          *ex6-server-event-base*
                          (make-instance 'event-base))

                    (run-ex6-server-helper port))

                ;; handle a common signal
                (socket-address-in-use-error ()
                  (format t "Bind: Address already in use, forget :reuse-addr t?")))


9. Cleanup the client connections and close the event-base:

   When the server exits we walk the \*ex6-server-open-connections\* hash and
   eagerly close every client we find there. After we are done, we close the
   event-base. This ensures every thing is cleaned up properly.

   .. code:: lisp

      ;; Cleanup form for uw-p
      ;; Close all open connections to the clients, if any. We do this
      ;; because when the server goes away we want the clients to know
      ;; immediately. Sockets are not memory, and can't just be garbage
      ;; collected whenever. They have to be eagerly closed.
      (maphash #'(lambda (k v)
                   (format t "Closing a client connection to ~A~%" k)
                   ;; We don't want to signal any conditions on the close...
                   (close v :abort t))
               *ex6-server-open-connections*)

      ;; and clean up the multiplexer too!
      (when *ex6-server-event-base*
        (close *ex6-server-event-base*))
      (format t "Server Exited~%")
      (finish-output))))


This server uses the multiplexer in a simple fashion because only one handler
is registered for a client. That handler reads, then writes the data back to
the client.  The scope of the data read from the client never has to leave the
handler function.


Echo Server IPV4/TCP: ex7-server.lisp
-------------------------------------------------------------------------------

This example is different than ex6-server because it fully separates the
reading and writing of data to a client into different handler functions. This
requires an architectural change to the server in order to be able to keep the
data from the client "somewhere" before being able to write it back to the
client when the multiplexer determines it can written to the client. We
introduce an io-buffer object, implemented in terms of a closure and one per
client, which stores the in-flight data until the client is ready to accept the
writes from the server.

Storage of client data introduces a problem in that if the client writes lots
of data to the server but happens to never be ready to accept it back from the
server, the server will consume all memory and run out of resources.  We
attempt to prevent this from happening, though not perfectly.

When the io-buffer is created for a client, we state we only would like a
certain number of bytes to be read from the client. Of course, since we're
using read-line with blocking I/O and the client could write a tremendous
amount of data before a newline, we can't completely enforce our storage policy
in this server. If the client, though, is well-behaved in that it sends
reasonable sized lines of text--a rarity in the real world, our implemented
policy is sufficient. When we reach the nonblocking I/O server example, we'll
find that we can perfectly enforce the per client data storage policy.

This server honors batch input from the client. When it sees the END-OF-FILE
from the client, and it still has data to write, the server will attempt to
write the rest of the data out as the multiplexer says the client is ready to
receive it.

Since this example is quite long the server portion will just be shown as a
difference to ex6-server.

0. The listener handler:

   The important code in this function is the call to make-ex7-io-buffer.
   This function returns a closure, here called io-buffer, which takes one
   argument, either \:read-a-line or \:write-a-line. When the funcall of
   io-buffer with the appropriate argument happens, *another* closure is
   returned and this is the closure registered with the appropriate ready
   state in the multiplexer.

   This returned closure has bound in its lexical scope the storage needed for
   the client.

   Both closures returned by \:read-a-line and \:write-a-line have access to the
   same storage space unique to this object io-buffer. This is the means by
   which the client's write handler can get access to the data read by the
   client's read handler.

   .. code:: lisp

      ;; Create the listener closure which accepts the client and registers the
      ;; buffer functions with it.
      (defun make-ex7-server-listener-handler (socket)
        (lambda (fd event exception)
          ;; do a blocking accept, returning nil if no socket
          (let* ((client (accept-connection socket :wait t)))
            (when client
              (multiple-value-bind (who port)
                  (remote-name client)
                (format t "Accepted a client from ~A:~A~%" who port)

                ;; save the client connection in case we need to close it later.
                (setf (gethash `(,who ,port) *ex7-open-connections*) client)

                ;; We make an io-buffer, which takes care of reading from the
                ;; socket and echoing the information it read back onto the
                ;; socket.  The buffer takes care of this with two internal
                ;; handlers, a read handler and a write handler.
                (let ((io-buffer (make-ex7-io-buffer client who port
                                                     (make-ex7-server-disconnector client))))

                  ;; set up an line echo function for the client socket.  The
                  ;; internals of the buffer will perform the appropriate
                  ;; registration/unregistration of the required handlers at
                  ;; the right time depending upon data availability.

                  (set-io-handler *ex7-event-base*
                                  (socket-os-fd client)
                                  :read (funcall io-buffer :read-a-line))

                  (set-io-handler *ex7-event-base*
                                  (socket-os-fd client)
                                  :write (funcall io-buffer :write-a-line))))))))


1. The disconnector function:

   This function is almost identical to a previous example used in
   ex5a-client.  The only difference is the special variable it references.

   Since the io-buffer knows under what conditions it should register or
   unregister specific handlers for the client socket, we need to be able to
   selectively remove them without disturbing the others.

   .. code:: lisp

      (defun make-ex7-server-disconnector (socket)
        ;; When this function is called, it can be told which callback to remove, if
        ;; no callbacks are specified, all of them are removed! The socket can be
        ;; additionally told to be closed.
        (lambda (who port &rest events)
          (let ((fd (socket-os-fd socket)))
            (if (not (intersection '(:read :write :error) events))
                (remove-fd-handlers *ex7-event-base* fd :read t :write t :error t)
                (progn
                  (when (member :read events)
                    (remove-fd-handlers *ex7-event-base* fd :read t))
                  (when (member :write events)
                    (remove-fd-handlers *ex7-event-base* fd :write t))
                  (when (member :error events)
                    (remove-fd-handlers *ex7-event-base* fd :error t)))))
          ;; and finally if were asked to close the socket, we do so here
          (when (member :close events)
            (format t "Closing connection to ~A:~A~%" who port)
            (finish-output)
            (close socket)
            (remhash `(,who ,port) *ex7-open-connections*))))


Now we come to the description of the ex7-io-buffer code base. This code base
interacts directly with the event-base multiplexer instance in order to
register and unregister handlers to the client. Handlers are only registered
when there is data to write, or room to read more data up to the buffer size.

0. The io-buffer closure generator and associated lexical storage:

   These are the variables closed over which represent the internal state of
   the closure and hold the data from the client. In particular note is the
   fact we keep track of when a handler is registered (since this object can
   register or unregister the handlers in and of itself) and whether or not
   we've seen the END-OF-FILE from a client. The line-queue will hold the
   actual data from the client.

   .. code:: lisp

      (defun make-ex7-io-buffer (socket who port disconnector &key (max-bytes 4096))
        (let ((line-queue (make-queue))
              (bytes-left-to-write 0)
              (read-handler-registered nil)
              (write-handler-registered nil)
              (eof-seen nil))


1. The read-a-line closure:

   This is the function which will ultimately be registered with the
   multiplexer hence the arguments it expects. Its job is to read a line from
   the client when the multiplexer said the client was readable and then store
   the line into the line-queue. If we have read a line, we immediately
   register the write-a-line handler with the multiplexer since we need to
   know when the client will be ready to accept it. If it turns out there is
   more data stored than the high-water mark we set, we unregister the read
   handler so we don't continue to keep reading data. If we get END-OF-FILE,
   but there is nothing left to write, then this handler performs a small
   optimization and closes the socket to the client and unregisters
   everything. This prevents a needless loop through the multiplexer in this
   case.

   The handling of END-OF-FILE is interesting in that we unregister the read
   handler, since we won't need it anymore, and mark that we've seen the
   END-OF-FILE. At this point, the only thing the multiplexer has to do with
   respect to this client is to write all of the lines stored in the
   line-queue out to the client and close the connection to the client.

   Of the various conditions that can be signaled, the
   SOCKET-CONNECTION-RESET-ERROR condition is the one which will shut down the
   whole connection by removing all handlers in the multiplexer for this
   client and ultimately throw away any in-flight data.

   .. code:: lisp

      (labels
        ;; If this function notices that there is data to write, it will
        ;; set the io-handler on the socket for the write handler.
        ;; If the function notices it has read >= than the max-bytes
        ;; it will remove itself from the handler *after* ensuring the
        ;; write handler is set up properly.
        ((read-a-line (fd event exception)
           (handler-case
               (let ((line (format nil "~A~%" (read-line socket)))) ; add a \n
                 (format t "Read from ~A:~A: ~A" who port line)
                 (enqueue line line-queue)
                 (incf bytes-left-to-write (length line))

                 (when (> bytes-left-to-write 0)
                   ;; If the write handler isn't registered, then do
                   ;; it now since I have data to write.
                   (unless write-handler-registered
                     (set-io-handler *ex7-event-base*
                                     (socket-os-fd socket)
                                     :write
                                     #'write-a-line)
                     (setf write-handler-registered t)))

                 ;; Now, if there is more data than I should be
                 ;; reading, remove myself from the io handler. When
                 ;; the write handler notices that, after writing some
                 ;; data, more of it can be read, it will reregister
                 ;; the io handler for the read socket.
                 (when (>= bytes-left-to-write max-bytes)
                   (funcall disconnector who port :read)
                   (setf read-handler-registered nil)))

             (socket-connection-reset-error ()
               ;; If the client resets its connection, we close
               ;; everything down.
               (format t "Client ~A:~A: Connection reset by peer~%" who port)
               (funcall disconnector who port :close))

             (end-of-file ()
               ;; When we get an end of file, that doesn't necessarily
               ;; mean the client went away, it could just mean that
               ;; the client performed a shutdown on the write end of
               ;; its socket and it is expecting the data stored in
               ;; the server to be written to it.  However, if there
               ;; is nothing left to write and our read end is close,
               ;; we shall consider it that the client went away and
               ;; close the connection.
               (format t "Client ~A:~A produced end-of-file on a read.~%"
                       who port)
               (if (zerop bytes-left-to-write)
                   (funcall disconnector who port :close)
                   (progn
                     (funcall disconnector who port :read)
                     (setf read-handler-registered nil)
                     (setf eof-seen t))))))


2. The write-a-line closure:

   This function is somewhat symmetrical to read-a-line. It will register and
   unregister itself or the read handler based upon how much data is available
   to read/write. If the END-OF-FILE is seen and there is nothing left to
   write, it will close the connection to the client and unregister
   everything.

   .. code:: lisp

      ;; This function will notice that if it has written enough bytes to
      ;; bring the bytes-left-to-write under max-bytes, it will re-register
      ;; the reader io handler. If there is no data to write, it will,
      ;; after ensuring the read handler is registered, unregister itself
      ;; as to not be called constantly on a write ready socket with no
      ;; data to write.
      (write-a-line (fd event exception)
        (handler-case
            (progn
              ;; If we have something to write to the client, do so.
              (when (> bytes-left-to-write 0)
                (let ((line (dequeue line-queue)))
                      (format socket "~A" line) ;; newline is in the string.
                      (finish-output socket)
                      (format t "Wrote to ~A:~A: ~A" who port line)
                      (decf bytes-left-to-write (length line))))

                  ;; If we see we've fallen below the max-bytes mark,
                  ;; re-register the read handler to get more data for
                  ;; us. However, don't reregister the read handler if
                  ;; we've seen that the client closed our read end of
                  ;; our socket.
                  (when (< bytes-left-to-write max-bytes)
                    (unless (or eof-seen read-handler-registered)
                      (set-io-handler *ex7-event-base*
                                      (socket-os-fd socket)
                                      :read
                                      #'read-a-line)
                      (setf read-handler-registered t)))

                  ;; If we notice that we don't have any data to write
                  ;; AND have seen the end of file from the client,
                  ;; then we close the connection to the client since
                  ;; it will never speak to us again and we're done
                  ;; speaking to it.
                  ;;
                  ;; If notice we've written all of our data and there
                  ;; might be more to do later, then unregister the
                  ;; write handler so we don't get called
                  ;; unnecesarily. This might mean that sometimes we'll
                  ;; have to make an extra trip through the
                  ;; event-dispatcher to perform the write if we read
                  ;; more from the client and it reregisters us.
                  (when (zerop bytes-left-to-write)
                    (if eof-seen
                        (funcall disconnector who port :close)
                        (progn
                          (funcall disconnector who port :write)
                          (setf write-handler-registered nil)))))

              (socket-connection-reset-error ()
                ;; If I happen to get a reset, make sure the connection
                ;; is closed.  I shouldn't get this here, but if you
                ;; tinker with the flow of this example, it is a good
                ;; guard to have.
                (format t "Client ~A:~A: connection reset by peer.~%" who port)
                (funcall disconnector who port :close))

              (hangup ()
                ;; In this server, if the client doesn't accept data,
                ;; it also means it will never send us data again. So
                ;; close the connection for good.
                (format t "Client ~A:~A got hangup on write.~%" who port)
                (funcall disconnector who port :close)))))


3. The returned closure, which represents the io-buffer:

   This is the actual closure returned by make-ex7-io-buffer and which is used
   to gain access into the read-a-line and write-a-line functions. It takes a
   single argument, either the keywords \:read-a-line or \:write-a-line, and
   returns a reference to either internal function.

   .. code:: lisp

      ;; This is the actual function returned from make-ex7-io-buffer
      ;; which allows us access to the read/writer in the scope of the
      ;; closure.  We will ask for the correct functions when setting
      ;; up the io handlers.  NOTE: By simply asking for the handler,
      ;; I've assumed it is to be immediately put into an iolib event
      ;; handler. This is why they are considered registered at this point.
      (lambda (msg)
        (cond
          ((equalp msg :read-a-line)
           (setf read-handler-registered t)
           #'read-a-line)
          ((equalp msg :write-a-line)
           (setf write-handler-registered t)
           #'write-a-line)
          (t
           (error "make-ex7-buffer: Please supply :read-a-line or :write-a-line~%")))))))


While this server still uses blocking I/O, we've laid the foundations for
nonblocking I/O and memory storage enforcement. The foundations specifically
are separating the read/write handlers into different pieces and having shared
lexical bindings between them.


Echo Server IPV4/TCP: ex8-server.lisp
-------------------------------------------------------------------------------

This server uses nonblocking I/O and the multiplexer to concurrently talk to
the clients.

Architecturally, it is very similar to ex7-server, but the io-buffer for this
server is implemented with much different internals. Whereas in ex7-server
reading from a client used the stream function READ-LINE, writing used the
stream function FORMAT, and the strings from the client were kept in a queue,
now we use RECEIVE-FROM and SEND-TO along with an array of unsigned-bytes as a
buffer to read/write actual bytes from the socket.

Accessing the socket through the stream API is different than doing it through
the almost raw socket API which we are about to use.  RECEIVE-FROM and SEND-TO
are not part of the stream interface. They are a lower level API in IOLib being
closer to the underlying OS abstraction and as a consequence have a somewhat
different set of conditions that they can signal.  These different conditions
have the form isys\: like\: isys\:epipe, isys\:ewouldblock, etc.
There is some intersection with the condition names signaled by the stream API,
such as: SOCKET-CONNECTION-RESET-ERROR, and SOCKET-CONNECTION-REFUSED.

[TODO figure out complete list!]

An example of the ramifications of this API is RECEIVE-FROM. Comparing against
the stream interface whose READ-LINE will signal an END-OF-FILE when the
reading socket has been closed by the client, the function RECEIVE-FROM will
return 0, signifying the end of file. The stream function FORMAT will signal
HANGUP if it tries to write to a socket where the client has gone away. SEND-TO
might not signal, or otherwise produce, any error at all when writing to a
socket where the client has gone away--usually it is on the next RECEIVE-FROM
that it is discovered the client went away. The bytes that SEND-TO wrote simply
vanish!

With IOLib, it may surprise you to be told that all underlying fds in the
previous examples have been nonblocking! This is why we specified \:wait t for
ACCEPT-CONNECTION and CONNECT.

The IOLib library internally ensures that the stream interface blocks according
to the requirements of ANSI Common Lisp. However, when we use SEND-TO and
RECEIVE-FROM we automatically gain the benefit of the non-blocking status on
the underlying fd. This is why in this example we don't explicitly set the
underlying fd to non-blocking status--it already is!

The server code itself is described as a difference from ex7-server, but the
io-buffer for this nonblocking server (in file ex8-buffer.lisp) will be
described in its entirety. Also, this server honors the batch input requirement
from example client ex-5b-client, which you should use against this server.

The ex8-server codes:

0. The listener handler (first half):

   Accept and store the client connection.

   .. code:: lisp

      (defun make-ex8-server-listener-handler (socket)
        (lambda (fd event exception)
          ;; do a blocking accept, returning nil if no socket
          (let* ((client (accept-connection socket :wait t)))
            (when client
              (multiple-value-bind (who port)
                  (remote-name client)
                (format t "Accepted a client from ~A:~A~%" who port)

                ;; save the client connection in case we need to close it later.
                (setf (gethash `(,who ,port) *ex8-open-connections*) client)


1. The listener handler (second half):

   Like ex7-server, we register the read and write handlers. Notice though
   that we changed the keywords to the io-buffer closure to be
   \:read-some-bytes and \:write-some-bytes. This better represents what the
   io-buffer is actually doing.

   .. code:: lisp

          ;; We make an io-buffer, which takes care of reading from the
          ;; socket and echoing the information it read back onto the
          ;; socket.  The buffer takes care of this with two internal
          ;; handlers, a read handler and a write handler.
          (let ((io-buffer
                 (make-ex8-io-buffer client who port
                                     (make-ex8-server-disconnector client))))

            ;; set up an unsigned byte echo function for the
            ;; client socket.  The internals of the buffer will
            ;; perform the appropriate registration/unregistration of
            ;; the required handlers at the right time depending upon
            ;; data availability.

            (set-io-handler *ex8-event-base*
                            (socket-os-fd client)
                            :read
                            (funcall io-buffer :read-some-bytes))

            (set-io-handler *ex8-event-base*

                            (socket-os-fd client)
                            :write
                            (funcall io-buffer :write-some-bytes))))))))


The rest of the server is extremely similar to ex7-server.

Now, we'll show the io-buffer specific to ex8-server.

0. The internal state of the io-buffer closure:

   The binding echo-buf is an unsigned-byte array of size max-bytes.  This is
   where data from the client is stored before it is written back to the
   client.

   The binding read-index keeps track of the beginning of the empty space in
   the echo-buf buffer where more data could be stored during a read.

    The binding write-index keeps track of how much data has been written to
    the client. It moves towards read-index, and when it has the same value as
    read-index it means that there is no data left to write to the client.

    The bindings read-handler-registered and write-handler-registered allow the
    io-buffer to know when it has registered a handler for reading and writing
    data.

    The binding eof-seen marks when the client has closed its write connection
    to the server. The server will push out all data to the client, then close
    socket to the client.

   .. code:: lisp

      (defun make-ex8-io-buffer (socket who port disconnector &key (max-bytes 16384))
        (let ((echo-buf (make-array max-bytes :element-type 'unsigned-byte))
              (read-index 0)
              (write-index 0)
              (read-handler-registered nil)
              (write-handler-registered nil)
              (eof-seen nil))


1. Reading bytes form the client:

   In this function, we will convert the return value 0 of RECEIVE-FROM on the
   read of a closed socket into a signaled END-OF-FILE condition to keep the
   structure of our code similar to what has transpired before. Once we read
   some bytes, we increment the read-index pointer and ensure to register a
   write handler to write the data back out. We optimize the writing process a
   little bit and try to write the data out immediately without checking to
   see if the socket is ready. Then if there is no more room in the echo-buf
   array, we unregister ourselves so we don't try and read more data from the
   client until we are ready to accept it (by having written all of the data
   back to the client). We mark the END-OF-FILE flag and unregister the read
   handler if we see the client has closed its connection. We optimize the
   knowledge that if we have no more data to write we just close the
   connection to the client.

   .. code:: lisp

      (labels
        ;; This is the function responsible for reading bytes from the client.
        ((read-some-bytes (fd event exception)
           (handler-case
               (progn
                 ;; Read however much we are able.
                 (multiple-value-bind (buf bytes-read)
                     (receive-from socket
                                   :buffer echo-buf
                                   :start read-index
                                   :end max-bytes)

                   ;; Unlike read-ing from a stream, receive-from
                   ;; returns zero on an end-of-file read, so we turn
                   ;; around and signal that condition so our
                   ;; handler-case can deal with it properly like our
                   ;; other examples.
                   (when (zerop bytes-read)
                     (error 'end-of-file))

                   (format t "Read ~A bytes from ~A:~A~%" bytes-read who port)
                   (incf read-index bytes-read))

                 ;; Register the write handler if there is data to
                 ;; write.
                 ;;
                 ;; Then, try to write some data to the socket right
                 ;; away even though it might not be ready simply to
                 ;; avoid another go around. The write-some-bytes
                 ;; function must be able to catch econnreset because
                 ;; this connection may be closed at the time of this
                 ;; call. Normally, if the multiplexer has told me I
                 ;; could write then it'd be ok, but since this write
                 ;; is outside of the multiplexer and an optimization,
                 ;; it needs to check.
                 (when (/= write-index read-index)
                   (unless write-handler-registered
                     (set-io-handler *ex8-event-base*
                                     (socket-os-fd socket)
                                     :write
                                     #'write-some-bytes)
                     (setf write-handler-registered t))

                   ;; See if I can write it right away!
                   (write-some-bytes fd :write nil))

                 ;; If I'm out of room to store more data then remove
                 ;; myself from the io handler. When the write handler
                 ;; notices that it has finished writing everything,
                 ;; all indicies get set back to zero and the write
                 ;; handler removes itself.  If write-some-bytes in
                 ;; the call above worked, then read-index might not
                 ;; equal max-bytes when this line of code gets
                 ;; executed.
                 (when (= read-index max-bytes)
                   (funcall disconnector who port :read)
                   (setf read-handler-registered nil)))

             (socket-connection-reset-error ()
               ;; Handle the client sending a reset.
               (format t "Client ~A:~A: connection reset by peer.~%" who port)
               (funcall disconnector who port :close))

             (end-of-file ()
               ;; When we get an end of file, that doesn't necessarily
               ;; mean the client went away, it could just mean that
               ;; the client performed a shutdown on the write end of
               ;; its socket and it is expecting the data stored in
               ;; the server to be written to it.  However, if there
               ;; is nothing left to write and our read end is closed,
               ;; we shall consider it that the client went away and
               ;; close the connection.
               (format t "Client ~A:~A produced end-of-file on a read.~%"
                       who port)
               (if (= read-index write-index)
                   (funcall disconnector who port :close)
                   (progn
                     (funcall disconnector who port :read)
                     (setf read-handler-registered nil)
                     (setf eof-seen t))))))


2. Writing bytes to the client:

   While there are more bytes to write, we write them, keeping track of how
   much we wrote. Once we are out of data to write, we unregister the write
   handler, since we don't want to be called unnecessarily--usually the client
   socket is always ready to write. If we've seen the eof marker and are out
   of data, we close the client connection and are done. If we haven't seen
   it, then we determine if we are at the end of the buffer, if so, we reset
   the indices to the beginning.  Either way, we re-register the read handler
   to acquire more data.

   We handle some new conditions here: isys:ewouldblock is needed because
   sometimes the underlying OS will mark an fd as ready to write when in fact
   it isn't when we get around to writing it. We might also see this condition
   when we tried to optimize the write of the data in the read handler since
   we did it outside of the multiplexer--this is idiomatic and saves a trip
   through the multiplexer more often than not. Seeing isys:ewouldblock simply
   aborts the write and we'll try again later. Under some conditions, send-to
   will signal an isys:epipe error, which means the client closed its
   connection. It is similar to a HANGUP condition in a format call with the
   stream API. We treat it similarly to a HANGUP.


   .. code:: lisp

         ;; This is the function responsible for writing bytes to the client.
         (write-some-bytes (fd event exception)
           (handler-case
               (progn
                 ;; If there is data to be written, write it.  NOTE:
                 ;; There is often no indication of failure to write
                 ;; with send-to. If I'm writing to a closed (by the
                 ;; client) socket, it could be that send-to tells me
                 ;; nothing is wrong and returns the number of bytes
                 ;; wrotten. In this case, nothing was written but we
                 ;; have no way of knowing. Usually in this case, the
                 ;; read handler will get a 0 bytes read on the socket
                 ;; and we can know the connection is broken.
                 (when (> read-index write-index)
                   (let ((wrote-bytes (send-to socket echo-buf
                                               :start write-index
                                               :end read-index)))
                     (format t "Wrote ~A bytes to ~A:~A~%" wrote-bytes who port)
                     (incf write-index wrote-bytes)))

                 ;; If we see we're out of data to write and we saw an eof,
                 ;; then close the connection, we're done. If we didn't see an
                 ;; eof, then unregister the write handler and reregister the
                 ;; read handler to get more data. If the buffer indices
                 ;; are at the very end, reset them to the beginning.
                 (when (= read-index write-index)
                   (if eof-seen
                       (funcall disconnector who port :close)
                       (progn

                         ;; nothing more to write, so unregister writer
                         (funcall disconnector who port :write)
                         (setf write-handler-registered nil)

                         ;; If we're at the end of the buffer, move to the
                         ;; beginning so there is more room for data.
                         (when (= read-index write-index max-bytes)
                           (setf read-index 0
                                 write-index 0))

                         ;; Reregister the read handler to get more data
                         (unless read-handler-registered
                           (set-io-handler *ex8-event-base*
                                           (socket-os-fd socket)
                                           :read
                                           #'read-some-bytes)
                           (setf read-handler-registered t))))))

             (socket-connection-reset-error ()
               ;; If for somer eaon the client reset the network connection,
               ;; we'll get this signal.
               (format t "Client ~A:~A: connection reset by peer.~%" who port)
               (funcall disconnector who port :close))

             (isys:ewouldblock ()
               ;; Sometimes this happens on a write even though it
               ;; might have been marked as ready. Also we might have
               ;; asked to write on an unknown status socket. Ignore
               ;; it and we will try again later.
               (format t "write-some-bytes: ewouldblock~%")
               nil)

             (isys:epipe ()
               ;; In this server, if the client doesn't accept data,
               ;; it also means it will never send us data again. So
               ;; close the connection for good.
               (format t "Client ~A:~A got hangup on write.~%" who port)
               (funcall disconnector who port :close)))))


3. The returned closure of the io-buffer:

   Much like make-ex7-io-buffer, we return one of the internal closures which
   are appropriate for reading or writing by the multiplexer.

   .. code:: lisp

      ;; This is the function returned from make-ex8-io-buffer which
      ;; allows us access to the read/writer in the scope of the
      ;; closure.  We will ask for the correct functions when setting
      ;; up the io handlers.  NOTE: By simply asking for the handler,
      ;; I've assumed it is to be immediately put into an iolib event
      ;; handler. This is why they are considered registered at this
      ;; point.
      (lambda (msg)
        (cond
          ((equalp msg :read-some-bytes)
           (setf read-handler-registered t)
           #'read-some-bytes)
          ((equalp msg :write-some-bytes)
           (setf write-handler-registered t)
           #'write-some-bytes)
          (t
           (error "make-ex8-buffer: Please supply :read-some-bytes or :write-some-bytes~%")))))))


.. comment: end of file
