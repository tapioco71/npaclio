.. comment: -*- mode:rst; coding:utf-8; electric-indent-mode:nil; tab-always-indent:t -*-
    ###
    +++
    ===
    ---
    """
    ^^^
    +++


.. comment: Diagrams are made with: http://yuml.me/diagram/nofunky/class/draw
            This is a reStructuredText file. http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html
            Process it with rst2pdf and rst2html or pandoc.


.. meta::
   :description: Network Programming in ANSI Common Lisp with IOLib
   :keywords: Documentation, Network Programming, ANSI Common Lisp, IOLib, UNIX, POSIX, sockets
.. header:: ###Section###
.. footer::
   +-------------------------------------+-------------+
   | https://pages.cs.wisc.edu/~psilord/ |  ###Page### |
   +-------------------------------------+-------------+


################################################################################
Network Programming in ANSI Common Lisp with IOLib
################################################################################

.. raw:: pdf

    PageBreak


.. contents:: Table of Contents
   :depth: 3

.. sectnum::
   :depth: 4
   :start: 1

.. raw:: pdf

    PageBreak


.. list-table:: Revisions
  :widths: 15 10 10 45
  :header-rows: 1

  * - Date
    - Version
    - Revision
    - Name
  * - 04/02/2010
    - 0
    - 0
    - Peter Keller (psilord@cs.wisc.edu)
  * - 02/11/2022
    - 0
    - 1
    - Angelo Rossi (angelo.rossi.homelab@gmail.com)

.. raw:: pdf

    PageBreak


What is IOLib?
 IOLib is a portable I/O library for ANSI Common Lisp. It includes socket
 interfaces for network programming with IPV4/IPV6 TCP and UDP, an I/O
 multiplexer that includes nonblocking I/O, a DNS resolver library, and a
 pathname library.

Where do I get IOLib?
 The current version of IOLib is found here:

 <http://common-lisp.net/project/iolib/download.shtml>

 Please use the repository located in the Live Sources section for the most up
 to date version of IOLib.

.. raw:: pdf

    PageBreak


.. include:: c-introduction.rst

.. raw:: pdf

    PageBreak


IPV4 TCP Client/Server Blocking and nonblocking I/O
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. comment:

   ;; to insert a new chapter, add its name (without o- or extension) to the list in dolist,
   ;; and type C-x C-e after the sexp.

   (let* ((start (point))
         (end (if (search-forward ".. comment: end-chapters" nil t)
                 (progn (goto-char (match-end 0))
                        (end-of-line)
                        (point))
                 start)))
    (delete-region start end)
    (dolist (chapter '(overview-examples
                       daytime-clients
                       daytime-servers
                       echo-line-clients-servers
                       echo-line-clients
                       echo-line-servers
                       future-directions
                       appendix-a)
              (insert "\n.. comment: end-chapters   "))
      (insert (format  "\n.. raw:: pdf\n\n    PageBreak\n\n.. include:: c-%s.rst\n" chapter))))
.. raw:: pdf

    PageBreak

.. include:: c-overview-examples.rst

.. raw:: pdf

    PageBreak

.. include:: c-daytime-clients.rst

.. raw:: pdf

    PageBreak

.. include:: c-daytime-servers.rst

.. raw:: pdf

    PageBreak

.. include:: c-echo-line-clients-servers.rst

.. raw:: pdf

    PageBreak

.. include:: c-echo-line-clients.rst

.. raw:: pdf

    PageBreak

.. include:: c-echo-line-servers.rst

.. raw:: pdf

    PageBreak

.. include:: c-future-directions.rst

.. raw:: pdf

    PageBreak

.. include:: c-appendix-a.rst

.. comment: end-chapters

.. raw:: pdf

    PageBreak


Bibliography
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.. [STEVENS1997] Stevens, W. Richard (1997)

                 UNIX Network Programming, Networking APIs: Sockets and XTI

                 2nd Ed. Prentice Hall.

                 ISBN‎ 978-0134900124.


.. comment: the end
