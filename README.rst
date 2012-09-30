pas-tcl -- Object Oriented wrapper for TCL for Pascal
=====================================================

`TCL (Tool Command Language) <http://www.tcl.tk/>`_ is a scripting language
widely used as integrated scripting engine.

This project provides Pascal header translations plus an object-oriented
wrapper for convenience. Code examples on the usage are included.

License
-------

Each file contains a header showing the according license.

 - Tcl and its header translation is licensed under the Tcl license (see the
   file "license.terms").
 - All Pascal units and examples are licensed under the same terms.
 - Note that some Pascal units link to GNU Readline which is licensed under
   the GPL.

Directory Structure
-------------------

  ``src/``
    Header translations and OOP wrapper.

  ``src/examples/``
    Examples for the direct usage of the header translations as well as
    for the usage of the OOP wrapper. This directory also has a
    ``Makefile``.

Build
-----

::

  $ cd src/examples/
  $ make

Usage
-----

Simply add the units ``TCL`` and ``TclOOP`` to the uses-clause of your
program. Then follow `src/examples/demo2.pas
<pas-tcl/blob/master/src/examples/demo2.pas>`_.

Alternatively you can use the class ``TTclCmdLine`` in `src/tclcmdline.pas
<pas-tcl/blob/master/src/tclcmdline.pas>`_. This includes a command line
interface using the `GNU Readline library with a Pascal wrapper
<https://github.com/hansiglaser/pas-readline>`_.

The class ``TTclCmdLinePredef`` in `src/tclcmdlinepredef.pas
<pas-tcl/blob/master/src/tclcmdlinepredef.pas>`_ is derived from
``TTclCmdLine`` and implements some useful TCL commands like ``man`` and
``history``. It also re-implements ``exit`` to provide a graceful exit of
the command loop and finalization of the application.

In `src/tclapp.pas <pas-tcl/blob/master/src/tclapp.pas>`_, the class
``TTclApp`` uses ``TTclCmdLinePredef`` to implement a basic command line
application. Derive from this class and implement your own (additional) TCL
commands to build a powerful TCL command line application.

Platform
--------

This project was compiled with `FreePascal <http://www.freepascal.org/>`_
2.6.0 on Linux.

Other Projects
--------------

**k7103-usb**
  The USB Interface of the Velleman k7103 PC Storage Oscilloscope
  http://k7103.sourceforge.net/ includes a command line tool to test the
  hardware and to program the CPLD. This is a bigger usage example which
  also includes a the `GNU Readline library with a Pascal wrapper
  <https://github.com/hansiglaser/pas-readline>`_. See k7103usbtest.pas_.

  .. _k7103usbtest.pas: http://k7103.svn.sourceforge.net/viewvc/k7103/branch/usb/host-test/k7103usbtest.pas?view=markup
