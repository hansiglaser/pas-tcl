pas-tcl -- Object Oriented wrapper for TCL for Pascal
=====================================================

`TCL (Tool Command Language) <http://www.tcl.tk/>`_ is a scripting language
widely used as integrated scripting engine.

This project provides Pascal header translations plus an object-oriented
wrapper for convenience. Code examples on the usage are included.

License
-------

    Copyright (C) 2012 Johann Glaser <Johann.Glaser@gmx.at>

    This program is free software; you can redistribute it and/or modify  
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or  
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


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
