(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   Pascal include file translated from the declarations of platform      *
 *   specific Tcl APIs.                                                    *
 *                                                                         *
 *   This Pascal file is licensed under the same terms as the original     *
 *   Tcl C header file. See the file "license.terms" for information on    *
 *   usage and redistribution of this file, and for a DISCLAIMER OF ALL    *
 *   WARRANTIES.                                                           *
 *                                                                         *
 ***************************************************************************)
(*
 * tclPlatDecls.h --
 *
 *	Declarations of platform specific Tcl APIs.
 *
 * Copyright (c) 1998-1999 by Scriptics Corporation.
 * All rights reserved.
 *
 * RCS: @(#) $Id: tclPlatDecls.h,v 1.27.2.2 2010/05/21 12:18:17 nijtmans Exp $
 *)

(*
 *  Pull in the typedef of TCHAR for windows.
 *)

{$if 0} // defined(__WIN32__) && !defined(_TCHAR_DEFINED)}
type
  PTCHAR = ^TCHAR;
  TCHAR = _TCHAR;
{$define _TCHAR_DEFINED}
{$endif}

(* !BEGIN!: Do not edit below this line. *)

(*
 * Exported function declarations:
 *)

{$ifdef __WIN32__ /* WIN */}
function Tcl_WinUtfToTChar(str:PChar; len:cint; dsPtr:PTcl_DString):PTCHAR;extdecl;external;
function Tcl_WinTCharToUtf(str:PTCHAR; len:cint; dsPtr:PTcl_DString):PChar;extdecl;external;
{$endif}  { WIN }

{$ifdef MAC_OSX_TCL /* MACOSX */}
function Tcl_MacOSXOpenBundleResources(interp:PTcl_Interp; bundleName:PChar; hasResourceFile:cint; maxPathLen:cint; libraryPath:PChar):cint;extdecl;external;
function Tcl_MacOSXOpenVersionedBundleResources(interp:PTcl_Interp; bundleName:PChar; bundleVersion:PChar; hasResourceFile:cint; maxPathLen:cint;
           libraryPath:PChar):cint;extdecl;external;
{$endif}  { MACOSX }

Type
  PTclPlatStubs = ^TclPlatStubs;
  TclPlatStubs = record
      magic : cint;
      hooks : Pointer {PTclPlatStubHooks};
{$ifdef __WIN32__ /* WIN */}
      tcl_WinUtfToTChar : function (str:PChar; len:cint; dsPtr:PTcl_DString):PTCHAR;extdecl;
      tcl_WinTCharToUtf : function (str:PTCHAR; len:cint; dsPtr:PTcl_DString):PChar;
{$endif}  { WIN }
{$ifdef MAC_OSX_TCL /* MACOSX */}
      tcl_MacOSXOpenBundleResources : function (interp:PTcl_Interp; bundleName:PChar; hasResourceFile:cint; maxPathLen:cint; libraryPath:PChar):cint;
      tcl_MacOSXOpenVersionedBundleResources : function (interp:PTcl_Interp; bundleName:PChar; bundleVersion:PChar; hasResourceFile:cint; maxPathLen:cint;
                   libraryPath:PChar):cint;
{$endif}  { MACOSX }
    end;

Var
  tclPlatStubsPtr : PTclPlatStubs;cvar;external;

{$if 0} // defined(USE_TCL_STUBS) && !defined(USE_TCL_STUB_PROCS)}

(*
 * Inline function declarations:
 *)

Const
{$ifdef __WIN32__ /* WIN */}
  Tcl_WinUtfToTChar = tclPlatStubsPtr^.tcl_WinUtfToTChar;
  Tcl_WinTCharToUtf = tclPlatStubsPtr^.tcl_WinTCharToUtf;  
{$endif} { WIN }
{$ifdef MAC_OSX_TCL /* MACOSX */}
  Tcl_MacOSXOpenBundleResources = tclPlatStubsPtr^.tcl_MacOSXOpenBundleResources;
  Tcl_MacOSXOpenVersionedBundleResources = tclPlatStubsPtr^.tcl_MacOSXOpenVersionedBundleResources;  
{$endif}  { MACOSX }

{$endif}  { defined(USE_TCL_STUBS) && !defined(USE_TCL_STUB_PROCS) }

