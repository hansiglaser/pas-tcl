(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   Pascal unit translated from the externally-visible facilities of the  *
 *   Tcl interpreter.                                                      *
 *                                                                         *
 *   This Pascal unit is licensed under the same terms as the original     *
 *   Tcl C header file. See the file "license.terms" for information on    *
 *   usage and redistribution of this file, and for a DISCLAIMER OF ALL    *
 *   WARRANTIES.                                                           *
 *                                                                         *
 ***************************************************************************)
{$MODE ObjFPC}
Unit TCL;

Interface

Uses ctypes;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

{$macro on}
{$ifdef windows}
  {$define extdecl:=stdcall}
{$else}
  {$define extdecl:=cdecl}
{$endif}

{$LINKLIB tcl8.5}

(*
 * tcl.h --
 *
 *	This header file describes the externally-visible facilities of the
 *	Tcl interpreter.
 *
 * Copyright (c) 1987-1994 The Regents of the University of California.
 * Copyright (c) 1993-1996 Lucent Technologies.
 * Copyright (c) 1994-1998 Sun Microsystems, Inc.
 * Copyright (c) 1998-2000 by Scriptics Corporation.
 * Copyright (c) 2002 by Kevin B. Kenny.  All rights reserved.
 *
 * See the file "license.terms" for information on usage and redistribution of
 * this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * RCS: @(#) $Id: tcl.h,v 1.254.2.16 2010/08/04 17:02:39 dgp Exp $
 *)

(*
 * The following defines are used to indicate the various release levels.
 *)

Const
  TCL_ALPHA_RELEASE	= 0;
  TCL_BETA_RELEASE	= 1;
  TCL_FINAL_RELEASE	= 2;

(*
 * When version numbers change here, must also go into the following files and
 * update the version numbers:
 *
 * library/init.tcl	(1 LOC patch)
 * unix/configure.in	(2 LOC Major, 2 LOC minor, 1 LOC patch)
 * win/configure.in	(as above)
 * win/tcl.m4		(not patchlevel)
 * win/makefile.bc	(not patchlevel) 2 LOC
 * README		(sections 0 and 2, with and without separator)
 * macosx/Tcl.pbproj/project.pbxproj (not patchlevel) 1 LOC
 * macosx/Tcl.pbproj/default.pbxuser (not patchlevel) 1 LOC
 * macosx/Tcl.xcode/project.pbxproj (not patchlevel) 2 LOC
 * macosx/Tcl.xcode/default.pbxuser (not patchlevel) 1 LOC
 * macosx/Tcl-Common.xcconfig (not patchlevel) 1 LOC
 * win/README		(not patchlevel) (sections 0 and 2)
 * unix/tcl.spec	(1 LOC patch)
 * tools/tcl.hpj.in	(not patchlevel, for windows installer)
 * tools/tcl.wse.in	(for windows installer)
 * tools/tclSplash.bmp	(not patchlevel)
 *)

  TCL_MAJOR_VERSION   = 8;
  TCL_MINOR_VERSION   = 5;
  TCL_RELEASE_LEVEL   = TCL_FINAL_RELEASE;
  TCL_RELEASE_SERIAL  = 9;

  TCL_VERSION     = '8.5';
  TCL_PATCH_LEVEL = '8.5.9';

(*
 * Utility macros: STRINGIFY takes an argument and wraps it in "" (double
 * quotation marks), JOIN joins two arguments.
 *

#ifndef STRINGIFY
#  define STRINGIFY(x) STRINGIFY1(x)
#  define STRINGIFY1(x) #x
#endif
#ifndef JOIN
#  define JOIN(a,b) JOIN1(a,b)
#  define JOIN1(a,b) a##b
#endif
*)

(*
 * A special definition used to allow this header file to be included from
 * windows resource files so that they can obtain version information.
 * RC_INVOKED is defined by default by the windows RC tool.
 *
 * Resource compilers don't like all the C stuff, like typedefs and function
 * declarations, that occur below, so block them out.
 *)

(*
 * Special macro to define mutexes, that doesn't do anything if we are not
 * using threads.
 *

#ifdef TCL_THREADS
#define TCL_DECLARE_MUTEX(name) static Tcl_Mutex name;
#else
#define TCL_DECLARE_MUTEX(name)
#endif
*)
(*
 * Tcl's public routine Tcl_FSSeek() uses the values SEEK_SET, SEEK_CUR, and
 * SEEK_END, all #define'd by stdio.h .
 *
 * Also, many extensions need stdio.h, and they've grown accustomed to tcl.h
 * providing it for them rather than #include-ing it themselves as they
 * should, so also for their sake, we keep the #include to be consistent with
 * prior Tcl releases.
 *)

{ $ i nclude <stdio.h>}

(*
 * Support for functions with a variable number of arguments.
 *
 * The following TCL_VARARGS* macros are to support old extensions
 * written for older versions of Tcl where the macros permitted
 * support for the varargs.h system as well as stdarg.h .
 *
 * New code should just directly be written to use stdarg.h conventions.
 *

#include <stdarg.h>
#ifndef TCL_NO_DEPRECATED
#    define TCL_VARARGS(type, name) (type name, ...)
#    define TCL_VARARGS_DEF(type, name) (type name, ...)
#    define TCL_VARARGS_START(type, name, list) (va_start(list, name), name)
#endif
*)
(*
 * Macros used to declare a function to be exported by a DLL. Used by Windows,
 * maps to no-op declarations on non-Windows systems. The default build on
 * windows is for a DLL, which causes the DLLIMPORT and DLLEXPORT macros to be
 * nonempty. To build a static library, the macro STATIC_BUILD should be
 * defined.
 *
 * Note: when building static but linking dynamically to MSVCRT we must still
 *       correctly decorate the C library imported function.  Use CRTIMPORT
 *       for this purpose.  _DLL is defined by the compiler when linking to
 *       MSVCRT.
 *

#if (defined(__WIN32__) && (defined(_MSC_VER) || (__BORLANDC__ >= 0x0550) || defined(__LCC__) || defined(__WATCOMC__) || (defined(__GNUC__) && defined(__declspec)))
#   define HAVE_DECLSPEC 1
#   ifdef STATIC_BUILD
#       define DLLIMPORT
#       define DLLEXPORT
#       ifdef _DLL
#           define CRTIMPORT __declspec(dllimport)
#       else
#           define CRTIMPORT
#       endif
#   else
#       define DLLIMPORT __declspec(dllimport)
#       define DLLEXPORT __declspec(dllexport)
#       define CRTIMPORT __declspec(dllimport)
#   endif
#else
#   define DLLIMPORT
#   if defined(__GNUC__) && __GNUC__ > 3
#       define DLLEXPORT __attribute__ (visibility("default"))
#   else
#       define DLLEXPORT
#   endif
#   define CRTIMPORT
#endif
*)
(*
 * These macros are used to control whether functions are being declared for
 * import or export. If a function is being declared while it is being built
 * to be included in a shared library, then it should have the DLLEXPORT
 * storage class. If is being declared for use by a module that is going to
 * link against the shared library, then it should have the DLLIMPORT storage
 * class. If the symbol is beind declared for a static build or for use from a
 * stub library, then the storage class should be empty.
 *
 * The convention is that a macro called BUILD_xxxx, where xxxx is the name of
 * a library we are building, is set on the compile line for sources that are
 * to be placed in the library. When this macro is set, the storage class will
 * be set to DLLEXPORT. At the end of the header file, the storage class will
 * be reset to DLLIMPORT.
 *

#undef TCL_STORAGE_CLASS
#ifdef BUILD_tcl
#   define TCL_STORAGE_CLASS DLLEXPORT
#else
#   ifdef USE_TCL_STUBS
#      define TCL_STORAGE_CLASS
#   else
#      define TCL_STORAGE_CLASS DLLIMPORT
#   endif
#endif
*)
(*
 * Definitions that allow this header file to be used either with or without
 * ANSI C features like function prototypes.
 *

#undef 
#undef 
#ifndef INLINE
#   define INLINE
#endif

#ifndef NO_
#   define
#else
#   define 
#endif

#ifndef NO_PROTOTYPES
#   define (x)	x
#else
#   define (x)	()
#endif

#ifdef USE_NON_
#   ifdef USE_COMPAT_
#      error define at most one of USE_NON_ and USE_COMPAT_
#   endif
#   define 84
#   define 84_RETURN
#else
#   ifdef USE_COMPAT_
#      define 84
#      define 84_RETURN 
#   else
#      define 84 
#      define 84_RETURN 
#   endif
#endif
*)
(*
 * Make sure EXTERN isn't defined elsewhere.
 *

#ifdef EXTERN
#   undef EXTERN
#endif * EXTERN *

#ifdef __cplusplus
#   define EXTERN extern "C" TCL_STORAGE_CLASS
#else
#   define EXTERN extern TCL_STORAGE_CLASS
#endif*)

(*
 * The following code is copied from winnt.h. If we don't replicate it here,
 * then <windows.h> can't be included after tcl.h, since tcl.h also defines
 * void. This block is skipped under Cygwin and Mingw.
 *

#if defined(__WIN32__) && !defined(HAVE_WINNT_IGNORE_void)
#ifndef void
#define void void
typedef char CHAR;
typedef short SHORT;
typedef long LONG;
#endif
#endif * __WIN32__ && !HAVE_WINNT_IGNORE_void *)

(*
 * Macro to use instead of "void" for arguments that must have type "void *"
 * in ANSI C; maps them to type "char *" in non-ANSI systems.
 *

#ifndef NO_void
#define void	void
#else
#define void	char
#endif
*)
(*
 * Miscellaneous declarations.
 *)

//#ifndef _CLIENTDATA
//#   ifndef NO_void
Type
  ClientData = Pointer;
//#   else
//	typedef int *ClientData;
//#   endif
//#   define _CLIENTDATA
//#endif

(*
 * Darwin specific configure overrides (to support fat compiles, where
 * configure runs only once for multiple architectures):
 *

#ifdef __APPLE__
#   ifdef __LP64__
#	undef TCL_WIDE_INT_TYPE
#	define TCL_WIDE_INT_IS_LONG 1
#	define TCL_CFG_DO64BIT 1
#    else * !__LP64__ *
#	define TCL_WIDE_INT_TYPE long long
#	undef TCL_WIDE_INT_IS_LONG
#	undef TCL_CFG_DO64BIT
#    endif * __LP64__ *
#    undef HAVE_STRUCT_STAT64
#endif * __APPLE__ *)

(*
 * Define Tcl_WideInt to be a type that is (at least) 64-bits wide, and define
 * Tcl_WideUInt to be the unsigned variant of that type (assuming that where
 * we have one, we can have the other.)
 *
 * Also defines the following macros:
 * TCL_WIDE_INT_IS_LONG - if wide ints are really longs (i.e. we're on a real
 *	64-bit system.)
 * Tcl_WideAsLong - forgetful converter from wideInt to long.
 * Tcl_LongAsWide - sign-extending converter from long to wideInt.
 * Tcl_WideAsDouble - converter from wideInt to double.
 * Tcl_DoubleAsWide - converter from double to wideInt.
 *
 * The following invariant should hold for any long value 'longVal':
 *	longVal == Tcl_WideAsLong(Tcl_LongAsWide(longVal)
 *
 * Note on converting between Tcl_WideInt and strings. This implementation (in
 * tclObj.c) depends on the function
 * sprintf(...,"%" TCL_LL_MODIFIER "d",...).
 *

#if !defined(TCL_WIDE_INT_TYPE)&&!defined(TCL_WIDE_INT_IS_LONG)
#   if defined(__GNUC__)
#      define TCL_WIDE_INT_TYPE long long
#      if defined(__WIN32__) && !defined(__CYGWIN__)
#         define TCL_LL_MODIFIER        "I64"
#      else
#         define TCL_LL_MODIFIER	"ll"
#      endif
*)
Type
  Pstat = Pointer;
  PTcl_StatBuf = Pointer;
(*
typedef struct stat	Tcl_StatBuf;
#   elif defined(__WIN32__)
#      define TCL_WIDE_INT_TYPE __int64
#      ifdef __BORLANDC__
typedef struct stati64 Tcl_StatBuf;
#         define TCL_LL_MODIFIER	"L"
#      else * __BORLANDC__ *
#         if _MSC_VER < 1400 || !defined(_M_IX86)
typedef struct _stati64	Tcl_StatBuf;
#         else
typedef struct _stat64	Tcl_StatBuf;
#         endif * _MSC_VER < 1400 *
#         define TCL_LL_MODIFIER	"I64"
#      endif * __BORLANDC__ *
#   else * __WIN32__ *
*
 * Don't know what platform it is and configure hasn't discovered what is
 * going on for us. Try to guess...
 *
#      ifdef NO_LIMITS_H
#	  error please define either TCL_WIDE_INT_TYPE or TCL_WIDE_INT_IS_LONG
#      else * !NO_LIMITS_H *
#	  include <limits.h>
#	  if (INT_MAX < LONG_MAX)
#	     define TCL_WIDE_INT_IS_LONG	1
#	  else
#	     define TCL_WIDE_INT_TYPE long long
#         endif
#      endif * NO_LIMITS_H *
#   endif * __WIN32__ *
#endif * !TCL_WIDE_INT_TYPE & !TCL_WIDE_INT_IS_LONG *
#ifdef TCL_WIDE_INT_IS_LONG
#   undef TCL_WIDE_INT_TYPE
#   define TCL_WIDE_INT_TYPE	long
#endif * TCL_WIDE_INT_IS_LONG *)

type
  PTcl_WideInt = ^Tcl_WideInt;
  Tcl_WideInt = clonglong;

  PTcl_WideUInt = ^Tcl_WideUInt;
  Tcl_WideUInt = culonglong;

{$ifdef TCL_WIDE_INT_IS_LONG}
  stat = Tcl_StatBuf;
(* was #define dname(params) para_def_expr*)
(* argument types are unknown*)

function Tcl_WideAsLong(val : longint) : clong;

(* was #define dname(params) para_def_expr*)
(* argument types are unknown*)
function Tcl_LongAsWide(val : longint) : clong;

(* was #define dname(params) para_def_expr*)
(* argument types are unknown*)
function Tcl_WideAsDouble(val : longint) : double;

(* was #define dname(params) para_def_expr*)
(* argument types are unknown*)
function Tcl_DoubleAsWide(val : longint) : clong;

(*{$ifdef HAVE_STRUCT_STAT64}
Type
  Tcl_StatBuf = stat64;
{$else}
Type
  Tcl_StatBuf = stat;
{$endif} *) (* HAVE_STRUCT_STAT64 *)

(* was #define dname(params) para_def_expr*)
(* argument types are unknown*)
function Tcl_WideAsLong(val : longint) : clong;

(* was #define dname(params) para_def_expr*)
(* argument types are unknown*)
function Tcl_LongAsWide(val : longint) : Tcl_WideInt;

(* was #define dname(params) para_def_expr*)
(* argument types are unknown*)
function Tcl_WideAsDouble(val : longint) : double;

(* was #define dname(params) para_def_expr*)
(* argument types are unknown*)
function Tcl_DoubleAsWide(val : longint) : Tcl_WideInt;

{$endif}  (* TCL_WIDE_INT_IS_LONG *)

(*
 * Data structures defined opaquely in this module. The definitions below just
 * provide dummy types. A few fields are made visible in Tcl_Interp
 * structures, namely those used for returning a string result from commands.
 * Direct access to the result field is discouraged in Tcl 8.0. The
 * interpreter result is either an object or a string, and the two values are
 * kept consistent unless some C code sets interp->result directly.
 * Programmers should use either the function Tcl_GetObjResult() or
 * Tcl_GetStringResult() to read the interpreter's result. See the SetResult
 * man page for details.
 *
 * Note: any change to the Tcl_Interp definition below must be mirrored in the
 * "real" definition in tclInt.h.
 *
 * Note: Tcl_ObjCmdProc functions do not directly set result and freeProc.
 * Instead, they set a Tcl_Obj member in the "real" structure that can be
 * accessed with Tcl_GetObjResult() and Tcl_SetObjResult().
 *)

type
  PPTcl_Interp = ^PTcl_Interp;
  PTcl_Interp = ^Tcl_Interp;
  Tcl_Interp = record
      result : PChar;           (* If the last command returned a string
				 * result, this points to it. *)
      freeProc : procedure (blockPtr:PChar);extdecl;
                                (* Zero means the string result is statically
				 * allocated. TCL_DYNAMIC means it was
				 * allocated with ckalloc and should be freed
				 * with ckfree. Other values give the address
				 * of function to invoke to free the result.
				 * Tcl_Eval must free it before executing next
				 * command. *)
      errorLine : cint;         (* When TCL_ERROR is returned, this gives the
				 * line number within the command where the
				 * error occurred (1 if first line). *)
  end;

  PTcl_AsyncHandler       = Pointer;
  PTcl_Channel            = Pointer;
  PTcl_ChannelTypeVersion = Pointer;
  PTcl_Command            = Pointer;
  PTcl_Condition          = Pointer;
  PTcl_Dict               = Pointer;
  PTcl_EncodingState      = Pointer;
  PTcl_Encoding           = Pointer;
  PTcl_InterpState        = Pointer;
  PTcl_LoadHandle         = Pointer;
  PTcl_Mutex              = Pointer;
  PTcl_Pid                = Pointer;
  PTcl_RegExp             = Pointer;
  PTcl_ThreadDataKey      = Pointer;
  PTcl_ThreadId           = Pointer;
  PTcl_TimerToken         = Pointer;
  PTcl_Trace              = Pointer;
  PTcl_Var                = Pointer;

(*
 * Definition of the interface to functions implementing threads. A function
 * following this definition is given to each call of 'Tcl_CreateThread' and
 * will be called as the main fuction of the new thread created by that call.
 *)

(*#if defined __WIN32__*)
(*typedef unsigned (__stdcall Tcl_ThreadCreateProc) (ClientData clientData);*)
(*#else*)
  Tcl_ThreadCreateProc = Procedure(clientData:ClientData);
(*#endif*)

(*
 * Threading function return types used for abstracting away platform
 * differences when writing a Tcl_ThreadCreateProc. See the NewThread function
 * in generic/tclThreadTest.c for it's usage.
 *)

(*#if defined __WIN32__*)
(*#   define Tcl_ThreadCreateType		unsigned __stdcall*)
(*#   define TCL_THREAD_CREATE_RETURN	return 0*)
(*#else*)
(*#   define Tcl_ThreadCreateType		void*)
(*#   define TCL_THREAD_CREATE_RETURN*)
(*#endif*)

(*
 * Definition of values for default stacksize and the possible flags to be
 * given to Tcl_CreateThread.
 *)

const
  TCL_THREAD_STACK_DEFAULT 	= 0;	(* Use default size for stack. *)
  TCL_THREAD_NOFLAGS		= 0000;	(* Standard flags, default
					 * behaviour. *)
  TCL_THREAD_JOINABLE		= 0001;	(* Mark the thread as joinable. *)

(*
 * Flag values passed to Tcl_StringCaseMatch.
 *)

  TCL_MATCH_NOCASE = 1 shl 0;

(*
 * Flag values passed to Tcl_GetRegExpFromObj.
 *)

  TCL_REG_BASIC		= $000000;	(* BREs (convenience). *)
  TCL_REG_EXTENDED	= $000001;	(* EREs. *)
  TCL_REG_ADVF		= $000002;	(* Advanced features in EREs. *)
  TCL_REG_ADVANCED	= $000003;	(* AREs (which are also EREs). *)
  TCL_REG_QUOTE		= $000004;	(* No special characters, none. *)
  TCL_REG_NOCASE	= $000008;	(* Ignore case. *)
  TCL_REG_NOSUB		= $000010;	(* Don't care about subexpressions. *)
  TCL_REG_EXPANDED	= $000020;	(* Expanded format, white space &
					 * comments. *)
  TCL_REG_NLSTOP	= $000040;	(* \n doesn't match . or [^ ] *)
  TCL_REG_NLANCH	= $000080;	(* ^ matches after \n, $ before. *)
  TCL_REG_NEWLINE	= $0000C0;	(* Newlines are line terminators. *)
  TCL_REG_CANMATCH	= $000100;	(* Report details on partial/limited
					 * matches. *)

(*
 * Flags values passed to Tcl_RegExpExecObj.
 *)

  TCL_REG_NOTBOL = 0001;	(* Beginning of string does not match ^.  *)
  TCL_REG_NOTEOL = 0002;	(* End of string does not match $. *)

(*
 * Structures filled in by Tcl_RegExpInfo. Note that all offset values are
 * relative to the start of the match string, not the beginning of the entire
 * string.
 *)

type
  PTcl_RegExpIndices = ^Tcl_RegExpIndices;
  Tcl_RegExpIndices = record
      start : clong;		(* Character offset of first character in
				 * match. *)
      _end : clong;		(* Character offset of first character after
				 * the match. *)
    end;

  PTcl_RegExpInfo = ^Tcl_RegExpInfo;
  Tcl_RegExpInfo = record
      nsubs : cint;			(* Number of subexpressions in the compiled
					 * expression. *)
      matches : PTcl_RegExpIndices;	(* Array of nsubs match offset pairs. *)
      extendStart : clong;		(* The offset at which a subsequent match
					 * might begin. *)
      reserved : clong;			(* Reserved for later use. *)
    end;

(*
 * Picky compilers complain if this typdef doesn't appear before the struct's
 * reference in tclDecls.h.
 *)

(*  PTcl_Stat_ = ^Tcl_Stat_;
  Tcl_Stat_ = PTcl_StatBuf;
  PTcl_OldStat_ = ^Tcl_OldStat_;
  Tcl_OldStat_ = Pstat;*)

(*
 * When a TCL command returns, the interpreter contains a result from the
 * command. Programmers are strongly encouraged to use one of the functions
 * Tcl_GetObjResult() or Tcl_GetStringResult() to read the interpreter's
 * result. See the SetResult man page for details. Besides this result, the
 * command function returns an integer code, which is one of the following:
 *
 * TCL_OK		Command completed normally; the interpreter's result
 *			contains the command's result.
 * TCL_ERROR		The command couldn't be completed successfully; the
 *			interpreter's result describes what went wrong.
 * TCL_RETURN		The command requests that the current function return;
 *			the interpreter's result contains the function's
 *			return value.
 * TCL_BREAK		The command requests that the innermost loop be
 *			exited; the interpreter's result is meaningless.
 * TCL_CONTINUE		Go on to the next iteration of the current loop; the
 *			interpreter's result is meaningless.
 *)

const
  TCL_OK		= 0;
  TCL_ERROR		= 1;
  TCL_RETURN		= 2;
  TCL_BREAK		= 3;
  TCL_CONTINUE		= 4;

  TCL_RESULT_SIZE	 = 200;

(*
 * Flags to control what substitutions are performed by Tcl_SubstObj():
 *)

  TCL_SUBST_COMMANDS	= $001;
  TCL_SUBST_VARIABLES	= $002;
  TCL_SUBST_BACKSLASHES	= $004;
  TCL_SUBST_ALL		= $007;

(*
 * Argument descriptors for math function callbacks in expressions:
 *)

type
  PPTcl_ValueType = ^PTcl_ValueType;
  PTcl_ValueType = ^Tcl_ValueType;
  Tcl_ValueType = (TCL_INT,TCL_DOUBLE,TCL_EITHER,TCL_WIDE_INT);

  PTcl_Value = ^Tcl_Value;
  Tcl_Value = record
      _type : Tcl_ValueType;	(* Indicates intValue or doubleValue is valid,
				 * or both. *)
      intValue : clong;		(* Integer value. *)
      doubleValue : double;	(* Double-precision floating value. *)
      wideValue : Tcl_WideInt;	(* Wide (min. 64-bit) integer value. *)
    end;

(*
 * Forward declaration of Tcl_Obj to prevent an error when the forward
 * reference to Tcl_Obj is encountered in the function types declared below.
 *)

  PTcl_Obj = ^Tcl_Obj;
  APTcl_Obj = Array[0..65535] of PTcl_Obj;
  PPTcl_Obj = ^APTcl_Obj;
  PPPTcl_Obj = ^PPTcl_Obj;

  PTcl_Event = ^Tcl_Event;

(*
 * Function types defined by Tcl:
 *)

  Tcl_AppInitProc = function(var interp:Tcl_Interp):cint;extdecl;
  Tcl_AsyncProc = function(clientData:ClientData; var interp:
	Tcl_Interp; code:cint):cint;extdecl;
  Tcl_ChannelProc = procedure(clientData:ClientData; mask:cint);extdecl;
  Tcl_CloseProc = procedure(data:ClientData);extdecl;
  Tcl_CmdDeleteProc = procedure(clientData:ClientData);extdecl;
  Tcl_CmdProc = function(clientData:ClientData; interp:PTcl_Interp; argc:cint; argv:array of PChar):cint;extdecl;
  Tcl_CmdTraceProc = procedure(clientData:ClientData; interp:PTcl_Interp; level:cint; command:PChar; proc:Tcl_CmdProc;
             cmdClientData:ClientData; argc:cint; argv:array of PChar);extdecl;
  Tcl_CmdObjTraceProc = function(clientData:ClientData; interp:PTcl_Interp; level:cint; command:PChar; commandInfo:PTcl_Command;
            objc:cint; var objv:PTcl_Obj):cint;extdecl;
  Tcl_CmdObjTraceDeleteProc = procedure(clientData:ClientData);extdecl;
  Tcl_DupInternalRepProc = procedure(srcPtr:PTcl_Obj; dupPtr:PTcl_Obj);extdecl;
  Tcl_EncodingConvertProc = function(clientData:ClientData; src:PChar; srcLen:cint; flags:cint; statePtr:PTcl_EncodingState;
            dst:PChar; dstLen:cint; srcReadPtr:pcint; var dstWrotePtr:cint; var dstCharsPtr:cint):cint;extdecl;
  Tcl_EncodingFreeProc = procedure(clientData:ClientData);extdecl;
  Tcl_EventProc = function(evPtr:PTcl_Event; flags:cint):cint;extdecl;
  Tcl_EventCheckProc = procedure(clientData:ClientData; flags:cint);extdecl;
  Tcl_EventDeleteProc = function(evPtr:PTcl_Event; clientData:ClientData):cint;extdecl;
  Tcl_EventSetupProc = procedure(clientData:ClientData; flags:cint);extdecl;
  Tcl_ExitProc = procedure(clientData:ClientData);extdecl;
  Tcl_FileProc = procedure(clientData:ClientData; mask:cint);extdecl;
  Tcl_FileFreeProc = procedure(clientData:ClientData);extdecl;
  Tcl_FreeInternalRepProc = procedure(objPtr:PTcl_Obj);extdecl;
  Tcl_FreeProc = procedure(blockPtr:PChar);extdecl;
  Tcl_IdleProc = procedure(clientData:ClientData);extdecl;
  Tcl_InterpDeleteProc = procedure(clientData:ClientData; interp:PTcl_Interp);extdecl;
  Tcl_MathProc = function(clientData:ClientData; interp:PTcl_Interp; args:PTcl_Value; resultPtr:PTcl_Value):cint;extdecl;
  Tcl_NamespaceDeleteProc = procedure(clientData:ClientData);extdecl;
  Tcl_ObjCmdProc = function(clientData:ClientData; interp:PTcl_Interp; objc:cint; objv:PPTcl_Obj):cint;extdecl;
  Tcl_PackageInitProc = function(var interp:Tcl_Interp):cint;extdecl;
  Tcl_PackageUnloadProc = function(var interp:Tcl_Interp; flags:cint):cint;extdecl;
  Tcl_PanicProc = procedure(format:PChar; args:array of const);extdecl;
  Tcl_TcpAcceptProc = procedure(callbackData:ClientData; chan:PTcl_Channel; address:PChar; port:cint);extdecl;
  Tcl_TimerProc = procedure(clientData:ClientData);extdecl;
  Tcl_SetFromAnyProc = function(interp:PTcl_Interp; objPtr:PTcl_Obj):cint;extdecl;
  Tcl_UpdateStringProc = procedure(objPtr:PTcl_Obj);extdecl;
  Tcl_VarTraceProc = function(clientData:ClientData; interp:PTcl_Interp; part1:PChar; part2:PChar; flags:cint):PChar;extdecl;
  PTcl_CommandTraceProc = procedure(clientData:ClientData; interp:PTcl_Interp; oldName:PChar; newName:PChar; flags:cint);extdecl;
  Tcl_CreateFileHandlerProc = procedure(fd:cint; mask:cint; proc:Tcl_FileProc; clientData:ClientData);extdecl;
  Tcl_DeleteFileHandlerProc = procedure(fd:cint);extdecl;
  Tcl_AlertNotifierProc = procedure(clientData:ClientData);extdecl;
  Tcl_ServiceModeHookProc = procedure(mode:cint);extdecl;
  Tcl_InitNotifierProc = Function:ClientData;extdecl;
  Tcl_FinalizeNotifierProc = procedure(clientData:ClientData);extdecl;
  Tcl_MainLoopProc = procedure;extdecl;

(*
 * The following structure represents a type of object, which is a particular
 * internal representation for an object plus a set of functions that provide
 * standard operations on objects of that type.
 *)

  PTcl_ObjType = ^Tcl_ObjType;
  Tcl_ObjType = record
    name : PChar;		(* Name of the type, e.g. "int". *)
    freeIntRepProc : Tcl_FreeInternalRepProc;
				(* Called to free any storage for the type's
				 * internal rep. NULL if the internal rep does
				 * not need freeing. *)
    dupIntRepProc : Tcl_DupInternalRepProc;
				(* Called to create a new object as a copy of
				 * an existing object. *)
    updateStringProc : Tcl_UpdateStringProc;
				(* Called to update the string rep from the
				 * type's internal representation. *)
    setFromAnyProc : Tcl_SetFromAnyProc;
				(* Called to convert the object's internal rep
				 * to this type. Frees the internal rep of the
				 * old type. Returns TCL_ERROR on failure. *)
    end;

(*
 * One of the following structures exists for each object in the Tcl system.
 * An object stores a value as either a string, some internal representation,
 * or both.
 *)

  Tcl_Obj = record
      refCount : cint;		(* When 0 the object will be freed. *)
      bytes : PChar;		(* This points to the first byte of the
				 * object's string representation. The array
				 * must be followed by a null byte (i.e., at
				 * offset length) but may also contain
				 * embedded null characters. The array's
				 * storage is allocated by ckalloc. NULL means
				 * the string rep is invalid and must be
				 * regenerated from the internal rep.  Clients
				 * should use Tcl_GetStringFromObj or
				 * Tcl_GetString to get a pointer to the byte
				 * array as a readonly value. *)
      length : cint;		(* The number of bytes at *bytes, not
				 * including the terminating null. *)
      typePtr : PTcl_ObjType;	(* Denotes the object's type. Always
				 * corresponds to the type of the object's
				 * internal rep. NULL indicates the object has
				 * no internal rep (has no type). *)
      internalRep : record	(* The internal representation: *)
          case longint of
            0 : ( longValue : clong );		(*   - an long integer value. *)
            1 : ( doubleValue : double );	(*   - a double-precision floating value. *)
            2 : ( otherValuePtr : pointer );	(*   - another, type-specific value. *)
            3 : ( wideValue : Tcl_WideInt );	(*   - a long long value. *)
            4 : ( twoPtrValue : record		(*   - internal rep as two pointers. *)
                ptr1 : pointer;
                ptr2 : pointer;
              end );
            5 : ( ptrAndLongRep : record	(*   - internal rep as a wide int, tightly
				 *     packed fields. *)
                ptr : pointer;	(* Pointer to digits. *)
                value : culong;	(* Alloc, used, and signum packed into a
				 * single word. *)
              end );
          end;
    end;

(*
 * The following structure defines a generic event for the Tcl event system.
 * These are the things that are queued in calls to Tcl_QueueEvent and
 * serviced later by Tcl_DoOneEvent. There can be many different kinds of
 * events with different fields, corresponding to window events, timer events,
 * etc. The structure for a particular event consists of a Tcl_Event header
 * followed by additional information specific to that event.
 *)

  Tcl_Event = record
    proc : Tcl_EventProc;	(* Function to call to service this event. *)
    nextPtr : PTcl_Event;	(* Next in list of pending events, or NULL. *)
  end;

(*
 * Macros to increment and decrement a Tcl_Obj's reference count, and to test
 * whether an object is shared (i.e. has reference count > 1). Note: clients
 * should use Tcl_DecrRefCount() when they are finished using an object, and
 * should never call TclFreeObj() directly. TclFreeObj() is only defined and
 * made public in tcl.h to support Tcl_DecrRefCount's macro definition. Note
 * also that Tcl_DecrRefCount() refers to the parameter "obj" twice. This
 * means that you should avoid calling it with an expression that is expensive
 * to compute or has side effects.
 *)

procedure Tcl_IncrRefCount(objPtr:PTcl_Obj);extdecl;external;
procedure Tcl_DecrRefCount(objPtr:PTcl_Obj);extdecl;external;
function Tcl_IsShared(objPtr:PTcl_Obj):cint;extdecl;external;

(*
 * The following structure contains the state needed by Tcl_SaveResult. No-one
 * outside of Tcl should access any of these fields. This structure is
 * typically allocated on the stack.
 *)

Type
  PTcl_SavedResult = ^Tcl_SavedResult;
  Tcl_SavedResult = record
      result : PChar;
      freeProc : Tcl_FreeProc;
      objResultPtr : PTcl_Obj;
      appendResult : PChar;
      appendAvl : cint;
      appendUsed : cint;
      resultSpace : array[0..(TCL_RESULT_SIZE+1)-1] of cchar;
    end;

(*
 * The following definitions support Tcl's namespace facility. Note: the first
 * five fields must match exactly the fields in a Namespace structure (see
 * tclInt.h).
 *)

  PPTcl_Namespace = ^PTcl_Namespace;
  PTcl_Namespace = ^Tcl_Namespace;
  Tcl_Namespace = record
      name : PChar;		(* The namespace's name within its parent
                                 * namespace. This contains no ::'s. The name
                                 * of the global namespace is "" although "::"
                                 * is an synonym. *)
      fullName : PChar;         (* The namespace's fully qualified name. This
                                 * starts with ::. *)
      clientData : ClientData;	(* Arbitrary value associated with this
                                 * namespace. *)
      deleteProc : Tcl_NamespaceDeleteProc;
                                (* Function invoked when deleting the
                                 * namespace to, e.g., free clientData. *)
      parentPtr : PTcl_Namespace;
                                (* Points to the namespace that contains this
                                 * one. NULL if this is the global
                                 * namespace. *)
  end;

(*
 * The following structure represents a call frame, or activation record. A
 * call frame defines a naming context for a procedure call: its local scope
 * (for local variables) and its namespace scope (used for non-local
 * variables; often the global :: namespace). A call frame can also define the
 * naming context for a namespace eval or namespace inscope command: the
 * namespace in which the command's code should execute. The Tcl_CallFrame
 * structures exist only while procedures or namespace eval/inscope's are
 * being executed, and provide a Tcl call stack.
 *
 * A call frame is initialized and pushed using Tcl_PushCallFrame and popped
 * using Tcl_PopCallFrame. Storage for a Tcl_CallFrame must be provided by the
 * Tcl_PushCallFrame caller, and callers typically allocate them on the C call
 * stack for efficiency. For this reason, Tcl_CallFrame is defined as a
 * structure and not as an opaque token. However, most Tcl_CallFrame fields
 * are hidden since applications should not access them directly; others are
 * declared as "dummyX".
 *
 * WARNING!! The structure definition must be kept consistent with the
 * CallFrame structure in tclInt.h. If you change one, change the other.
 *)

  PTcl_CallFrame = ^Tcl_CallFrame;
  Tcl_CallFrame = record
      nsPtr : PTcl_Namespace;
      dummy1 : cint;
      dummy2 : cint;
      dummy3 : pointer;
      dummy4 : pointer;
      dummy5 : pointer;
      dummy6 : cint;
      dummy7 : pointer;
      dummy8 : pointer;
      dummy9 : cint;
      dummy10 : pointer;
      dummy11 : pointer;
      dummy12 : pointer;
      dummy13 : pointer;
    end;

(*
 * Information about commands that is returned by Tcl_GetCommandInfo and
 * passed to Tcl_SetCommandInfo. objProc is an objc/objv object-based command
 * function while proc is a traditional Tcl argc/argv string-based function.
 * Tcl_CreateObjCommand and Tcl_CreateCommand ensure that both objProc and
 * proc are non-NULL and can be called to execute the command. However, it may
 * be faster to call one instead of the other. The member isNativeObjectProc
 * is set to 1 if an object-based function was registered by
 * Tcl_CreateObjCommand, and to 0 if a string-based function was registered by
 * Tcl_CreateCommand. The other function is typically set to a compatibility
 * wrapper that does string-to-object or object-to-string argument conversions
 * then calls the other function.
 *)

  PTcl_CmdInfo = ^Tcl_CmdInfo;
  Tcl_CmdInfo = record
    isNativeObjectProc : cint;	(* 1 if objProc was registered by a call to
				 * Tcl_CreateObjCommand; 0 otherwise.
				 * Tcl_SetCmdInfo does not modify this
				 * field. *)
    objProc : Tcl_ObjCmdProc;	(* Command's object-based function. *)
    objClientData : ClientData;	(* ClientData for object proc. *)
    proc : Tcl_CmdProc;	        (* Command's string-based function. *)
    clientData : ClientData;	(* ClientData for string proc. *)
    deleteProc : Tcl_CmdDeleteProc;
				(* Function to call when command is
				 * deleted. *)
    deleteData : ClientData;	(* Value to pass to deleteProc (usually the
				 * same as clientData). *)
    namespacePtr : PTcl_Namespace;(* Points to the namespace that contains this
				 * command. Note that Tcl_SetCmdInfo will not
				 * change a command's namespace; use
				 * TclRenameCommand or Tcl_Eval (of 'rename')
				 * to do that. *)
  end;

(*
 * The structure defined below is used to hold dynamic strings. The only
 * fields that clients should use are string and length, accessible via the
 * macros Tcl_DStringValue and Tcl_DStringLength.
 *)

const
  TCL_DSTRING_STATIC_SIZE = 200;
type
  PTcl_DString = ^Tcl_DString;
  Tcl_DString = record
    _string : PChar;		(* Points to beginning of string: either
				 * staticSpace below or a malloced array. *)
    length : cint;		(* Number of non-NULL characters in the
				 * string. *)
    spaceAvl : cint;		(* Total number of bytes available for the
				 * string and its terminating NULL char. *)
    staticSpace : array[0..(TCL_DSTRING_STATIC_SIZE)-1] of cchar;
				(* Space to use in common case where string is
				 * small. *)
  end;

//#define Tcl_DStringLength(dsPtr) ((dsPtr)->length)
//function Tcl_DStringLength(dsPtr : PTcl_DString) : cint;
//#define Tcl_DStringValue(dsPtr) ((dsPtr)->string)
//function Tcl_DStringValue(dsPtr : PTcl_DString) : PChar;
//#define Tcl_DStringTrunc Tcl_DStringSetLength
{$DEFINE Tcl_DStringTrunc = Tcl_DStringSetLength}

(*
 * Definitions for the maximum number of digits of precision that may be
 * specified in the "tcl_precision" variable, and the number of bytes of
 * buffer space required by Tcl_PrintDouble.
 *)

Const
  TCL_MAX_PREC		= 17;
  TCL_DOUBLE_SPACE	= TCL_MAX_PREC+10;

(*
 * Definition for a number of bytes of buffer space sufficient to hold the
 * string representation of an integer in base 10 (assuming the existence of
 * 64-bit integers).
 *)

  TCL_INTEGER_SPACE	= 24;

(*
 * Flag values passed to Tcl_ConvertElement.
 * TCL_DONT_USE_BRACES forces it not to enclose the element in braces, but to
 *	use backslash quoting instead.
 * TCL_DONT_QUOTE_HASH disables the default quoting of the '#' character. It
 *	is safe to leave the hash unquoted when the element is not the first
 *	element of a list, and this flag can be used by the caller to indicate
 *	that condition.
 * (Careful! If you change these flag values be sure to change the definitions
 * at the front of tclUtil.c).
 *)

  TCL_DONT_USE_BRACES	= 1;
  TCL_DONT_QUOTE_HASH	= 8;

(*
 * Flag that may be passed to Tcl_GetIndexFromObj to force it to disallow
 * abbreviated strings.
 *)

  TCL_EXACT	= 1;

(*
 * Flag values passed to Tcl_RecordAndEval, Tcl_EvalObj, Tcl_EvalObjv.
 * WARNING: these bit choices must not conflict with the bit choices for
 * evalFlag bits in tclInt.h!
 *
 * Meanings:
 *	TCL_NO_EVAL:		Just record this command
 *	TCL_EVAL_GLOBAL:	Execute script in global namespace
 *	TCL_EVAL_DIRECT:	Do not compile this script
 *	TCL_EVAL_INVOKE:	Magical Tcl_EvalObjv mode for aliases/ensembles
 *				o Run in iPtr->lookupNsPtr or global namespace
 *				o Cut out of error traces
 *				o Don't reset the flags controlling ensemble
 *				  error message rewriting.
 *)
  TCL_NO_EVAL		= $10000;
  TCL_EVAL_GLOBAL	= $20000;
  TCL_EVAL_DIRECT	= $40000;
  TCL_EVAL_INVOKE	= $80000;

(*
 * Special freeProc values that may be passed to Tcl_SetResult (see the man
 * page for details):
 *)

  TCL_VOLATILE		= {Tcl_FreeProc}(1);
  TCL_STATIC		= {Tcl_FreeProc}(0);
  TCL_DYNAMIC		= {Tcl_FreeProc}(3);

(*
 * Flag values passed to variable-related functions.
 *)

const
  TCL_GLOBAL_ONLY	= 1;
  TCL_NAMESPACE_ONLY	= 2;
  TCL_APPEND_VALUE	= 4;
  TCL_LIST_ELEMENT	= 8;
  TCL_TRACE_READS	= $10;
  TCL_TRACE_WRITES	= $20;
  TCL_TRACE_UNSETS	= $40;
  TCL_TRACE_DESTROYED	= $80;
  TCL_INTERP_DESTROYED	= $100;
  TCL_LEAVE_ERR_MSG	= $200;
  TCL_TRACE_ARRAY	= $800;
{$ifndef TCL_REMOVE_OBSOLETE_TRACES}
(* Required to support old variable/vdelete/vinfo traces *)
  TCL_TRACE_OLD_STYLE	= $1000;
{$endif}
(* Indicate the semantics of the result of a trace *)
  TCL_TRACE_RESULT_DYNAMIC	= $8000;
  TCL_TRACE_RESULT_OBJECT	= $10000;

(*
 * Flag values for ensemble commands.
 *)

  TCL_ENSEMBLE_PREFIX = $02;	(* Flag value to say whether to allow
				 * unambiguous prefixes of commands or to
				 * require exact matches for command names. *)

(*
 * Flag values passed to command-related functions.
 *)

  TCL_TRACE_RENAME = $2000;
  TCL_TRACE_DELETE = $4000;

  TCL_ALLOW_INLINE_COMPILATION = $20000;

(*
 * The TCL_PARSE_PART1 flag is deprecated and has no effect. The part1 is now
 * always parsed whenever the part2 is NULL. (This is to avoid a common error
 * when converting code to use the new object based APIs and forgetting to
 * give the flag)
 *)

{$ifndef TCL_NO_DEPRECATED}
  TCL_PARSE_PART1	= $400;
{$endif}

(*
 * Types for linked variables:
 *)

const
  TCL_LINK_INT		= 1;
  TCL_LINK_DOUBLE	= 2;
  TCL_LINK_BOOLEAN	= 3;
  TCL_LINK_STRING	= 4;
  TCL_LINK_WIDE_INT	= 5;
  TCL_LINK_CHAR		= 6;
  TCL_LINK_UCHAR	= 7;
  TCL_LINK_SHORT	= 8;
  TCL_LINK_USHORT	= 9;
  TCL_LINK_UINT		= 10;
  TCL_LINK_LONG		= 11;
  TCL_LINK_ULONG	= 12;
  TCL_LINK_FLOAT	= 13;
  TCL_LINK_WIDE_UINT	= 14;
  TCL_LINK_READ_ONLY	= $80;

(*
 * This flag controls whether the hash table stores the hash of a key, or
 * recalculates it. There should be no reason for turning this flag off as it
 * is completely binary and source compatible unless you directly access the
 * bucketPtr member of the Tcl_HashTableEntry structure. This member has been
 * removed and the space used to store the hash value.
 *)

{$ifndef TCL_HASH_KEY_STORE_HASH}
  TCL_HASH_KEY_STORE_HASH = 1;
{$endif}

(*
 * Flags used in Tcl_HashKeyType.
 *
 * TCL_HASH_KEY_RANDOMIZE_HASH -
 *				There are some things, pointers for example
 *				which don't hash well because they do not use
 *				the lower bits. If this flag is set then the
 *				hash table will attempt to rectify this by
 *				randomising the bits and then using the upper
 *				N bits as the index into the table.
 * TCL_HASH_KEY_SYSTEM_HASH -	If this flag is set then all memory internally
 *                              allocated for the hash table that is not for an
 *                              entry will use the system heap.
 *)

  TCL_HASH_KEY_RANDOMIZE_HASH	= $1;
  TCL_HASH_KEY_SYSTEM_HASH	= $2;

  TCL_HASH_KEY_TYPE_VERSION = 1;

  TCL_SMALL_HASH_TABLE = 4;

(*
 * Forward declarations of Tcl_HashTable and related types.
 *)

Type
  PTcl_HashKeyType = ^Tcl_HashKeyType;
  PTcl_HashTable = ^Tcl_HashTable;
  PTcl_HashEntry = ^Tcl_HashEntry;

  Tcl_HashKeyProc = function(tablePtr:PTcl_HashTable; keyPtr:pointer):cuint;extdecl;
  Tcl_CompareHashKeysProc = function(keyPtr:pointer; hPtr:PTcl_HashEntry):cint;extdecl;
  Tcl_AllocHashEntryProc = function(tablePtr:PTcl_HashTable; keyPtr:pointer):PTcl_HashEntry;extdecl;
  Tcl_FreeHashEntryProc = procedure(hPtr:PTcl_HashEntry);extdecl;

(*
 * Structure definition for an entry in a hash table. No-one outside Tcl
 * should access any of these fields directly; use the macros defined below.
 *)

  Tcl_HashEntry = record
    nextPtr : PTcl_HashEntry;	(* Pointer to next entry in this hash bucket,
				 * or NULL for end of chain. *)
    tablePtr : PTcl_HashTable;(* Pointer to table containing entry. *)
{$ifdef TCL_HASH_KEY_STORE_HASH}
    hash : pointer;		(* Hash value, stored as pointer to ensure
				 * that the offsets of the fields in this
				 * structure are not changed. *)
{$else}
    bucketPtr : ^PTcl_HashEntry;(* Pointer to bucket that points to first
				 * entry in this entry's chain: used for
				 * deleting the entry. *)
{$endif}
    clientData : ClientData;	(* Application stores something here with
				 * Tcl_SetHashValue. *)
    key : record		(* Key has one of these forms: *)
      case longint of
        0 : ( oneWordValue : PChar );	(* One-word value for key. *)
        1 : ( objPtr : PTcl_Obj );		(* Tcl_Obj * key value. *)
        2 : ( words : array[0..0] of cint );(* Multiple integer words for key. The actual
				 * size will be as large as necessary for this
				 * table's keys. *)
        3 : ( _string : array[0..3] of cchar );(* String for key. The actual size will be as
				 * large as needed to hold the key. *)
      end;			(* MUST BE LAST FIELD IN RECORD!! *)
  end;

(*
 * Structure definition for the methods associated with a hash table key type.
 *)

  Tcl_HashKeyType = record
      version : cint;		(* Version of the table. If this structure is
				 * extended in future then the version can be
				 * used to distinguish between different
				 * structures. *)
      flags : cint;		(* Flags, see above for details. *)
      hashKeyProc : Tcl_HashKeyProc;
				(* Calculates a hash value for the key. If
				 * this is NULL then the pointer itself is
				 * used as a hash value. *)
      compareKeysProc : Tcl_CompareHashKeysProc;
				(* Compares two keys and returns zero if they
				 * do not match, and non-zero if they do. If
				 * this is NULL then the pointers are
				 * compared. *)
      allocEntryProc : Tcl_AllocHashEntryProc;
				(* Called to allocate memory for a new entry,
				 * i.e. if the key is a string then this could
				 * allocate a single block which contains
				 * enough space for both the entry and the
				 * string. Only the key field of the allocated
				 * Tcl_HashEntry structure needs to be filled
				 * in. If something else needs to be done to
				 * the key, i.e. incrementing a reference
				 * count then that should be done by this
				 * function. If this is NULL then Tcl_Alloc is
				 * used to allocate enough space for a
				 * Tcl_HashEntry and the key pointer is
				 * assigned to key.oneWordValue. *)
      freeEntryProc : Tcl_FreeHashEntryProc;
				(* Called to free memory associated with an
				 * entry. If something else needs to be done
				 * to the key, i.e. decrementing a reference
				 * count then that should be done by this
				 * function. If this is NULL then Tcl_Free is
				 * used to free the Tcl_HashEntry. *)
    end;

(*
 * Structure definition for a hash table.  Must be in tcl.h so clients can
 * allocate space for these structures, but clients should never access any
 * fields in this structure.
 *)

  Tcl_HashTableFindProc   = function (tablePtr:PTcl_HashTable; key:PChar):PTcl_HashEntry; extdecl;
  Tcl_HashTableCreateProc = function (tablePtr:PTcl_HashTable; key:PChar; newPtr:pcint):PTcl_HashEntry; extdecl;
  Tcl_HashTable = record
    buckets : ^PTcl_HashEntry;	(* Pointer to bucket array. Each element
				 * points to first entry in bucket's hash
				 * chain, or NULL. *)
    staticBuckets : array[0..(TCL_SMALL_HASH_TABLE)-1] of PTcl_HashEntry;
				(* Bucket array used for small tables (to
				 * avoid mallocs and frees). *)
    numBuckets : cint;		(* Total number of buckets allocated at
				 * **bucketPtr. *)
    numEntries : cint;		(* Total number of entries present in
				 * table. *)
    rebuildSize : cint;		(* Enlarge table when numEntries gets to be
				 * this large. *)
    downShift : cint;		(* Shift count used in hashing function.
				 * Designed to use high-order bits of
				 * randomized keys. *)
    mask : cint;		(* Mask value used in hashing function. *)
    keyType : cint;		(* Type of keys used in this table. It's
				 * either TCL_CUSTOM_KEYS, TCL_STRING_KEYS,
				 * TCL_ONE_WORD_KEYS, or an integer giving the
				 * number of ints that is the size of the
				 * key. *)
    findProc   : Tcl_HashTableFindProc;
    createProc : Tcl_HashTableCreateProc;
    typePtr : PTcl_HashKeyType;	(* Type of the keys used in the
				 * Tcl_HashTable. *)
    end;

(*
 * Structure definition for information used to keep track of searches through
 * hash tables:
 *)

  PTcl_HashSearch = ^Tcl_HashSearch;
  Tcl_HashSearch = record
    tablePtr : PTcl_HashTable;(* Table being searched. *)
    nextIndex : cint;		(* Index of next bucket to be enumerated after
				 * present one. *)
    nextEntryPtr : PTcl_HashEntry;(* Next entry to be enumerated in the current
				 * bucket. *)
  end;

(*
 * Acceptable key types for hash tables:
 *
 * TCL_STRING_KEYS:		The keys are strings, they are copied into the
 *				entry.
 * TCL_ONE_WORD_KEYS:		The keys are pointers, the pointer is stored
 *				in the entry.
 * TCL_CUSTOM_TYPE_KEYS:	The keys are arbitrary types which are copied
 *				into the entry.
 * TCL_CUSTOM_PTR_KEYS:		The keys are pointers to arbitrary types, the
 *				pointer is stored in the entry.
 *
 * While maintaining binary compatability the above have to be distinct values
 * as they are used to differentiate between old versions of the hash table
 * which don't have a typePtr and new ones which do. Once binary compatability
 * is discarded in favour of making more wide spread changes TCL_STRING_KEYS
 * can be the same as TCL_CUSTOM_TYPE_KEYS, and TCL_ONE_WORD_KEYS can be the
 * same as TCL_CUSTOM_PTR_KEYS because they simply determine how the key is
 * accessed from the entry and not the behaviour.
 *)

const
  TCL_STRING_KEYS	= 0;
  TCL_ONE_WORD_KEYS	= 1;
  TCL_CUSTOM_TYPE_KEYS	= -2;
  TCL_CUSTOM_PTR_KEYS	= -1;

(*
 * Structure definition for information used to keep track of searches through
 * dictionaries. These fields should not be accessed by code outside
 * tclDictObj.c
 *)

type
  PTcl_DictSearch = ^Tcl_DictSearch;
  Tcl_DictSearch = record
    next : pointer;		(* Search position for underlying hash
				 * table. *)
    epoch : cint;		(* Epoch marker for dictionary being searched,
				 * or -1 if search has terminated. *)
    dictionaryPtr : PTcl_Dict;	(* Reference to dictionary being searched. *)
  end;

(*
 * Flag values to pass to Tcl_DoOneEvent to disable searches for some kinds of
 * events:
 *)

const
  TCL_DONT_WAIT		= 1 shl 1;
  TCL_WINDOW_EVENTS	= 1 shl 2;
  TCL_FILE_EVENTS	= 1 shl 3;
  TCL_TIMER_EVENTS	= 1 shl 4;
  TCL_IDLE_EVENTS	= 1 shl 5;	(* WAS 0x10 ???? *)
  TCL_ALL_EVENTS	=  not (TCL_DONT_WAIT);

(*
 * Positions to pass to Tcl_QueueEvent:
 *)

Type
  Tcl_QueuePosition = (
    TCL_QUEUE_TAIL, TCL_QUEUE_HEAD, TCL_QUEUE_MARK
  );

(*
 * Values to pass to Tcl_SetServiceMode to specify the behavior of notifier
 * event routines.
 *)

const
  TCL_SERVICE_NONE = 0;
  TCL_SERVICE_ALL = 1;

(*
 * The following structure keeps is used to hold a time value, either as an
 * absolute time (the number of seconds from the epoch) or as an elapsed time.
 * On Unix systems the epoch is Midnight Jan 1, 1970 GMT.
 *)

type
  PTcl_Time = ^Tcl_Time;
  Tcl_Time = record
      sec : clong;		(* Seconds. *)
      usec : clong;		(* Microseconds. *)
    end;

  Tcl_SetTimerProc = Procedure(timePtr:PTcl_Time); extdecl;
  Tcl_WaitForEventProc = Function(timePtr:PTcl_Time):cint; extdecl;

(*
 * TIP #233 (Virtualized Time)
 *)

  Tcl_GetTimeProc   = Procedure(timebuf : PTcl_Time;clientData:ClientData); extdecl;
  Tcl_ScaleTimeProc = Procedure(timebuf : PTcl_Time;clientData:ClientData); extdecl;

(*
 * Bits to pass to Tcl_CreateFileHandler and Tcl_CreateChannelHandler to
 * indicate what sorts of events are of interest:
 *)

const
  TCL_READABLE		= 1 shl 1;
  TCL_WRITABLE		= 1 shl 2;
  TCL_EXCEPTION		= 1 shl 3;

(*
 * Flag values to pass to Tcl_OpenCommandChannel to indicate the disposition
 * of the stdio handles. TCL_STDIN, TCL_STDOUT, TCL_STDERR, are also used in
 * Tcl_GetStdChannel.
 *)

  TCL_STDIN		= 1 shl 1;
  TCL_STDOUT		= 1 shl 2;
  TCL_STDERR		= 1 shl 3;
  TCL_ENFORCE_MODE	= 1 shl 4;

(*
 * Bits passed to Tcl_DriverClose2Proc to indicate which side of a channel
 * should be closed.
 *)

  TCL_CLOSE_READ		= 1 shl 1;
  TCL_CLOSE_WRITE		= 1 shl 2;

(*
 * Value to use as the closeProc for a channel that supports the close2Proc
 * interface.
 *)

  TCL_CLOSE2PROC  = {Tcl_DriverCloseProc}(1);

(*
 * Channel version tag. This was introduced in 8.3.2/8.4.
 *)

  TCL_CHANNEL_VERSION_1 = {Tcl_ChannelTypeVersion}($1);
  TCL_CHANNEL_VERSION_2 = {Tcl_ChannelTypeVersion}($2);
  TCL_CHANNEL_VERSION_3 = {Tcl_ChannelTypeVersion}($3);
  TCL_CHANNEL_VERSION_4 = {Tcl_ChannelTypeVersion}($4);
  TCL_CHANNEL_VERSION_5 = {Tcl_ChannelTypeVersion}($5);

(*
 * TIP #218: Channel Actions, Ids for Tcl_DriverThreadActionProc.
 *)

const
  TCL_CHANNEL_THREAD_INSERT = 0;
  TCL_CHANNEL_THREAD_REMOVE = 1;

(*
 * Typedefs for the various operations in a channel type:
 *)

Type
  Tcl_DriverBlockModeProc = function(instanceData:ClientData; mode:cint):cint;extdecl;
  Tcl_DriverCloseProc = function(instanceData:ClientData; interp:PTcl_Interp):cint;extdecl;
  Tcl_DriverClose2Proc = function(instanceData:ClientData; interp:PTcl_Interp; flags:cint):cint;extdecl;
  Tcl_DriverInputProc = function(instanceData:ClientData; buf:PChar; toRead:cint; errorCodePtr:pcint):cint;extdecl;
  Tcl_DriverOutputProc = function(instanceData:ClientData; buf:PChar; toWrite:cint; errorCodePtr:pcint):cint;extdecl;
  Tcl_DriverSeekProc = function(instanceData:ClientData; offset:clong; mode:cint; errorCodePtr:pcint):cint;extdecl;
  Tcl_DriverSetOptionProc = function(instanceData:ClientData; interp:PTcl_Interp; optionName:PChar; value:PChar):cint;extdecl;
  Tcl_DriverGetOptionProc = function(instanceData:ClientData; interp:PTcl_Interp; optionName:PChar; dsPtr:PTcl_DString):cint;extdecl;
  Tcl_DriverWatchProc = procedure(instanceData:ClientData; mask:cint);extdecl;
  Tcl_DriverGetHandleProc = function(instanceData:ClientData; direction:cint; Var handlePtr:ClientData):cint;extdecl;
  Tcl_DriverFlushProc = function(instanceData:ClientData):cint;extdecl;
  Tcl_DriverHandlerProc = function(instanceData:ClientData; interestMask:cint):cint;extdecl;
  Tcl_DriverWideSeekProc = function(instanceData:ClientData; offset:Tcl_WideInt; mode:cint; errorCodePtr:pcint):Tcl_WideInt;extdecl;

(*
 * TIP #218, Channel Thread Actions
 *)
  Tcl_DriverThreadActionProc = Procedure(instanceData:ClientData;action:cint); extdecl;
(*
 * TIP #208, File Truncation (etc.)
 *)
  Tcl_DriverTruncateProc = Function(instanceData:ClientData;length:Tcl_WideInt):cint; extdecl;

(*
 * struct Tcl_ChannelType:
 *
 * One such structure exists for each type (kind) of channel. It collects
 * together in one place all the functions that are part of the specific
 * channel type.
 *
 * It is recommend that the PTcl_Channel* functions are used to access elements
 * of this structure, instead of direct accessing.
 *)

  PTcl_ChannelType = ^Tcl_ChannelType;
  Tcl_ChannelType = record
    typeName : PChar;	(* The name of the channel type in Tcl
				 * commands. This storage is owned by channel
				 * type. *)
    version : PTcl_ChannelTypeVersion;
				(* Version of the channel type. *)
    closeProc : Tcl_DriverCloseProc;
				(* Function to call to close the channel, or
				 * TCL_CLOSE2PROC if the close2Proc should be
				 * used instead. *)
    inputProc : Tcl_DriverInputProc;
				(* Function to call for input on channel. *)
    outputProc : Tcl_DriverOutputProc;
				(* Function to call for output on channel. *)
    seekProc : Tcl_DriverSeekProc;
				(* Function to call to seek on the channel.
				 * May be NULL. *)
    setOptionProc : Tcl_DriverSetOptionProc;
				(* Set an option on a channel. *)
    getOptionProc : Tcl_DriverGetOptionProc;
				(* Get an option from a channel. *)
    watchProc : Tcl_DriverWatchProc;
				(* Set up the notifier to watch for events on
				 * this channel. *)
    getHandleProc : Tcl_DriverGetHandleProc;
				(* Get an OS handle from the channel or NULL
				 * if not supported. *)
    close2Proc : Tcl_DriverClose2Proc;
				(* Function to call to close the channel if
				 * the device supports closing the read &
				 * write sides independently. *)
    blockModeProc :  Tcl_DriverBlockModeProc;
				(* Set blocking mode for the raw channel. May
				 * be NULL. *)
    (*
     * Only valid in TCL_CHANNEL_VERSION_2 channels or later.
     *)
    flushProc : Tcl_DriverFlushProc;
				(* Function to call to flush a channel. May be
				 * NULL. *)
    handlerProc : Tcl_DriverHandlerProc;
				(* Function to call to handle a channel event.
				 * This will be passed up the stacked channel
				 * chain. *)
    (*
     * Only valid in TCL_CHANNEL_VERSION_3 channels or later.
     *)
    wideSeekProc : Tcl_DriverWideSeekProc;
				(* Function to call to seek on the channel
				 * which can handle 64-bit offsets. May be
				 * NULL, and must be NULL if seekProc is
				 * NULL. *)
    (*
     * Only valid in TCL_CHANNEL_VERSION_4 channels or later.
     * TIP #218, Channel Thread Actions.
     *)
    threadActionProc : Tcl_DriverThreadActionProc;
				(* Function to call to notify the driver of
				 * thread specific activity for a channel. May
				 * be NULL. *)
    (*
     * Only valid in TCL_CHANNEL_VERSION_5 channels or later.
     * TIP #208, File Truncation.
     *)
    truncateProc : Tcl_DriverTruncateProc;
				(* Function to call to truncate the underlying
				 * file to a particular length. May be NULL if
				 * the channel does not support truncation. *)
  end;

(*
 * The following flags determine whether the blockModeProc above should set
 * the channel into blocking or nonblocking mode. They are passed as arguments
 * to the blockModeProc function in the above structure.
 *)

const
  TCL_MODE_BLOCKING		= 0;	(* Put channel into blocking mode. *)
  TCL_MODE_NONBLOCKING		= 1;	(* Put channel into nonblocking
					 * mode. *)

(*
 * Enum for different types of file paths.
 *)

type
  Tcl_PathType = (
    TCL_PATH_ABSOLUTE,
    TCL_PATH_RELATIVE,
    TCL_PATH_VOLUME_RELATIVE
  );

(*
 * The following structure is used to pass glob type data amongst the various
 * glob routines and Tcl_FSMatchInDirectory.
 *)

  PTcl_GlobTypeData = ^Tcl_GlobTypeData;
  Tcl_GlobTypeData = record
      _type : cint;		(* Corresponds to bcdpfls as in 'find -t'. *)
      perm : cint;		(* Corresponds to file permissions. *)
      macType : PTcl_Obj;	(* Acceptable Mac type. *)
      macCreator : PTcl_Obj;	(* Acceptable Mac creator. *)
    end;

(*
 * Type and permission definitions for glob command.
 *)

const
  TCL_GLOB_TYPE_BLOCK		= 1 shl 0;
  TCL_GLOB_TYPE_CHAR		= 1 shl 1;
  TCL_GLOB_TYPE_DIR		= 1 shl 2;
  TCL_GLOB_TYPE_PIPE		= 1 shl 3;
  TCL_GLOB_TYPE_FILE		= 1 shl 4;
  TCL_GLOB_TYPE_LINK		= 1 shl 5;
  TCL_GLOB_TYPE_SOCK		= 1 shl 6;
  TCL_GLOB_TYPE_MOUNT		= 1 shl 7;

  TCL_GLOB_PERM_RONLY		= 1 shl 0;
  TCL_GLOB_PERM_HIDDEN		= 1 shl 1;
  TCL_GLOB_PERM_R		= 1 shl 2;
  TCL_GLOB_PERM_W		= 1 shl 3;
  TCL_GLOB_PERM_X		= 1 shl 4;

(*
 * Flags for the unload callback function.
 *)

  TCL_UNLOAD_DETACH_FROM_INTERPRETER	= 1 shl 0;
  TCL_UNLOAD_DETACH_FROM_PROCESS	= 1 shl 1;

(*
 * Typedefs for the various filesystem operations:
 *)

type
  Putimbuf = Pointer;
  PPChar = ^PChar;
  PPPChar = ^pPChar;
  PClientData = ^ClientData;
(* We have to declare the utime structure here. *)

  Tcl_FSStatProc = function(pathPtr:PTcl_Obj; buf:PTcl_StatBuf):cint;extdecl;
  Tcl_FSAccessProc = function(pathPtr:PTcl_Obj; mode:cint):cint;extdecl;
  Tcl_FSOpenFileChannelProc = function(interp:PTcl_Interp; pathPtr:PTcl_Obj; mode:cint; permissions:cint):PTcl_Channel;extdecl;
  Tcl_FSMatchInDirectoryProc = function(interp:PTcl_Interp; result:PTcl_Obj; pathPtr:PTcl_Obj; pattern:PChar; types:PTcl_GlobTypeData):cint;extdecl;
  Tcl_FSGetCwdProc = function(interp:PTcl_Interp):PTcl_Obj;extdecl;
  Tcl_FSChdirProc = function(pathPtr:PTcl_Obj):cint;extdecl;
  Tcl_FSLstatProc = function(pathPtr:PTcl_Obj; buf:PTcl_StatBuf):cint;extdecl;
  Tcl_FSCreateDirectoryProc = function(pathPtr:PTcl_Obj):cint;extdecl;
  Tcl_FSDeleteFileProc = function(pathPtr:PTcl_Obj):cint;extdecl;
  Tcl_FSCopyDirectoryProc = function(srcPathPtr:PTcl_Obj; destPathPtr:PTcl_Obj; errorPtr:PPTcl_Obj):cint;extdecl;
  Tcl_FSCopyFileProc = function(srcPathPtr:PTcl_Obj; destPathPtr:PTcl_Obj):cint;extdecl;
  Tcl_FSRemoveDirectoryProc = function(pathPtr:PTcl_Obj; recursive:cint; errorPtr:PPTcl_Obj):cint;extdecl;
  Tcl_FSRenameFileProc = function(srcPathPtr:PTcl_Obj; destPathPtr:PTcl_Obj):cint;extdecl;
  Tcl_FSUnloadFileProc = procedure(loadHandle:PTcl_LoadHandle);extdecl;
  Tcl_FSListVolumesProc = function:PTcl_Obj;extdecl;
  Tcl_FSUtimeProc = function(pathPtr:PTcl_Obj; tval:Putimbuf):cint;extdecl;
  Tcl_FSNormalizePathProc = function(interp:PTcl_Interp; pathPtr:PTcl_Obj; nextCheckpoint:cint):cint;extdecl;
  Tcl_FSFileAttrsGetProc = function(interp:PTcl_Interp; index:cint; pathPtr:PTcl_Obj; objPtrRef:PPTcl_Obj):cint;extdecl;
  Tcl_FSFileAttrStringsProc = function(pathPtr:PTcl_Obj; objPtrRef:PPTcl_Obj):pPChar;extdecl;
  Tcl_FSFileAttrsSetProc = function(interp:PTcl_Interp; index:cint; pathPtr:PTcl_Obj; objPtr:PTcl_Obj):cint;extdecl;
  Tcl_FSLinkProc = function(pathPtr:PTcl_Obj; toPtr:PTcl_Obj; linkType:cint):PTcl_Obj;extdecl;
  Tcl_FSLoadFileProc = function(interp:PTcl_Interp; pathPtr:PTcl_Obj; handlePtr:PTcl_LoadHandle; unloadProcPtr:Tcl_FSUnloadFileProc):cint;extdecl;
  Tcl_FSPathInFilesystemProc = function(pathPtr:PTcl_Obj; clientDataPtr:PClientData):cint;extdecl;
  Tcl_FSFilesystemPathTypeProc = function(pathPtr:PTcl_Obj):PTcl_Obj;extdecl;
  Tcl_FSFilesystemSeparatorProc = function(pathPtr:PTcl_Obj):PTcl_Obj;extdecl;
  Tcl_FSFreeInternalRepProc = procedure(clientData:ClientData);extdecl;
  Tcl_FSDupInternalRepProc = function(clientData:ClientData):ClientData;extdecl;
  Tcl_FSInternalToNormalizedProc = function(clientData:ClientData):PTcl_Obj;extdecl;
  Tcl_FSCreateInternalRepProc = function(pathPtr:PTcl_Obj):ClientData;extdecl;

  PTcl_FSVersion = ^Tcl_FSVersion;
  Tcl_FSVersion = record End;

(*
 *----------------------------------------------------------------
 * Data structures related to hooking into the filesystem
 *----------------------------------------------------------------
 *)

(*
 * Filesystem version tag.  This was introduced in 8.4.
 *)
Const
  TCL_FILESYSTEM_VERSION_1 = {PTcl_FSVersion}($1);

(*
 * struct Tcl_Filesystem:
 *
 * One such structure exists for each type (kind) of filesystem. It collects
 * together in one place all the functions that are part of the specific
 * filesystem. Tcl always accesses the filesystem through one of these
 * structures.
 *
 * Not all entries need be non-NULL; any which are NULL are simply ignored.
 * However, a complete filesystem should provide all of these functions. The
 * explanations in the structure show the importance of each function.
 *)

type
  PTcl_Filesystem = ^Tcl_Filesystem;
  Tcl_Filesystem = record
    typeName : PChar;		(* The name of the filesystem. *)
    structureLength : cint;	(* Length of this structure, so future binary
				 * compatibility can be assured. *)
    version : Tcl_FSVersion;	(* Version of the filesystem type. *)
    pathInFilesystemProc : Tcl_FSPathInFilesystemProc;
				(* Function to check whether a path is in this
				 * filesystem. This is the most important
				 * filesystem function. *)
    dupInternalRepProc : Tcl_FSDupInternalRepProc;
				(* Function to duplicate internal fs rep. May
				 * be NULL (but then fs is less efficient). *)
    freeInternalRepProc : Tcl_FSFreeInternalRepProc;
				(* Function to free internal fs rep. Must be
				 * implemented if internal representations
				 * need freeing, otherwise it can be NULL. *)
    internalToNormalizedProc : Tcl_FSInternalToNormalizedProc;
				(* Function to convert internal representation
				 * to a normalized path. Only required if the
				 * fs creates pure path objects with no
				 * string/path representation. *)
    createInternalRepProc : Tcl_FSCreateInternalRepProc;
				(* Function to create a filesystem-specific
				 * internal representation. May be NULL if
				 * paths have no internal representation, or
				 * if the Tcl_FSPathInFilesystemProc for this
				 * filesystem always immediately creates an
				 * internal representation for paths it
				 * accepts. *)
    normalizePathProc : Tcl_FSNormalizePathProc;
				(* Function to normalize a path.  Should be
				 * implemented for all filesystems which can
				 * have multiple string representations for
				 * the same path object. *)
    filesystemPathTypeProc : Tcl_FSFilesystemPathTypeProc;
				(* Function to determine the type of a path in
				 * this filesystem. May be NULL. *)
    filesystemSeparatorProc : Tcl_FSFilesystemSeparatorProc;
				(* Function to return the separator
				 * character(s) for this filesystem. Must be
				 * implemented. *)
    statProc : Tcl_FSStatProc;
				(* Function to process a 'Tcl_FSStat()' call.
				 * Must be implemented for any reasonable
				 * filesystem. *)
    accessProc : Tcl_FSAccessProc;
				(* Function to process a 'Tcl_FSAccess()'
				 * call. Must be implemented for any
				 * reasonable filesystem. *)
    openFileChannelProc : Tcl_FSOpenFileChannelProc;
				(* Function to process a
				 * 'Tcl_FSOpenFileChannel()' call. Must be
				 * implemented for any reasonable
				 * filesystem. *)
    matchInDirectoryProc : Tcl_FSMatchInDirectoryProc;
				(* Function to process a
				 * 'Tcl_FSMatchInDirectory()'.  If not
				 * implemented, then glob and recursive copy
				 * functionality will be lacking in the
				 * filesystem. *)
    utimeProc : Tcl_FSUtimeProc;
				(* Function to process a 'Tcl_FSUtime()' call.
				 * Required to allow setting (not reading) of
				 * times with 'file mtime', 'file atime' and
				 * the open-r/open-w/fcopy implementation of
				 * 'file copy'. *)
    linkProc : Tcl_FSLinkProc;
				(* Function to process a 'Tcl_FSLink()' call.
				 * Should be implemented only if the
				 * filesystem supports links (reading or
				 * creating). *)
    listVolumesProc : Tcl_FSListVolumesProc;
				(* Function to list any filesystem volumes
				 * added by this filesystem. Should be
				 * implemented only if the filesystem adds
				 * volumes at the head of the filesystem. *)
    fileAttrStringsProc : Tcl_FSFileAttrStringsProc;
				(* Function to list all attributes strings
				 * which are valid for this filesystem. If not
				 * implemented the filesystem will not support
				 * the 'file attributes' command. This allows
				 * arbitrary additional information to be
				 * attached to files in the filesystem. *)
    fileAttrsGetProc : Tcl_FSFileAttrsGetProc;
				(* Function to process a
				 * 'Tcl_FSFileAttrsGet()' call, used by 'file
				 * attributes'. *)
    fileAttrsSetProc : Tcl_FSFileAttrsSetProc;
				(* Function to process a
				 * 'Tcl_FSFileAttrsSet()' call, used by 'file
				 * attributes'.  *)
    createDirectoryProc : Tcl_FSCreateDirectoryProc;
				(* Function to process a
				 * 'Tcl_FSCreateDirectory()' call. Should be
				 * implemented unless the FS is read-only. *)
    removeDirectoryProc : Tcl_FSRemoveDirectoryProc;
				(* Function to process a
				 * 'Tcl_FSRemoveDirectory()' call. Should be
				 * implemented unless the FS is read-only. *)
    deleteFileProc : Tcl_FSDeleteFileProc;
				(* Function to process a 'Tcl_FSDeleteFile()'
				 * call. Should be implemented unless the FS
				 * is read-only. *)
    copyFileProc : Tcl_FSCopyFileProc;
				(* Function to process a 'Tcl_FSCopyFile()'
				 * call. If not implemented Tcl will fall back
				 * on open-r, open-w and fcopy as a copying
				 * mechanism, for copying actions initiated in
				 * Tcl (not C). *)
    renameFileProc : Tcl_FSRenameFileProc;
				(* Function to process a 'Tcl_FSRenameFile()'
				 * call. If not implemented, Tcl will fall
				 * back on a copy and delete mechanism, for
				 * rename actions initiated in Tcl (not C). *)
    copyDirectoryProc : Tcl_FSCopyDirectoryProc;
				(* Function to process a
				 * 'Tcl_FSCopyDirectory()' call. If not
				 * implemented, Tcl will fall back on a
				 * recursive create-dir, file copy mechanism,
				 * for copying actions initiated in Tcl (not
				 * C). *)
    lstatProc : Tcl_FSLstatProc;
				(* Function to process a 'Tcl_FSLstat()' call.
				 * If not implemented, Tcl will attempt to use
				 * the 'statProc' defined above instead. *)
    loadFileProc : Tcl_FSLoadFileProc;
				(* Function to process a 'Tcl_FSLoadFile()'
				 * call. If not implemented, Tcl will fall
				 * back on a copy to native-temp followed by a
				 * Tcl_FSLoadFile on that temporary copy. *)
    getCwdProc : Tcl_FSGetCwdProc;
				(* Function to process a 'Tcl_FSGetCwd()'
				 * call. Most filesystems need not implement
				 * this. It will usually only be called once,
				 * if 'getcwd' is called before 'chdir'. May
				 * be NULL. *)
    chdirProc : Tcl_FSChdirProc;	(* Function to process a 'Tcl_FSChdir()' call.
				 * If filesystems do not implement this, it
				 * will be emulated by a series of directory
				 * access checks. Otherwise, virtual
				 * filesystems which do implement it need only
				 * respond with a positive return result if
				 * the dirName is a valid directory in their
				 * filesystem. They need not remember the
				 * result, since that will be automatically
				 * remembered for use by GetCwd. Real
				 * filesystems should carry out the correct
				 * action (i.e. call the correct system
				 * 'chdir' api). If not implemented, then 'cd'
				 * and 'pwd' will fail inside the
				 * filesystem. *)
    end;

(*
 * The following definitions are used as values for the 'linkAction' flag to
 * Tcl_FSLink, or the linkProc of any filesystem. Any combination of flags can
 * be given. For link creation, the linkProc should create a link which
 * matches any of the types given.
 *
 * TCL_CREATE_SYMBOLIC_LINK -	Create a symbolic or soft link.
 * TCL_CREATE_HARD_LINK -	Create a hard link.
 *)

const
  TCL_CREATE_SYMBOLIC_LINK	= $01;
  TCL_CREATE_HARD_LINK		= $02;

(*
 * The following structure represents the Notifier functions that you can
 * override with the Tcl_SetNotifier call.
 *)

type
  PTcl_NotifierProcs = ^Tcl_NotifierProcs;
  Tcl_NotifierProcs = record
      setTimerProc          : Tcl_SetTimerProc;
      waitForEventProc      : Tcl_WaitForEventProc;
      createFileHandlerProc : Tcl_CreateFileHandlerProc;
      deleteFileHandlerProc : Tcl_DeleteFileHandlerProc;
      initNotifierProc      : Tcl_InitNotifierProc;
      finalizeNotifierProc  : Tcl_FinalizeNotifierProc;
      alertNotifierProc     : Tcl_AlertNotifierProc;
      serviceModeHookProc   : Tcl_ServiceModeHookProc;
    end;

(*
 * The following structure represents a user-defined encoding. It collects
 * together all the functions that are used by the specific encoding.
 *)

  PTcl_EncodingType = ^Tcl_EncodingType;
  Tcl_EncodingType = record
      encodingName : PChar;	(* The name of the encoding, e.g. "euc-jp".
				 * This name is the unique key for this
				 * encoding type. *)
      toUtfProc : Tcl_EncodingConvertProc;
				(* Function to convert from external encoding
				 * into UTF-8. *)
      fromUtfProc : Tcl_EncodingConvertProc;
				(* Function to convert from UTF-8 into
				 * external encoding. *)
      freeProc : Tcl_EncodingFreeProc;
				(* If non-NULL, function to call when this
				 * encoding is deleted. *)
      clientData : ClientData;	(* Arbitrary value associated with encoding
				 * type. Passed to conversion functions. *)
      nullSize : cint;
				(* Number of zero bytes that signify
				 * end-of-string in this encoding. This number
				 * is used to determine the source string
				 * length when the srcLen argument is
				 * negative. Must be 1 or 2. *)
    end;

(*
 * The following definitions are used as values for the conversion control
 * flags argument when converting text from one character set to another:
 *
 * TCL_ENCODING_START -		Signifies that the source buffer is the first
 *				block in a (potentially multi-block) input
 *				stream. Tells the conversion function to reset
 *				to an initial state and perform any
 *				initialization that needs to occur before the
 *				first byte is converted. If the source buffer
 *				contains the entire input stream to be
 *				converted, this flag should be set.
 * TCL_ENCODING_END -		Signifies that the source buffer is the last
 *				block in a (potentially multi-block) input
 *				stream. Tells the conversion routine to
 *				perform any finalization that needs to occur
 *				after the last byte is converted and then to
 *				reset to an initial state. If the source
 *				buffer contains the entire input stream to be
 *				converted, this flag should be set.
 * TCL_ENCODING_STOPONERROR -	If set, then the converter will return
 *				immediately upon encountering an invalid byte
 *				sequence or a source character that has no
 *				mapping in the target encoding. If clear, then
 *				the converter will skip the problem,
 *				substituting one or more "close" characters in
 *				the destination buffer and then continue to
 *				convert the source.
 *)

const
  TCL_ENCODING_START		= $01;
  TCL_ENCODING_END		= $02;
  TCL_ENCODING_STOPONERROR	= $04;

(*
 * The following data structures and declarations are for the new Tcl parser.
 *)

(*
 * For each word of a command, and for each piece of a word such as a variable
 * reference, one of the following structures is created to describe the
 * token.
 *)

type
  PTcl_Token = ^Tcl_Token;
  Tcl_Token = record
    _type : cint;		(* Type of token, such as TCL_TOKEN_WORD; see
				 * below for valid types. *)
    start : PChar;		(* First character in token. *)
    size : cint;		(* Number of bytes in token. *)
    numComponents : cint;	(* If this token is composed of other tokens,
				 * this field tells how many of them there are
				 * (including components of components, etc.).
				 * The component tokens immediately follow
				 * this one. *)
  end;

(*
 * Type values defined for Tcl_Token structures. These values are defined as
 * mask bits so that it's easy to check for collections of types.
 *
 * TCL_TOKEN_WORD -		The token describes one word of a command,
 *				from the first non-blank character of the word
 *				(which may be " or {) up to but not including
 *				the space, semicolon, or bracket that
 *				terminates the word. NumComponents counts the
 *				total number of sub-tokens that make up the
 *				word. This includes, for example, sub-tokens
 *				of TCL_TOKEN_VARIABLE tokens.
 * TCL_TOKEN_SIMPLE_WORD -	This token is just like TCL_TOKEN_WORD except
 *				that the word is guaranteed to consist of a
 *				single TCL_TOKEN_TEXT sub-token.
 * TCL_TOKEN_TEXT -		The token describes a range of literal text
 *				that is part of a word. NumComponents is
 *				always 0.
 * TCL_TOKEN_BS -		The token describes a backslash sequence that
 *				must be collapsed. NumComponents is always 0.
 * TCL_TOKEN_COMMAND -		The token describes a command whose result
 *				must be substituted into the word. The token
 *				includes the enclosing brackets. NumComponents
 *				is always 0.
 * TCL_TOKEN_VARIABLE -		The token describes a variable substitution,
 *				including the dollar sign, variable name, and
 *				array index (if there is one) up through the
 *				right parentheses. NumComponents tells how
 *				many additional tokens follow to represent the
 *				variable name. The first token will be a
 *				TCL_TOKEN_TEXT token that describes the
 *				variable name. If the variable is an array
 *				reference then there will be one or more
 *				additional tokens, of type TCL_TOKEN_TEXT,
 *				TCL_TOKEN_BS, TCL_TOKEN_COMMAND, and
 *				TCL_TOKEN_VARIABLE, that describe the array
 *				index; numComponents counts the total number
 *				of nested tokens that make up the variable
 *				reference, including sub-tokens of
 *				TCL_TOKEN_VARIABLE tokens.
 * TCL_TOKEN_SUB_EXPR -		The token describes one subexpression of an
 *				expression, from the first non-blank character
 *				of the subexpression up to but not including
 *				the space, brace, or bracket that terminates
 *				the subexpression. NumComponents counts the
 *				total number of following subtokens that make
 *				up the subexpression; this includes all
 *				subtokens for any nested TCL_TOKEN_SUB_EXPR
 *				tokens. For example, a numeric value used as a
 *				primitive operand is described by a
 *				TCL_TOKEN_SUB_EXPR token followed by a
 *				TCL_TOKEN_TEXT token. A binary subexpression
 *				is described by a TCL_TOKEN_SUB_EXPR token
 *				followed by the TCL_TOKEN_OPERATOR token for
 *				the operator, then TCL_TOKEN_SUB_EXPR tokens
 *				for the left then the right operands.
 * TCL_TOKEN_OPERATOR -		The token describes one expression operator.
 *				An operator might be the name of a math
 *				function such as "abs". A TCL_TOKEN_OPERATOR
 *				token is always preceeded by one
 *				TCL_TOKEN_SUB_EXPR token for the operator's
 *				subexpression, and is followed by zero or more
 *				TCL_TOKEN_SUB_EXPR tokens for the operator's
 *				operands. NumComponents is always 0.
 * TCL_TOKEN_EXPAND_WORD -	This token is just like TCL_TOKEN_WORD except
 *				that it marks a word that began with the
 *				literal character prefix "{*}". This word is
 *				marked to be expanded - that is, broken into
 *				words after substitution is complete.
 *)

const
  TCL_TOKEN_WORD	= 1;
  TCL_TOKEN_SIMPLE_WORD	= 2;
  TCL_TOKEN_TEXT	= 4;
  TCL_TOKEN_BS		= 8;
  TCL_TOKEN_COMMAND	= 16;
  TCL_TOKEN_VARIABLE	= 32;
  TCL_TOKEN_SUB_EXPR	= 64;
  TCL_TOKEN_OPERATOR	= 128;
  TCL_TOKEN_EXPAND_WORD	= 256;

(*
 * Parsing error types. On any parsing error, one of these values will be
 * stored in the error field of the Tcl_Parse structure defined below.
 *)

  TCL_PARSE_SUCCESS		= 0;
  TCL_PARSE_QUOTE_EXTRA		= 1;
  TCL_PARSE_BRACE_EXTRA		= 2;
  TCL_PARSE_MISSING_BRACE	= 3;
  TCL_PARSE_MISSING_BRACKET	= 4;
  TCL_PARSE_MISSING_PAREN	= 5;
  TCL_PARSE_MISSING_QUOTE	= 6;
  TCL_PARSE_MISSING_VAR_BRACE	= 7;
  TCL_PARSE_SYNTAX		= 8;
  TCL_PARSE_BAD_NUMBER		= 9;

(*
 * A structure of the following type is filled in by Tcl_ParseCommand. It
 * describes a single command parsed from an input string.
 *)

  NUM_STATIC_TOKENS = 20;

Type
  PTcl_Parse = ^Tcl_Parse;
  Tcl_Parse = record
    commentStart : PChar;	(* Pointer to # that begins the first of one
				 * or more comments preceding the command. *)
    commentSize : cint;	(* Number of bytes in comments (up through
				 * newline character that terminates the last
				 * comment). If there were no comments, this
				 * field is 0. *)
    commandStart : PChar;	(* First character in first word of
				 * command. *)
    commandSize : cint;		(* Number of bytes in command, including first
				 * character of first word, up through the
				 * terminating newline, close bracket, or
				 * semicolon. *)
    numWords : cint;		(* Total number of words in command. May be
				 * 0. *)
    tokenPtr : PTcl_Token;	(* Pointer to first token representing the
				 * words of the command. Initially points to
				 * staticTokens, but may change to point to
				 * malloc-ed space if command exceeds space in
				 * staticTokens. *)
    numTokens : cint;		(* Total number of tokens in command. *)
    tokensAvailable : cint;	(* Total number of tokens available at
				 * *tokenPtr. *)
    errorType : cint;		(* One of the parsing error types defined
				 * above. *)
    (*
     * The fields below are intended only for the private use of the parser.
     * They should not be used by functions that invoke Tcl_ParseCommand.
     *)

    _string : PChar;		(* The original command string passed to
				 * Tcl_ParseCommand. *)
    _end : PChar;		(* Points to the character just after the last
				 * one in the command string. *)
    interp : PTcl_Interp;	(* Interpreter to use for error reporting, or
				 * NULL. *)
    term : PChar;		(* Points to character in string that
				 * terminated most recent token. Filled in by
				 * ParseTokens. If an error occurs, points to
				 * beginning of region where the error
				 * occurred (e.g. the open brace if the close
				 * brace is missing). *)
    incomplete : cint;	(* This field is set to 1 by Tcl_ParseCommand
				 * if the command appears to be incomplete.
				 * This information is used by
				 * PTcl_CommandComplete. *)
    staticTokens : array[0..(NUM_STATIC_TOKENS)-1] of Tcl_Token;
				(* Initial space for tokens for command. This
				 * space should be large enough to accommodate
				 * most commands; dynamic space is allocated
				 * for very large commands that don't fit
				 * here. *)
    end;

(*
 * The following definitions are the error codes returned by the conversion
 * routines:
 *
 * TCL_OK -			All characters were converted.
 * TCL_CONVERT_NOSPACE -	The output buffer would not have been large
 *				enough for all of the converted data; as many
 *				characters as could fit were converted though.
 * TCL_CONVERT_MULTIBYTE -	The last few bytes in the source string were
 *				the beginning of a multibyte sequence, but
 *				more bytes were needed to complete this
 *				sequence. A subsequent call to the conversion
 *				routine should pass the beginning of this
 *				unconverted sequence plus additional bytes
 *				from the source stream to properly convert the
 *				formerly split-up multibyte sequence.
 * TCL_CONVERT_SYNTAX -		The source stream contained an invalid
 *				character sequence. This may occur if the
 *				input stream has been damaged or if the input
 *				encoding method was misidentified. This error
 *				is reported only if TCL_ENCODING_STOPONERROR
 *				was specified.
 * TCL_CONVERT_UNKNOWN -	The source string contained a character that
 *				could not be represented in the target
 *				encoding. This error is reported only if
 *				TCL_ENCODING_STOPONERROR was specified.
 *)

const
  TCL_CONVERT_MULTIBYTE	= -1;
  TCL_CONVERT_SYNTAX	= -2;
  TCL_CONVERT_UNKNOWN	= -3;
  TCL_CONVERT_NOSPACE	= -4;

(*
 * The maximum number of bytes that are necessary to represent a single
 * Unicode character in UTF-8. The valid values should be 3 or 6 (or perhaps 1
 * if we want to support a non-unicode enabled core). If 3, then Tcl_UniChar
 * must be 2-bytes in size (UCS-2) (the default). If 6, then Tcl_UniChar must
 * be 4-bytes in size (UCS-4). At this time UCS-2 mode is the default and
 * recommended mode. UCS-4 is experimental and not recommended. It works for
 * the core, but most extensions expect UCS-2.
 *)

{$ifndef TCL_UTF_MAX}
Const
  TCL_UTF_MAX		= 3;
{$endif}

(*
 * This represents a Unicode character. Any changes to this should also be
 * reflected in regcustom.h.
 *)

Type
  PTcl_UniChar = ^Tcl_UniChar;
{$if TCL_UTF_MAX > 3}
    (*
     * unsigned int isn't 100% accurate as it should be a strict 4-byte value
     * (perhaps wchar_t). 64-bit systems may have troubles. The size of this
     * value must be reflected correctly in regcustom.h and
     * in tclEncoding.c.
     * XXX: Tcl is currently UCS-2 and planning UTF-16 for the Unicode
     * XXX: string rep that Tcl_UniChar represents.  Changing the size
     * XXX: of Tcl_UniChar is /not/ supported.
     *)
  Tcl_UniChar = cuint;
{$else}
  Tcl_UniChar = cushort;
{$endif}

(*
 * TIP #59: The following structure is used in calls 'Tcl_RegisterConfig' to
 * provide the system with the embedded configuration data.
 *)

type
  PTcl_Config = ^Tcl_Config;
  Tcl_Config = record
      key : PChar;		(* Configuration key to register. ASCII
				 * encoded, thus UTF-8. *)
      value : PChar;		(* The value associated with the key. System
				 * encoding. *)
    end;

(*
 * Flags for TIP#143 limits, detailing which limits are active in an
 * interpreter. Used for Tcl_{Add,Remove}LimitHandler type argument.
 *)

Const
  TCL_LIMIT_COMMANDS	= $01;
  TCL_LIMIT_TIME	= $02;

(*
 * Structure containing information about a limit handler to be called when a
 * command- or time-limit is exceeded by an interpreter.
 *)

Type
  Tcl_LimitHandlerProc = Procedure (clientData:ClientData;interp:PTcl_Interp);extdecl;
  Tcl_LimitHandlerDeleteProc = Procedure(clientData:ClientData);extdecl;

type
  Pmp_int = ^mp_int;
  mp_int = record End;
{$define MP_INT_DECLARED}
  mp_digit = cuint;
{$define MP_DIGIT_DECLARED}

(*
 * The following ant is used to test for older versions of Tcl in the
 * stubs tables.
 *
 * Jan Nijtman's plus patch uses 0xFCA1BACF, so we need to pick a different
 * value since the stubs tables don't match.
 *)

Const
  TCL_STUB_MAGIC	= $FCA3BACF;

(*
 * The following function is required to be defined in all stubs aware
 * extensions. The function is actually implemented in the stub library, not
 * the main Tcl library, although there is a trivial implementation in the
 * main library in case an extension is statically linked into an application.
 *)

function Tcl_InitStubs(var interp:Tcl_Interp;
			version:PChar; exact:cint):PChar;extdecl;external;
function TclTomMathInitializeStubs(
			    var interp:Tcl_Interp; version:PChar;
			    epoch:cint; revision:cint):PChar;extdecl;external;

{$ifndef USE_TCL_STUBS}

(*
 * When not using stubs, make it a macro.
 *)

//#define Tcl_InitStubs(interp, version, exact) \
//    Tcl_PkgInitStubsCheck(interp, version, exact)

{$endif}

    (*
     * TODO - tommath stubs export goes here!
     *)


(*
 * Public functions that are not accessible via the stubs table.
 * Tcl_GetMemoryInfo is needed for AOLserver. [Bug 1868171]
 *)

procedure Tcl_Main(argc:cint; argv:PPChar; var appInitProc:Tcl_AppInitProc);extdecl;external;
function Tcl_PkgInitStubsCheck(var interp:Tcl_Interp; version:PChar; exact:cint):PChar;extdecl;external;
{$if defined(TCL_THREADS) and defined(USE_THREAD_ALLOC)}
procedure Tcl_GetMemoryInfo(var dsPtr:Tcl_DString);extdecl;external;
{$endif}

(*
 * Include the public function declarations that are accessible via the stubs
 * table.
 *)

{$I tclDecls.inc.pas}

(*
 * Include platform specific public function declarations that are accessible
 * via the stubs table.
 *)

{$I tclPlatDecls.inc.pas}

(*
 * The following declarations either map ckalloc and ckfree to malloc and
 * free, or they map them to functions with all sorts of debugging hooks
 * defined in tclCkalloc.c.
 *)

{$ifdef TCL_MEM_DEBUG}

(*#   define ckalloc(x) Tcl_DbCkalloc(x, __FILE__, __LINE__)*)
(*#   define ckfree(x)  Tcl_DbCkfree(x, __FILE__, __LINE__)*)
(*#   define ckrealloc(x,y) Tcl_DbCkrealloc((x), (y),__FILE__, __LINE__)*)
(*#   define attemptckalloc(x) Tcl_AttemptDbCkalloc(x, __FILE__, __LINE__)*)
(*#   define attemptckrealloc(x,y) Tcl_AttemptDbCkrealloc((x), (y), __FILE__, __LINE__)*)

{$else} { !TCL_MEM_DEBUG }

(*
 * If we are not using the debugging allocator, we should call the Tcl_Alloc,
 * et al. routines in order to guarantee that every module is using the same
 * memory allocator both inside and outside of the Tcl library.
 *)

(*#   define ckalloc(x) Tcl_Alloc(x)*)
(*#   define ckfree(x) Tcl_Free(x)*)
(*#   define ckrealloc(x,y) Tcl_Realloc(x,y)*)
(*#   define attemptckalloc(x) Tcl_AttemptAlloc(x)*)
(*#   define attemptckrealloc(x,y) Tcl_AttemptRealloc(x,y)*)
(*#   undef  Tcl_InitMemory*)
(*#   define Tcl_InitMemory(x)*)
(*#   undef  Tcl_DumpActiveMemory*)
(*#   define Tcl_DumpActiveMemory(x)*)
(*#   undef  Tcl_ValidateAllMemory*)
(*#   define Tcl_ValidateAllMemory(x,y)*)

{$endif} { !TCL_MEM_DEBUG }

(*Procedure Tcl_IncrRefCount(objPtr : PTcl_Obj); inline;
Procedure Tcl_DecrRefCount(objPtr : PTcl_Obj); inline;
Function  Tcl_IsShared    (objPtr : PTcl_Obj):Boolean; inline;*)

(*
 * Macros and definitions that help to debug the use of Tcl objects. When
 * TCL_MEM_DEBUG is defined, the Tcl_New declarations are overridden to call
 * debugging versions of the object creation functions.
 *)

// not applicable

(*
 * Macros for clients to use to access fields of hash entries:
 *)

Function  Tcl_GetHashValue(h : PTcl_HashEntry) : ClientData; inline;
Procedure Tcl_SetHashValue(h : PTcl_HashEntry; Value:ClientData); inline;
Function  Tcl_GetHashKey  (tablePtr:PTcl_HashTable;h : PTcl_HashEntry) : PChar; inline;

(*
 * Macros to use for clients to use to invoke find and create functions for
 * hash tables:
 *)

//Function Tcl_FindHashEntry  (tablePtr:PTcl_HashTable;key : PChar) : PTcl_HashEntry; inline;
//Function Tcl_CreateHashEntry(tablePtr:PTcl_HashTable;key : PChar;newPtr : pcint) : PTcl_HashEntry; inline;

(*
 * Macros that eliminate the overhead of the thread synchronization functions
 * when compiling without thread support.
 *)

// not applicable

    (*
     * Deprecated Tcl functions:
     *)

// not applicable

(*
 * Convenience declaration of Tcl_AppInit for backwards compatibility. This
 * function is not *implemented* by the tcl library, so the storage class is
 * neither DLLEXPORT nor DLLIMPORT.
 *)

function Tcl_AppInit(var interp:Tcl_Interp):cint;extdecl;external;

implementation

(*
function Tcl_WideAsLong(val : longint) : clong;
begin
  Tcl_WideAsLong:=clong(val);
end;

function Tcl_LongAsWide(val : longint) : clong;
begin
  Tcl_LongAsWide:=clong(val);
end;

function Tcl_WideAsDouble(val : longint) : double;
begin
  Tcl_WideAsDouble:=double(clong(val));
end;

function Tcl_DoubleAsWide(val : longint) : clong;
begin
  Tcl_DoubleAsWide:=clong(double(val));
end;

function Tcl_WideAsLong(val : longint) : clong;
begin
  Tcl_WideAsLong:=clong(Tcl_WideInt(val));
end;

function Tcl_LongAsWide(val : longint) : Tcl_WideInt;
begin
  Tcl_LongAsWide:=Tcl_WideInt(clong(val));
end;

function Tcl_WideAsDouble(val : longint) : double;
begin
  Tcl_WideAsDouble:=double(Tcl_WideInt(val));
end;

function Tcl_DoubleAsWide(val : longint) : Tcl_WideInt;
begin
  Tcl_DoubleAsWide:=Tcl_WideInt(double(val));
end;

function Tcl_DStringLength(dsPtr : PTcl_DString) : cint;
begin
  Tcl_DStringLength:=dsPtr^.length;
end;

function Tcl_DStringValue(dsPtr : PTcl_DString) : PChar;
begin
  Tcl_DStringValue:=dsPtr^._string;
end;

function TCL_VOLATILE : PTcl_FreeProc;
  begin
    TCL_VOLATILE:=PTcl_FreeProc(1);
  end;

function TCL_STATIC : PTcl_FreeProc;
  begin
    TCL_STATIC:=PTcl_FreeProc(0);
  end;

function TCL_DYNAMIC : PTcl_FreeProc;
  begin
    TCL_DYNAMIC:=PTcl_FreeProc(3);
  end;

function TCL_CLOSE2PROC : PTcl_DriverCloseProc;
  begin
    TCL_CLOSE2PROC:=PTcl_DriverCloseProc(1);
  end;

function TCL_CHANNEL_VERSION_1 : Tcl_ChannelTypeVersion;
  begin
    TCL_CHANNEL_VERSION_1:=Tcl_ChannelTypeVersion($1);
  end;

function TCL_CHANNEL_VERSION_2 : Tcl_ChannelTypeVersion;
  begin
    TCL_CHANNEL_VERSION_2:=Tcl_ChannelTypeVersion($2);
  end;

function TCL_CHANNEL_VERSION_3 : Tcl_ChannelTypeVersion;
  begin
    TCL_CHANNEL_VERSION_3:=Tcl_ChannelTypeVersion($3);
  end;

function TCL_CHANNEL_VERSION_4 : Tcl_ChannelTypeVersion;
  begin
    TCL_CHANNEL_VERSION_4:=Tcl_ChannelTypeVersion($4);
  end;

function TCL_CHANNEL_VERSION_5 : Tcl_ChannelTypeVersion;
  begin
    TCL_CHANNEL_VERSION_5:=Tcl_ChannelTypeVersion($5);
  end;

function TCL_FILESYSTEM_VERSION_1 : Tcl_FSVersion;
  begin
    TCL_FILESYSTEM_VERSION_1:=Tcl_FSVersion($1);
  end;

function TCL_STUB_MAGIC : cint;
  begin
    TCL_STUB_MAGIC:=cint($FCA3BACF);
  end;

function Tcl_InitStubs(interp,version,exact : longint) : longint;
begin
  Tcl_InitStubs:=Tcl_PkgInitStubsCheck(interp,version,exact);
end;



{$ifdef TCL_MEM_DEBUG}

Procedure Tcl_IncrRefCount(objPtr : PTcl_Obj); inline;
Begin
  Tcl_DbIncrRefCount(objPtr,{$I %FILE%},{$I %LINE%});
End;

Procedure Tcl_DecrRefCount(objPtr : PTcl_Obj); inline;
Begin
  Tcl_DbDecrRefCount(objPtr,{$I %FILE%},{$I %LINE%});
End;

Function Tcl_IsShared    (objPtr : PTcl_Obj):Boolean; inline;
Begin
  Tcl_DbIsShared(objPtr,{$I %FILE%},{$I %LINE%});
End;

{$else}

Procedure Tcl_IncrRefCount(objPtr : PTcl_Obj); inline;
Begin
  Inc(objPtr^.refCount);
End;

Procedure Tcl_DecrRefCount(objPtr : PTcl_Obj); inline;
Begin
  Dec(objPtr^.refCount);
  if objPtr^.refCount <= 0 then
    TclFreeObj(objPtr);
End;

Function Tcl_IsShared    (objPtr : PTcl_Obj):Boolean; inline;
Begin
  Result := objPtr^.refCount > 1;
End;

{$endif}

function Tcl_NewBignumObj(val : longint) : longint;
begin
  Tcl_NewBignumObj:=Tcl_DbNewBignumObj(val,__FILE__,__LINE__);
end;

function Tcl_NewBooleanObj(val : longint) : longint;
begin
  Tcl_NewBooleanObj:=Tcl_DbNewBooleanObj(val,__FILE__,__LINE__);
end;

function Tcl_NewByteArrayObj(bytes,len : longint) : longint;
begin
  Tcl_NewByteArrayObj:=Tcl_DbNewByteArrayObj(bytes,len,__FILE__,__LINE__);
end;

function Tcl_NewDoubleObj(val : longint) : longint;
begin
  Tcl_NewDoubleObj:=Tcl_DbNewDoubleObj(val,__FILE__,__LINE__);
end;

function Tcl_NewIntObj(val : longint) : longint;
begin
  Tcl_NewIntObj:=Tcl_DbNewLongObj(val,__FILE__,__LINE__);
end;

function Tcl_NewListObj(objc,objv : longint) : longint;
begin
  Tcl_NewListObj:=Tcl_DbNewListObj(objc,objv,__FILE__,__LINE__);
end;

function Tcl_NewLongObj(val : longint) : longint;
begin
  Tcl_NewLongObj:=Tcl_DbNewLongObj(val,__FILE__,__LINE__);
end;

function Tcl_NewObj : longint;
begin
  Tcl_NewObj:=Tcl_DbNewObj(__FILE__,__LINE__);
end;

function Tcl_NewStringObj(bytes,len : longint) : longint;
begin
  Tcl_NewStringObj:=Tcl_DbNewStringObj(bytes,len,__FILE__,__LINE__);
end;

function Tcl_NewWideIntObj(val : longint) : longint;
begin
  Tcl_NewWideIntObj:=Tcl_DbNewWideIntObj(val,__FILE__,__LINE__);
end;
*)

Function  Tcl_GetHashValue(h : PTcl_HashEntry) : ClientData; inline;
Begin
  Result := h^.clientData;
End;

Procedure Tcl_SetHashValue(h : PTcl_HashEntry; Value:ClientData); inline;
Begin
  h^.clientData := Value;
End;

Function  Tcl_GetHashKey  (tablePtr:PTcl_HashTable;h : PTcl_HashEntry) : PChar; inline;
Begin
  if (tablePtr^.keyType = TCL_ONE_WORD_KEYS) or (tablePtr^.keyType = TCL_CUSTOM_PTR_KEYS) then
    Result := h^.key.oneWordValue
  else
    Result := @(h^.key._string);
End;

(*Function Tcl_FindHashEntry  (tablePtr:PTcl_HashTable;key : PChar) : PTcl_HashEntry; inline;
Begin
  Result := tablePtr^.findProc(tablePtr,key);
End;

Function Tcl_CreateHashEntry(tablePtr:PTcl_HashTable;key : PChar;newPtr : pcint) : PTcl_HashEntry; inline;
Begin
  Result := tablePtr^.createProc(tablePtr,key,newPtr);
End;*)


(*
function Tcl_EvalObj(interp,objPtr : longint) : longint;
begin
  Tcl_EvalObj:=Tcl_EvalObjEx(interp,objPtr,0);
end;

function Tcl_GlobalEvalObj(interp,objPtr : longint) : longint;
begin
  Tcl_GlobalEvalObj:=Tcl_EvalObjEx(interp,objPtr,TCL_EVAL_GLOBAL);
end;
*)

end.
