(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   Improved command line interpreter with a few predefined commands.     *
 *                                                                         *
 *   This Pascal unit is licensed under the same terms as the original     *
 *   Tcl C header file. See the file "license.terms" for information on    *
 *   usage and redistribution of this file, and for a DISCLAIMER OF ALL    *
 *   WARRANTIES.                                                           *
 *                                                                         *
 ***************************************************************************)

Unit TclCmdLinePredef;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, ReadlineOOP, TclCmdLine, TclOOP;

Type

  { TTclCmdLinePredef }

  (**
   * Improved version of TTclCmdLine which can implement a few common commands
   *)
  TTclCmdLinePredef = class(TTclCmdLine)
  private
    FExitStatus : Integer;
    FManPath    : String;
    Function ManPathVar : TDynArrString;
    Procedure CmdExit(ObjC:Integer;ObjV:PPTcl_Object);
    Procedure History(ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CmdMan (ObjC:Integer;ObjV:PPTcl_Object);
  protected
    Function Execute(Command : String) : Boolean; override;
  public
    Constructor Create(AHistoryFile:String;AReadlineName:String;ATCL:TTCLInterpreter);
    // create the commands implemented by this class
    Procedure CreateCommandExit   (ACommand:String);
    Procedure CreateCommandHistory(ACommand:String);
    Procedure CreateCommandMan    (ACommand:String;AManPath:String);
    // internal functions, but also provided to the user
    Procedure Man(APage:String);
    Procedure WhatIs(APage:String);
    Procedure Apropos(AWord:String);
    // properties
    property ExitStatus : Integer read FExitStatus;
  End;

Implementation
Uses BaseUnix,Unix;

{ TTclCmdLinePredef }

Constructor TTclCmdLinePredef.Create(AHistoryFile : String; AReadlineName : String; ATCL : TTCLInterpreter);
Begin
  inherited Create(AHistoryFile,AReadlineName,ATCL);
  FExitStatus := -1;
End;

(**
 * Create a command to exit the TCL command loop
 *
 * The standard TCL command "exit" will terminate the program by its own (using
 * Tcl_Exit() (see http://www.tcl.tk/man/tcl8.5/TclLib/Exit.htm). For our own
 * clean up, we could register our own exit procedure using Tcl_SetExitProc()
 * which also has to call Tcl_Finalize() to cleanup the TCL interpreter stuff
 * and then Halt() to quit the program.
 *
 * For a cleaner approach, the TCL command "exit" can be overridden here. When
 * the user calls "exit", the procedure CmdExit() (see below) is executed. This
 * sets our member variable to a non-negative value. This is detected by the
 * method Execute() (see below), which in turn returns "true" to signal the
 * TCmdLine.CommandLoop to exit the loop.
 *
 *)
Procedure TTclCmdLinePredef.CreateCommandExit(ACommand : String);
Begin
  FTCL.CreateObjCommand(ACommand,@Self.CmdExit,nil);
End;

Procedure TTclCmdLinePredef.CreateCommandHistory(ACommand : String);
Begin
  FTCL.CreateObjCommand(ACommand,@Self.History,nil);
End;

Procedure TTclCmdLinePredef.CreateCommandMan(ACommand:String;AManPath:String);
Begin
  FManPath := AManPath;
  FTCL.CreateObjCommand(ACommand,@Self.CmdMan,nil);
End;

(*****************************************************************************)
(***  TCL Functions  *********************************************************)
(*****************************************************************************)

Procedure TTclCmdLinePredef.CmdExit(ObjC : Integer; ObjV : PPTcl_Object);
Begin
  if ObjC = 1 then
    Begin
      // "exit" without parameter, default to 0
      FExitStatus := 0;
    End
  else if ObjC = 2 then
    Begin
      // "exit n" with one numeric parameter
      FExitStatus := ObjV^[1].AsInteger(FTCL) and $000000FF;
    End
  else
    Begin
      // wrong number of parameters
      raise Exception.Create('wrong # args: should be "exit ?returnCode?"');
    End;
End;

Procedure TTclCmdLinePredef.History(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  inherited History.Print;
End;

Function Exec(Command:AnsiString;Args:Array of AnsiString;Env:Array of AnsiString;Wait:Boolean):CInt;
Var Pid     : TPid;
    CArgs   : PPChar;
    CEnv    : PPChar;
    I,J     : Integer;
Begin
  Pid := FpFork;
  if Pid < 0 then
    // error
    Exit(-fpGetErrNo)
  else if Pid = 0 then
    // we are in the child process
    Begin
      // prepare parameters
      GetMem(CArgs,(2+Length(Args))*SizeOf(PChar));
      CArgs[0] := PChar(Command);
      For I := 0 to Length(Args)-1 do
        CArgs[I+1] := PChar(Args[I]);
      CArgs[Length(Args)+1] := Nil;
      // prepare environment
      if Length(Env) > 0 then
        Begin
          I := 0;
          While EnvP[I] <> Nil do   // determine size of current environment
            Inc(I);
          GetMem(CEnv,(I+Length(Env)+1) * SizeOf(PChar));  // allocate new list
          Move(EnvP^,CEnv^,I * SizeOf(PChar));  // copy list
          For J := 0 to Length(Env)-1 do
            CEnv[I+J] := PChar(Env[J]);
          CEnv[I+Length(Env)] := Nil;
        End
      else
        CEnv := EnvP;
      // execute
      FpExecve(Command,CArgs,CEnv);  // will not return
    End
  else
    // we are still in the parent process: wait if Block = 1
    Begin
      if Wait then
        Result := WaitProcess(Pid)
      else
        Result := 0;
    End;
End;

Procedure TTclCmdLinePredef.CmdMan(ObjC : Integer; ObjV: PPTcl_Object);
Begin
  if ObjC = 2 then  // one argument -> show man page
    Man(ObjV^[1].AsPChar)
  else              // no argument -> list all commands
    WhatIs('');
End;

(*****************************************************************************)
(***  Internal Functions  ****************************************************)
(*****************************************************************************)

Function TTclCmdLinePredef.Execute(Command : String) : Boolean;
Begin
  Result := Inherited Execute(Command);
  // if "exit" was called, FExitStatus >= 0)
  if FExitStatus < 0 then
    Exit(false);
  // FExitStatus >= 0 -> "exit" was called, prepare to quit the application
  // exit CommandLoop
  Result := true;
  FTCL.FreeFromEval := False; // reactivate Tcl_DeleteInterp()
End;

Function TTclCmdLinePredef.ManPathVar : TDynArrString;
Begin
  if FManPath > '' then
    Begin
      SetLength(Result,1);
      Result[0] := 'MANPATH=' + FManPath;
    End
  else
    SetLength(Result,0);
End;

Procedure TTclCmdLinePredef.Man(APage:String);
Begin
  Exec('/usr/bin/man',[APage],ManPathVar,true);
End;

Procedure TTclCmdLinePredef.WhatIs(APage:String);
Begin
  if APage = '' then
    Exec('/usr/bin/whatis',['--wildcard','*'],ManPathVar,true)
  else
    Exec('/usr/bin/whatis',[APage],ManPathVar,true);
End;

Procedure TTclCmdLinePredef.Apropos(AWord : String);
Begin
  Exec('/usr/bin/apropos',[AWord],ManPathVar,true);
End;

End.

