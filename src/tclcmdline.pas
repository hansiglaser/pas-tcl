(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   Command line interpreter using GNU Readline and Tcl.                  *
 *                                                                         *
 *   This Pascal unit is licensed under the same terms as the original     *
 *   Tcl C header file. See the file "license.terms" for information on    *
 *   usage and redistribution of this file, and for a DISCLAIMER OF ALL    *
 *   WARRANTIES.                                                           *
 *                                                                         *
 ***************************************************************************)

Unit TclCmdLine;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, BaseUnix, CmdLine, ReadlineOOP, TCL, TclOOP;

Type

  { TTclCmdLine }

  (**
   * Command line handler for TCL interpreter
   *
   * Important: Only a single instance of this class is allowed! The reason is
   * that only one ^C signal handler is available.
   *)
  TTclCmdLine = class(TCmdLine)
  protected
    FTCL      : TTCLInterpreter;
    FSigCtrlC : Boolean;
    Function SigCtrlC(Code:Integer):Integer;
    Function GetCompletions(Text:PChar;Start,TheEnd:Integer):TDynArrString;
    Function GetCommands   (Text:PChar;Start,TheEnd:Integer):TDynArrString;
    Function GetVariables  (Text:PChar;Start,TheEnd:Integer):TDynArrString;
    Function IsComplete(Line:String) : Boolean;
    Function Execute(Command : String) : Boolean; virtual;
  public
    Constructor Create(AHistoryFile:String;AReadlineName:String;ATCL:TTCLInterpreter);
    Destructor  Destroy; override;

    Procedure CommandLoop; override;
  End;

Implementation

Var SigCtrlCAsyncHandler : PTcl_AsyncHandler;

{ TTclCmdLine }

Constructor TTclCmdLine.Create(AHistoryFile : String; AReadlineName : String; ATCL : TTCLInterpreter);
Begin
  inherited Create(AHistoryFile,AReadlineName);
  Readline.GetCompletions           := @Self.GetCompletions;
  Readline.IsMultiLineComplete      := @Self.IsComplete;
  inherited Execute                 := @Self.Execute;

  FTCL := ATCL;
  SigCtrlCAsyncHandler := FTCL.AsyncCreate(@Self.SigCtrlC);
End;

Destructor TTclCmdLine.Destroy;
Begin
  FTCL.AsyncDelete(SigCtrlCAsyncHandler);
  Inherited Destroy;
End;

Procedure TTclCmdLine.CommandLoop;
Begin
  FTCL.FreeFromEval := True; // avoid Tcl_DeleteInterp()
  // command line
  Inherited CommandLoop; // will exit from this loop with command "exit"
End;

(**
 * Signal handler for SIGINT
 *
 * The TCL interpreter doesn't like interruptions while it is doing its job
 * (e.g. if this signal is converted to an exception), so we have to rely on
 * the asynchronous events functionality.
 *)
Procedure SigCtrlC(Signal:Longint;Info:PSigInfo;Context:PSigContext); CDecl;
Begin
  Tcl_AsyncMark(SigCtrlCAsyncHandler);
End;

(**
 * Asynchronous event handler
 *
 * This function is executed to handle the asynchronous event as issued by the
 * SIGINT handler above. The interpreter checks after every command (also inside
 * of loops, ...) whether there are event handlers to process. This means that
 * it completes all commands before this handler is executed. E.g. "after 2000"
 * really delays for 2 seconds. Even playing with LimitSetTime() didn't help to
 * quit the delay (see function AfterDelay at
 * /tcl8.5.9/generic/tclTimer.c:1039).
 *)
Function TTclCmdLine.SigCtrlC(Code:Integer):Integer;
Begin
  WriteLn;   // pressing Ctrl-C only prints '^C' without a newline, so we add it
  FSigCtrlC := true;
  Result := TCL_ERROR;  // set to TCL_ERROR to stop the current script
End;

(*****************************************************************************)
(***  Completion  ************************************************************)
(*****************************************************************************)

Function TTclCmdLine.GetCompletions(Text:PChar;Start,TheEnd:Integer):TDynArrString;
//Type TTokenType = (ttUndefined,ttCommand,ttVariable,ttFilename);
//Var TokenType : TTokenType;
  Function IsCommand : Boolean;
  Var L : PChar;
      I : Integer;
  Begin
    { first word -> command }
    if Start = 0 then
      Exit(True);
    { search backwards in LineBuffer for a '[' or start-of-line }
    L := Readline.LineBuffer;
    I := Start-1;
    While (I >= 0) and (L[I] in [' ',^J,^M,^I]) do
      Dec(I);
    if L[I] = '[' then   // "backtick"-operator found -> command
      Exit(True);
    if I < 0 then    // start-of-line found -> command
      Exit(True);
    { no, other character -> no command }
    Result := False;
  End;

Begin
  Result := Nil;

//WriteLn('Text = ',Text,', Start = ',Start,', End = ',TheEnd);
//WriteLn('Line Buffer = ''',FReadline.LineBuffer,'''');
//WriteLn('Command Complete = ',Tcl_CommandComplete(FReadline.LineBuffer));

  { context dependent completion }
  if IsCommand then
    Result := GetCommands(Text,Start,TheEnd)
  else if Text[0] = '$' then
    { starts with '$' -> variable }
    Result := GetVariables(Text,Start,TheEnd)
  else
    { 2nd or later word -> filename }
    Readline.UseFilenames;   // re-enable filename completions
End;

Function TTclCmdLine.GetCommands(Text:PChar;Start,TheEnd:Integer):TDynArrString;
Var I : Integer;
    N : Integer;
    R : PTcl_Obj;
    P : PTcl_Obj;
Begin
  Result := Nil;
  if FTCL.Eval('info commands {'+Text+'*}') <> TCL_OK then
    Exit;

  // Get result object
  // Attention: this only works with Tcl_EvalEx (but not Tcl_Eval!) and only
  // before Tcl_GetStringResult!
  R := FTCL.GetObjResult;
  N := FTCL.ListObjLength(R);
  if N = 0 then Exit;

  SetLength(Result,N);
  For I := 0 to N-1 do
    Begin
      P := FTCL.ListObjIndex(R,I);
      Result[I] := Tcl_GetString(P);
    End;
End;

Function TTclCmdLine.GetVariables(Text:PChar;Start,TheEnd:Integer):TDynArrString;
Var I : Integer;
    N : Integer;
    R : PTcl_Obj;
    P : PTcl_Obj;
Begin
  Result := Nil;
  if FTCL.Eval('info vars {'+PChar(@(Text[1]))+'*}') <> TCL_OK then
    Exit;

  // Get result object
  // Attention: this only works with Tcl_EvalEx (but not Tcl_Eval!) and only
  // before Tcl_GetStringResult!
  R := FTCL.GetObjResult;
  N := FTCL.ListObjLength(R);
  if N = 0 then Exit;

  SetLength(Result,N);
  For I := 0 to N-1 do
    Begin
      P := FTCL.ListObjIndex(R,I);
      Result[I] := '$' + Tcl_GetString(P);
    End;
End;

Function TTclCmdLine.IsComplete(Line : String) : Boolean;
Begin
  Result := (Tcl_CommandComplete(PChar(Line)) <> 0);
End;

(*****************************************************************************)
(***  Command Execution ******************************************************)
(*****************************************************************************)

Function TTclCmdLine.Execute(Command:String) : Boolean;
Var Code          : Integer;
    St            : String;
    NewAct,OldAct : SigActionRec;
Begin
  // never exit CommandLoop
  Result := false;

  // install ^C handler
  NewAct.sa_handler := @TclCmdLine.SigCtrlC;
  NewAct.sa_flags   := SA_RESTART or SA_SIGINFO;  // SA_SIGINFO required that sa_sigaction(int, siginfo_t*, void*) instead of sa_handler(int) is called
  FpSigEmptySet(NewAct.sa_mask);
  if FpSigAction(SIGINT,@NewAct,@OldAct) <> 0 then
    raise EReadline.CreateFmt('Error installing SIGINT handler: %d (%s)',[FpGetErrno, '']);
  FSigCtrlC := false;

  // execute
  Code := FTCL.Eval(Command);

  // deactivate ^C handler
  if FpSigAction(SIGINT,@OldAct,Nil) <> 0 then
    raise EReadline.CreateFmt('Error installing old SIGINT handler: %d (%s)',[FpGetErrno, '']);
  if FSigCtrlC then
    Exit;   // don't handle result if ^C was pressed

  // handle result
  if (Code <> TCL_OK) then
    Write('Error: ');
  St := FTCL.GetStringResult;   // use 'Line' to avoid additional variable
  if St > '' then
    WriteLn(St);
End;

End.

