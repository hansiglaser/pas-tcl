(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   Test program demonstrating tcloop.pas. Creating the Tcl interpreter,  *
 *   executing file, registering new commands, call a function defined     *
 *   in the script.                                                        *
 *                                                                         *
 *   This is free and unencumbered software released into the public       *
 *   domain.                                                               *
 *                                                                         *
 *   Anyone is free to copy, modify, publish, use, compile, sell, or       *
 *   distribute this software, either in source code form or as a compiled *
 *   binary, for any purpose, commercial or non-commercial, and by any     *
 *   means.                                                                *
 *                                                                         *
 *   In jurisdictions that recognize copyright laws, the author or authors *
 *   of this software dedicate any and all copyright interest in the       *
 *   software to the public domain. We make this dedication for the        *
 *   benefit of the public at large and to the detriment of our heirs and  *
 *   successors. We intend this dedication to be an overt act of           *
 *   relinquishment in perpetuity of all present and future rights to this *
 *   software under copyright law.                                         *
 *                                                                         *
 *   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *
 *   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF    *
 *   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND                 *
 *   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY      *
 *   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,  *
 *   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE     *
 *   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                *
 *                                                                         *
 ***************************************************************************)

Program TclDemo2;

{$MODE ObjFpc}{$H+}

Uses
  Classes, SysUtils, TCL, TclOOP;

Type

  { TTestCmds }

  TTestCmds = class
  private
    FTCL : TTCLInterpreter;
  public
    Constructor Create(ATCL:TTCLInterpreter);
    Procedure CmdMax    (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CmdWriteLn(ObjC:Integer;ObjV:PPTcl_Object);
  End;

{ TTestCmds }

Constructor TTestCmds.Create(ATCL:TTCLInterpreter);
Begin
  inherited Create;
  FTCL := ATCL;
  // Register/override in the Tcl engine our new functions
  FTCL.CreateObjCommand('max',     @CmdMax,     nil);
  FTCL.CreateObjCommand('writeln', @CmdWriteLn, nil);
End;

Procedure TTestCmds.CmdMax(ObjC : Integer; ObjV: PPTcl_Object);
Var Idx, Value, MaxVal : Integer;
Begin
  MaxVal := 0;

  // `max` can be done with at least two digits, ObjV[0] holds the function name
  if ObjC < 3 then
    raise Exception.Create('bad # arg: ' + ObjV^[0].AsPChar + ' num1 num2 [..numN]');

  // ObjV[0] holds the function name, so start from index 1 for the first argument
  For Idx := 1 to ObjC-1 do
    Begin
      Value := ObjV^[Idx].AsInteger(FTCL);    // get argument as integer
      if (Value > MaxVal) then
        MaxVal := Value;          {* Calculate maximum number *}
    End;

  // Set the result
  FTCL.SetObjResult(FTCL.NewIntObj(MaxVal));
end;

Procedure TTestCmds.CmdWriteLn(ObjC : Integer; ObjV: PPTcl_Object);
Var I    : Integer;
    Buff : String;
Begin
  Buff := '';
  if ObjC > 1 then
    Buff := objv^[1].AsPChar;
  for I := 2 to objc-1 do
    Buff := Buff + ' ' + objv^[i].AsPChar;
  WriteLn(Buff);
End;

(*****************************************************************************)

Var
  TCLI : TTCLInterpreter;
  Test : TTestCmds;
  Code : Integer;
  St   : String;

begin
  TCLI := TTCLInterpreter.Create;
  Test := TTestCmds.Create(TCLI);  // create and register test commandos

  // Execute script
  Code := TCLI.EvalFile('test.tcl');
  if (Code <> TCL_OK) then                   // Is all okay?
    WriteLn(TCLI.GetStringResult);

  // Call a function `foo` defined in the script
  Code := TCLI.Eval('foo 1 2 3');
  if (Code <> TCL_OK) then
    WriteLn(TCLI.GetStringResult);

  // Command line
  repeat
    Write('>> ');
    ReadLn(St);
    Code := TCLI.Eval(St);
    if (Code <> TCL_OK) then
      Write('Error: ');
    St := TCLI.GetStringResult;
    if St > '' then
      WriteLn(St);
  Until false;

  // Release interpreter
  TCLI.Free;
End.

