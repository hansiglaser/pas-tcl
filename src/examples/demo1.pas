(* demo1.pas
 * ------------------------------------------------
 * Copyright 2002 by Bert Raccoon aka Max Artemev
 * (bert@furry.ru, bert_raccoon@freemail.ru)
 * ------------------------------------------------
 * Copyright 2011 by Johann Glaser
 * Improved to use new TCL 8.5 binding, everything
 * with internal object representation.
 * ------------------------------------------------
 * Demo for tcl.pas unit.
 * Creating the Tcl interpreter, executing file, registering
 * new commands, call a function defined in the script.
 *
 *)
Program TclDemo1;

{$MODE ObjFpc}{$H+}

Uses
  SysUtils, TCL;

(**
 * Calculate integer maximum of arguments
 *)
Function CmdMax(ClientData:ClientData;Interp:PTcl_Interp;ObjC:Integer;ObjV:PPTcl_Obj):Integer; CDecl;
Var Idx, Value, MaxVal : Integer;
Begin
  Result := TCL_ERROR;         // default return value: error
  MaxVal := 0;

  // `max` can be done with at least two digits, ObjV[0] holds the function name
  if ObjC < 3 then
    Begin
      Tcl_AppendResult(Interp,['bad # arg: ', Tcl_GetString(ObjV^[0]), ' num1 num2 [..numN]', nil]);
      Exit;
    End;

  // ObjV[0] holds the function name, so start from index 1 for the first argument
  For Idx := 1 to ObjC-1 do
    Begin
      if Tcl_GetIntFromObj(Interp,ObjV^[Idx],Value) <> TCL_OK then    // get argument as integer
        Exit;    // GetIntFromObj already set result to 'expected integer but got "..."'
      if (Value > MaxVal) then
        MaxVal := Value;          {* Calculate maximum number *}
    End;

  // Set the result
  Tcl_SetObjResult(Interp,Tcl_NewIntObj(MaxVal));

  // exit successfully
  Result := TCL_OK;
End;

{*
 *  Old and good known Pascal procedure :)
 *}
Function CmdWriteLn(ClientData:ClientData;Interp:PTcl_Interp;ObjC:Integer;ObjV:PPTcl_Obj):Integer; CDecl;
Var I    : Integer;
    Buff : String;
Begin
  Buff := '';
  If ObjC > 1 then
    Buff := Tcl_GetString(ObjV^[1]);
  For I := 2 to ObjC-1 do
    Buff := Buff + ' ' + Tcl_GetString(ObjV^[I]);
  WriteLn(Buff);
  Result := TCL_OK;
End;

Var
  Interp : PTcl_Interp;
  Code   : Integer;
  St     : String;
Begin
  Interp := Tcl_CreateInterp();  // Create an interpreter
  Tcl_Init(interp);              // Initialize

  // Register/override in the Tcl engine our new functions
  Tcl_CreateObjCommand(Interp,'max',    @CmdMax,    nil,nil);
  Tcl_CreateObjCommand(Interp,'writeln',@CmdWriteLn,nil,nil);

  // Execute script
  Code := Tcl_EvalFile(interp,'test.tcl');
  if (Code <> TCL_OK) then                   // Is all okay?
    WriteLn(Tcl_GetStringResult(Interp));

  // Call a function `foo` defined in the script
  Code := Tcl_VarEval(Interp,['foo ','1 2 3',nil]);
  if (Code <> TCL_OK) then
    WriteLn(Tcl_GetStringResult(Interp));

  // Command line
  repeat
    Write('>> ');
    ReadLn(St);
    Code := Tcl_Eval(Interp,PChar(St));
    if (Code <> TCL_OK) then
      Write('Error: ');
    St := Tcl_GetStringResult(Interp);
    if St > '' then
      WriteLn(St);
  Until false;
  
  // Release interpreter
  Tcl_DeleteInterp(interp);                  
End.

