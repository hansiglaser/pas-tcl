(* demo3.pas
 * ------------------------------------------------
 * Copyright 2011 by Johann Glaser
 * ------------------------------------------------
 * Demo for tcloop.pas unit.
 * Creating the Tcl interpreter, registering new commands and a new data type,
 * command line.
 *
 * Try these commands with
 *   >> listobjtypes
 *   >> set x [randdata 12]
 *   >> setdata $x 4 0x99 0x88 0x77
 *   >> dump $x
 *   >> writeln $x
 *)
Program TclDemo3;

{$MODE ObjFpc}{$H+}

Uses
  Classes, SysUtils, TCL, TclOOP;

Type

  { TMyData }

  TMyData = class(TTCLObj)
  public
    class Function  GetTypePtr : PTcl_ObjType;          override;
    Function  SetFromAny(Interp:PTcl_Interp) : Integer; override;
    Function  UpdateString : AnsiString;                override;
    Procedure Copy(ASrc : TTclObj);                     override; // Copy Constructor
  protected
    Addr : Word;
    Data : AnsiString;    // use AnsiString to simplifiy memory management
  public
    Constructor Create(AAddr:Word;AData:AnsiString);
  End;

{ TMyData }

Class Function TMyData.GetTypePtr:PTcl_ObjType;
Const
  ObjType : Tcl_ObjType = (
    Name             : 'mydata';
    freeIntRepProc   : @FreeIntRepProc;
    dupIntRepProc    : @DupIntRepProc;
    updateStringProc : @UpdateStringProc;
    setFromAnyProc   : Nil; //@SetFromAnyProc;  // if this is nil, better not register the type
  );
Begin
  Result := @ObjType;
End;

Function TMyData.SetFromAny(Interp:PTcl_Interp):Integer;
Begin
WriteLn('TMyData.SetFromAny');
  Result := TCL_ERROR;
End;

(**
 * Helper function to convert binary data to a Hex dump
 *)
Function StrHexDump(Addr:Integer;Const Buf;Length:SizeUInt) : String;
Var I : SizeUInt;
Begin
  Result := IntToHex(Addr and $FFF0,4)+': '+StringOfChar(' ',3*(Addr and $000F));
  For I := 0 to Length-1 do
    Begin
      Result := Result + IntToHex((PByte(@Buf + I))^,2)+' ';
      if ((Addr+I) and $000F = $000F) and (I <> Length-1) then
        Begin
          Result := Result + ^J + IntToHex((Addr + I+1) and $FFF0,4) + ': ';
        End;
    End;
End;

Function TMyData.UpdateString:AnsiString;
Begin
WriteLn('TMyData.UpdateString');
  Result := StrHexDump(Addr,Data[1],Length(Data));
End;

Procedure TMyData.Copy(ASrc:TTclObj);
Begin
WriteLn('TMyData.Copy');
  Addr := (ASrc as TMyData).Addr;
  Data := (ASrc as TMyData).Data;
End;

Constructor TMyData.Create(AAddr:Word;AData:AnsiString);
Begin
WriteLn('TMyData.Create');
  inherited Create;
  Addr := AAddr;
  Data := AData;
  InvalidateString;
End;

(*****************************************************************************)

Type

  { TTestCmds }

  TTestCmds = class
  private
    FTCL : TTCLInterpreter;
    Procedure CheckObjType(Obj:TTCL_Object;ReqType:TTCLObjClass);
  public
    Constructor Create(ATCL:TTCLInterpreter);
    Procedure CmdMax         (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CmdWriteLn     (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CmdRandData    (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CmdDump        (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CmdSetData     (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure CmdListObjTypes(ObjC:Integer;ObjV:PPTcl_Object);
  End;

{ TTestCmds }

Constructor TTestCmds.Create(ATCL:TTCLInterpreter);
Begin
  inherited Create;
  FTCL := ATCL;
  // Register/override in the Tcl engine our new functions
  FTCL.CreateObjCommand('max',         @CmdMax,         nil);
  FTCL.CreateObjCommand('writeln',     @CmdWriteLn,     nil);
  FTCL.CreateObjCommand('randdata',    @CmdRandData,    nil);
  FTCL.CreateObjCommand('dump',        @CmdDump,        nil);
  FTCL.CreateObjCommand('setdata',     @CmdSetData,     nil);
  FTCL.CreateObjCommand('listobjtypes',@CmdListObjTypes,nil);
  // register new type (http://www.tcl.tk/man/tcl8.5/TclLib/ObjectType.htm)
  FTCL.RegisterObjType(TMyData.GetTypePtr);
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

(**
 * Create random data and return in a custom object type
 *)
Procedure TTestCmds.CmdRandData(ObjC : Integer; ObjV: PPTcl_Object);
Var Num  : Integer;
    Obj  : TMyData;
    Data : AnsiString;
    I    : Integer;
Begin
  // check argument count
  if ObjC <> 2 then
    raise Exception.Create('bad # arg: ' + objv^[0].AsPChar + ' num');
  // get argument
  Num := objv^[1].AsInteger(FTCL);
  // Check Num
  if (Num <= 0) or (Num > 1024) then
    raise Exception.Create('num must be between 1 and 1024');
  // Create object
  System.WriteLn('Creating ',Num,' Bytes of random data');
  SetLength(Data,Num);
  For I := 1 to Num do
    Data[I] := Chr(I);
  Obj := TMyData.Create($0000,Data);

  // Set the result for the our function.
  FTCL.SetObjResult(Obj.TCLObj);
End;

Procedure TTestCmds.CheckObjType(Obj:TTCL_Object;ReqType:TTCLObjClass);
Begin
  if ReqType.IsMyType(Obj) then
    Exit;
  // wrong type, complain
  if PTcl_Obj(Obj)^.typePtr <> Nil then
    raise Exception.Create('Expected data type "'+ReqType.GetTypePtr^.name+'" but got "'+PTcl_Obj(Obj)^.typePtr^.name+'"')
  else
    raise Exception.Create('Expected data type "'+ReqType.GetTypePtr^.name+'" but got an unknown type');
End;

(**
 * Print data to screen
 *)
Procedure TTestCmds.CmdDump(ObjC : Integer; ObjV: PPTcl_Object);
Var MyData : TMyData;
Begin
  // check argument count
  if ObjC <> 2 then
    raise Exception.Create('bad # arg: ' + objv^[0].AsPChar + ' data');
  // get argument
  CheckObjType(ObjV^[1],TMyData);
  MyData := TMyData.Get(ObjV^[1]) as TMyData;
  System.WriteLn(StrHexDump(MyData.Addr,MyData.Data[1],Length(MyData.Data)));
End;

(**
 * Modify binary data
 *)
Procedure TTestCmds.CmdSetData(ObjC : Integer; ObjV: PPTcl_Object);
Var MyData : TMyData;
    Pos    : Integer;
    Num    : Cardinal;
    I      : Integer;
    Val    : Integer;
    Bytes  : AnsiString;
Begin
  // check argument count
  if ObjC < 4 then
    raise Exception.Create('bad # arg: ' + objv^[0].AsPChar + ' data pos b0 [b1 ...]');
  // get arguments
  CheckObjType(ObjV^[1],TMyData);
  MyData := TMyData.Get(ObjV^[1]) as TMyData;
  Pos := objv^[2].AsInteger(FTCL); // raises an exception on error
  if Pos >= Length(MyData.Data) then
    raise Exception.Create('Position '+IntToStr(Pos)+' is out of range 0 to '+IntToStr(Length(MyData.Data)-1));
  Num := ObjC-3;
  SetLength(Bytes,Num);  // allocate memory
  For I := 3 to ObjC-1 do
    Begin
      Val := objv^[I].AsInteger(FTCL);
      if (Val > 255) or (Val < -128) then
        raise Exception.Create('Values cannot exceed byte range 0 to 0xFF or -128 to 127.');
      Bytes[I-2] := Chr(Val);
    End;
  Move(Bytes[1],MyData.Data[Pos+1],Num);
  MyData.InvalidateString;
End;

(**
 * Get a list of all object types
 *)
Procedure TTestCmds.CmdListObjTypes(ObjC : Integer; ObjV: PPTcl_Object);
Var Obj : PTcl_Obj;
Begin
  Obj := FTCL.NewObj;
  FTCL.AppendAllObjTypes(Obj);
  FTCL.SetObjResult(Obj);
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

