(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   TCL OOP Wrapper Unit                                                  *
 *                                                                         *
 *   This Pascal unit is licensed under the same terms as the original     *
 *   Tcl C header file. See the file "license.terms" for information on    *
 *   usage and redistribution of this file, and for a DISCLAIMER OF ALL    *
 *   WARRANTIES.                                                           *
 *                                                                         *
 ***************************************************************************)

(**
 * TCL OOP Wrapper Unit
 *
 * (c) 2011 by Johann Glaser
 *
 * This OOP wrapper is based on the TCL header translations for TCL 8.5.9.
 *
 * Documentation
 *  - TCL C functions (TCL Library) http://www.tcl.tk/man/tcl8.5/TclLib/contents.htm
 *  - TCL commands http://www.tcl.tk/man/tcl8.5/TclCmd/contents.htm
 *
 *)
Unit TclOOP;

{$MODE ObjFpc}{$H+}

Interface

Uses
  Classes, SysUtils, TCL;

Type
  TTCLInterpreter = class;    // forward declataion


  (**
   * TTcl_Object: Trick to "overload" PTcl_Obj with methods
   *
   * We create an "alias" for PTcl_Obj as a class (note: not a pointer to a
   * class) which has the same memory footprint as the the PTcl_Obj itself:
   * it contains one pointer variable, nothing else.
   *
   * PPTcl_Obj is a pointer to an array of PTcl_Obj.
   * PPTcl_Object is a pointer to an array of TTcl_Object.
   *
   * TObjCmdProc uses ObjV : PPTcl_Object, so it is easy to to use the nice
   * methods:
   *   ObjV^[1].AsString
   *   ObjV^[1].AsInteger(FTCL)
   *
   *)
  TTcl_Object = object
    PObj : PTcl_Obj;
    Function AsInteger(ATCL:TTCLInterpreter) : Integer;
    Function AsPChar   : PChar;
    Function AsString  : String;
  End;
  APTcl_Object = Array[0..65535] of TTcl_Object;
  PPTcl_Object = ^APTcl_Object;

  // function pointer type for TCL commands implemented in Pascal
  TObjCmdProc    = Procedure(ObjC:Integer;ObjV:PPTcl_Object) of object;
  // default is TCL_OK, if an exception is thrown: TCL_ERROR
  // function pointer type for the delete callback when removing functions implemented in Pascal
  TCmdDeleteProc = procedure of object;

  // information record for all custom TCL commands implemented in Pascal
  PTclCmdProcInfo = ^TTclCmdProcInfo;
  TTclCmdProcInfo = record
    TCL     : TTCLInterpreter;
    Proc    : TObjCmdProc;
    DelProc : TCmdDeleteProc;
    // we don't implement an client data field here, because we assume the user
    // has stored all context information in the class instance of Proc.
  End;

  TAsyncProc = Function(Code:Integer):Integer of object;
  // information record for all custom TCL async handlers implemented in Pascal
  PTclAsyncProcInfo = ^TTclAsyncProcInfo;
  TTclAsyncProcInfo = record
    TCL     : TTCLInterpreter;
    Proc    : TAsyncProc;
    // we don't implement an client data field here, because we assume the user
    // has stored all context information in the class instance of Proc.
  End;

  { TTCLInterpreter }

  (**
   * OOP Wrapper for the TCL interpreter
   *
   *
   *)
  TTCLInterpreter = class
  protected
    FInterp   : PTcl_Interp;
    FProcInfo : Array of PTclCmdProcInfo;  // list of our own commands
    FFreeFromEval : Boolean;
  public
    Constructor Create;
    Destructor  Destroy; override;
    property FreeFromEval : Boolean read FFreeFromEval write FFreeFromEval;  // set to true if you Free() this object from within an eval()d command

    // Register a new TCL command implemented by Proc
    Function  CreateObjCommand(CmdName:String;Proc:Tcl_ObjCmdProc;ClientData:ClientData;DeleteProc:Tcl_CmdDeleteProc) : PTcl_Command;
    // Register a new TCL command implemented by the pascal function Proc
    Function  CreateObjCommand(CmdName:String;Proc:TObjCmdProc;DeleteProc:TCmdDeleteProc) : PTcl_Command;
    // Tcl_EvalFile
    Function  EvalFile(Filename : String) : Integer;
    Function  GetObjResult : PTcl_Obj;
    Function  GetStringResult : String;
    Function  Eval(Script : String) : Integer;

    //procedure AppendResult(args: array of PChar);
    Procedure AppendResult(St:String);
    Procedure SetObjResult(resultObjPtr:PTcl_Obj);
    Procedure SetObjResult(intValue:Integer);
    Procedure SetResult(St:String);
    Function  NewIntObj(intValue:Integer) : PTcl_Obj;
    Function  GetIntFromObj(pObj:pTcl_Obj; Out Int:Integer):Integer;
    Function  GetIntFromObj(pObj:pTcl_Obj):Integer;
    Function  NewObj : PTcl_Obj;

    (**
     * List Object
     *)
    Procedure ListObjAppendList   (listPtr:PTcl_Obj; elemListPtr:PTcl_Obj);
    Procedure ListObjAppendElement(listPtr:PTcl_Obj; objPtr:PTcl_Obj);
    Function  NewListObj          (                 objc:Integer; objv:PPTcl_Obj):PTcl_Obj;
    Procedure SetListObj          (objPtr:PTcl_Obj; objc:Integer; objv:PPTcl_Obj);
    Procedure ListObjGetElements  (listPtr:PTcl_Obj; Out objc:Integer; Out objv:PPTcl_Obj);
    Function  ListObjLength       (listPtr:PTcl_Obj) : Integer;
    Function  ListObjIndex        (listPtr:PTcl_Obj; index:Integer):PTcl_Obj;
    Procedure ListObjReplace      (listPtr:PTcl_Obj; first:Integer; count:Integer; objc:Integer;objv:PPTcl_Obj);

    (**
     * ObjType
     *)
    Procedure RegisterObjType(TypePtr:PTcl_ObjType);
    Function  GetObjType(TypeName:String) : PTcl_ObjType;
    Function  AppendAllObjTypes(Obj:PTcl_Obj) : Integer;
    Function  ConvertToType(Obj:PTcl_Obj;TypePtr:PTcl_ObjType) : Integer;

    (**
     * TCL Variables
     *)
    Procedure SetVar  (Name,             Value:String;  Flags:Integer=0);
    Procedure SetVar  (Name:      String;Value:PChar;   Flags:Integer=0);
    Procedure SetVar  (Name,Index,       Value:String;  Flags:Integer=0);
    Procedure SetVar  (Name,Index:String;Value:PChar;   Flags:Integer=0);
    Procedure SetVar  (Name:      String;Value:PTcl_Obj;Flags:Integer=0);
    Procedure SetVar  (Name,Index:String;Value:PTcl_Obj;Flags:Integer=0);
    Function  GetVar  (Name:      String;Flags:Integer=0) : PChar;
    Function  GetVar  (Name,Index:String;Flags:Integer=0) : PChar;
    Function  GetVarEx(Name:      String;Flags:Integer=0) : PTcl_Obj;
    Function  GetVarEx(Name,Index:String;Flags:Integer=0) : PTcl_Obj;

    (**
     * Async Handlers
     *)
    Function  AsyncCreate(proc:Tcl_AsyncProc; clientData:ClientData):PTcl_AsyncHandler;
    Function  AsyncCreate(Proc:TAsyncProc):PTcl_AsyncHandler;
    Procedure AsyncDelete(async:PTcl_AsyncHandler);
    Function  AsyncInvoke(code:Integer):Integer;
    Procedure AsyncMark(async:PTcl_AsyncHandler);
    Function  AsyncReady:Boolean;

    (**
     * Limit
     *)
    Procedure LimitAddHandler(_type:Integer; handlerProc:Tcl_LimitHandlerProc; clientData:ClientData; deleteProc:Tcl_LimitHandlerDeleteProc);
    Procedure LimitRemoveHandler(_type:Integer; handlerProc:Tcl_LimitHandlerProc; clientData:ClientData);
    Function  LimitReady:Integer;
    Function  LimitCheck:Integer;
    Function  LimitExceeded:Integer;
    Procedure LimitSetCommands(commandLimit:Integer);
    Procedure LimitSetTime(timeLimitPtr:PTcl_Time);
    Procedure LimitSetGranularity(_type:Integer; granularity:Integer);
    Function  LimitTypeEnabled(_type:Integer):Integer;
    Function  LimitTypeExceeded(_type:Integer):Integer;
    Procedure LimitTypeSet(_type:Integer);
    Procedure LimitTypeReset(_type:Integer);
    Function  LimitGetCommands:Integer;
    Procedure LimitGetTime(timeLimitPtr:PTcl_Time);
    Function  LimitGetGranularity(_type:Integer):Integer;

  private
    Function  NewProc : PTclCmdProcInfo;
    Procedure DelProc(ProcInfo:PTclCmdProcInfo);
  End;

  { TTCLObj }

  (**
   * Custom TCL object type base class
   *
   * http://www.tcl.tk/man/tcl8.5/TclLib/ObjectType.htm
   *
   * To implement custom TCL object types, derive from this class and implement
   * all abstract methods.
   *
   * GetTypePtr has to return a pointer to a Tcl_ObjType record with the four
   * function pointers approprietly set. Use the three cdecl functions below
   * and implement your own SetFormAnyProc.
   *)
  TTCLObj = class
  private
    FTclObj : PTcl_Obj;
  public
    class Function  Get(Obj:PTcl_Obj)    : TTclObj; inline;
    class Function  Get(Obj:TTcl_Object) : TTclObj; inline;
    class Function  GetTypePtr : PTcl_ObjType;          virtual; abstract; // return static information on this type
    Function  SetFromAny(Interp:PTcl_Interp) : Integer; virtual; abstract; // set value from any other representation
    Function  UpdateString : AnsiString;                virtual; abstract; // update string representation
    Procedure Copy(ASrc : TTclObj);                     virtual; abstract; // Copy Constructor
  public
    Constructor Create;
    Constructor Create(ATclObj : PTcl_Obj);
    Procedure InvalidateString;
    class Function  IsMyType(ATclObj : PTcl_Obj) : Boolean;
    class Function  IsMyType(ATclObj : TTcl_Object) : Boolean;
    property TclObj : PTcl_Obj read FTclObj;
  End;
  TTCLObjClass = class of TTCLObj;

Procedure UpdateStringProc(Obj:PTcl_Obj); cdecl;
Procedure DupIntRepProc   (Src:PTcl_Obj;Dst:PTcl_Obj); cdecl;
Procedure FreeIntRepProc  (Obj:PTcl_Obj); cdecl;

{ helpber functions }

Procedure VarDump(Obj:PTcl_Obj);

Implementation

{ TTCLInterpreter }

Constructor TTCLInterpreter.Create;
Begin
  inherited Create;
  FInterp := Tcl_CreateInterp();  // Create an interpreter
  Tcl_Init(FInterp);              // Initialize: source init.tcl from TCL library path
End;

Destructor TTCLInterpreter.Destroy;
Begin
  (* If the user called the TCL "exit" function and he installed his own
   * ExitHandler from where he Free()s this object, then TCL will complain
   *   "DeleteInterpProc called with active evals"
   * because FInterp is still interpreting the "exit" command. Looking at
   * Tcl_Exit shows, that they don't care about the memory occupied by this
   * interpreter (and all its variables and procs), since the application will
   * be exited anyway.
   *
   * Unfortunately we don't have the possibility to find out whether this
   * object is Free()d from "exit" or without that. Therefore we introduce the
   * variable FFreeFromEval, which should be set to True before Free()ing
   * this class, if executed from somewhere inside an eval()d command.
   *)
  if not FFreeFromEval then
    Tcl_DeleteInterp(FInterp);      // Release interpreter
  // finalize TCL
  Tcl_Finalize;
  Inherited Destroy;
End;

Function TTCLInterpreter.CreateObjCommand(CmdName:String;Proc:Tcl_ObjCmdProc;ClientData:ClientData;DeleteProc:Tcl_CmdDeleteProc):PTcl_Command;
Begin
  Result := Tcl_CreateObjCommand(FInterp,PChar(CmdName),Proc,ClientData,DeleteProc);
End;

(**
 * Custom TCL command caller helper function
 *
 * This function is used for every registered custom TCL commands implemented
 * as Pascal Functions.
 *)
Function CmdCaller(clientData:ClientData;interp:pTcl_Interp;objc:Integer;objv:PPTcl_Obj): Integer; cdecl;
Var ProcInfo : PTclCmdProcInfo;
Begin
  ProcInfo := PTclCmdProcInfo(clientData);
  try
    ProcInfo^.Proc(objc,PPTcl_Object(objv));
    Result := TCL_OK;
  except  // catch exceptions and "convert" to a TCL error
    on E : Exception do
      Begin
        ProcInfo^.TCL.SetResult(E.Message);     // replace previous error strings in result (instead of AppendResult)
        Result := TCL_ERROR;
      End;
  End;
End;

(**
 * Custom TCL command deletion helper function
 *
 * This function is used for every registered custom TCL commands implemented
 * as Pascal Functions to remove.
 *)
Procedure CmdDeleter(clientData:ClientData); cdecl;
Var ProcInfo : PTclCmdProcInfo;
Begin
  { call delete procedure }
  ProcInfo := PTclCmdProcInfo(clientData);
  if ProcInfo^.DelProc <> Nil then
    ProcInfo^.DelProc;
  { remove this cmd from the TCL interpreter }
  ProcInfo^.TCL.DelProc(ProcInfo);
End;

(**
 * Register a custom TCL function implemented in Pascal
 *
 * http://www.tcl.tk/man/tcl8.5/TclLib/CrtObjCmd.htm
 *)
Function TTCLInterpreter.CreateObjCommand(CmdName:String;Proc:TObjCmdProc;DeleteProc:TCmdDeleteProc):PTcl_Command;
Var ProcInfo : PTclCmdProcInfo;
Begin
  ProcInfo := NewProc;
  ProcInfo^.TCL     := Self;
  ProcInfo^.Proc    := Proc;
  ProcInfo^.DelProc := DeleteProc;
  Result := CreateObjCommand(CmdName,@CmdCaller,ClientData(ProcInfo),@CmdDeleter);
End;

Function TTCLInterpreter.EvalFile(Filename:String):Integer;
Begin
  Result := Tcl_EvalFile(FInterp,PChar(Filename));   // Execute script
End;

Function TTCLInterpreter.GetObjResult:PTcl_Obj;
Begin
  Result := Tcl_GetObjResult(FInterp);
End;

Function TTCLInterpreter.GetStringResult:String;
Begin
  Result := Tcl_GetStringResult(FInterp);   // http://www.tcl.tk/man/tcl8.5/TclLib/SetResult.htm
End;

// http://www.tcl.tk/man/tcl8.5/TclLib/Eval.htm
Function TTCLInterpreter.Eval(Script:String):Integer;
Begin
  Result := Tcl_EvalEx(FInterp,PChar(Script),Length(Script),TCL_EVAL_DIRECT);
End;

(*Procedure TTCLInterpreter.AppendResult(args:Array Of PChar);
Var I : Integer;
Begin
  For I := 0 to Length(Args)-1 do
    Tcl_AppendResult(FInterp,[Args[I],Nil]);
End;*)

Procedure TTCLInterpreter.AppendResult(St:String);
Begin
  Tcl_AppendResult(FInterp,[PChar(St),Nil]);
End;

Procedure TTCLInterpreter.SetObjResult(resultObjPtr:PTcl_Obj);
Begin
  Tcl_SetObjResult(FInterp,resultObjPtr);
End;

Procedure TTCLInterpreter.SetObjResult(intValue : Integer);
Begin
  Tcl_SetObjResult(FInterp,Tcl_NewIntObj(intValue));
End;

Procedure TTCLInterpreter.SetResult(St:String);
Begin
  Tcl_SetObjResult(FInterp,Tcl_NewStringObj(PChar(St),Length(St)));
End;

Function TTCLInterpreter.NewIntObj(intValue:Integer):PTcl_Obj;
Begin
  Result := Tcl_NewIntObj(intValue);
End;

Function TTCLInterpreter.GetIntFromObj(pObj:pTcl_Obj;Out Int:Integer):Integer;
Begin
  Result := Tcl_GetIntFromObj(FInterp,pObj,Int);
End;

Function TTCLInterpreter.GetIntFromObj(pObj:pTcl_Obj):Integer;
Begin
  if Tcl_GetIntFromObj(FInterp,pObj,Result) <> TCL_OK then
    raise Exception.Create(Tcl_GetStringResult(FInterp));// Fmt('Couldn''t convert "%s" to an integer',[Tcl_GetString(pObj)]);
End;

Function TTCLInterpreter.NewObj:PTcl_Obj;
Begin
  Result := Tcl_NewObj;     // http://www.tcl.tk/man/tcl8.5/TclLib/Object.htm
End;

(**
 * List Object
 *)

Procedure TTCLInterpreter.ListObjAppendList(listPtr:PTcl_Obj;elemListPtr:PTcl_Obj);
Begin
  if Tcl_ListObjAppendList(FInterp,listPtr,elemListPtr) <> TCL_OK then
    raise Exception.Create(Tcl_GetStringResult(FInterp));
End;

Procedure TTCLInterpreter.ListObjAppendElement(listPtr:PTcl_Obj;objPtr:PTcl_Obj);
Begin
  if Tcl_ListObjAppendElement(FInterp,listPtr,objPtr) <> TCL_OK then
    raise Exception.Create(Tcl_GetStringResult(FInterp));
End;

Function TTCLInterpreter.NewListObj(objc:Integer;objv:PPTcl_Obj):PTcl_Obj;
Begin
  Result := Tcl_NewListObj(objc,objv);
End;

Procedure TTCLInterpreter.SetListObj(objPtr:PTcl_Obj;objc:Integer;objv:PPTcl_Obj);
Begin
  Tcl_SetListObj(objPtr,objc,objv);
End;

Procedure TTCLInterpreter.ListObjGetElements(listPtr:PTcl_Obj;Out objc:Integer;Out objv:PPTcl_Obj);
Begin
  if Tcl_ListObjGetElements(FInterp,listPtr,objc,objv) <> TCL_OK then
    raise Exception.Create(Tcl_GetStringResult(FInterp));
End;

Function TTCLInterpreter.ListObjLength(listPtr:PTcl_Obj):Integer;
Begin
  if Tcl_ListObjLength(FInterp,listPtr,Result) <> TCL_OK then
    raise Exception.Create(Tcl_GetStringResult(FInterp));
End;

Function TTCLInterpreter.ListObjIndex(listPtr:PTcl_Obj;index:Integer):PTcl_Obj;
Begin
  if Tcl_ListObjIndex(FInterp,listPtr,index,Result) <> TCL_OK then
    raise Exception.Create(Tcl_GetStringResult(FInterp));
End;

Procedure TTCLInterpreter.ListObjReplace(listPtr:PTcl_Obj;first:Integer;count:Integer;objc:Integer;objv:PPTcl_Obj);
Begin
  if Tcl_ListObjReplace(FInterp,listPtr,first,count,objc,objv) <> TCL_OK then
    raise Exception.Create(Tcl_GetStringResult(FInterp));
End;

(**
 * ObjType
 *)

Procedure TTCLInterpreter.RegisterObjType(TypePtr:PTcl_ObjType);
Begin
  Tcl_RegisterObjType(TypePtr);
End;

Function TTCLInterpreter.GetObjType(TypeName:String):PTcl_ObjType;
Begin
  Result := Tcl_GetObjType(PChar(TypeName));
End;

Function TTCLInterpreter.AppendAllObjTypes(Obj:PTcl_Obj):Integer;
Begin
  Result := Tcl_AppendAllObjTypes(FInterp,Obj);
End;

Function TTCLInterpreter.ConvertToType(Obj:PTcl_Obj;TypePtr:PTcl_ObjType):Integer;
Begin
  Result := Tcl_ConvertToType(FInterp,Obj,TypePtr);
End;

Procedure TTCLInterpreter.SetVar(Name, Value: String; Flags: Integer);
Begin
  Tcl_SetVar(FInterp,PChar(Name),PChar(Value),Flags);
End;

Procedure TTCLInterpreter.SetVar(Name: String; Value: PChar; Flags: Integer);
Begin
  Tcl_SetVar(FInterp,PChar(Name),Value,Flags);
End;

Procedure TTCLInterpreter.SetVar(Name, Index, Value: String; Flags: Integer);
Begin
  Tcl_SetVar2(FInterp,PChar(Name),PChar(Index),PChar(Value),Flags);
End;

Procedure TTCLInterpreter.SetVar(Name, Index: String; Value: PChar; Flags: Integer);
Begin
  Tcl_SetVar2(FInterp,PChar(Name),PChar(Index),Value,Flags);
End;

Procedure TTCLInterpreter.SetVar(Name: String; Value: PTcl_Obj; Flags: Integer);
Begin
  Tcl_SetVar2Ex(FInterp,PChar(Name),Nil,Value,Flags);
End;

Procedure TTCLInterpreter.SetVar(Name, Index: String; Value: PTcl_Obj; Flags: Integer);
Begin
  Tcl_SetVar2Ex(FInterp,PChar(Name),PChar(Index),Value,Flags);
End;

Function TTCLInterpreter.GetVar(Name: String; Flags: Integer): PChar;
Begin
  Result := Tcl_GetVar(FInterp,PChar(Name),Flags);
End;

Function TTCLInterpreter.GetVar(Name, Index: String; Flags: Integer): PChar;
Begin
  Result := Tcl_GetVar2(FInterp,PChar(Name),PChar(Index),Flags);
End;

Function TTCLInterpreter.GetVarEx(Name: String; Flags: Integer): PTcl_Obj;
Begin
  Result := Tcl_GetVar2Ex(FInterp,PChar(Name),Nil,Flags);
End;

Function TTCLInterpreter.GetVarEx(Name, Index: String; Flags: Integer): PTcl_Obj;
Begin
  Result := Tcl_GetVar2Ex(FInterp,PChar(Name),PChar(Index),Flags);
End;

Function TTCLInterpreter.AsyncCreate(proc: Tcl_AsyncProc; clientData: ClientData): PTcl_AsyncHandler;
Begin
  Result := Tcl_AsyncCreate(proc, clientData);
End;

Function AsyncSignalHandler(clientData:ClientData; var interp:Tcl_Interp; code:Integer):Integer;cdecl;
Var ProcInfo : PTclAsyncProcInfo;
Begin
  ProcInfo := PTclAsyncProcInfo(clientData);
  Result := ProcInfo^.Proc(Code);
End;

Function TTCLInterpreter.AsyncCreate(Proc: TAsyncProc): PTcl_AsyncHandler;
Var ProcInfo : PTclAsyncProcInfo;
Begin
  New(ProcInfo);   // probably we should also make an internal list, similar to CreateObjCommand
  ProcInfo^.TCL     := Self;
  ProcInfo^.Proc    := Proc;
  Result := Tcl_AsyncCreate(@AsyncSignalHandler,ClientData(ProcInfo));
End;

Procedure TTCLInterpreter.AsyncDelete(async: PTcl_AsyncHandler);
Begin
  Tcl_AsyncDelete(async);
End;

Function TTCLInterpreter.AsyncInvoke(code: Integer): Integer;
Begin
  Result := Tcl_AsyncInvoke(FInterp,code);
End;

Procedure TTCLInterpreter.AsyncMark(async: PTcl_AsyncHandler);
Begin
  Tcl_AsyncMark(async);
End;

Function TTCLInterpreter.AsyncReady: Boolean;
Begin
  Result := (Tcl_AsyncReady <> 0);
End;

Procedure TTCLInterpreter.LimitAddHandler(_type: Integer; handlerProc: Tcl_LimitHandlerProc; clientData: ClientData; deleteProc: Tcl_LimitHandlerDeleteProc);
Begin
  Tcl_LimitAddHandler(FInterp,_type,handlerProc,clientData,deleteProc);
End;

Procedure TTCLInterpreter.LimitRemoveHandler(_type: Integer; handlerProc: Tcl_LimitHandlerProc; clientData: ClientData);
Begin
  Tcl_LimitRemoveHandler(FInterp,_type,handlerProc,clientData);
End;

Function TTCLInterpreter.LimitReady: Integer;
Begin
  Result := Tcl_LimitReady(FInterp);
End;

Function TTCLInterpreter.LimitCheck: Integer;
Begin
  Result := Tcl_LimitCheck(FInterp);
End;

Function TTCLInterpreter.LimitExceeded: Integer;
Begin
  Result := Tcl_LimitExceeded(FInterp);
End;

Procedure TTCLInterpreter.LimitSetCommands(commandLimit: Integer);
Begin
  Tcl_LimitSetCommands(FInterp,commandLimit);
End;

Procedure TTCLInterpreter.LimitSetTime(timeLimitPtr: PTcl_Time);
Begin
  Tcl_LimitSetTime(FInterp,timeLimitPtr);
End;

Procedure TTCLInterpreter.LimitSetGranularity(_type: Integer; granularity: Integer);
Begin
  Tcl_LimitSetGranularity(FInterp,_type,granularity);
End;

Function TTCLInterpreter.LimitTypeEnabled(_type: Integer): Integer;
Begin
  Result := Tcl_LimitTypeEnabled(FInterp,_type);
End;

Function TTCLInterpreter.LimitTypeExceeded(_type: Integer): Integer;
Begin
  Result := Tcl_LimitTypeExceeded(FInterp,_type);
End;

Procedure TTCLInterpreter.LimitTypeSet(_type: Integer);
Begin
  Tcl_LimitTypeSet(FInterp,_type);
End;

Procedure TTCLInterpreter.LimitTypeReset(_type: Integer);
Begin
  Tcl_LimitTypeReset(FInterp,_type)
End;

Function TTCLInterpreter.LimitGetCommands: Integer;
Begin
  Result := Tcl_LimitGetCommands(FInterp);
End;

Procedure TTCLInterpreter.LimitGetTime(timeLimitPtr: PTcl_Time);
Begin
  Tcl_LimitGetTime(FInterp,timeLimitPtr);
End;

Function TTCLInterpreter.LimitGetGranularity(_type: Integer): Integer;
Begin
  Result := Tcl_LimitGetGranularity(FInterp,_type);
End;

Function TTCLInterpreter.NewProc:PTclCmdProcInfo;
Begin
  // create new record
  New(Result);
  // append to our list
  SetLength(FProcInfo,Length(FProcInfo)+1);
  FProcInfo[Length(FProcInfo)-1] := Result;
End;

Procedure TTCLInterpreter.DelProc(ProcInfo:PTclCmdProcInfo);
Var Index : Integer;
Begin
  For Index := 0 to Length(FProcInfo)-1 do
    if FProcInfo[Index] = ProcInfo then
      Begin
        { free memory }
        Dispose(ProcInfo);
        { remove from list }
        Move(FProcInfo[Index+1],FProcInfo[Index],(Length(FProcInfo)-Index-1)*SizeOf(FProcInfo[0]));
        SetLength(FProcInfo,Length(FProcInfo)-1);
        Exit;
      End;
  // if ProcInfo was not found, we end up here
  raise Exception.Create('DelProc: Invalid ProcInfo');
End;

{ TTCLObj }

Constructor TTCLObj.Create;
Begin
  Create(Tcl_NewObj);
End;

Constructor TTCLObj.Create(ATclObj:PTcl_Obj);
Begin
  inherited Create;
  FTclObj := ATclObj;
  // remove old internal representation
  if FTclObj^.typePtr <> Nil then
    FTclObj^.typePtr^.freeIntRepProc(FTclObj);
  // set new internal representation type pointer
  FTclObj^.typePtr := GetTypePtr;
  FTclObj^.internalRep.ptrAndLongRep.Ptr := Self;
End;

Procedure TTCLObj.InvalidateString;
Begin
  Tcl_InvalidateStringRep(FTclObj);
End;

class Function TTCLObj.IsMyType(ATclObj:PTcl_Obj):Boolean;
Begin
  Result := (ATclObj^.typePtr = GetTypePtr);
End;

Class Function TTCLObj.IsMyType(ATclObj:TTcl_Object):Boolean;
Begin
  Result := (PTcl_Obj(ATclObj)^.typePtr = GetTypePtr);
End;

Class Function TTCLObj.Get(Obj:PTcl_Obj):TTclObj;
Begin
  Result := TTclObj(Obj^.internalRep.ptrAndLongRep.Ptr);
End;

Class Function TTCLObj.Get(Obj:TTCL_Object):TTclObj;Inline;
Begin
  Result := TTclObj(PTcl_Obj(Obj)^.internalRep.ptrAndLongRep.Ptr);
End;

(**
 * This function must be implemented for any of your custom types. The only
 * difference is the type of the created MyObj.
 *
 * This is necessary becase here we can't know what is the desired class to
 * create.
 *)
Function SetFromAnyProc(Interp:PTcl_Interp;Obj:PTcl_Obj):Integer;cdecl;
Var MyObj : TTclObj;
Begin
WriteLn('SetFromAny');
  MyObj := TTCLObj.Create(Obj);   // create new instance, remove old internal representation, set us as new internal representation
  // TODO: check wheter we have this method overridden, i.e. non-abstract any more, otherwise return TCL_ERROR
  Result := MyObj.SetFromAny(Interp);
End;

Procedure UpdateStringProc(Obj:PTcl_Obj);cdecl;
Var St : String;
Begin
WriteLn('UpdateString');
  St := TTCLObj.Get(Obj).UpdateString;
  { Obj^.Bytes is Nil, we have to set it non-Nil }
  Obj^.Bytes  := Tcl_Alloc(Length(St)+1);   // http://www.tcl.tk/man/tcl8.5/TclLib/Alloc.htm
  Move(St[1],Obj^.Bytes^,Length(St)+1);
  Obj^.Length := Length(St);
End;

Procedure DupIntRepProc(Src:PTcl_Obj;Dst:PTcl_Obj);cdecl;
Var SrcData,DstData : TTclObj;
    SrcClass        : TTCLObjClass;
Begin
WriteLn('DupIntRep');
  SrcData := TTCLObj.Get(Src);
  SrcClass := TTCLObjClass(SrcData.ClassType);
  DstData := SrcClass.Create(Dst);
  DstData.Copy(SrcData);
  Dst^.internalRep.ptrAndLongRep.Ptr := DstData;
End;

Procedure FreeIntRepProc(Obj:PTcl_Obj);cdecl;
Begin
WriteLn('MyDataFreeIntRep');
  TTCLObj.Get(Obj).Free;
End;

{ TTCL_Object }

Function TTCL_Object.AsInteger(ATCL:TTCLInterpreter):Integer;
Begin
  Result := ATCL.GetIntFromObj(PObj);
End;

Function TTCL_Object.AsPChar:PChar;
Begin
  Result := Tcl_GetString(PObj);
End;

Function TTCL_Object.AsString:String;
Begin
  Result := Tcl_GetString(PObj);
End;

{ helper functions }

Procedure VarDump(Obj:PTcl_Obj);
Begin
  WriteLn('Obj = ',IntToHex(PtrUInt(Obj),2*SizeOf(PtrUInt)));
  if Obj <> Nil then
    Begin
      WriteLn('  refCount = ',Obj^.refCount);
      WriteLn('  bytes    = ',IntToHex(PtrUInt(Obj^.bytes),2*SizeOf(PtrUInt)));
      WriteLn('  bytes    = ',Obj^.bytes);
      WriteLn('  length   = ',Obj^.length);
      Write  ('  TypePtr  = ',IntToHex(PtrUInt(Obj^.TypePtr),2*SizeOf(PtrUInt)));
      if Obj^.TypePtr <> Nil then
        Write(' (',Obj^.typePtr^.name,')');
      WriteLn;
    End;
End;

End.

