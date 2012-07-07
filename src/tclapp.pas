Unit TclApp;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, TclCmdLinePredef, TclOOP;

Type

  { TTclApp }

  (**
   * Base class for a full application with TCL and Readline
   *)
  TTclApp = class
  protected
    FTCL      : TTCLInterpreter;
    FCmdLine  : TTclCmdLinePredef;
  public
    Constructor Create(AHistoryFile:String;AReadlineName:String);
    Destructor  Destroy; override;

    Function Run : Byte;
  End;

Implementation

{ TTclApp }

Constructor TTclApp.Create(AHistoryFile:String;AReadlineName:String);
Begin
  inherited Create;

  FTCL := TTCLInterpreter.Create;

  FCmdLine := TTclCmdLinePredef.Create(AHistoryFile,AReadlineName,FTCL);
End;

Destructor TTclApp.Destroy;
Begin
  FCmdLine.Free;   // writes history to file
  FTCL.Free;
  Inherited Destroy;
End;

Function TTclApp.Run : Byte;
Begin
  // command line
  FCmdLine.CommandLoop;  // will exit from this loop with command "exit"
  Result := FCmdLine.ExitStatus;
End;

End.

