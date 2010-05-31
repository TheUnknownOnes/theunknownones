unit uIBEScriptComponent;

interface

uses
  Classes, Windows, SysUtils, Controls;

type
  TIBESCDLLConnection = class;

  TIBESCConnectErrorProc = procedure(AError : String) of object;
  TIBESCScriptErrorProc = procedure(AStatement, AError : String; var AContinue : Boolean) of object;
  TIBESCBeforeStatementProc = procedure(AStatement, AText : String; var AContinue : Boolean) of object;
  TIBESCAfterStatementProc = procedure(AStatement : String; ASuccess : Boolean; var AContinue : Boolean) of object;
  TIBESCIBEBlockProgressProc = procedure(AProgressMessage : String; var AContinue : Boolean) of object;

  EIBESCConnectError = type Exception;
  EIBESCScriptError = type Exception;

  TIBEScriptComponent = class(TComponent)
  private
    FConnection : TIBESCDLLConnection;
    FScript : TStringList;
    FIBEScriptDLL: String;
    FOnConnectError: TIBESCConnectErrorProc;
    FOnScriptError: TIBESCScriptErrorProc;
    FOnBeforeStatement: TIBESCBeforeStatementProc;
    FOnAfterStatement: TIBESCAfterStatementProc;
    FOnIBEBlockProgress: TIBESCIBEBlockProgressProc;

    procedure DoConnectError(AErrorMessage : String);
    function DoScriptError(AStatement, AError : String) : Boolean;
    function DoBeforeStatement(AStatement, AText : String) : Boolean;
    function DoAfterStatement(AStatement : String; ASuccess : Boolean) : Boolean;
    function DoIBEBlockProgress(AProgressMessage : String) : Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Connect(AConnectionString : AnsiString) : Boolean;
    procedure ExecuteScript;
  published
    property IBEScriptDLL : String read FIBEScriptDLL write FIBEScriptDLL;
    property Script : TStringList read FScript;

    property OnConnectError : TIBESCConnectErrorProc read FOnConnectError write FOnConnectError;
    property OnScriptError : TIBESCScriptErrorProc read FOnScriptError write FOnScriptError;
    property OnBeforeStatement : TIBESCBeforeStatementProc read FOnBeforeStatement write FOnBeforeStatement;
    property OnAfterStatement : TIBESCAfterStatementProc read FOnAfterStatement write FOnAfterStatement;
    property OnIBEBlockProgress : TIBESCIBEBlockProgressProc read FOnIBEBlockProgress write FOnIBEBlockProgress;
  end;
  

  TIBESCConnectErrorCallbackProc = function (AErrorMessage : PAnsiChar) : Integer;  stdcall;
  TIBESCScriptErrorCallbackProc = function (AStmtText, AErrMessage : PAnsiChar) : Integer; stdcall;
  TIBESCBeforeExecStatementCallbackProc = function (AStmtText, AText : PAnsiChar) : Integer; stdcall;
  TIBESCAfterExecStatementCallbackProc = function (AStmtText : PAnsiChar; Success : Integer) : integer; stdcall;
  TIBESCIBEBlockProgressCallbackProc = function (AProgressMessage : PAnsiChar) : Integer; stdcall;

  TIBESCExecuteScriptProc = procedure (AScript : PAnsiChar;
                                  AErrorCallbackProc : TIBESCScriptErrorCallbackProc;
                                  ABeforeCallbackProc : TIBESCBeforeExecStatementCallbackProc;
                                  AAfterCallbackProc : TIBESCAfterExecStatementCallbackProc;
                                  AIBEBlockProgressProc : TIBESCIBEBlockProgressCallbackProc); stdcall;

  TIBESCConnectDBProc = function (AConnectParams : PAnsiChar;
                             AConnectErrorCallbacFunc : TIBESCConnectErrorCallbackProc) : Integer; stdcall;

  TIBESCDLLConnection = class
  private
    FConnectProc : TIBESCConnectDBProc;
    FExecProc : TIBESCExecuteScriptProc;
    FParent : TIBEScriptComponent;
    FLibHandle : Cardinal;

    function Connect(AConnectionString : AnsiString) : Boolean;
    procedure ExecuteScript(AScript : AnsiString);
  public
    constructor Create(AParent : TIBEScriptComponent);
    destructor Destroy(); override;
  end;

implementation

var
  FActiveConnection : TIBESCDLLConnection;

function CBScriptError(AStmtText, AErrMessage : PAnsiChar) : Integer; stdcall;
begin
  if FActiveConnection.FParent.DoScriptError(AStmtText, AErrMessage) then
    Result := 0
  else
    Result := 1;
end;

function CBIBEBlockProgress(AMessage : PAnsiChar) : Integer; stdcall;
begin
  if FActiveConnection.FParent.DoIBEBlockProgress(AMessage) then
    Result := 0
  else
    Result := 1;
end;

function CBBeforeExec(AStmtText, AText : PAnsiChar) : Integer; stdcall;
begin
  if FActiveConnection.FParent.DoBeforeStatement(AStmtText, AText) then
    Result := 0
  else
    Result := 1;
end;

function CBAfterExec(AStmtText : PAnsiChar; Success : Integer) : Integer; stdcall;
begin
  if FActiveConnection.FParent.DoAfterStatement(AStmtText, Success = 1) then
    Result := 0
  else
    Result := 1;
end;

function CBConnectError(AErrorMessage : PAnsiChar) : Integer;  stdcall;
begin
  Result := 0;

  FActiveConnection.FParent.DoConnectError(StrPas(AErrorMessage));
end;

{ TIBEScriptComponent }

function TIBEScriptComponent.Connect(AConnectionString: AnsiString): Boolean;
begin
  Result := FConnection.Connect(AConnectionString);
end;

constructor TIBEScriptComponent.Create(AOwner: TComponent);
begin
  inherited;

  FIBEScriptDLL := 'IBEScript.dll';
  FScript := TStringList.Create;

  if not (csDesigning in ComponentState) then
    FConnection := TIBESCDLLConnection.Create(Self)
  else
    FConnection := nil;
end;

destructor TIBEScriptComponent.Destroy;
begin
  if Assigned(FScript) then
    FScript.Free;

  if Assigned(FConnection) then
    FConnection.Free;

  inherited;
end;

function TIBEScriptComponent.DoAfterStatement(AStatement: String;
  ASuccess: Boolean): Boolean;
begin
  Result := ASuccess;

  if Assigned(FOnAfterStatement) then
    FOnAfterStatement(AStatement, ASuccess, Result);
end;

function TIBEScriptComponent.DoBeforeStatement(AStatement,
  AText: String): Boolean;
begin
  Result := true;

  if Assigned(FOnBeforeStatement) then
    FOnBeforeStatement(AStatement, AText, Result);
end;

procedure TIBEScriptComponent.DoConnectError(AErrorMessage: String);
begin
  if Assigned(FOnConnectError) then
    FOnConnectError(AErrorMessage)
  else
    raise EIBESCConnectError.Create(AErrorMessage);
end;

function TIBEScriptComponent.DoIBEBlockProgress(
  AProgressMessage: String): Boolean;
begin
  Result := true;

  if Assigned(FOnIBEBlockProgress) then
    FOnIBEBlockProgress(AProgressMessage, Result);
end;

function TIBEScriptComponent.DoScriptError(AStatement,
  AError: String): Boolean;
begin
  Result := false;

  if Assigned(FOnScriptError) then
    FOnScriptError(AStatement, AError, Result)
  else
    raise EIBESCScriptError.Create(AError + ' ("' + AStatement + '")');
end;

procedure TIBEScriptComponent.ExecuteScript;
begin
  FConnection.ExecuteScript(FScript.Text);
end;

{ TIBESCDLLConnection }

function TIBESCDLLConnection.Connect(AConnectionString: AnsiString) : Boolean;
begin
  FActiveConnection := Self;
  Result := FConnectProc(PAnsiChar(AConnectionString), @CBConnectError) = 0;
end;

constructor TIBESCDLLConnection.Create(AParent: TIBEScriptComponent);
begin
  FParent := AParent;

  FLibHandle := LoadLibrary(PChar(FParent.IBEScriptDLL));
  if FLibHandle = 0 then
    RaiseLastOSError
  else
  begin
    FExecProc := GetProcAddress(FLibHandle, 'ExecScriptText2');
    FConnectProc := GetProcAddress(FLibHandle, 'Connect');

    if (not Assigned(@FExecProc)) or (not Assigned(@FConnectProc)) then
      raise Exception.Create('Invalid Library');
  end;
end;

destructor TIBESCDLLConnection.Destroy;
begin
  if FLibHandle <> 0 then
    FreeLibrary(FLibHandle);

  inherited;
end;

procedure TIBESCDLLConnection.ExecuteScript(AScript: AnsiString);
begin
  FActiveConnection := Self;
  FExecProc(PAnsiChar(AScript), @CBScriptError, @CBBeforeExec, @CBAfterExec, @CBIBEBlockProgress);
end;

end.
