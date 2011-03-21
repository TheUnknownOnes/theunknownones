unit uFBPScript;

interface

{$UNDEF PS_USESUPPORT}

uses
  SysUtils, Classes, uPSRuntime, uPSDebugger, uPSUtils, uPSCompiler,
  StrUtils, DateUtils, Windows, UIB, UibLib,
  uSysTools;

type
  TfbpLogProc = procedure(ALog : String) of object;

  TFBPScript = class
  private
    FExec: TPSExec;
    FCompiler: TPSPascalCompiler;
    FRuntimeClassImporter: TPSRuntimeClassImporter;

    FCurrentQuery : TUIBQuery;
    FOnLog: TfbpLogProc;
    FDefaultTransaction: TUIBTransaction;
    FDefaultDatabase: TUIBDatabase;
    FDefaultQuery: TUIBQuery;

    function OnUses(AUnit : TbtString) : Boolean; virtual;

    procedure DoBeforeCompile; virtual;
    procedure DoAfterCompile; virtual;

    procedure DoBeforeExecLoad; virtual;
    procedure DoAfterExecLoad; virtual;

    procedure DoBeforeExecute; virtual;
    procedure DoAfterExecute; virtual;

    function GetCurrentQuery : TUIBQuery;

    procedure Short_Query(ASQL : String);
    procedure Short_Next;
    procedure Short_Close;
    function Short_EOF : Boolean;
    procedure Short_Commit;
    procedure Short_CommitRetaining;
    procedure Short_Rollback;
    procedure Short_RollbackRetaining;
    procedure Short_UseQuery(AQuery : TUIBQuery);
    function Short_DefaultQuery : TUIBQuery;
    function Short_NewQuery() : TUIBQuery;
    function Short_CurrentQuery : TUIBQuery;
    function Short_Fields : TSQLResult;
    function Short_Params : TSQLParams;
    procedure Short_Prepare(ASQL : String);
    procedure Short_Open;
    function Short_DefaultDatabase : TUIBDataBase;
    function Short_DefaultTransaction : TUIBTransaction;

    procedure Log(ALog : String);

  public
    constructor Create(); virtual;
    destructor Destroy; override;

    function Compile(ACode : AnsiString; AAutoLoadExecutor : Boolean = true) : Boolean;
    function LoadExecutor(ABinCode : AnsiString) : Boolean;
    function Execute : Boolean;

    property Compiler : TPSPascalCompiler read FCompiler;
    property Exec : TPSExec read FExec;
    property RuntimeClassImporter : TPSRuntimeClassImporter read FRuntimeClassImporter;

    property DefaultDatabase: TUIBDatabase read FDefaultDatabase;
    property DefaultTransaction: TUIBTransaction read FDefaultTransaction;
    property DefaultQuery: TUIBQuery read FDefaultQuery;

    property CurrentQuery : TUIBQuery read GetCurrentQuery;


    property OnLog : TfbpLogProc read FOnLog write FOnLog;
  end;


implementation

uses uPS_All;

var
  FScriptList : TThreadList;

function _OnUses(Sender: TPSPascalCompiler; const Name: tbtString): Boolean;
var
  lst : TList;
  p : Pointer;
  s : TFBPScript absolute p;
begin
  Result := true;

  lst := FScriptList.LockList;
  try
    for p in lst do
    begin
      if s.Compiler = Sender then
      begin
        s.OnUses(Name);
      end;
    end;
  finally
    FScriptList.UnlockList;
  end;
end;


function TFBPScript.Compile(ACode : AnsiString; AAutoLoadExecutor : Boolean): Boolean;
var
  BinCode : AnsiString;
begin
  DoBeforeCompile;

  Result := FCompiler.Compile(ACode);

  if Result then
  begin
    DoAfterCompile;

    FCompiler.GetOutput(BinCode);

    if AAutoLoadExecutor then
      Result := LoadExecutor(BinCode)
  end;
end;

constructor TFBPScript.Create();
begin
  inherited;

  FScriptList.Add(Self);

  FCompiler := TPSPascalCompiler.Create;
  FCompiler.OnUses := _OnUses;

  FExec := TPSExec.Create;
  FRuntimeClassImporter := TPSRuntimeClassImporter.Create();

  RegisterClassLibraryRuntime(FExec, FRuntimeClassImporter);

  FDefaultDatabase := TUIBDatabase.Create(nil);

  FDefaultTransaction := TUIBTransaction.Create(FDefaultDatabase);
  FDefaultTransaction.DataBase := FDefaultDatabase;

  FDefaultQuery := TUIBQuery.Create(FDefaultDatabase);
  FDefaultQuery.Database := FDefaultDatabase;
  FDefaultQuery.Transaction := FDefaultTransaction;

  FCurrentQuery := DefaultQuery;
end;

destructor TFBPScript.Destroy;
begin
  FScriptList.Remove(Self);

  FRuntimeClassImporter.Free;
  FCompiler.Free;
  FExec.Free;

  inherited;
end;

procedure TFBPScript.DoAfterCompile;
begin

end;

procedure TFBPScript.DoAfterExecLoad;
begin

end;

procedure TFBPScript.DoAfterExecute;
begin

end;

procedure TFBPScript.DoBeforeCompile;
begin
  
end;

procedure TFBPScript.DoBeforeExecLoad;
begin
  PS_Register_All_R(FExec, FRuntimeClassImporter);

  FExec.RegisterDelphiMethod(Self, @TFBPScript.Log, 'Log', cdRegister);

  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_Query, 'Query', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_Next, 'Next', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_Close, 'Close', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_EOF, 'EOF', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_Commit, 'Commit', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_CommitRetaining, 'CommitRetaining', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_Rollback, 'Rollback', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_RollbackRetaining, 'RollbackRetaining', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_UseQuery, 'UseQuery', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_DefaultQuery, 'DefaultQuery', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_NewQuery, 'NewQuery', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_CurrentQuery, 'CurrentQuery', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_Fields, 'Fields', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_Params, 'Params', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_Prepare, 'Prepare', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_Open, 'Open', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_DefaultDatabase, 'DefaultDatabase', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_DefaultTransaction, 'DefaultTransaction', cdRegister);

end;

procedure TFBPScript.DoBeforeExecute;
begin

end;

function TFBPScript.Execute: Boolean;
begin
  DoBeforeExecute;

  Result := FExec.RunScript;

  if Result then
    DoAfterExecute;
end;

function TFBPScript.Short_Fields : TSQLResult;
begin
  Result := CurrentQuery.Fields;
end;

function TFBPScript.GetCurrentQuery: TUIBQuery;
begin
  if not Assigned(FCurrentQuery) then
    Result := DefaultQuery
  else
    Result := FCurrentQuery;
end;

function TFBPScript.LoadExecutor(ABinCode: AnsiString): Boolean;
begin
  DoBeforeExecLoad;

  Result := FExec.LoadData(ABinCode);

  if Result then
    DoAfterExecLoad;
end;

procedure TFBPScript.Log(ALog: String);
begin
  if Assigned(FOnLog) then
    FOnLog(ALog);
end;

function TFBPScript.OnUses(AUnit: TbtString): Boolean;
begin
  if AUnit = 'SYSTEM' then
  begin
    Result := true;

    PS_Register_All_C(FCompiler);


    FCompiler.AddDelphiFunction('procedure Log(ALog : String)');

    FCompiler.AddDelphiFunction('procedure Query(ASQL : String)');
    FCompiler.AddDelphiFunction('procedure Next');
    FCompiler.AddDelphiFunction('procedure Close');
    FCompiler.AddDelphiFunction('function EOF : Boolean');
    FCompiler.AddDelphiFunction('procedure Commit');
    FCompiler.AddDelphiFunction('procedure CommitRetaining');
    FCompiler.AddDelphiFunction('procedure Rollback');
    FCompiler.AddDelphiFunction('procedure RollbackRetaining');
    FCompiler.AddDelphiFunction('procedure UseQuery(AQuery : TUIBQuery)');
    FCompiler.AddDelphiFunction('function DefaultQuery : TUIBQuery');
    FCompiler.AddDelphiFunction('function NewQuery : TUIBQuery');
    FCompiler.AddDelphiFunction('function CurrentQuery : TUIBQuery');
    FCompiler.AddDelphiFunction('function Fields : TSQLResult');
    FCompiler.AddDelphiFunction('function Params : TSQLParams');
    FCompiler.AddDelphiFunction('procedure Prepare(ASQL : String)');
    FCompiler.AddDelphiFunction('procedure Open');
    FCompiler.AddDelphiFunction('function DefaultDatabase : TUIBDatabase;');
    FCompiler.AddDelphiFunction('function DefaultTransaction : TUIBTransaction;');


  end
  else
    Result := false;
end;

procedure TFBPScript.Short_Close;
begin
  CurrentQuery.Close;
end;

procedure TFBPScript.Short_Commit;
begin
  CurrentQuery.Transaction.Commit;
end;

procedure TFBPScript.Short_CommitRetaining;
begin
  CurrentQuery.Transaction.CommitRetaining;
end;

function TFBPScript.Short_CurrentQuery: TUIBQuery;
begin
  Result := CurrentQuery;
end;

function TFBPScript.Short_DefaultDatabase: TUIBDataBase;
begin
  Result := DefaultDatabase;
end;

function TFBPScript.Short_DefaultQuery: TUIBQuery;
begin
  Result := DefaultQuery;
end;

function TFBPScript.Short_DefaultTransaction: TUIBTransaction;
begin
  Result := FDefaultTransaction;
end;

function TFBPScript.Short_EOF: Boolean;
begin
  Result := CurrentQuery.Eof;
end;

function TFBPScript.Short_NewQuery: TUIBQuery;
begin
  Result := TUIBQuery.Create(nil);
  Result.Database := DefaultDatabase;
  Result.Transaction := TUIBTransaction.Create(Result);
  Result.Transaction.DataBase := DefaultDatabase;
end;

procedure TFBPScript.Short_Next;
begin
  CurrentQuery.Next;
end;

procedure TFBPScript.Short_Open;
begin
  CurrentQuery.Open;
end;

procedure TFBPScript.Short_Prepare(ASQL: String);
begin
  CurrentQuery.SQL.Text := ASQL;
  CurrentQuery.Prepare;
end;

function TFBPScript.Short_Params: TSQLParams;
begin
  Result := CurrentQuery.Params;
end;

procedure TFBPScript.Short_Query(ASQL: String);
begin
  with CurrentQuery do
  begin
    Close;
    SQL.Text := ASQL;
    Open;
  end;
end;

procedure TFBPScript.Short_Rollback;
begin
  CurrentQuery.Transaction.Rollback;
end;

procedure TFBPScript.Short_RollbackRetaining;
begin
  CurrentQuery.Transaction.RollbackRetaining;
end;

procedure TFBPScript.Short_UseQuery(AQuery: TUIBQuery);
begin
  if Assigned(AQuery) then
    FCurrentQuery := AQuery
  else
    FCurrentQuery := DefaultQuery;
end;

initialization
  FScriptList := TThreadList.Create;

finalization
  FScriptList.Free;

end.
