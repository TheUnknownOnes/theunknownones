unit uFBPScript;

interface

{$UNDEF PS_USESUPPORT}

uses
  SysUtils, Classes, uPSRuntime, uPSDebugger, uPSUtils, uPSCompiler,
  StrUtils, DateUtils, Windows, DB, IBDatabase, IBCustomDataSet, IBQuery,
  uPSC_std, uPSR_std, uPSC_controls, uPSR_controls, uPSC_DB, uPSR_DB,
  uPSC_Classes, uPSR_Classes, uPSC_Graphics, uPSR_Graphics;

type
  TfbpLogProc = procedure(ALog : String) of object;

  TFBPScript = class(TDataModule)
    DefaultDatabase: TIBDatabase;
    DefaultTransaction: TIBTransaction;
    DefaultQuery: TIBQuery;
  private
    FExec: TPSExec;
    FCompiler: TPSPascalCompiler;
    FRuntimeClassImporter: TPSRuntimeClassImporter;

    FCurrentQuery : TIBQuery;
    FOnLog: TfbpLogProc;

    function OnUses(AUnit : TbtString) : Boolean; virtual;

    procedure DoBeforeCompile; virtual;
    procedure DoAfterCompile; virtual;

    procedure DoBeforeExecLoad; virtual;
    procedure DoAfterExecLoad; virtual;

    procedure DoBeforeExecute; virtual;
    procedure DoAfterExecute; virtual;

    function GetCurrentQuery : TIBQuery;

    procedure CurrentQuery_Query(ASQL : String);
    procedure CurrentQuery_Next;
    procedure CurrentQuery_Close;
    function CurrentQuery_EOF : Boolean;
    procedure CurrentQuery_Commit;
    procedure CurrentQuery_CommitRetaining;
    procedure CurrentQuery_Rollback;
    procedure CurrentQuery_RollbackRetaining;
    procedure CurrentQuery_UseQuery(AQuery : TIBQuery);
    function CurrentQuery_DefaultQuery : TIBQuery;
    function CurrentQuery_NewQuery() : TIBQuery;
    function CurrentQuery_CurrentQuery : TIBQuery;
    function CurrentQuery_FieldByName(AName : String) : TField;
    function CurrentQuery_Field(AIndex : Word) : TField;
    function CurrentQuery_FieldCount : Word;
    function CurrentQuery_Param(AIndex : Word) : TParam;
    function CurrentQuery_ParamByName(AName : String) : TParam;
    function CurrentQuery_ParamCount : Word;
    procedure CurrentQuery_Prepare(ASQL : String);
    procedure CurrentQuery_Open;

    procedure Log(ALog : String);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Compile(ACode : AnsiString; AAutoLoadExecutor : Boolean = true) : Boolean;
    function LoadExecutor(ABinCode : AnsiString) : Boolean;
    function Execute : Boolean;

    property Compiler : TPSPascalCompiler read FCompiler;
    property Exec : TPSExec read FExec;
    property RuntimeClassImporter : TPSRuntimeClassImporter read FRuntimeClassImporter;

    property CurrentQuery : TIBQuery read GetCurrentQuery;

    property OnLog : TfbpLogProc read FOnLog write FOnLog;
  end;

implementation

{$R *.dfm}

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


{$REGION 'Tools'}
procedure _OutputDebugString(AString : String);
var
  ws : WideString;
begin
  ws := AString;
  OutputDebugStringW(PWideChar(ws));
end;
{$ENDREGION}

{$REGION 'Files'}
function _CopyFile(AOldName : String; ANewName : String; AFailIfExists : Boolean) : Boolean;
begin
  Result := CopyFileW(PWideChar(WideString(AOldName)), PWideChar(WideString(ANewName)), AFailIfExists);
end;

function _MoveFile(AOldName : String; ANewName : String) : Boolean;
begin
  Result := MoveFileW(PWideChar(WideString(AOldName)), PWideChar(WideString(ANewName)));
end;
{$ENDREGION}

{$REGION 'TIBTransaction'}
procedure TIBTransactionActiveR(Self: TIBTransaction; var T: Boolean); begin T := Self.Active; end;
procedure TIBTransactionActiveW(Self: TIBTransaction; T: Boolean); begin Self.Active := T; end;

procedure TIBTransactionParamsR(Self: TIBTransaction; var T: TStrings); begin T := Self.Params; end;
{$ENDREGION}

{$REGION 'TIBQuery'}
procedure TIBQuerySQLR(Self: TIBQuery; var T: TStrings); begin T := Self.SQL; end;

procedure TIBQueryParamsR(Self: TIBQuery; var T: TParams); begin T := Self.Params; end;
procedure TIBQueryParamsW(Self: TIBQuery; T: TParams); begin Self.Params := T; end;

procedure TIBQueryParamCountR(Self: TIBQuery; var T: Word); begin T := Self.ParamCount; end;

procedure TIBQueryPreparedR(Self: TIBQuery; var T: Boolean); begin T := Self.Prepared; end;
procedure TIBQueryPreparedW(Self: TIBQuery; T: Boolean); begin Self.Prepared := T; end;

procedure TIBQueryActiveR(Self: TIBQuery; var T: Boolean); begin T := Self.Active; end;
procedure TIBQueryActiveW(Self: TIBQuery; T: Boolean); begin Self.Active := T; end;
{$ENDREGION}

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

constructor TFBPScript.Create(AOwner: TComponent);
begin
  FScriptList.Add(Self);

  FCompiler := TPSPascalCompiler.Create;
  FCompiler.OnUses := _OnUses;

  FExec := TPSExec.Create;
  FRuntimeClassImporter := TPSRuntimeClassImporter.Create();

  RegisterClassLibraryRuntime(FExec, FRuntimeClassImporter);

  inherited;

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
  RIRegister_Std(FRuntimeClassImporter);
  RIRegister_Controls(FRuntimeClassImporter);
  RIRegister_DB(FRuntimeClassImporter);
  RIRegister_Classes(FRuntimeClassImporter, true);
  RIRegister_Graphics(FRuntimeClassImporter, true);

  {$REGION 'TIBTransaction'}
  with FRuntimeClassImporter.Add(TIBTransaction) do
  begin
    RegisterMethod(@TIBTransaction.Commit, 'Commit');
    RegisterMethod(@TIBTransaction.CommitRetaining, 'CommitRetaining');
    RegisterMethod(@TIBTransaction.Rollback, 'Rollback');
    RegisterMethod(@TIBTransaction.RollbackRetaining, 'RollbackRetaining');

    RegisterPropertyHelper(@TIBTransactionActiveR, @TIBTransactionActiveW, 'Active');
    RegisterPropertyHelper(@TIBTransactionParamsR, nil, 'Params');
  end;
  {$ENDREGION}

  {$REGION 'TIBQuery'}
  FRuntimeClassImporter.Add(TWideDataSet);
  FRuntimeClassImporter.Add(TIBCustomDataSet);

  with FRuntimeClassImporter.Add(TIBQuery) do
  begin
    RegisterMethod(@TIBQuery.Prepare, 'Prepare');
    RegisterMethod(@TIBQuery.Unprepare, 'Unprepare');

    RegisterPropertyHelper(@TIBQueryActiveR, @TIBQueryActiveW, 'Active');
    RegisterPropertyHelper(@TIBQuerySQLR, nil, 'SQL');
    RegisterPropertyHelper(@TIBQueryParamsR, @TIBQueryParamsW, 'Params');
    RegisterPropertyHelper(@TIBQueryParamCountR, nil, 'ParamCount');
    RegisterPropertyHelper(@TIBQueryPreparedR, @TIBQueryPreparedW, 'Prepared');
  end;
  {$ENDREGION}

  {$REGION 'Windows'}
  FExec.RegisterDelphiFunction(@Sleep, 'Sleep', cdPascal);
  {$ENDREGION}

  {$REGION 'Tools'}
  FExec.RegisterDelphiFunction(@_OutputDebugString, 'OutputDebugString', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Log, 'Log', cdRegister);
  {$ENDREGION}

  {$REGION 'CurrentQuery'}
  FExec.RegisterDelphiMethod(Self, @TFBPScript.CurrentQuery_Query, 'Query', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.CurrentQuery_Next, 'Next', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.CurrentQuery_Close, 'Close', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.CurrentQuery_EOF, 'EOF', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.CurrentQuery_Commit, 'Commit', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.CurrentQuery_CommitRetaining, 'CommitRetaining', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.CurrentQuery_Rollback, 'Rollback', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.CurrentQuery_RollbackRetaining, 'RollbackRetaining', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.CurrentQuery_UseQuery, 'UseQuery', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.CurrentQuery_DefaultQuery, 'DefaultQuery', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.CurrentQuery_NewQuery, 'NewQuery', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.CurrentQuery_CurrentQuery, 'CurrentQuery', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.CurrentQuery_Field, 'Field', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.CurrentQuery_FieldByName, 'FieldByName', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.CurrentQuery_FieldCount, 'FieldCount', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.CurrentQuery_Param, 'Param', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.CurrentQuery_ParamByName, 'ParamByName', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.CurrentQuery_ParamCount, 'ParamCount', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.CurrentQuery_Prepare, 'Prepare', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.CurrentQuery_Open, 'Open', cdRegister);
  {$ENDREGION}

  {$REGION 'Strings'}
  FExec.RegisterDelphiFunction(@Format, 'Format', cdRegister);
  {$ENDREGION}

  {$REGION 'Files'}
  FExec.RegisterDelphiFunction(@_CopyFile, 'CopyFile', cdRegister);
  FExec.RegisterDelphiFunction(@_MoveFile, 'MoveFile', cdRegister);


    FCompiler.AddDelphiFunction('function FileExists(const FileName: string): Boolean');
    FCompiler.AddDelphiFunction('function IncludeTrailingPathDelimiter(const S: string): string');
    FCompiler.AddDelphiFunction('function ExcludeTrailingPathDelimiter(const S: string): string;');
    FCompiler.AddDelphiFunction('function DirectoryExists(const Directory: string): Boolean');
    FCompiler.AddDelphiFunction('function ForceDirectories(Dir: string): Boolean');

    Fcompiler.AddDelphiFunction('function FindFirst(const Path: string; Attr: Integer; var F: TSearchRec): Integer');
    FCompiler.AddDelphiFunction('function FindNext(var F: TSearchRec): Integer');
    FCompiler.AddDelphiFunction('procedure FindClose(var F: TSearchRec)');

    FCompiler.AddDelphiFunction('function DeleteFile(const FileName: string): Boolean');
    FCompiler.AddDelphiFunction('function RenameFile(const OldName, NewName: string): Boolean');
    FCompiler.AddDelphiFunction('function ChangeFileExt(const FileName, Extension: string): string');
    FCompiler.AddDelphiFunction('function ChangeFilePath(const FileName, Path: string): string');
    FCompiler.AddDelphiFunction('function ExtractFilePath(const FileName: string): string');
    FCompiler.AddDelphiFunction('function ExtractFileDir(const FileName: string): string');
    FCompiler.AddDelphiFunction('function ExtractFileDrive(const FileName: string): string');
    FCompiler.AddDelphiFunction('function ExtractFileName(const FileName: string): string');
    FCompiler.AddDelphiFunction('function ExtractFileExt(const FileName: string): string');
    FCompiler.AddDelphiFunction('function GetCurrentDir: string');
    FCompiler.AddDelphiFunction('function SetCurrentDir(const Dir: string): Boolean');
    FCompiler.AddDelphiFunction('function CreateDir(const Dir: string): Boolean');
    FCompiler.AddDelphiFunction('function RemoveDir(const Dir: string): Boolean');
    FCompiler.AddDelphiFunction('function FileAge(const FileName: string; out FileDateTime: TDateTime): Boolean');
  {$ENDREGION}
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

function TFBPScript.CurrentQuery_Field(AIndex: Word): TField;
begin
  Result := CurrentQuery.Fields[AIndex];
end;

function TFBPScript.CurrentQuery_FieldByName(AName: String): TField;
begin
  Result := CurrentQuery.FieldByName(AName);
end;

function TFBPScript.CurrentQuery_FieldCount: Word;
begin
  Result := CurrentQuery.FieldCount;
end;

function TFBPScript.GetCurrentQuery: TIBQuery;
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

    SIRegister_Std(FCompiler);
    SIRegister_Controls(FCompiler);
    SIRegister_DB(FCompiler);
    SIRegister_Classes(FCompiler, true);
    SIRegister_Graphics(FCompiler, true);

    {$REGION 'TIBTransaction'}
    with FCompiler.AddClass(FCompiler.FindClass('TComponent'), TIBTransaction) do
    begin
      RegisterMethod('procedure Commit');
      RegisterMethod('procedure CommitRetaining');
      RegisterMethod('procedure Rollback');
      RegisterMethod('procedure RollbackRetaining');

      RegisterProperty('Active', 'Boolean', iptRW);
      RegisterProperty('Params', 'TStrings', iptR);
    end;

    {$ENDREGION}

    {$REGION 'TIBQuery'}
    FCompiler.AddClass(FCompiler.FindClass('TDataSet'), TWideDataSet);
    FCompiler.AddClass(FCompiler.FindClass('TWideDataSet'), TIBCustomDataSet);

    with FCompiler.AddClass(FCompiler.FindClass('TIBCustomDataSet'), TIBQuery) do
    begin
      RegisterMethod('procedure Prepare');
      RegisterMethod('procedure Unprepare');

      RegisterProperty('Active', 'Boolean', iptRW);
      RegisterProperty('SQL', 'TStrings', iptR);
      RegisterProperty('Params', 'TParams', iptRW);
      RegisterProperty('ParamCount', 'Word', iptR);
      RegisterProperty('Prepared', 'Boolean', iptRW);
    end;

    {$ENDREGION}

    {$REGION 'Windows'}
    FCompiler.AddTypeCopyN('DWORD', 'Cardinal');
    FCompiler.AddTypeCopyN('TFilename', 'String');
    FCompiler.AddConstantN('MAX_PATH', 'Integer').SetInt(260);
    FCompiler.AddTypeS('TFileTime', 'record dwLowDateTime: DWORD; dwHighDateTime: DWORD; end;');

    FCompiler.AddDelphiFunction('procedure Sleep(AMilliseconds : Cardinal)');
    {$ENDREGION}

    {$REGION 'Tools'}
    FCompiler.AddDelphiFunction('procedure OutputDebugString(AString : String)');
    FCompiler.AddDelphiFunction('procedure Log(ALog : String)');
    {$ENDREGION}

    {$REGION 'CurrentQuery'}
    FCompiler.AddDelphiFunction('procedure Query(ASQL : String)');
    FCompiler.AddDelphiFunction('procedure Next');
    FCompiler.AddDelphiFunction('procedure Close');
    FCompiler.AddDelphiFunction('function EOF : Boolean');
    FCompiler.AddDelphiFunction('procedure Commit');
    FCompiler.AddDelphiFunction('procedure CommitRetaining');
    FCompiler.AddDelphiFunction('procedure Rollback');
    FCompiler.AddDelphiFunction('procedure RollbackRetaining');
    FCompiler.AddDelphiFunction('procedure UseQuery(AQuery : TIBQuery)');
    FCompiler.AddDelphiFunction('function DefaultQuery : TIBQuery');
    FCompiler.AddDelphiFunction('function NewQuery : TIBQuery');
    FCompiler.AddDelphiFunction('function CurrentQuery : TIBQuery');
    FCompiler.AddDelphiFunction('function Field(AIndex : Word) : TField');
    FCompiler.AddDelphiFunction('function FieldByName(AName : String) : TField');
    FCompiler.AddDelphiFunction('function FieldCount : Word');
    FCompiler.AddDelphiFunction('function Param(AIndex : Word) : TParam');
    FCompiler.AddDelphiFunction('function ParamByName(AName : String) : TParam');
    FCompiler.AddDelphiFunction('function ParamCount : Word');
    FCompiler.AddDelphiFunction('procedure Prepare(ASQL : String)');
    FCompiler.AddDelphiFunction('procedure Open');
    {$ENDREGION}

    {$REGION 'Strings'}
    FCompiler.AddDelphiFunction('function Format(const Format: string; const Args: array of const) : String');
    {$ENDREGION}

    {$REGION 'Files'}
    FCompiler.AddDelphiFunction('function CopyFile(AOldName : String; ANewName : String; AFailIfExists : Boolean) : Boolean');
    FCompiler.AddDelphiFunction('function MoveFile(AOldName : String; ANewName : String) : Boolean');

    FCompiler.AddDelphiFunction('function FileExists(const FileName: string): Boolean');
    FCompiler.AddDelphiFunction('function IncludeTrailingPathDelimiter(const S: string): string');
    FCompiler.AddDelphiFunction('function ExcludeTrailingPathDelimiter(const S: string): string;');
    FCompiler.AddDelphiFunction('function DirectoryExists(const Directory: string): Boolean');
    FCompiler.AddDelphiFunction('function ForceDirectories(Dir: string): Boolean');

    FCompiler.AddTypeS('TWin32FindData', 'record dwFileAttributes: DWORD; ftCreationTime: TFileTime; ftLastAccessTime: TFileTime; '+
                                         'ftLastWriteTime: TFileTime; nFileSizeHigh: DWORD; nFileSizeLow: DWORD; dwReserved0: DWORD; '+
                                         'dwReserved1: DWORD; cFileName: array[0..MAX_PATH - 1] of WideChar; cAlternateFileName: array[0..13] of WideChar; end;');
    FCompiler.AddTypeS('TSearchRec', 'record Time: Integer; Size: Int64; Attr: Integer; Name: TFileName; ExcludeAttr: Integer; FindHandle: Cardinal; FindData: TWin32FindData; end;');

    Fcompiler.AddDelphiFunction('function FindFirst(const Path: string; Attr: Integer; var F: TSearchRec): Integer');
    FCompiler.AddDelphiFunction('function FindNext(var F: TSearchRec): Integer');
    FCompiler.AddDelphiFunction('procedure FindClose(var F: TSearchRec)');

    FCompiler.AddDelphiFunction('function DeleteFile(const FileName: string): Boolean');
    FCompiler.AddDelphiFunction('function RenameFile(const OldName, NewName: string): Boolean');
    FCompiler.AddDelphiFunction('function ChangeFileExt(const FileName, Extension: string): string');
    FCompiler.AddDelphiFunction('function ChangeFilePath(const FileName, Path: string): string');
    FCompiler.AddDelphiFunction('function ExtractFilePath(const FileName: string): string');
    FCompiler.AddDelphiFunction('function ExtractFileDir(const FileName: string): string');
    FCompiler.AddDelphiFunction('function ExtractFileDrive(const FileName: string): string');
    FCompiler.AddDelphiFunction('function ExtractFileName(const FileName: string): string');
    FCompiler.AddDelphiFunction('function ExtractFileExt(const FileName: string): string');
    FCompiler.AddDelphiFunction('function GetCurrentDir: string');
    FCompiler.AddDelphiFunction('function SetCurrentDir(const Dir: string): Boolean');
    FCompiler.AddDelphiFunction('function CreateDir(const Dir: string): Boolean');
    FCompiler.AddDelphiFunction('function RemoveDir(const Dir: string): Boolean');
    FCompiler.AddDelphiFunction('function FileAge(const FileName: string; out FileDateTime: TDateTime): Boolean');


    //FCompiler.AddDelphiFunction('')
    {$ENDREGION}
  end
  else
    Result := false;
end;

procedure TFBPScript.CurrentQuery_Close;
begin
  CurrentQuery.Close;
end;

procedure TFBPScript.CurrentQuery_Commit;
begin
  CurrentQuery.Transaction.Commit;
end;

procedure TFBPScript.CurrentQuery_CommitRetaining;
begin
  CurrentQuery.Transaction.CommitRetaining;
end;

function TFBPScript.CurrentQuery_CurrentQuery: TIBQuery;
begin
  Result := CurrentQuery;
end;

function TFBPScript.CurrentQuery_DefaultQuery: TIBQuery;
begin
  Result := DefaultQuery;
end;

function TFBPScript.CurrentQuery_EOF: Boolean;
begin
  Result := CurrentQuery.Eof;
end;

function TFBPScript.CurrentQuery_NewQuery: TIBQuery;
begin
  Result := TIBQuery.Create(nil);
  Result.Database := DefaultDatabase;
  Result.Transaction := TIBTransaction.Create(Result);
  Result.Transaction.DefaultDatabase := DefaultDatabase;
end;

procedure TFBPScript.CurrentQuery_Next;
begin
  CurrentQuery.Next;
end;

procedure TFBPScript.CurrentQuery_Open;
begin
  CurrentQuery.Open;
end;

function TFBPScript.CurrentQuery_ParamCount: Word;
begin
  Result := CurrentQuery.ParamCount;
end;

procedure TFBPScript.CurrentQuery_Prepare(ASQL: String);
begin
  CurrentQuery.SQL.Text := ASQL;
  CurrentQuery.Prepare;
end;

function TFBPScript.CurrentQuery_ParamByName(AName: String): TParam;
begin
  Result := CurrentQuery.ParamByName(AName);
end;

function TFBPScript.CurrentQuery_Param(AIndex : Word): TParam;
begin
  Result := CurrentQuery.Params[AIndex];
end;

procedure TFBPScript.CurrentQuery_Query(ASQL: String);
begin
  with CurrentQuery do
  begin
    Close;
    SQL.Text := ASQL;
    Open;
  end;
end;

procedure TFBPScript.CurrentQuery_Rollback;
begin
  CurrentQuery.Transaction.Rollback;
end;

procedure TFBPScript.CurrentQuery_RollbackRetaining;
begin
  CurrentQuery.Transaction.RollbackRetaining;
end;

procedure TFBPScript.CurrentQuery_UseQuery(AQuery: TIBQuery);
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
