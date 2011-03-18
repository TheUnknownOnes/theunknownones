unit uPS_UIB;

interface

uses
  Classes,
  uPSCompiler,
  uPSRuntime,
  UIB,
  UIBLib;

procedure PS_Register_UIB_C(ACompiler : TPSPascalCompiler);
procedure PS_Register_UIB_R(AExec : TPSExec; ARCi : TPSRuntimeClassImporter);

implementation

uses uPS_Utils;

function _StrToCharacterSet(const CharacterSet: String): TCharacterSet;
begin
  Result := StrToCharacterSet(CharacterSet);
end;


procedure TUIBDatabase_Params_R(Self: TUIBDataBase; var T: TStrings); begin T := Self.Params; end;
procedure TUIBDatabase_Params_W(Self: TUIBDataBase; const T: TStrings); begin Self.Params := T; end;
procedure TUIBDatabase_DatabaseName_R(Self: TUIBDataBase; var T: String); begin T := Self.DatabaseName; end;
procedure TUIBDatabase_DatabaseName_W(Self: TUIBDataBase; const T: String); begin Self.DatabaseName := T; end;
procedure TUIBDatabase_SQLDialect_R(Self: TUIBDataBase; var T: Integer); begin T := Self.SQLDialect; end;
procedure TUIBDatabase_SQLDialect_W(Self: TUIBDataBase; const T: Integer); begin Self.SQLDialect := T; end;
procedure TUIBDatabase_CharacterSet_R(Self: TUIBDataBase; var T: TCharacterSet); begin T := Self.CharacterSet; end;
procedure TUIBDatabase_CharacterSet_W(Self: TUIBDataBase; const T: TCharacterSet); begin Self.CharacterSet := T; end;
procedure TUIBDatabase_Username_R(Self: TUIBDataBase; var T: String); begin T := Self.UserName; end;
procedure TUIBDatabase_Username_W(Self: TUIBDataBase; const T: String); begin Self.UserName := T; end;
procedure TUIBDatabase_Password_R(Self: TUIBDataBase; var T: String); begin T := Self.Password; end;
procedure TUIBDatabase_Password_W(Self: TUIBDataBase; const T: String); begin Self.Password := T; end;
procedure TUIBDatabase_LibraryName_R(Self: TUIBDataBase; var T: String); begin T := Self.LibraryName; end;
procedure TUIBDatabase_LibraryName_W(Self: TUIBDataBase; const T: String); begin Self.LibraryName := T; end;
procedure TUIBDatabase_Connected_R(Self: TUIBDataBase; var T: Boolean); begin T := Self.Connected; end;
procedure TUIBDatabase_Connected_W(Self: TUIBDataBase; const T: Boolean); begin Self.Connected := T; end;
procedure TUIBDatabase_Role_R(Self: TUIBDataBase; var T: String); begin T := Self.Role; end;
procedure TUIBDatabase_Role_W(Self: TUIBDataBase; const T: String); begin Self.Role := T; end;

procedure TUIBTransaction_InTransaction_R(Self: TUIBTransaction; var T: Boolean); begin T := Self.InTransaction; end;
procedure TUIBTransaction_Database_R(Self: TUIBTransaction; var T: TUIBDataBase); begin T := Self.DataBase; end;
procedure TUIBTransaction_Database_W(Self: TUIBTransaction; var T: TUIBDataBase); begin Self.DataBase := T; end;
procedure TUIBTransaction_Options_R(Self: TUIBTransaction; var T: TTransParams); begin T := Self.Options; end;
procedure TUIBTransaction_Options_W(Self: TUIBTransaction; var T: TTransParams); begin Self.Options := T; end;
procedure TUIBTransaction_AutoStart_R(Self: TUIBTransaction; var T: Boolean); begin T := Self.AutoStart; end;
procedure TUIBTransaction_AutoStart_W(Self: TUIBTransaction; var T: Boolean); begin Self.AutoStart := T; end;
procedure TUIBTransaction_AutoStop_R(Self: TUIBTransaction; var T: Boolean); begin T := Self.AutoStop; end;
procedure TUIBTransaction_AutoStop_W(Self: TUIBTransaction; var T: Boolean); begin Self.AutoStop := T; end;
procedure TUIBTransaction_DefaultAction_R(Self: TUIBTransaction; var T: TEndTransMode); begin T := Self.DefaultAction; end;
procedure TUIBTransaction_DefaultAction_W(Self: TUIBTransaction; var T: TEndTransMode); begin Self.DefaultAction := T; end;

procedure TSQLDA_IsBlob_R(Self: TSQLDA; var T: Boolean; I : Word); begin T := Self.IsBlob[I]; end;
procedure TSQLDA_IsBlobText_R(Self: TSQLDA; var T: Boolean; I : Word); begin T := Self.IsBlobText[I]; end;
procedure TSQLDA_IsNullable_R(Self: TSQLDA; var T: Boolean; I : Word); begin T := Self.IsNullable[I]; end;
procedure TSQLDA_IsNumeric_R(Self: TSQLDA; var T: Boolean; I : Word); begin T := Self.IsNumeric[I]; end;
procedure TSQLDA_FieldCount_R(Self: TSQLDA; var T: Integer); begin T := Self.FieldCount; end;
procedure TSQLDA_SQLType_R(Self: TSQLDA; var T: SmallInt; I : Word); begin T := Self.SQLType[I]; end;
procedure TSQLDA_SQLLen_R(Self: TSQLDA; var T: SmallInt; I : Word); begin T := Self.SQLLen[I]; end;
procedure TSQLDA_SQLScale_R(Self: TSQLDA; var T: SmallInt; I : Word); begin T := Self.SQLScale[I]; end;
procedure TSQLDA_FieldType_R(Self: TSQLDA; var T: TUIBFieldType; I : Word); begin T := Self.FieldType[I]; end;
procedure TSQLDA_CharacterSet_R(Self: TSQLDA; var T: TCharacterSet); begin T := Self.CharacterSet; end;
procedure TSQLDA_CharacterSet_W(Self: TSQLDA; const T: TCharacterSet); begin Self.CharacterSet := T; end;
procedure TSQLDA_IsNull_R(Self: TSQLDA; var T: Boolean; I : Word); begin T := Self.IsNull[I]; end;
procedure TSQLDA_IsNull_W(Self: TSQLDA; const T: Boolean; I : Word); begin Self.IsNull[I] := T; end;
procedure TSQLDA_AsSmallint_R(Self: TSQLDA; var T: Smallint; I : Word); begin T := Self.AsSmallint[I]; end;
procedure TSQLDA_AsSmallint_W(Self: TSQLDA; const T: Smallint; I : Word); begin Self.AsSmallint[I] := T; end;
procedure TSQLDA_AsInteger_R(Self: TSQLDA; var T: Integer; I : Word); begin T := Self.AsInteger[I]; end;
procedure TSQLDA_AsInteger_W(Self: TSQLDA; const T: Integer; I : Word); begin Self.AsInteger[I] := T; end;
procedure TSQLDA_AsSingle_R(Self: TSQLDA; var T: Single; I : Word); begin T := Self.AsSingle[I]; end;
procedure TSQLDA_AsSingle_W(Self: TSQLDA; const T: Single; I : Word); begin Self.AsSingle[I] := T; end;
procedure TSQLDA_AsDouble_R(Self: TSQLDA; var T: Double; I : Word); begin T := Self.AsDouble[I]; end;
procedure TSQLDA_AsDouble_W(Self: TSQLDA; const T: Double; I : Word); begin Self.AsDouble[I] := T; end;
procedure TSQLDA_AsInt64_R(Self: TSQLDA; var T: Int64; I : Word); begin T := Self.AsInt64[I]; end;
procedure TSQLDA_AsInt64_W(Self: TSQLDA; const T: Int64; I : Word); begin Self.AsInt64[I] := T; end;
procedure TSQLDA_AsString_R(Self: TSQLDA; var T: String; I : Word); begin T := Self.AsString[I]; end;
procedure TSQLDA_AsString_W(Self: TSQLDA; const T: String; I : Word); begin Self.AsString[I] := T; end;
procedure TSQLDA_AsDateTime_R(Self: TSQLDA; var T: TDateTime; I : Word); begin T := Self.AsDateTime[I]; end;
procedure TSQLDA_AsDateTime_W(Self: TSQLDA; const T: TDateTime; I : Word); begin Self.AsDateTime[I] := T; end;
procedure TSQLDA_AsBoolean_R(Self: TSQLDA; var T: Boolean; I : Word); begin T := Self.AsBoolean[I]; end;
procedure TSQLDA_AsBoolean_W(Self: TSQLDA; const T: Boolean; I : Word); begin Self.AsBoolean[I] := T; end;
procedure TSQLDA_AsDate_R(Self: TSQLDA; var T: Integer; I : Word); begin T := Self.AsDate[I]; end;
procedure TSQLDA_AsDate_W(Self: TSQLDA; const T: Integer; I : Word); begin Self.AsDate[I] := T; end;
procedure TSQLDA_AsTime_R(Self: TSQLDA; var T: Cardinal; I : Word); begin T := Self.AsTime[I]; end;
procedure TSQLDA_AsTime_W(Self: TSQLDA; const T: Cardinal; I : Word); begin Self.AsTime[I] := T; end;
procedure TSQLDA_AsVariant_R(Self: TSQLDA; var T: Variant; I : Word); begin T := Self.AsVariant[I]; end;
procedure TSQLDA_AsVariant_W(Self: TSQLDA; const T: Variant; I : Word); begin Self.AsVariant[I] := T; end;
procedure TSQLDA_ByNameIsBlob_R(Self: TSQLDA; var T: Boolean; S : String); begin T := Self.ByNameIsBlob[S]; end;
procedure TSQLDA_ByNameIsBlobText_R(Self: TSQLDA; var T: Boolean; S : String); begin T := Self.ByNameIsBlobText[S]; end;
procedure TSQLDA_ByNameIsNull_R(Self: TSQLDA; var T: Boolean; S : String); begin T := Self.ByNameIsNull[S]; end;
procedure TSQLDA_ByNameAsSmallint_R(Self: TSQLDA; var T: Smallint; S : String); begin T := Self.ByNameAsSmallint[S]; end;
procedure TSQLDA_ByNameAsSmallint_W(Self: TSQLDA; const T: Smallint; S : String); begin Self.ByNameAsSmallint[S] := T; end;
procedure TSQLDA_ByNameAsInteger_R(Self: TSQLDA; var T: Integer; S : String); begin T := Self.ByNameAsInteger[S]; end;
procedure TSQLDA_ByNameAsInteger_W(Self: TSQLDA; const T: Integer; S : String); begin Self.ByNameAsInteger[S] := T; end;
procedure TSQLDA_ByNameAsSingle_R(Self: TSQLDA; var T: Single; S : String); begin T := Self.ByNameAsSingle[S]; end;
procedure TSQLDA_ByNameAsSingle_W(Self: TSQLDA; const T: Single; S : String); begin Self.ByNameAsSingle[S] := T; end;
procedure TSQLDA_ByNameAsDouble_R(Self: TSQLDA; var T: Double; S : String); begin T := Self.ByNameAsDouble[S]; end;
procedure TSQLDA_ByNameAsDouble_W(Self: TSQLDA; const T: Double; S : String); begin Self.ByNameAsDouble[S] := T; end;
procedure TSQLDA_ByNameAsInt64_R(Self: TSQLDA; var T: Int64; S : String); begin T := Self.ByNameAsInt64[S]; end;
procedure TSQLDA_ByNameAsInt64_W(Self: TSQLDA; const T: Int64; S : String); begin Self.ByNameAsInt64[S] := T; end;
procedure TSQLDA_ByNameAsString_R(Self: TSQLDA; var T: String; S : String); begin T := Self.ByNameAsString[S]; end;
procedure TSQLDA_ByNameAsString_W(Self: TSQLDA; const T: String; S : String); begin Self.ByNameAsString[S] := T; end;
procedure TSQLDA_ByNameAsVariant_R(Self: TSQLDA; var T: Variant; S : String); begin T := Self.ByNameAsVariant[S]; end;
procedure TSQLDA_ByNameAsVariant_W(Self: TSQLDA; const T: Variant; S : String); begin Self.ByNameAsVariant[S] := T; end;
procedure TSQLDA_ByNameAsDateTime_R(Self: TSQLDA; var T: TDateTime; S : String); begin T := Self.ByNameAsDateTime[S]; end;
procedure TSQLDA_ByNameAsDateTime_W(Self: TSQLDA; const T: TDateTime; S : String); begin Self.ByNameAsDateTime[S] := T; end;
procedure TSQLDA_ByNameAsBoolean_R(Self: TSQLDA; var T: Boolean; S : String); begin T := Self.ByNameAsBoolean[S]; end;
procedure TSQLDA_ByNameAsBoolean_W(Self: TSQLDA; const T: Boolean; S : String); begin Self.ByNameAsBoolean[S] := T; end;
procedure TSQLDA_ByNameAsDate_R(Self: TSQLDA; var T: Integer; S : String); begin T := Self.ByNameAsDate[S]; end;
procedure TSQLDA_ByNameAsDate_W(Self: TSQLDA; const T: Integer; S : String); begin Self.ByNameAsDate[S] := T; end;

procedure TSQLResult_SqlName_R(Self: TSQLResult; var T: String; I : Word); begin T := Self.SqlName[I]; end;
procedure TSQLResult_RelName_R(Self: TSQLResult; var T: String; I : Word); begin T := Self.RelName[I]; end;
procedure TSQLResult_OwnName_R(Self: TSQLResult; var T: String; I : Word); begin T := Self.OwnName[I]; end;
procedure TSQLResult_AliasName_R(Self: TSQLResult; var T: String; I : Word); begin T := Self.AliasName[I]; end;
procedure TSQLResult_ByNameIsNullable_R(Self: TSQLResult; var T: Boolean; S : String); begin T := Self.ByNameIsNullable[S]; end;
procedure TSQLResult_ByNameAsTime_R(Self: TSQLResult; var T: Cardinal; S : String); begin T := Self.ByNameAsTime[S]; end;
procedure TSQLResult_Values_R(Self: TSQLResult; var T: Variant; S : String); begin T := Self.Values[S]; end;
procedure TSQLResult_AsStream_R(Self: TSQLResult; var T: TStream; I : Word);
begin
  T := TMemoryStream.Create;
  try Self.ReadBlob(i, T);
  except t.Free; raise; end;
end;
procedure TSQLResult_ByNameAsStream_R(Self: TSQLResult; var T: TStream; S : String);
begin
  T := TMemoryStream.Create;
  try Self.ReadBlob(s, T);
  except t.Free; raise; end;
end;

procedure TSQLParams_Values_R(Self: TSQLParams; var T: Variant; S : String); begin T := Self.Values[S]; end;
procedure TSQLParams_FieldName_R(Self: TSQLParams; var T: String; I : Word); begin T := Self.FieldName[I]; end;
procedure TSQLParams_ParamCount_R(Self: TSQLParams; var T: Word); begin T := Self.ParamCount; end;
procedure TSQLParams_MaxSQLLen_R(Self: TSQLParams; var T: Smallint; I : Word); begin T := Self.MaxSqlLen[I]; end;

procedure TUIBStatement_Fields_R(Self: TUIBStatement; var T: TSQLResult); begin T := Self.Fields; end;
procedure TUIBStatement_Params_R(Self: TUIBStatement; var T: TSQLParams); begin T := Self.Params; end;
procedure TUIBStatement_CursorName_R(Self: TUIBStatement; var T: String); begin T := Self.CursorName; end;
procedure TUIBStatement_CurrentState_R(Self: TUIBStatement; var T: TQueryState); begin T := Self.CurrentState; end;
procedure TUIBStatement_EOF_R(Self: TUIBStatement; var T: Boolean); begin T := Self.Eof; end;
procedure TUIBStatement_BOF_R(Self: TUIBStatement; var T: Boolean); begin T := Self.Bof; end;
procedure TUIBStatement_ParseParams_R(Self: TUIBStatement; var T: Boolean); begin T := Self.ParseParams; end;
procedure TUIBStatement_ParseParams_W(Self: TUIBStatement; const T: Boolean); begin Self.ParseParams := T; end;
procedure TUIBStatement_Plan_R(Self: TUIBStatement; var T: String); begin T := Self.Plan; end;
procedure TUIBStatement_StatementType_R(Self: TUIBStatement; var T: TUIBStatementType); begin T := Self.StatementType; end;
procedure TUIBStatement_RowsAffected_R(Self: TUIBStatement; var T: Cardinal); begin T := Self.RowsAffected; end;
procedure TUIBStatement_ParamAsStream_W(Self: TUIBStatement; const T: TStream; I : Word); begin Self.ParamsSetBlob(I, T); end;
procedure TUIBStatement_ParamByNameAsStream_W(Self: TUIBStatement; const T: TStream; S : String); begin Self.ParamsSetBlob(S, T); end;
procedure TUIBStatement_SQL_R(Self: TUIBStatement; var T: TStrings); begin T := Self.SQL; end;
procedure TUIBStatement_SQL_W(Self: TUIBStatement; const T: TStrings); begin Self.SQL := T; end;
procedure TUIBStatement_Transaction_R(Self: TUIBStatement; var T: TUIBTransaction); begin T := Self.Transaction; end;
procedure TUIBStatement_Transaction_W(Self: TUIBStatement; const T: TUIBTransaction); begin Self.Transaction := T; end;
procedure TUIBStatement_Database_R(Self: TUIBStatement; var T: TUIBDataBase); begin T := Self.DataBase; end;
procedure TUIBStatement_Database_W(Self: TUIBStatement; const T: TUIBDataBase); begin Self.DataBase := T; end;
procedure TUIBStatement_CachedFetch_R(Self: TUIBStatement; var T: Boolean); begin T := Self.CachedFetch; end;
procedure TUIBStatement_CachedFetch_W(Self: TUIBStatement; const T: Boolean); begin Self.CachedFetch := T; end;
procedure TUIBStatement_FetchBlobs_R(Self: TUIBStatement; var T: Boolean); begin T := Self.FetchBlobs; end;
procedure TUIBStatement_FetchBlobs_W(Self: TUIBStatement; const T: Boolean); begin Self.FetchBlobs := T; end;
procedure TUIBStatement_BufferChunks_R(Self: TUIBStatement; var T: Cardinal); begin T := Self.BufferChunks; end;
procedure TUIBStatement_BufferChunks_W(Self: TUIBStatement; const T: Cardinal); begin Self.BufferChunks := T; end;
procedure TUIBStatement_ParsedSQL_R(Self: TUIBStatement; var T: String); begin T := Self.ParsedSQL; end;

procedure TUIBQuery_QuickScript_R(Self: TUIBQuery; var T: Boolean); begin T := Self.QuickScript; end;
procedure TUIBQuery_QuickScript_W(Self: TUIBQuery; const T: Boolean); begin Self.QuickScript := T; end;

procedure PS_Register_UIB_C(ACompiler : TPSPascalCompiler);
var
  pscTUIBComponent,
  pscTUIBStatement,
  pscTUIBDatabase,
  pscTUIBTransaction,
  pscTSQLDA,
  pscTSQLResult,
  pscTUIBQuery,
  pscTSQLParams : TPSCompileTimeClass;
begin
  RegisterEnum(ACompiler, TypeInfo(TCharacterSet));
  RegisterEnum(ACompiler, TypeInfo(TEndTransMode));
  RegisterEnum(ACompiler, TypeInfo(TTransParam));
  ACompiler.AddTypeS('TTransParams', 'set of TTransParam');
  RegisterEnum(ACompiler, TypeInfo(TUIBFieldType));
  RegisterEnum(ACompiler, TypeInfo(TQueryState));
  RegisterEnum(ACompiler, TypeInfo(TUIBStatementType));

  ACompiler.AddDelphiFunction('function StrToCharacterSet(const CharacterSet: String): TCharacterSet;');

  pscTUIBComponent := ACompiler.AddClass(ACompiler.FindClass('TComponent'), TUIBComponent);

  pscTUIBDatabase := ACompiler.AddClass(pscTUIBComponent, TUIBDataBase);
  with pscTUIBDatabase do
  begin
    RegisterMethod('constructor Create(AOwner : TComponent);');
    RegisterMethod('procedure ExecuteImmediate(const Statement: string);');
    RegisterMethod('procedure CreateDatabase(DefaultCharacterSet: TCharacterSet; PageSize: Integer);');
    RegisterMethod('procedure DropDatabase;');
    RegisterMethod('procedure ActiveAllTriggers;');
    RegisterMethod('procedure DeactiveAllTriggers;');
    RegisterMethod('procedure RecomputeSelectivityIndices;');
    RegisterMethod('procedure RecompileAllProcedures;');
    RegisterMethod('procedure RecompileAllTriggers;');
    RegisterProperty('Params', 'TStrings', iptRW);
    RegisterProperty('DatabaseName', 'String', iptRW);
    RegisterProperty('SQLDialect', 'Integer', iptRW);
    RegisterProperty('CharacterSet', 'TCharacterSet', iptRW);
    RegisterProperty('Username', 'String', iptRW);
    RegisterProperty('Password', 'String', iptRW);
    RegisterProperty('LibraryName', 'String', iptRW);
    RegisterProperty('Connected', 'Boolean', iptRW);
    RegisterProperty('Role', 'String', iptRW);
  end;

  pscTUIBTransaction := ACompiler.AddClass(pscTUIBComponent, TUIBTransaction);
  with pscTUIBTransaction do
  begin
    RegisterMethod('constructor Create(AOwner : TComponent);');
    RegisterMethod('Procedure StartTransaction;');
    RegisterMethod('procedure Commit;');
    RegisterMethod('procedure CommitRetaining;');
    RegisterMethod('procedure RollBack;');
    RegisterMethod('procedure RollBackRetaining;');
    RegisterMethod('procedure ExecuteImmediate(const sql: string);');
    RegisterMethod('function GetTransactionID: Cardinal;');
    RegisterProperty('InTransaction', 'Boolean', iptR);
    RegisterProperty('Database', 'TUIBDatabase', iptRW);
    RegisterProperty('Options', 'TTransParams', iptRW);
    RegisterProperty('AutoStart', 'Boolean', iptRW);
    RegisterProperty('AutoStop', 'Boolean', iptRW);
    RegisterProperty('DefaultAction', 'TEndTransMode', iptRW);
  end;


  pscTSQLDA := ACompiler.AddClass(ACompiler.FindClass('TObject'), TSQLDA);
  with pscTSQLDA do
  begin
    RegisterMethod('constructor Create(ACharacterSet: TCharacterSet); virtual;');
    RegisterMethod('procedure CheckRange(const Index: Word); virtual;');
    RegisterMethod('function GetFieldIndex(const name: AnsiString): Word; virtual;');
    RegisterMethod('function TryGetFieldIndex(const name: AnsiString; out index: Word): Boolean; virtual;');
    RegisterProperty('IsBlob', 'Boolean Word', iptR);
    RegisterProperty('IsBlobText', 'Boolean Word', iptR);
    RegisterProperty('IsNullable', 'Boolean Word', iptR);
    RegisterProperty('IsNumeric', 'Boolean Word', iptR);
    RegisterProperty('FieldCount', 'Integer', iptR);
    RegisterProperty('SQLType', 'Smallint Word', iptR);
    RegisterProperty('SQLLen', 'Smallint Word', iptR);
    RegisterProperty('SQLScale', 'Smallint Word', iptR);
    RegisterProperty('FieldType', 'TUIBFieldType Word', iptR);
    RegisterProperty('CharacterSet', 'TCharacterSet', iptRW);
    RegisterProperty('IsNull', 'Boolean Word', iptRW);
    RegisterProperty('AsSmallint', 'Smallint Word', iptRW);
    RegisterProperty('AsInteger', 'Integer Word', iptRW);
    RegisterProperty('AsSingle', 'Single Word', iptRW);
    RegisterProperty('AsDouble', 'Double Word', iptRW);
    RegisterProperty('AsInt64', 'Int64 Word', iptRW);
    RegisterProperty('AsString', 'String Word', iptRW);
    RegisterProperty('AsDateTime', 'TDateTime Word', iptRW);
    RegisterProperty('AsBoolean', 'Boolean Word', iptRW);
    RegisterProperty('AsDate', 'Integer Word', iptRW);
    RegisterProperty('AsTime', 'Cardinal Word', iptRW);
    RegisterProperty('AsVariant', 'Variant Word', iptRW);
    RegisterProperty('ByNameIsBlob', 'Boolean String', iptRW);
    RegisterProperty('ByNameIsBlobText', 'Boolean String', iptRW);
    RegisterProperty('ByNameIsNull', 'Boolean String', iptRW);
    RegisterProperty('ByNameAsSmallint', 'Smallint String', iptRW);
    RegisterProperty('ByNameAsInteger', 'Integer String', iptRW);
    RegisterProperty('ByNameAsSingle', 'Single String', iptRW);
    RegisterProperty('ByNameAsDouble', 'Double String', iptRW);
    RegisterProperty('ByNameAsInt64', 'Int64 String', iptRW);
    RegisterProperty('ByNameAsString', 'String String', iptRW);
    RegisterProperty('ByNameAsVariant', 'Variant String', iptRW);
    RegisterProperty('ByNameAsDateTime', 'TDateTime String', iptRW);
    RegisterProperty('ByNameAsBoolean', 'Boolean String', iptRW);
    RegisterProperty('ByNameAsDate', 'Integer String', iptRW);
  end;

  pscTSQLResult := ACompiler.AddClass(pscTSQLDA, TSQLResult);
  with pscTSQLResult do
  begin
    RegisterMethod('constructor Create(Charset: TCharacterSet; Fields: SmallInt; CachedFetch, FetchBlobs: boolean; BufferChunks: Cardinal); reintroduce;');
    RegisterMethod('procedure ClearRecords; virtual;');
    RegisterMethod('procedure GetRecord(const Index : Integer); virtual;');
    RegisterMethod('procedure SaveToStream(Stream: TStream); virtual;');
    RegisterMethod('procedure LoadFromStream(Stream: TStream); virtual;');
    RegisterMethod('procedure Next; virtual;');
    RegisterMethod('function GetBlobSize(const Index: Word): Cardinal; virtual;');
    RegisterProperty('SqlName', 'String Word', iptR);
    RegisterProperty('RelName', 'String Word', iptR);
    RegisterProperty('OwnName', 'String Word', iptR);
    RegisterProperty('AliasName', 'String Word', iptR);
    RegisterProperty('ByNameIsNullable', 'Boolean String', iptR);
    RegisterProperty('ByNameAsTime', 'Cardinal String', iptR);
    RegisterProperty('AsStream', 'TStream Word', iptR);
    RegisterProperty('ByNameAsStream', 'TStream String', iptR);
    RegisterProperty('Values', 'Variant String', iptR);
    SetDefaultPropery('Values');
  end;

  pscTSQLParams := ACompiler.AddClass(pscTSQLDA, TSQLParams);
  with pscTSQLParams do
  begin
    RegisterMethod('constructor Create(Charset: TCharacterSet); override;');
    RegisterMethod('procedure Clear; virtual;');
    RegisterMethod('function Parse(const SQL: string): string; virtual;');
    RegisterMethod('function TryGetFieldIndex(const name: AnsiString; out Index: Word): Boolean; override;');
    RegisterMethod('function GetFieldIndex(const name: AnsiString): Word; override;');
    RegisterProperty('Values', 'Variant String', iptR);
    SetDefaultPropery('Values');
    RegisterProperty('FieldName', 'String Word', iptR);
    RegisterProperty('ParamCount', 'Word', iptR);
    RegisterProperty('MaxSQLLen', 'Smallint Word', iptR);
  end;

  pscTUIBStatement := ACompiler.AddClass(pscTUIBComponent, TUIBStatement);
  with pscTUIBStatement do
  begin
    RegisterMethod('constructor Create(AOwner : TComponent);');
    RegisterMethod('procedure Close();');
    RegisterMethod('procedure CloseCursor;');
    RegisterMethod('procedure FetchAll;');
    RegisterMethod('procedure Open();');
    RegisterMethod('procedure Prepare();');
    RegisterMethod('procedure Execute;');
    RegisterMethod('procedure ExecSQL;');
    RegisterMethod('procedure Next;');
    RegisterMethod('procedure Prior;');
    RegisterMethod('procedure Last;');
    RegisterMethod('procedure First;');
    RegisterMethod('function FieldBlobSize(const Index: Word): Cardinal;');
    RegisterMethod('function ParamBlobSize(const Index: Word): Cardinal;');
    RegisterMethod('procedure AffectedRows(out SelectedRows, InsertedRows, UpdatedRows, DeletedRows: Cardinal);');
    RegisterProperty('Fields', 'TSQLResult', iptR);
    RegisterProperty('Params', 'TSQLParams', iptR);
    RegisterProperty('CursorName', 'String', iptR);
    RegisterProperty('CurrentState', 'TQueryState', iptR);
    RegisterProperty('EOF', 'Boolean', iptR);
    RegisterProperty('BOF', 'Boolean', iptR);
    RegisterProperty('ParseParams', 'Boolean', iptRW);
    RegisterProperty('Plan', 'String', iptR);
    RegisterProperty('StatementType', 'TUIBStatementType', iptR);
    RegisterProperty('RowsAffected', 'Cardinal', iptR);
    RegisterProperty('ParamAsStream', 'TStream Word', iptW);
    RegisterProperty('ParamByNameAsStream', 'TStream String', iptW);
    RegisterProperty('SQL', 'TStrings', iptRW);
    RegisterProperty('Transaction', 'TUIBTransaction', iptRW);
    RegisterProperty('Database', 'TUIBDatabase', iptRW);
    RegisterProperty('CachedFetch', 'Boolean', iptRW);
    RegisterProperty('FetchBlobs', 'Boolean', iptRW);
    RegisterProperty('BufferChunks', 'Cardinal', iptRW);
    RegisterProperty('ParsedSQL', 'String', iptR);
  end;

  pscTUIBQuery := ACompiler.AddClass(pscTUIBStatement, TUIBQuery);
  with pscTUIBQuery do
  begin
    RegisterProperty('QuickScript', 'Boolean', iptRW);
  end;
end;

procedure PS_Register_UIB_R(AExec : TPSExec; ARCi : TPSRuntimeClassImporter);
begin
  AExec.RegisterDelphiFunction(@_StrToCharacterSet, 'StrToCharacterSet', cdRegister);

  with ARCi.Add(TUIBDataBase) do
  begin
    RegisterConstructor(@TUIBDataBase.Create, 'Create');
    RegisterMethod(@TUIBDatabase.ExecuteImmediate, 'ExecuteImmediate');
    RegisterMethod(@TUIBDatabase.CreateDatabase, 'CreateDatabase');
    RegisterMethod(@TUIBDatabase.DropDatabase, 'DropDatabase');
    RegisterMethod(@TUIBDatabase.ActiveAllTriggers, 'ActiveAllTriggers');
    RegisterMethod(@TUIBDatabase.DeactiveAllTriggers, 'DeactiveAllTriggers');
    RegisterMethod(@TUIBDatabase.RecomputeSelectivityIndices, 'RecomputeSelectivityIndices');
    RegisterMethod(@TUIBDatabase.RecompileAllProcedures, 'RecompileAllProcedures');
    RegisterMethod(@TUIBDatabase.RecompileAllTriggers, 'RecompileAllTriggers');
    RegisterPropertyHelper(@TUIBDatabase_Params_R, @TUIBDatabase_Params_W, 'Params');
    RegisterPropertyHelper(@TUIBDatabase_DatabaseName_R, @TUIBDatabase_DatabaseName_W, 'DatabaseName');
    RegisterPropertyHelper(@TUIBDatabase_SQLDialect_R, @TUIBDatabase_SQLDialect_W, 'SQLDialect');
    RegisterPropertyHelper(@TUIBDatabase_CharacterSet_R, @TUIBDatabase_CharacterSet_W, 'CharacterSet');
    RegisterPropertyHelper(@TUIBDatabase_Username_R, @TUIBDatabase_Username_W, 'Username');
    RegisterPropertyHelper(@TUIBDatabase_Password_R, @TUIBDatabase_Password_W, 'Password');
    RegisterPropertyHelper(@TUIBDatabase_LibraryName_R, @TUIBDatabase_LibraryName_W, 'LibraryName');
    RegisterPropertyHelper(@TUIBDatabase_Connected_R, @TUIBDatabase_Connected_W, 'Connected');
    RegisterPropertyHelper(@TUIBDatabase_Role_R, @TUIBDatabase_Role_W, 'Role');
  end;

  with ARCi.Add(TUIBTransaction) do
  begin
    RegisterConstructor(@TUIBTransaction.Create, 'Create');
    RegisterMethod(@TUIBTransaction.StartTransaction, 'StartTransaction');
    RegisterMethod(@TUIBTransaction.Commit, 'Commit');
    RegisterMethod(@TUIBTransaction.CommitRetaining, 'CommitRetaining');
    RegisterMethod(@TUIBTransaction.Rollback, 'Rollback');
    RegisterMethod(@TUIBTransaction.RollbackRetaining, 'RollbackRetaining');
    RegisterMethod(@TUIBTransaction.ExecuteImmediate, 'ExecuteImmediate');
    RegisterMethod(@TUIBTransaction.GetTransactionID, 'GetTransactionID');
    RegisterPropertyHelper(@TUIBTransaction_InTransaction_R, nil, 'InTransaction');
    RegisterPropertyHelper(@TUIBTransaction_Database_R, @TUIBTransaction_Database_W, 'Database');
    RegisterPropertyHelper(@TUIBTransaction_Options_R, @TUIBTransaction_Options_W, 'Options');
    RegisterPropertyHelper(@TUIBTransaction_AutoStart_R, @TUIBTransaction_AutoStart_W, 'AutoStart');
    RegisterPropertyHelper(@TUIBTransaction_AutoStop_R, @TUIBTransaction_AutoStop_W, 'AutoStop');
    RegisterPropertyHelper(@TUIBTransaction_DefaultAction_R, @TUIBTransaction_DefaultAction_W, 'DefaultAction');
  end;

  with ARCi.Add(TSQLDA) do
  begin
    RegisterVirtualConstructor(@TSQLDa.Create, 'Create');
    RegisterVirtualMethod(@TSQLDa.CheckRange, 'CheckRange');
    RegisterVirtualMethod(@TSQLDA.GetFieldIndex, 'GetFieldIndex');
    RegisterVirtualMethod(@TSQLDA.TryGetFieldIndex, 'TryGetFieldIndex');
    RegisterPropertyHelper(@TSQLDA_IsBlob_R, nil, 'IsBlob');
    RegisterPropertyHelper(@TSQLDA_IsBlobText_R, nil, 'IsBlobText');
    RegisterPropertyHelper(@TSQLDA_IsBlobText_R, nil, 'IsBlobText');
    RegisterPropertyHelper(@TSQLDA_IsNullable_R, nil, 'IsNullable');
    RegisterPropertyHelper(@TSQLDA_IsNumeric_R, nil, 'IsNumeric');
    RegisterPropertyHelper(@TSQLDA_FieldCount_R, nil, 'FieldCount');
    RegisterPropertyHelper(@TSQLDA_SQLType_R, nil, 'SQLType');
    RegisterPropertyHelper(@TSQLDA_SQLLen_R, nil, 'SQLLen');
    RegisterPropertyHelper(@TSQLDA_SQLScale_R, nil, 'SQLScale');
    RegisterPropertyHelper(@TSQLDA_FieldType_R, nil, 'FieldType');
    RegisterPropertyHelper(@TSQLDA_CharacterSet_R, @TSQLDA_CharacterSet_W, 'CharacterSet');
    RegisterPropertyHelper(@TSQLDA_IsNull_R, @TSQLDA_IsNull_W, 'IsNull');
    RegisterPropertyHelper(@TSQLDA_AsSmallint_R, @TSQLDA_AsSmallint_W, 'AsSmallint');
    RegisterPropertyHelper(@TSQLDA_AsInteger_R, @TSQLDA_AsInteger_W, 'AsInteger');
    RegisterPropertyHelper(@TSQLDA_AsSingle_R, @TSQLDA_AsSingle_W, 'AsSingle');
    RegisterPropertyHelper(@TSQLDA_AsDouble_R, @TSQLDA_AsDouble_W, 'AsDouble');
    RegisterPropertyHelper(@TSQLDA_AsInt64_R, @TSQLDA_AsInt64_W, 'AsInt64');
    RegisterPropertyHelper(@TSQLDA_AsString_R, @TSQLDA_AsString_W, 'AsString');
    RegisterPropertyHelper(@TSQLDA_AsDateTime_R, @TSQLDA_AsDateTime_W, 'AsDateTime');
    RegisterPropertyHelper(@TSQLDA_AsBoolean_R, @TSQLDA_AsBoolean_W, 'AsBoolean');
    RegisterPropertyHelper(@TSQLDA_AsDate_R, @TSQLDA_AsDate_W, 'AsDate');
    RegisterPropertyHelper(@TSQLDA_AsTime_R, @TSQLDA_AsTime_W, 'AsTime');
    RegisterPropertyHelper(@TSQLDA_AsVariant_R, @TSQLDA_AsVariant_W, 'AsVariant');
    RegisterPropertyHelper(@TSQLDA_ByNameIsBlob_R, nil, 'ByNameIsBlob');
    RegisterPropertyHelper(@TSQLDA_ByNameIsBlobText_R, nil, 'ByNameIsBlobText');
    RegisterPropertyHelper(@TSQLDA_ByNameIsNull_R, nil, 'ByNameIsNull');
    RegisterPropertyHelper(@TSQLDA_ByNameAsSmallint_R, @TSQLDA_ByNameAsSmallint_W, 'ByNameAsSmallint');
    RegisterPropertyHelper(@TSQLDA_ByNameAsInteger_R, @TSQLDA_ByNameAsInteger_W, 'ByNameAsInteger');
    RegisterPropertyHelper(@TSQLDA_ByNameAsSingle_R, @TSQLDA_ByNameAsSingle_W, 'ByNameAsSingle');
    RegisterPropertyHelper(@TSQLDA_ByNameAsDouble_R, @TSQLDA_ByNameAsDouble_W, 'ByNameAsDouble');
    RegisterPropertyHelper(@TSQLDA_ByNameAsInt64_R, @TSQLDA_ByNameAsInt64_W, 'ByNameAsInt64');
    RegisterPropertyHelper(@TSQLDA_ByNameAsString_R, @TSQLDA_ByNameAsString_W, 'ByNameAsString');
    RegisterPropertyHelper(@TSQLDA_ByNameAsVariant_R, @TSQLDA_ByNameAsVariant_W, 'ByNameAsVariant');
    RegisterPropertyHelper(@TSQLDA_ByNameAsDateTime_R, @TSQLDA_ByNameAsDateTime_W, 'ByNameAsDateTime');
    RegisterPropertyHelper(@TSQLDA_ByNameAsBoolean_R, @TSQLDA_ByNameAsBoolean_W, 'ByNameAsBoolean');
    RegisterPropertyHelper(@TSQLDA_ByNameAsDate_R, @TSQLDA_ByNameAsDate_W, 'ByNameAsDate');
  end;

  with arci.Add(TSQLResult) do
  begin
    RegisterConstructor(@TSQLResult.Create, 'Create');
    RegisterVirtualMethod(@TSQLResult.ClearRecords, 'ClearRecords');
    RegisterVirtualMethod(@TSQLResult.GetRecord, 'GetRecord');
    RegisterVirtualMethod(@TSQLResult.SaveToStream, 'SaveToStream');
    RegisterVirtualMethod(@TSQLResult.LoadFromStream, 'LoadFromStream');
    RegisterVirtualMethod(@TSQLResult.Next, 'Next');

    RegisterVirtualMethod(@TSQLResult.GetBlobSize, 'GetBlobSize');
    RegisterPropertyHelper(@TSQLResult_SqlName_R, nil, 'SqlName');
    RegisterPropertyHelper(@TSQLResult_RelName_R, nil, 'RelName');
    RegisterPropertyHelper(@TSQLResult_OwnName_R, nil, 'OwnName');
    RegisterPropertyHelper(@TSQLResult_AliasName_R, nil, 'AliasName');
    RegisterPropertyHelper(@TSQLResult_ByNameIsNullable_R, nil, 'ByNameIsNullable');
    RegisterPropertyHelper(@TSQLResult_ByNameAsTime_R, nil, 'ByNameAsTime');
    RegisterPropertyHelper(@TSQLResult_Values_R, nil, 'Values');
    RegisterPropertyHelper(@TSQLResult_AsStream_R, nil, 'AsStream');
    RegisterPropertyHelper(@TSQLResult_ByNameAsStream_R, nil, 'ByNameAsStream');
  end;

  with ARCi.Add(TSQLParams) do
  begin
    RegisterVirtualConstructor(@TSQLParams.Create, 'Create');
    RegisterVirtualMethod(@TSQLParams.Clear, 'Clear');
    RegisterVirtualMethod(@TSQLParams.Parse, 'Parse');
    RegisterVirtualMethod(@TSQLPArams.TryGetFieldIndex, 'TryGetFieldIndex');
    RegisterVirtualMethod(@TSQLPArams.GetFieldIndex, 'GetFieldIndex');
    RegisterPropertyHelper(@TSQLParams_Values_R, nil, 'Values');
    RegisterPropertyHelper(@TSQLParams_FieldName_R, nil, 'FieldName');
    RegisterPropertyHelper(@TSQLParams_ParamCount_R, nil, 'ParamCount');
    RegisterPropertyHelper(@TSQLParams_MaxSQLLen_R, nil, 'MaxSQLLen');
  end;

  with ARCi.Add(TUIBStatement) do
  begin
    RegisterConstructor(@TUIBStatement.Create, 'Create');
    RegisterVirtualMethod(@TUIBStatement.Close, 'Close');
    RegisterMethod(@TUIBStatement.CloseCursor, 'CloseCursor');
    RegisterMethod(@TUIBStatement.FetchAll, 'FetchAll');
    RegisterMethod(@TUIBStatement.Open, 'Open');
    RegisterMethod(@TUIBStatement.Prepare, 'Prepare');
    RegisterMethod(@TUIBStatement.Execute, 'Execute');
    RegisterMethod(@TUIBStatement.ExecSQL, 'ExecSQL');
    RegisterMethod(@TUIBStatement.Next, 'Next');
    RegisterMethod(@TUIBStatement.Prior, 'Prior');
    RegisterMethod(@TUIBStatement.Last, 'Last');
    RegisterMethod(@TUIBStatement.First, 'First');
    RegisterMethod(@TUIBStatement.FieldBlobSize, 'FieldBlobSize');
    RegisterMethod(@TUIBStatement.ParamBlobSize, 'ParamBlobSize');
    RegisterMethod(@TUIBStatement.AffectedRows, 'AffectedRows');
    RegisterPropertyHelper(@TUIBStatement_Fields_R, nil, 'Fields');
    RegisterPropertyHelper(@TUIBStatement_Params_R, nil, 'Params');
    RegisterPropertyHelper(@TUIBStatement_CursorName_R, nil, 'CursorName');
    RegisterPropertyHelper(@TUIBStatement_CurrentState_R, nil, 'QueryState');
    RegisterPropertyHelper(@TUIBStatement_EOF_R, nil, 'EOF');
    RegisterPropertyHelper(@TUIBStatement_BOF_R, nil, 'BOF');
    RegisterPropertyHelper(@TUIBStatement_ParseParams_R, @TUIBStatement_ParseParams_W, 'ParseParams');
    RegisterPropertyHelper(@TUIBStatement_Plan_R, nil, 'Plan');
    RegisterPropertyHelper(@TUIBStatement_StatementType_R, nil, 'StatementType');
    RegisterPropertyHelper(@TUIBStatement_RowsAffected_R, nil, 'RowsAffected');
    RegisterPropertyHelper(nil, @TUIBStatement_ParamAsStream_W, 'ParamAsStream');
    RegisterPropertyHelper(nil, @TUIBStatement_ParamByNameAsStream_W, 'ParamByNameAsStream');
    RegisterPropertyHelper(@TUIBStatement_SQL_R, @TUIBStatement_SQL_W, 'SQL');
    RegisterPropertyHelper(@TUIBStatement_Transaction_R, @TUIBStatement_Transaction_W, 'Transaction');
    RegisterPropertyHelper(@TUIBStatement_Database_R, @TUIBStatement_Database_W, 'Database');
    RegisterPropertyHelper(@TUIBStatement_CachedFetch_R, @TUIBStatement_CachedFetch_W, 'CachedFetch');
    RegisterPropertyHelper(@TUIBStatement_FetchBlobs_R, @TUIBStatement_FetchBlobs_W, 'FetchBlobs');
    RegisterPropertyHelper(@TUIBStatement_BufferChunks_R, @TUIBStatement_BufferChunks_W, 'BufferChunks');
    RegisterPropertyHelper(@TUIBStatement_ParsedSQL_R, nil, 'ParsedSQL');
  end;

  with ARCi.Add(TUIBQuery) do
  begin

  end;
end;

end.
