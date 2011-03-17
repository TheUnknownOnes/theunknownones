unit uFBPScript;

interface

{$UNDEF PS_USESUPPORT}
{$DEFINE FBP_USE_TEXTSTREAM}

uses
  SysUtils, Classes, uPSRuntime, uPSDebugger, uPSUtils, uPSCompiler,
  StrUtils, DateUtils, Windows, UIB, UibLib,
  uPSC_std, uPSR_std, uPSC_controls, uPSR_controls,
  uPSC_Classes, uPSR_Classes, uPSC_Graphics, uPSR_Graphics,
  uPSC_dateutils, upsr_dateutils, uSysTools
  {$IFDEF FBP_USE_TEXTSTREAM},TextStream{$ENDIF};

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

  {$IFDEF FBP_USE_TEXTSTREAM}
  TCSVReader = class
  private
    FFields : array of String;
    FEOF : Boolean;

    FTextStream : TTextStream;
    FDelimiter: Char;
    FQuoteChar: Char;
    function GetActive: Boolean;
    function GetFieldCount: Integer;
  public
    constructor Create();
    destructor Destroy; override;

    procedure Open(AFilename : String);
    procedure Close;
    procedure Next;

    function Field(AIndex : Integer) : String;

    property Active : Boolean read GetActive;
    property EOF : Boolean read FEOF;
    property Delimiter : Char read FDelimiter write FDelimiter;
    property QuoteChar : Char read FQuoteChar write FQuoteChar;
    property FieldCount : Integer read GetFieldCount;
  end;
  {$ENDIF}

implementation

uses uPS_UIB;

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

{$REGION 'TextStream'}
{$IFDEF FBP_USE_TEXTSTREAM}
procedure _WriteCSV(AQuery : TUIBQuery; AFilename : String; ADelimiter, AQuote : Char);
var
  ts : TTextStream;
  idx : Integer;
  s : String;
begin
  ts := TTextStream.Create(AFilename, saWriteWithoutBOM);
  try
    while not AQuery.Eof do
    begin
      for idx := 0 to AQuery.Fields.FieldCount - 1 do
      begin
        s := AQuery.Fields.AsString[idx];

        if AQuote <> #0 then
          s := AQuote + s + AQuote;

        if idx > 0 then
          s := ADelimiter + s;

        ts.WritePart(s);
      end;
      ts.WriteLine('');

      AQuery.Next;
    end;
  finally
    ts.Free;
  end;
end;

procedure TCSVReaderDelimiterR(Self: TCSVReader; var T: Char); begin T := Self.Delimiter; end;
procedure TCSVReaderDelimiterW(Self: TCSVReader; T: Char); begin Self.Delimiter := T; end;

procedure TCSVReaderQuoteCharR(Self: TCSVReader; var T: Char); begin T := Self.QuoteChar; end;
procedure TCSVReaderQuoteCharW(Self: TCSVReader; T: Char); begin Self.QuoteChar := T; end;

procedure TCSVReaderActiveR(Self: TCSVReader; var T: Boolean); begin T := Self.Active; end;

procedure TCSVReaderEOFR(Self: TCSVReader; var T: Boolean); begin T := Self.EOF; end;

procedure TCSVReaderFieldCountR(Self: TCSVReader; var T: Integer); begin T := Self.FieldCount; end;
{$ENDIF}
{$ENDREGION}

{$REGION 'Windows'}
function _CopyFile(AOldName : String; ANewName : String; AFailIfExists : Boolean) : Boolean;
begin
  Result := CopyFileW(PWideChar(WideString(AOldName)), PWideChar(WideString(ANewName)), AFailIfExists);
end;

function _MoveFile(AOldName : String; ANewName : String) : Boolean;
begin
  Result := MoveFileW(PWideChar(WideString(AOldName)), PWideChar(WideString(ANewName)));
end;

function _GetFileTime(FileName: string) : TDateTime;
begin
  if not FileAge(FileName, Result) then
    Result := 0;
end;

function _GetFileSize(FileName: string) : Int64;
var
  fh : Integer;
  hdw : Cardinal;
begin
  fh := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := GetFileSize(fh, @hdw) + hdw;
  finally
    FileClose(fh);
  end;
end;
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
type
  TFDTS = function(const dt1 : TDateTime) : String;
  TFIIS = function(i1 : integer; i2 : integer) : String;
  TFI64IS = function(i641 : Int64; i2 : integer) : String;
  TFCSCAOCS = function(const s1 : String; const aoc1 : array of const) : String;
var
  fdts : TFDTS;
  fiis : TFIIS;
  fi64is : TFI64IS;
  fcscaocs : TFCSCAOCS;
begin
  RIRegister_Std(FRuntimeClassImporter);
  RIRegister_Controls(FRuntimeClassImporter);
  RIRegister_Classes(FRuntimeClassImporter, true);
  RIRegister_Graphics(FRuntimeClassImporter, true);
  RegisterDateTimeLibrary_R(FExec);
  PS_Register_UIB_R(FExec, FRuntimeClassImporter);

  {$REGION 'Windows'}
  FExec.RegisterDelphiFunction(@Sleep, 'Sleep', cdPascal);
  FExec.RegisterDelphiFunction(@_CopyFile, 'CopyFile', cdRegister);
  FExec.RegisterDelphiFunction(@_MoveFile, 'MoveFile', cdRegister);
  {$ENDREGION}

  {$REGION 'Tools'}
  FExec.RegisterDelphiFunction(@_OutputDebugString, 'OutputDebugString', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Log, 'Log', cdRegister);
  {$ENDREGION}

  {$REGION 'Short'}
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
  //FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_Params, 'Params', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_Prepare, 'Prepare', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_Open, 'Open', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_DefaultDatabase, 'DefaultDatabase', cdRegister);
  FExec.RegisterDelphiMethod(Self, @TFBPScript.Short_DefaultTransaction, 'DefaultTransaction', cdRegister);
  {$ENDREGION}

  {$REGION 'SysUtils'}
  fcscaocs := Format; FExec.RegisterDelphiFunction(@fcscaocs, 'Format', cdRegister);
  fiis := IntToHex; FExec.RegisterDelphiFunction(@fiis, 'IntToHex', cdRegister);
  fi64is := IntToHex; FExec.RegisterDelphiFunction(@fi64is, 'Int64ToHex', cdRegister);

  fdts := DateTimeToStr; FExec.RegisterDelphiFunction(@fdts, 'DateTimeToStr', cdRegister);
  fdts := TimeToStr; FExec.RegisterDelphiFunction(@fdts, 'TimeToStr', cdRegister);

  FExec.RegisterDelphiFunction(@FileExists, 'FileExists', cdRegister);
  FExec.RegisterDelphiFunction(@IncludeTrailingPathDelimiter, 'IncludeTrailingPathDelimiter', cdRegister);
  FExec.RegisterDelphiFunction(@ExcludeTrailingPathDelimiter, 'ExcludeTrailingPathDelimiter', cdRegister);
  FExec.RegisterDelphiFunction(@DirectoryExists, 'DirectoryExists', cdRegister);
  FExec.RegisterDelphiFunction(@ForceDirectories, 'ForceDirectories', cdRegister);

  FExec.RegisterDelphiFunction(@FindFirst, 'FindFirst', cdRegister);
  FExec.RegisterDelphiFunction(@FindNext, 'FindNext', cdRegister);
  FExec.RegisterDelphiFunction(@FindClose, 'FindClose', cdRegister);

  FExec.RegisterDelphiFunction(@DeleteFile, 'DeleteFile', cdRegister);
  FExec.RegisterDelphiFunction(@RenameFile, 'RenameFile', cdRegister);
  FExec.RegisterDelphiFunction(@ChangeFileExt, 'ChangeFileExt', cdRegister);
  FExec.RegisterDelphiFunction(@ChangeFilePath, 'ChangeFilePath', cdRegister);
  FExec.RegisterDelphiFunction(@ExtractFilePath, 'ExtractFilePath', cdRegister);
  FExec.RegisterDelphiFunction(@ExtractFileDir, 'ExtractFileDir', cdRegister);
  FExec.RegisterDelphiFunction(@ExtractFileDrive, 'ExtractFileDrive', cdRegister);
  FExec.RegisterDelphiFunction(@ExtractFileName, 'ExtractFileName', cdRegister);
  FExec.RegisterDelphiFunction(@ExtractFileExt, 'ExtractFileExt', cdRegister);
  FExec.RegisterDelphiFunction(@GetCurrentDir, 'GetCurrentDir', cdRegister);
  FExec.RegisterDelphiFunction(@SetCurrentDir, 'SetCurrentDir', cdRegister);
  FExec.RegisterDelphiFunction(@CreateDir, 'CreateDir', cdRegister);
  FExec.RegisterDelphiFunction(@RemoveDir, 'RemoveDir', cdRegister);
  FExec.RegisterDelphiFunction(@_GetFileTime, 'GetFileTime', cdRegister);
  FExec.RegisterDelphiFunction(@_GetFileSize, 'GetFileSize', cdRegister);

  FExec.RegisterDelphiFunction(@IncMonth, 'IncMonth', cdRegister);
  {$ENDREGION}

  {$REGION 'DateUtils'}
  FExec.RegisterDelphiFunction(@IncYear, 'IncYear', cdRegister);
  // function IncMonth is in SysUtils
  FExec.RegisterDelphiFunction(@IncWeek, 'IncWeek', cdRegister);
  FExec.RegisterDelphiFunction(@IncDay, 'IncDay', cdRegister);
  FExec.RegisterDelphiFunction(@IncHour, 'IncHour', cdRegister);
  FExec.RegisterDelphiFunction(@IncMinute, 'IncMinute', cdRegister);
  FExec.RegisterDelphiFunction(@IncSecond, 'IncSecond', cdRegister);
  FExec.RegisterDelphiFunction(@IncMilliSecond, 'IncMilliSecond', cdRegister);

  FExec.RegisterDelphiFunction(@EncodeDateTime, 'EncodeDateTime', cdRegister);
  FExec.RegisterDelphiFunction(@DecodeDateTime, 'DecodeDateTime', cdRegister);
  {$ENDREGION}

  {$REGION 'TextStream'}
  {$IFDEF FBP_USE_TEXTSTREAM}
  FExec.RegisterDelphiFunction(@_WriteCSV, 'WriteCSV', cdRegister);

  with FRuntimeClassImporter.Add(TCSVReader) do
  begin
    RegisterConstructor(@TCSVReader.Create, 'Create');

    RegisterMethod(@TCSVReader.Open, 'Open');
    RegisterMethod(@TCSVReader.Close, 'Close');
    RegisterMethod(@TCSVReader.Next, 'Next');
    RegisterMethod(@TCSVReader.Field, 'Field');

    RegisterPropertyHelper(@TCSVReaderActiveR, nil, 'Active');
    RegisterPropertyHelper(@TCSVReaderDelimiterR, @TCSVReaderDelimiterW, 'Delimiter');
    RegisterPropertyHelper(@TCSVReaderQuoteCharR, @TCSVReaderQuoteCharW, 'QuoteChar');
    RegisterPropertyHelper(@TCSVReaderEOFR, nil, 'EOF');
    RegisterPropertyHelper(@TCSVReaderFieldCountR, nil, 'FieldCount');
  end;

  {$ENDIF}
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

    SIRegister_Std(FCompiler);
    SIRegister_Controls(FCompiler);
    SIRegister_Classes(FCompiler, true);
    SIRegister_Graphics(FCompiler, true);
    RegisterDateTimeLibrary_C(FCompiler);

    PS_Register_UIB_C(FCompiler);

    {$REGION 'Windows'}
    FCompiler.AddTypeCopyN('DWORD', 'Cardinal');
    FCompiler.AddTypeCopyN('TFilename', 'String');
    FCompiler.AddConstantN('MAX_PATH', 'Integer').SetInt(260);
    FCompiler.AddTypeS('TFileTime', 'record dwLowDateTime: DWORD; dwHighDateTime: DWORD; end;');

    FCompiler.AddDelphiFunction('procedure Sleep(AMilliseconds : Cardinal)');

    FCompiler.AddDelphiFunction('function CopyFile(AOldName : String; ANewName : String; AFailIfExists : Boolean) : Boolean');
    FCompiler.AddDelphiFunction('function MoveFile(AOldName : String; ANewName : String) : Boolean');
    {$ENDREGION}

    {$REGION 'Tools'}
    FCompiler.AddDelphiFunction('procedure OutputDebugString(AString : String)');
    FCompiler.AddDelphiFunction('procedure Log(ALog : String)');
    {$ENDREGION}

    {$REGION 'Short'}
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
    //FCompiler.AddDelphiFunction('function Params : TSQLParams');
    FCompiler.AddDelphiFunction('procedure Prepare(ASQL : String)');
    FCompiler.AddDelphiFunction('procedure Open');
    FCompiler.AddDelphiFunction('function DefaultDatabase : TUIBDatabase;');
    FCompiler.AddDelphiFunction('function DefaultTransaction : TUIBTransaction;');
    {$ENDREGION}

    {$REGION 'DateUtils'}
    FCompiler.AddDelphiFunction('function IncYear(const AValue: TDateTime; const ANumberOfYears: Integer): TDateTime;');
    // function IncMonth is in SysUtils
    FCompiler.AddDelphiFunction('function IncWeek(const AValue: TDateTime; const ANumberOfWeeks: Integer): TDateTime;');
    FCompiler.AddDelphiFunction('function IncDay(const AValue: TDateTime; const ANumberOfDays: Integer): TDateTime;');
    FCompiler.AddDelphiFunction('function IncHour(const AValue: TDateTime; const ANumberOfHours: Int64): TDateTime;');
    FCompiler.AddDelphiFunction('function IncMinute(const AValue: TDateTime; const ANumberOfMinutes: Int64): TDateTime;');
    FCompiler.AddDelphiFunction('function IncSecond(const AValue: TDateTime; const ANumberOfSeconds: Int64): TDateTime;');
    FCompiler.AddDelphiFunction('function IncMilliSecond(const AValue: TDateTime; const ANumberOfMilliSeconds: Int64): TDateTime;');

    FCompiler.AddDelphiFunction('function EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;');
    FCompiler.AddDelphiFunction('procedure DecodeDateTime(const AValue: TDateTime; out AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word); ');
    {$ENDREGION}

    {$REGION 'Sysutils'}
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
    FCompiler.AddDelphiFunction('function GetFileTime(FileName: string) : TDateTime');
    FCompiler.AddDelphiFunction('function GetFileSize(FileName: string) : Int64');

    FCompiler.AddDelphiFunction('function Format(const Format: string; const Args: array of const) : String');
    FCompiler.AddDelphiFunction('function IntToHex(Value: Integer; Digits: Integer): string');
    FCompiler.AddDelphiFunction('function Int64ToHex(Value: Int64; Digits: Integer): string');

    FCompiler.AddDelphiFunction('function DateTimeToStr(const DateTime: TDateTime): string');
    FCompiler.AddDelphiFunction('function TimeToStr(const DateTime: TDateTime): string');
    FCompiler.AddDelphiFunction('function IncMonth(const DateTime: TDateTime; NumberOfMonths: Integer): TDateTime;');
    {$ENDREGION}

    {$REGION 'TextStream'}
    {$IFDEF FBP_USE_TEXTSTREAM}
    FCompiler.AddDelphiFunction('procedure WriteCSV(AQuery : TUIBQuery; AFilename : String; ADelimiter, AQuoteChar : Char);');

    with FCompiler.AddClass(FCompiler.FindClass('TObject'), TCSVReader) do
    begin
      RegisterMethod('constructor Create();');

      RegisterMethod('procedure Open(AFilename : String);');
      RegisterMethod('procedure Close;');
      RegisterMethod('procedure Next;');
      RegisterMethod('function Field(AIndex : Integer) : String;');

      RegisterProperty('Active', 'Boolean', iptR);
      RegisterProperty('Delimiter', 'Char', iptRW);
      RegisterProperty('QuoteChar', 'Char', iptRW);
      RegisterProperty('EOF', 'Boolean', iptR);
      RegisterProperty('FieldCount', 'Integer', iptR);
    end;
    {$ENDIF}
    {$ENDREGION}

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

{ TCSVReader }

procedure TCSVReader.Close;
begin
  if Active then
    FreeAndNil(FTextStream);

  SetLength(FFields, 0);
end;

constructor TCSVReader.Create;
begin
  inherited;

  FTextStream := nil;
  FDelimiter := ',';
  FQuoteChar := '"';
end;

destructor TCSVReader.Destroy;
begin
  Close;

  inherited;
end;

function TCSVReader.Field(AIndex: Integer): String;
begin
  Result := FFields[AIndex];
end;

function TCSVReader.GetActive: Boolean;
begin
  Result := Assigned(FTextStream);
end;

function TCSVReader.GetFieldCount: Integer;
begin
  Result := Length(FFields);
end;

procedure TCSVReader.Next;
var
  idx : Integer;
  line : TStringList;
begin
  if not Active then exit;

  FEOF := FTextStream.EoF;

  line := TStringList.Create;
  try
    line.QuoteChar := FQuoteChar;
    line.Delimiter := FDelimiter;
    line.StrictDelimiter := true;

    line.DelimitedText := FTextStream.ReadLine;

    SetLength(FFields, line.Count);

    for idx := 0 to line.Count - 1 do
    begin
      FFields[idx] := line[idx];
    end;
  finally
    line.Free;
  end;
end;

procedure TCSVReader.Open(AFilename: String);
begin
  if Active then
    Close;

  FTextStream := TTextStream.Create(AFilename, saRead);
  FEOF := false;

  Next;
end;

initialization
  FScriptList := TThreadList.Create;

finalization
  FScriptList.Free;

end.
