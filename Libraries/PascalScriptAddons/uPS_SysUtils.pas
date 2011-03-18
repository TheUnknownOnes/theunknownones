unit uPS_SysUtils;

interface

uses
  uPSCompiler,
  uPSRuntime,
  SysUtils,
  Windows;

procedure PS_Register_SysUtils_C(ACompiler : TPSPascalCompiler);
procedure PS_Register_SysUtils_R(AExec : TPSExec; ARCi : TPSRuntimeClassImporter);

implementation

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

procedure PS_Register_SysUtils_C(ACompiler : TPSPascalCompiler);
begin
  ACompiler.AddConstantN('fmOpenRead', 'Integer').Value.ts32 := fmOpenRead;
  ACompiler.AddConstantN('fmOpenWrite', 'Integer').Value.ts32 := fmOpenWrite;
  ACompiler.AddConstantN('fmOpenReadWrite', 'Integer').Value.ts32 := fmOpenReadWrite;
  ACompiler.AddConstantN('fmShareCompat', 'Integer').Value.ts32 := fmShareCompat;
  ACompiler.AddConstantN('fmShareExclusive', 'Integer').Value.ts32 := fmShareExclusive;
  ACompiler.AddConstantN('fmShareDenyWrite', 'Integer').Value.ts32 := fmShareDenyWrite;
  ACompiler.AddConstantN('fmShareDenyRead', 'Integer').Value.ts32 := fmShareDenyRead;
  ACompiler.AddConstantN('fmShareDenyNone', 'Integer').Value.ts32 := fmShareDenyNone;

  ACompiler.AddDelphiFunction('function FileExists(const FileName: string): Boolean');
  ACompiler.AddDelphiFunction('function IncludeTrailingPathDelimiter(const S: string): string');
  ACompiler.AddDelphiFunction('function ExcludeTrailingPathDelimiter(const S: string): string;');
  ACompiler.AddDelphiFunction('function DirectoryExists(const Directory: string): Boolean');
  ACompiler.AddDelphiFunction('function ForceDirectories(Dir: string): Boolean');

  ACompiler.AddTypeS('TWin32FindData', 'record dwFileAttributes: DWORD; ftCreationTime: TFileTime; ftLastAccessTime: TFileTime; '+
                                       'ftLastWriteTime: TFileTime; nFileSizeHigh: DWORD; nFileSizeLow: DWORD; dwReserved0: DWORD; '+
                                       'dwReserved1: DWORD; cFileName: array[0..MAX_PATH - 1] of WideChar; cAlternateFileName: array[0..13] of WideChar; end;');
  ACompiler.AddTypeS('TSearchRec', 'record Time: Integer; Size: Int64; Attr: Integer; Name: TFileName; ExcludeAttr: Integer; FindHandle: Cardinal; FindData: TWin32FindData; end;');

  ACompiler.AddDelphiFunction('function FindFirst(const Path: string; Attr: Integer; var F: TSearchRec): Integer');
  ACompiler.AddDelphiFunction('function FindNext(var F: TSearchRec): Integer');
  ACompiler.AddDelphiFunction('procedure FindClose(var F: TSearchRec)');

  ACompiler.AddDelphiFunction('function DeleteFile(const FileName: string): Boolean');
  ACompiler.AddDelphiFunction('function RenameFile(const OldName, NewName: string): Boolean');
  ACompiler.AddDelphiFunction('function ChangeFileExt(const FileName, Extension: string): string');
  ACompiler.AddDelphiFunction('function ChangeFilePath(const FileName, Path: string): string');
  ACompiler.AddDelphiFunction('function ExtractFilePath(const FileName: string): string');
  ACompiler.AddDelphiFunction('function ExtractFileDir(const FileName: string): string');
  ACompiler.AddDelphiFunction('function ExtractFileDrive(const FileName: string): string');
  ACompiler.AddDelphiFunction('function ExtractFileName(const FileName: string): string');
  ACompiler.AddDelphiFunction('function ExtractFileExt(const FileName: string): string');
  ACompiler.AddDelphiFunction('function GetCurrentDir: string');
  ACompiler.AddDelphiFunction('function SetCurrentDir(const Dir: string): Boolean');
  ACompiler.AddDelphiFunction('function CreateDir(const Dir: string): Boolean');
  ACompiler.AddDelphiFunction('function RemoveDir(const Dir: string): Boolean');
  ACompiler.AddDelphiFunction('function GetFileTime(FileName: string) : TDateTime');
  ACompiler.AddDelphiFunction('function GetFileSize(FileName: string) : Int64');

  ACompiler.AddDelphiFunction('function Format(const Format: string; const Args: array of const) : String');
  ACompiler.AddDelphiFunction('function IntToHex(Value: Integer; Digits: Integer): string');
  ACompiler.AddDelphiFunction('function Int64ToHex(Value: Int64; Digits: Integer): string');

  ACompiler.AddDelphiFunction('function DateTimeToStr(const DateTime: TDateTime): string');
  ACompiler.AddDelphiFunction('function TimeToStr(const DateTime: TDateTime): string');
  ACompiler.AddDelphiFunction('function IncMonth(const DateTime: TDateTime; NumberOfMonths: Integer): TDateTime;');
end;

procedure PS_Register_SysUtils_R(AExec : TPSExec; ARCi : TPSRuntimeClassImporter);
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
  fcscaocs := Format; AExec.RegisterDelphiFunction(@fcscaocs, 'Format', cdRegister);
  fiis := IntToHex; AExec.RegisterDelphiFunction(@fiis, 'IntToHex', cdRegister);
  fi64is := IntToHex; AExec.RegisterDelphiFunction(@fi64is, 'Int64ToHex', cdRegister);

  fdts := DateTimeToStr; AExec.RegisterDelphiFunction(@fdts, 'DateTimeToStr', cdRegister);
  fdts := TimeToStr; AExec.RegisterDelphiFunction(@fdts, 'TimeToStr', cdRegister);

  AExec.RegisterDelphiFunction(@FileExists, 'FileExists', cdRegister);
  AExec.RegisterDelphiFunction(@IncludeTrailingPathDelimiter, 'IncludeTrailingPathDelimiter', cdRegister);
  AExec.RegisterDelphiFunction(@ExcludeTrailingPathDelimiter, 'ExcludeTrailingPathDelimiter', cdRegister);
  AExec.RegisterDelphiFunction(@DirectoryExists, 'DirectoryExists', cdRegister);
  AExec.RegisterDelphiFunction(@ForceDirectories, 'ForceDirectories', cdRegister);

  AExec.RegisterDelphiFunction(@FindFirst, 'FindFirst', cdRegister);
  AExec.RegisterDelphiFunction(@FindNext, 'FindNext', cdRegister);
  AExec.RegisterDelphiFunction(@FindClose, 'FindClose', cdRegister);

  AExec.RegisterDelphiFunction(@DeleteFile, 'DeleteFile', cdRegister);
  AExec.RegisterDelphiFunction(@RenameFile, 'RenameFile', cdRegister);
  AExec.RegisterDelphiFunction(@ChangeFileExt, 'ChangeFileExt', cdRegister);
  AExec.RegisterDelphiFunction(@ChangeFilePath, 'ChangeFilePath', cdRegister);
  AExec.RegisterDelphiFunction(@ExtractFilePath, 'ExtractFilePath', cdRegister);
  AExec.RegisterDelphiFunction(@ExtractFileDir, 'ExtractFileDir', cdRegister);
  AExec.RegisterDelphiFunction(@ExtractFileDrive, 'ExtractFileDrive', cdRegister);
  AExec.RegisterDelphiFunction(@ExtractFileName, 'ExtractFileName', cdRegister);
  AExec.RegisterDelphiFunction(@ExtractFileExt, 'ExtractFileExt', cdRegister);
  AExec.RegisterDelphiFunction(@GetCurrentDir, 'GetCurrentDir', cdRegister);
  AExec.RegisterDelphiFunction(@SetCurrentDir, 'SetCurrentDir', cdRegister);
  AExec.RegisterDelphiFunction(@CreateDir, 'CreateDir', cdRegister);
  AExec.RegisterDelphiFunction(@RemoveDir, 'RemoveDir', cdRegister);
  AExec.RegisterDelphiFunction(@_GetFileTime, 'GetFileTime', cdRegister);
  AExec.RegisterDelphiFunction(@_GetFileSize, 'GetFileSize', cdRegister);

  AExec.RegisterDelphiFunction(@IncMonth, 'IncMonth', cdRegister);
end;

end.
