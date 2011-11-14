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

function TEncoding_Default : TEncoding;
begin Result := TEncoding.Default end;

function TEncoding_Ascii : TEncoding;
begin Result := TEncoding.ASCII end;

function TEncoding_Unicode : TEncoding;
begin Result := TEncoding.Unicode end;

function TEncoding_BigEndianUnicode : TEncoding;
begin Result := TEncoding.BigEndianUnicode end;

function TEncoding_UTF7 : TEncoding;
begin Result := TEncoding.UTF7 end;

function TEncoding_UTF8 : TEncoding;
begin Result := TEncoding.UTF8 end;

function TEncoding_GetEncoding(ACodePage : Integer) : TEncoding;
begin Result := TEncoding.GetEncoding(ACodePage); end;


procedure PS_Register_SysUtils_C(ACompiler : TPSPascalCompiler);
var
  pscTEncoding : TPSCompileTimeClass;
begin
  ACompiler.AddTypeS('TBytes', 'array of Byte');

  ACompiler.AddConstantN('fmOpenRead', 'Integer').Value.ts32 := fmOpenRead;
  ACompiler.AddConstantN('fmOpenWrite', 'Integer').Value.ts32 := fmOpenWrite;
  ACompiler.AddConstantN('fmOpenReadWrite', 'Integer').Value.ts32 := fmOpenReadWrite;
  ACompiler.AddConstantN('fmShareCompat', 'Integer').Value.ts32 := fmShareCompat;
  ACompiler.AddConstantN('fmShareExclusive', 'Integer').Value.ts32 := fmShareExclusive;
  ACompiler.AddConstantN('fmShareDenyWrite', 'Integer').Value.ts32 := fmShareDenyWrite;
  ACompiler.AddConstantN('fmShareDenyRead', 'Integer').Value.ts32 := fmShareDenyRead;
  ACompiler.AddConstantN('fmShareDenyNone', 'Integer').Value.ts32 := fmShareDenyNone;

  ACompiler.AddConstantN('faReadOnly', 'Integer').Value.ts32 := faReadOnly;
  ACompiler.AddConstantN('faHidden', 'Integer').Value.ts32 := faHidden;
  ACompiler.AddConstantN('faSysFile', 'Integer').Value.ts32 := faSysFile;
  ACompiler.AddConstantN('faDirectory', 'Integer').Value.ts32 := faDirectory;
  ACompiler.AddConstantN('faArchive', 'Integer').Value.ts32 := faArchive;
  ACompiler.AddConstantN('faSymLink', 'Integer').Value.ts32 := faSymLink;
  ACompiler.AddConstantN('faNormal', 'Integer').Value.ts32 := faNormal;
  ACompiler.AddConstantN('faTemporary', 'Integer').Value.ts32 := faTemporary;
  ACompiler.AddConstantN('faAnyFile', 'Integer').Value.ts32 := faAnyFile;


  ACompiler.AddDelphiFunction('function FileExists(const FileName: string): Boolean');
  ACompiler.AddDelphiFunction('function IncludeTrailingPathDelimiter(const S: string): string');
  ACompiler.AddDelphiFunction('function ExcludeTrailingPathDelimiter(const S: string): string;');
  ACompiler.AddDelphiFunction('function DirectoryExists(const Directory: string): Boolean');
  ACompiler.AddDelphiFunction('function ForceDirectories(Dir: string): Boolean');

  ACompiler.AddTypeS('TWin32FindData', 'record dwFileAttributes: DWORD; ftCreationTime: TFileTime; ftLastAccessTime: TFileTime; '+
                                       'ftLastWriteTime: TFileTime; nFileSizeHigh: DWORD; nFileSizeLow: DWORD; dwReserved0: DWORD; '+
                                       'dwReserved1: DWORD; cFileName: array[0..MAX_PATH - 1] of WideChar; cAlternateFileName: array[0..13] of WideChar; end;');
  ACompiler.AddTypeS('TSearchRec', 'record Time: Integer; Size: Int64; Attr: Integer; Name: String; ExcludeAttr: Integer; FindHandle: Cardinal; FindData: TWin32FindData; end;');

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

  ACompiler.AddDelphiFunction('function Now: TDateTime;');
  ACompiler.AddDelphiFunction('function Date: TDateTime;');
  ACompiler.AddDelphiFunction('function Time: TDateTime;');

  pscTEncoding := ACompiler.AddClass(ACompiler.FindClass('TObject'), TEncoding);

  ACompiler.AddDelphiFunction('function TEncoding_Default : TEncoding;');
  ACompiler.AddDelphiFunction('function TEncoding_ASCII : TEncoding;');
  ACompiler.AddDelphiFunction('function TEncoding_Unicode : TEncoding;');
  ACompiler.AddDelphiFunction('function TEncoding_BigEndianUnicode : TEncoding;');
  ACompiler.AddDelphiFunction('function TEncoding_UTF7 : TEncoding;');
  ACompiler.AddDelphiFunction('function TEncoding_UTF8 : TEncoding;');
  ACompiler.AddDelphiFunction('function TEncoding_GetEncoding : TEncoding');
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

  AExec.RegisterDelphiFunction(@Now, 'Now', cdRegister);
  AExec.RegisterDelphiFunction(@Date, 'Date', cdRegister);
  AExec.RegisterDelphiFunction(@Time, 'Time', cdRegister);

  AExec.RegisterDelphiFunction(@TEncoding_Default, 'TEncoding_Default', cdRegister);
  AExec.RegisterDelphiFunction(@TEncoding_ASCII, 'TEncoding_ASCII', cdRegister);
  AExec.RegisterDelphiFunction(@TEncoding_Unicode, 'TEncoding_Unicode', cdRegister);
  AExec.RegisterDelphiFunction(@TEncoding_BigEndianUnicode, 'TEncoding_BigEndianUnicode', cdRegister);
  AExec.RegisterDelphiFunction(@TEncoding_UTF7, 'TEncoding_UTF7', cdRegister);
  AExec.RegisterDelphiFunction(@TEncoding_UTF8, 'TEncoding_UTF8', cdRegister);
  AExec.RegisterDelphiFunction(@TEncoding_GetEncoding, 'TEncoding_GetEncoding', cdRegister);
end;

end.
