//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************

unit uMain;

interface

uses
  uTUOCommonIntf,
  Windows,
  Classes,
  SysUtils,
  Menus,
  Registry,
  StrUtils,
  ImageHlp,
  Dialogs,
  uHookTools;

type
  THistoryRelocator = class(TComponent)
  private
    FRTL_BPL_Handle: Cardinal;
    FConfigMenu : TMenuItem;
    FHistoryPath : String;
    FCOREIDE_BPL_Handle: Cardinal;
    procedure DoConfig(Sender : TObject);

    function GetSettingsReg : TRegistry;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure SetHistoryPath(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function RelocatePath(APath : String) : String;

    property RTL_BPL_Handle : Cardinal read FRTL_BPL_Handle;
    property COREIDE_BPL_Handle : Cardinal read FCOREIDE_BPL_Handle;

    property HistoryPath : String read FHistoryPath write SetHistoryPath;
  end;

var
  HistoryRelocator : THistoryRelocator;

const
{$IF Defined(VER230)}
  RTL_BPL_Filename = 'RTL160.bpl';
  COREIDE_BPL_Filename = 'coreide160.bpl';
{$ELSEIF Defined(VER220)}
  RTL_BPL_Filename = 'RTL150.bpl';     
  COREIDE_BPL_Filename = 'coreide150.bpl';
{$ELSEIF Defined(VER210)}
  RTL_BPL_Filename = 'RTL140.bpl';
  COREIDE_BPL_Filename = 'coreide140.bpl';
{$ELSEIF Defined(VER200)}
  RTL_BPL_Filename = 'RTL120.bpl';
  COREIDE_BPL_Filename = 'coreide120.bpl';
{$ELSEIF Defined(VER185)}
  RTL_BPL_Filename = 'RTL110.bpl';
  COREIDE_BPL_Filename = 'coreide110.bpl';
{$ELSEIF Defined(VER180)}
  RTL_BPL_Filename = 'RTL100.bpl';
  COREIDE_BPL_Filename = 'coreide100.bpl';     
{$IFEND}

implementation

uses uFormConfig;

var
  _OrigCreateFileW: function(lpFileName: PWideChar; dwDesiredAccess, dwShareMode: DWORD;
                             lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
                             hTemplateFile: THandle): THandle; stdcall;
_OrigCreateFileA : function(lpFileName: PAnsiChar; dwDesiredAccess, dwShareMode: DWORD;
                             lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
                             hTemplateFile: THandle): THandle; stdcall;

  _OrigMoveFileW: function(lpExistingFileName, lpNewFileName: PWideChar): BOOL; stdcall;
  _OrigMoveFileA : function(lpExistingFileName, lpNewFileName: PAnsiChar): BOOL; stdcall;

  _OrigFindFirstFileW_RTL : function(lpFileName: PWideChar; var lpFindFileData: TWIN32FindDataW): THandle; stdcall;
  _OrigFindFirstFileW_CoreIDE : function(lpFileName: PWideChar; var lpFindFileData: TWIN32FindDataW): THandle; stdcall;
  _OrigFindFirstFileA_RTL : function(lpFileName: PAnsiChar; var lpFindFileData: TWIN32FindDataW): THandle; stdcall;
  _OrigFindFirstFileA_CoreIDE : function(lpFileName: PAnsiChar; var lpFindFileData: TWIN32FindDataW): THandle; stdcall;

  _OrigGetFileSecurityW : function(lpFileName: PWideChar; RequestedInformation: SECURITY_INFORMATION;
                                   pSecurityDescriptor: PSecurityDescriptor; nLength: DWORD; var lpnLengthNeeded: DWORD): BOOL; stdcall;
  _OrigGetFileSecurityA : function(lpFileName: PAnsiChar; RequestedInformation: SECURITY_INFORMATION;
                                   pSecurityDescriptor: PSecurityDescriptor; nLength: DWORD; var lpnLengthNeeded: DWORD): BOOL; stdcall;

  _OrigGetFileAttributesW : function(lpFileName: PWideChar): DWORD; stdcall;
  _OrigGetFileAttributesA : function(lpFileName: PAnsiChar): DWORD; stdcall;

  _OrigCreateDirectoryW : function (lpPathName: PWideChar; lpSecurityAttributes: PSecurityAttributes): BOOL; stdcall;
  _OrigCreateDirectoryA : function (lpPathName: PAnsiChar; lpSecurityAttributes: PSecurityAttributes): BOOL; stdcall;


function _NewCreateFileW(lpFileName: PWideChar; dwDesiredAccess, dwShareMode: DWORD;
                        lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
                        hTemplateFile: THandle): THandle; stdcall;

begin
  Result := _OrigCreateFileW(StringToOleStr(HistoryRelocator.RelocatePath(lpFileName)), dwDesiredAccess, dwShareMode,
                             lpSecurityAttributes, dwCreationDisposition,
                             dwFlagsAndAttributes, hTemplateFile);
end;

function _NewCreateFileA(lpFileName: PAnsiChar; dwDesiredAccess, dwShareMode: DWORD;
                        lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
                        hTemplateFile: THandle): THandle; stdcall;

begin
  Result := _OrigCreateFileA(PAnsiChar(AnsiString(HistoryRelocator.RelocatePath(lpFileName))), dwDesiredAccess, dwShareMode,
                             lpSecurityAttributes, dwCreationDisposition,
                             dwFlagsAndAttributes, hTemplateFile);
end;

function _NewMoveFileW(lpExistingFileName, lpNewFileName: PWideChar): BOOL; stdcall;
begin
  Result := _OrigMoveFileW(lpExistingFileName, StringToOleStr(HistoryRelocator.RelocatePath(lpNewFileName)));
end;

function _NewMoveFileA(lpExistingFileName, lpNewFileName: PAnsiChar): BOOL; stdcall;
begin
  Result := _OrigMoveFileA(lpExistingFileName, PAnsiChar(AnsiString(HistoryRelocator.RelocatePath(lpNewFileName))));
end;

function _NewFindFirstFileW_RTL(lpFileName: PWideChar; var lpFindFileData: TWIN32FindDataW): THandle; stdcall;
begin
  Result := _OrigFindFirstFileW_RTL(StringToOleStr(HistoryRelocator.RelocatePath(lpFileName)), lpFindFileData);
end;

function _NewFindFirstFileW_CoreIDE(lpFileName: PWideChar; var lpFindFileData: TWIN32FindDataW): THandle; stdcall;
begin
  Result := _OrigFindFirstFileW_CoreIDE(StringToOleStr(HistoryRelocator.RelocatePath(lpFileName)), lpFindFileData);
end;

function _NewFindFirstFileA_RTL(lpFileName: PAnsiChar; var lpFindFileData: TWIN32FindDataW): THandle; stdcall;
begin
  Result := _OrigFindFirstFileA_RTL(PAnsiChar(AnsiString(HistoryRelocator.RelocatePath(lpFileName))), lpFindFileData);
end;

function _NewFindFirstFileA_CoreIDE(lpFileName: PAnsiChar; var lpFindFileData: TWIN32FindDataW): THandle; stdcall;
begin
  Result := _OrigFindFirstFileA_CoreIDE(PAnsiChar(AnsiString(HistoryRelocator.RelocatePath(lpFileName))), lpFindFileData);
end;

function _NewGetFileSecurityW(lpFileName: PWideChar; RequestedInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSecurityDescriptor; nLength: DWORD; var lpnLengthNeeded: DWORD): BOOL; stdcall;
begin
  Result := _OrigGetFileSecurityW(StringToOleStr(HistoryRelocator.RelocatePath(lpFileName)), RequestedInformation, pSecurityDescriptor, nLength, lpnLengthNeeded);
end;

function _NewGetFileSecurityA(lpFileName: PAnsiChar; RequestedInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSecurityDescriptor; nLength: DWORD; var lpnLengthNeeded: DWORD): BOOL; stdcall;
begin
  Result := _OrigGetFileSecurityA(PAnsiChar(AnsiString(HistoryRelocator.RelocatePath(lpFileName))), RequestedInformation, pSecurityDescriptor, nLength, lpnLengthNeeded);
end;

function _NewGetFileAttributesW(lpFileName: PWideChar): DWORD; stdcall;
begin
  Result := _OrigGetFileAttributesW(StringToOleStr(HistoryRelocator.RelocatePath(lpFileName)));
end;

function _NewGetFileAttributesA(lpFileName: PAnsiChar): DWORD; stdcall;
begin
  Result := _OrigGetFileAttributesA(PAnsiChar(AnsiString(HistoryRelocator.RelocatePath(lpFileName))));
end;

function _NewCreateDirectoryW(lpPathName: PWideChar; lpSecurityAttributes: PSecurityAttributes): BOOL; stdcall;
begin
  Result := _OrigCreateDirectoryW(PWideChar(WideString(HistoryRelocator.RelocatePath(lpPathName))), lpSecurityAttributes);
end;

function _NewCreateDirectoryA(lpPathName: PAnsiChar; lpSecurityAttributes: PSecurityAttributes): BOOL; stdcall;
begin
  Result := _OrigCreateDirectoryA(PAnsiChar(AnsiString(HistoryRelocator.RelocatePath(lpPathName))), lpSecurityAttributes);
end;

{ THistoryRelocator }

constructor THistoryRelocator.Create(AOwner: TComponent);
begin
  inherited;

  LoadSettings;

  FRTL_BPL_Handle := GetModuleHandle(RTL_BPL_Filename);
  FCOREIDE_BPL_Handle := GetModuleHandle(COREIDE_BPL_Filename);


  HookImport(FRTL_BPL_Handle, 'kernel32.dll', 'CreateFileW', @_NewCreateFileW, @_OrigCreateFileW);
  HookImport(FRTL_BPL_Handle, 'kernel32.dll', 'CreateFileA', @_NewCreateFileA, @_OrigCreateFileA);

  HookImport(FRTL_BPL_Handle, 'kernel32.dll', 'MoveFileW', @_NewMoveFileW, @_OrigMoveFileW);
  HookImport(FRTL_BPL_Handle, 'kernel32.dll', 'MoveFileA', @_NewMoveFileA, @_OrigMoveFileA);

  HookImport(FRTL_BPL_Handle, 'kernel32.dll', 'FindFirstFileW', @_NewFindFirstFileW_RTL, @_OrigFindFirstFileW_RTL);
  HookImport(FCOREIDE_BPL_Handle, 'kernel32.dll', 'FindFirstFileW', @_NewFindFirstFileW_COREIDE, @_OrigFindFirstFileW_CoreIDE);
  HookImport(FRTL_BPL_Handle, 'kernel32.dll', 'FindFirstFileA', @_NewFindFirstFileA_RTL, @_OrigFindFirstFileA_RTL);
  HookImport(FCOREIDE_BPL_Handle, 'kernel32.dll', 'FindFirstFileA', @_NewFindFirstFileA_COREIDE, @_OrigFindFirstFileA_CoreIDE);

  HookImport(FCOREIDE_BPL_Handle, 'advapi32.dll', 'GetFileSecurityW', @_NewGetFileSecurityW, @_OrigGetFileSecurityW);
  HookImport(FCOREIDE_BPL_Handle, 'advapi32.dll', 'GetFileSecurityA', @_NewGetFileSecurityA, @_OrigGetFileSecurityA);

  HookImport(FRTL_BPL_Handle, 'kernel32.dll', 'GetFileAttributesW', @_NewGetFileAttributesW, @_OrigGetFileAttributesW);
  HookImport(FRTL_BPL_Handle, 'kernel32.dll', 'GetFileAttributesA', @_NewGetFileAttributesA, @_OrigGetFileAttributesA);

  HookImport(FRTL_BPL_Handle, 'kernel32.dll', 'CreateDirectoryW', @_NewCreateDirectoryW, @_OrigCreateDirectoryW);
  HookImport(FRTL_BPL_Handle, 'kernel32.dll', 'CreateDirectoryA', @_NewCreateDirectoryA, @_OrigCreateDirectoryA);


  FConfigMenu := TMenuItem.Create(self);
  FConfigMenu.Caption := 'HistoryRelocator';
  FConfigMenu.OnClick := DoConfig;

  GetTUOCommon.ToolsMenuItem.Add(FConfigMenu);
end;

destructor THistoryRelocator.Destroy;
begin
  SaveSettings;

  UnHookImport(FRTL_BPL_Handle, 'kernel32.dll', 'CreateFileW', @_NewCreateFileW, @_OrigCreateFileW);
  UnHookImport(FRTL_BPL_Handle, 'kernel32.dll', 'CreateFileA', @_NewCreateFileA, @_OrigCreateFileA);

  UnHookImport(FRTL_BPL_Handle, 'kernel32.dll', 'MoveFileW', @_NewMoveFileW, @_OrigMoveFileW);
  UnHookImport(FRTL_BPL_Handle, 'kernel32.dll', 'MoveFileA', @_NewMoveFileA, @_OrigMoveFileA);

  UnHookImport(FRTL_BPL_Handle, 'kernel32.dll', 'FindFirstFileW', @_NewFindFirstFileW_RTL, @_OrigFindFirstFileW_RTL);
  UnHookImport(FCOREIDE_BPL_Handle, 'kernel32.dll', 'FindFirstFileW', @_NewFindFirstFileW_COREIDE, @_OrigFindFirstFileW_CoreIDE);
  UnHookImport(FRTL_BPL_Handle, 'kernel32.dll', 'FindFirstFileA', @_NewFindFirstFileA_RTL, @_OrigFindFirstFileA_RTL);
  UnHookImport(FCOREIDE_BPL_Handle, 'kernel32.dll', 'FindFirstFileA', @_NewFindFirstFileA_CoreIDE, @_OrigFindFirstFileA_CoreIDE);

  UnHookImport(FCOREIDE_BPL_Handle, 'advapi32.dll', 'GetFileSecurityW', @_NewGetFileSecurityW, @_OrigGetFileSecurityW);
  UnHookImport(FCOREIDE_BPL_Handle, 'advapi32.dll', 'GetFileSecurityA', @_NewGetFileSecurityA, @_OrigGetFileSecurityA);

  UnHookImport(FRTL_BPL_Handle, 'kernel32.dll', 'GetFileAttributesW', @_NewGetFileAttributesW, @_OrigGetFileAttributesW);
  UnHookImport(FRTL_BPL_Handle, 'kernel32.dll', 'GetFileAttributesA', @_NewGetFileAttributesA, @_OrigGetFileAttributesA);

  UnHookImport(FRTL_BPL_Handle, 'kernel32.dll', 'CreateDirectoryW', @_NewCreateDirectoryW, @_OrigCreateDirectoryW);
  UnHookImport(FRTL_BPL_Handle, 'kernel32.dll', 'CreateDirectoryA', @_NewCreateDirectoryA, @_OrigCreateDirectoryA);

  inherited;
end;

procedure THistoryRelocator.DoConfig(Sender: TObject);
var
  s : String;

begin
  s := HistoryPath;
  TformConfig.Execute(s);
  HistoryPath := s;
end;

function THistoryRelocator.GetSettingsReg: TRegistry;
begin
  Result := TRegistry.Create;
  Result.RootKey := HKEY_CURRENT_USER;
  Result.OpenKey(GetTUOCommon.RegistryRootKey + '\HistoryRelocator', true);

end;

procedure THistoryRelocator.LoadSettings;
begin
  with GetSettingsReg do
  begin
    HistoryPath := ReadString('Path');
    Free;
  end;
end;


function THistoryRelocator.RelocatePath(APath: String) : String;
const
  HistFolder = '__history';
  HistFileExt = '.~*~';

begin
  Result := APath;
  if HistoryPath = '' then exit;
  if not ContainsText(APath, HistFolder) then exit;

  if APath[2] = ':' then
    APath := Copy(APath, 4, Length(APath));

  Result := HistoryPath + APath;

  MakeSureDirectoryPathExists(PAnsiChar(AnsiString(ExtractFilePath(Result))));
end;


procedure THistoryRelocator.SaveSettings;
begin
  with GetSettingsReg do
  begin
    WriteString('Path', HistoryPath);
    Free;
  end;
end;

procedure THistoryRelocator.SetHistoryPath(const Value: String);
begin
  FHistoryPath := Trim(Value);
  if FHistoryPath <> '' then
    FHistoryPath := IncludeTrailingPathDelimiter(FHistoryPath);
end;

initialization
  HistoryRelocator := THistoryRelocator.Create(nil);

finalization
  HistoryRelocator.Free;

end.
