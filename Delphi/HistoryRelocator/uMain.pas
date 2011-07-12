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

    procedure RelocPath(ASource : String; var APath : String);

    property RTL_BPL_Handle : Cardinal read FRTL_BPL_Handle;
    property COREIDE_BPL_Handle : Cardinal read FCOREIDE_BPL_Handle;

    property HistoryPath : String read FHistoryPath write SetHistoryPath;
  end;

var
  HistoryRelocator : THistoryRelocator;

const
{$IFDEF VER200}
  RTL_BPL_Filename = 'RTL120.bpl';
  COREIDE_BPL_Filename = 'coreide120.bpl';
{$ENDIF}
{$IFDEF VER210}
  RTL_BPL_Filename = 'RTL140.bpl';
  COREIDE_BPL_Filename = 'coreide140.bpl';
{$ENDIF}
{$IFDEF VER220}
  RTL_BPL_Filename = 'RTL150.bpl';
  COREIDE_BPL_Filename = 'coreide150.bpl';
{$ENDIF}
{$IFDEF VER230}
  RTL_BPL_Filename = 'RTL160.bpl';
  COREIDE_BPL_Filename = 'coreide160.bpl';
{$ENDIF}

implementation

uses uFormConfig;

var
  _OrigCreateFileW: function(lpFileName: PWideChar; dwDesiredAccess, dwShareMode: DWORD;
                             lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
                             hTemplateFile: THandle): THandle; stdcall;

  _OrigMoveFileW: function(lpExistingFileName, lpNewFileName: PWideChar): BOOL; stdcall;

  _OrigFindFirstFileW : function(lpFileName: PWideChar; var lpFindFileData: TWIN32FindDataW): THandle; stdcall;

  _OrigGetFileSecurityW : function(lpFileName: PWideChar; RequestedInformation: SECURITY_INFORMATION;
                                   pSecurityDescriptor: PSecurityDescriptor; nLength: DWORD; var lpnLengthNeeded: DWORD): BOOL; stdcall;

  _OrigGetFileAttributesW : function(lpFileName: PWideChar): DWORD; stdcall;


function _NewCreateFileW(lpFileName: PWideChar; dwDesiredAccess, dwShareMode: DWORD;
                        lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
                        hTemplateFile: THandle): THandle; stdcall;
var
  s : String;
begin
  s := lpFileName;

  HistoryRelocator.RelocPath('CreateFileW', s);

  Result := _OrigCreateFileW(PWideChar(WideString(s)), dwDesiredAccess, dwShareMode,
                             lpSecurityAttributes, dwCreationDisposition,
                             dwFlagsAndAttributes, hTemplateFile);
end;

function _NewMoveFileW(lpExistingFileName, lpNewFileName: PWideChar): BOOL; stdcall;
var
  s : String;
begin
  s := lpNewFileName;

  HistoryRelocator.RelocPath('MoveFileW', s);

  Result := _OrigMoveFileW(lpExistingFileName, PWideChar(WideString(s)));
end;

function _NewFindFirstFileW(lpFileName: PWideChar; var lpFindFileData: TWIN32FindDataW): THandle; stdcall;
var
  s : String;
begin
  s := lpFileName;

  HistoryRelocator.RelocPath('FindFirstFileW', s);

  Result := _OrigFindFirstFileW(PWideChar(WideString(s)), lpFindFileData);
end;

function _NewGetFileSecurityW(lpFileName: PWideChar; RequestedInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSecurityDescriptor; nLength: DWORD; var lpnLengthNeeded: DWORD): BOOL; stdcall;
var
  s : String;
begin
  s := lpFileName;

  HistoryRelocator.RelocPath('GetFileSecurityW', s);

  Result := _OrigGetFileSecurityW(PWideChar(WideString(s)), RequestedInformation, pSecurityDescriptor, nLength, lpnLengthNeeded);
end;

function _NewGetFileAttributesW(lpFileName: PWideChar): DWORD; stdcall;
var
  s : String;
begin
  s := lpFileName;

  HistoryRelocator.RelocPath('GetFileAttributesW', s);

  Result := _OrigGetFileAttributesW(PWideChar(WideString(s)));
end;




{ THistoryRelocator }

constructor THistoryRelocator.Create(AOwner: TComponent);
begin
  inherited;

  //(BorlandIDEServices as IOTAMessageServices).ClearAllMessages;


  LoadSettings;

  FRTL_BPL_Handle := GetModuleHandle(RTL_BPL_Filename);
  FCOREIDE_BPL_Handle := GetModuleHandle(COREIDE_BPL_Filename);

  HookImport(FRTL_BPL_Handle, 'kernel32.dll', 'CreateFileW', @_NewCreateFileW, @_OrigCreateFileW);
  HookImport(FRTL_BPL_Handle, 'kernel32.dll', 'MoveFileW', @_NewMoveFileW, @_OrigMoveFileW);
  HookImport(FRTL_BPL_Handle, 'kernel32.dll', 'FindFirstFileW', @_NewFindFirstFileW, @_OrigFindFirstFileW);
  //HookImport(FCOREIDE_BPL_Handle, 'advapi32.dll', 'GetFileSecurityW', @_NewGetFileSecurityW, @_OrigGetFileSecurityW);
  HookImport(FRTL_BPL_Handle, 'kernel32.dll', 'GetFileAttributesW', @_NewGetFileAttributesW, @_OrigGetFileAttributesW);


  FConfigMenu := TMenuItem.Create(self);
  FConfigMenu.Caption := 'HistoryRelocator';
  FConfigMenu.OnClick := DoConfig;

  GetTUOCommon.ToolsMenuItem.Add(FConfigMenu);

end;

destructor THistoryRelocator.Destroy;
begin
  SaveSettings;

  UnHookImport(FRTL_BPL_Handle, 'kernel32.dll', 'CreateFileW', @_NewCreateFileW, @_OrigCreateFileW);
  UnHookImport(FRTL_BPL_Handle, 'kernel32.dll', 'MoveFileW', @_NewMoveFileW, @_OrigMoveFileW);
  UnHookImport(FRTL_BPL_Handle, 'kernel32.dll', 'FindFirstFileW', @_NewFindFirstFileW, @_OrigFindFirstFileW);
 // UnHookImport(FCOREIDE_BPL_Handle, 'advapi32.dll', 'GetFileSecurityW', @_NewGetFileSecurityW, @_OrigGetFileSecurityW);
  UnHookImport(FRTL_BPL_Handle, 'kernel32.dll', 'GetFileAttributesW', @_NewGetFileAttributesW, @_OrigGetFileAttributesW);

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

procedure THistoryRelocator.RelocPath(ASource : String; var APath: String);
const
  HistFolder = '__history';
  HistFileExt = '.~*~';
begin
  if HistoryPath = '' then exit;
  if not ContainsText(APath, HistFolder) then exit;

  if not DirectoryExists(HistoryPath) then
    CreateDir(HistoryPath);

  APath := HistoryPath + ExtractFileName(APath);

  //(BorlandIDEServices as IOTAMessageServices).AddWideTitleMessage('[' + ASource + '] ' + APath);
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
