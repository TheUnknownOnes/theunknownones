unit uDelphiRemoteIDEClient;

interface

uses
  Classes, ComObj, Forms, DelphiRemoteServer_TLB, ComObjWrapper,
  uDelphiRemoteIDEClientPlugin, ToolsAPI;

function GlobalDelphiRemoteIDEClient: TDelphiRemoteIDEClientPlugin;

implementation

uses
  Dialogs, ObjComAuto, Windows, SysUtils;
     
type
   {$TYPEINFO ON}
  {$METHODINFO ON}
  TDelphiRemoteIDEClient = class(TDelphiRemoteIDEClientPlugin)
  private
    function GetDelphiVersion(const Index: Integer): String;
  protected
    function GetName : string; override;
  public

  published
    property Version: String index 0 read GetDelphiVersion;
    property MajorVersion: String index 1 read GetDelphiVersion;
  end;
   {$TYPEINFO OFF}
  {$METHODINFO OFF}
var
  DelphiRemoteIDEClient : TDelphiRemoteIDEClient = nil;  
  DelphiRemoteServerService : Service;
  DelphiRemoteIDEClientIntf : IDispatch;

function GetFileInfo(
  var aValue: String; aPart, FileName: String): Boolean;
var
  VersionInfoSize, VerInfoSize, GetInfoSizeJunk: DWORD;
  VersionInfo, Translation, InfoPointer: Pointer;
  VersionValue: String;
begin
  Result := False;
  VerInfoSize := GetFileVersionInfoSize(PChar(FileName), GetInfoSizeJunk);
  if VerInfoSize > 0 then
  begin
    GetMem(VersionInfo, VerInfoSize);
    try
      GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, VersionInfo);
      VerQueryValue(
        VersionInfo, '\\VarFileInfo\\Translation', Translation, VerInfoSize
      );
      VersionValue :=
        '\\StringFileInfo\\' + IntToHex(LoWord(LongInt(Translation^)), 4) +
        IntToHex(HiWord(LongInt(Translation^)), 4) + '\\';
      VersionInfoSize := 0;

      VerQueryValue(
        VersionInfo, PChar(VersionValue + aPart), InfoPointer,
        VersionInfoSize
      );
      aValue := AnsiString(PChar(InfoPointer));
    finally
      FreeMem(VersionInfo);
    end;
    aValue := Trim(aValue);
    Result := (aValue <> '');
  end;
end;

function GlobalDelphiRemoteIDEClient: TDelphiRemoteIDEClientPlugin;
begin
  if not Assigned(DelphiRemoteIDEClient) then
    DelphiRemoteIDEClient:=TDelphiRemoteIDEClient.Create;

  Result:=DelphiRemoteIDEClient;
end;

{ TDelphiRemoteIDEClient }

function TDelphiRemoteIDEClient.GetDelphiVersion(const Index: Integer): String;
var
  Ver : String;
begin
  if GetFileInfo(Ver, 'FileVersion', Application.ExeName) then
  begin
    if Index=1 then
      Result:=Copy(Ver, 1, Pos('.',Ver)-1)
    else
      Result:=Ver;
  end
  else
    Result:='0';
end;

function TDelphiRemoteIDEClient.GetName: string;
var
  Nam : String;
begin
  if GetFileInfo(Nam, 'InternalName', Application.ExeName) then
    Result:=Nam
  else
    Result:=inherited GetName;
end;

initialization
  DelphiRemoteServerService:=CreateOleObject('DelphiRemoteServer.Service') as Service;
  DelphiRemoteIDEClientIntf:=GlobalDelphiRemoteIDEClient.GetDispatchInterface; // TObjectDispatch.Create(TDelphiRemoteIDEClient.Create);
  DelphiRemoteServerService.RegisterIDE(DelphiRemoteIDEClientIntf);

finalization
  try
    DelphiRemoteServerService.UnregisterIDE(DelphiRemoteIDEClientIntf);
    DelphiRemoteServerService:=nil;
    DelphiRemoteIDEClientIntf:=nil;
    GlobalDelphiRemoteIDEClient.Free;
  except
  end;
end.
