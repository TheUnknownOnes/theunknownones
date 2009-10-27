unit uTUOCommonMain;

interface

uses
  uTUOCommonIntf, Classes, Menus, ToolsAPI, Dialogs, SysUtils, Graphics;

type
  TTUOCommon = class(TInterfacedPersistent, ITUOCommon)
  private
    FToolsMenuItem : TMenuItem;

    function GetToolsMenu: TMenuItem;
  protected
    {$REGION 'ITUOCommon'}
    function GetRegistryRootKey: String;
    function GetToolsMenuItem: TMenuItem;
    {$ENDREGION}
  public
    constructor Create;
    destructor Destroy; override;

    property RegistryRootKey: String read GetRegistryRootKey;
    property ToolsMenuItem: TMenuItem read GetToolsMenuItem;
  end;

var
  FTUOCommon : TTUOCommon;

implementation

{ TTUOCommon }

constructor TTUOCommon.Create;
begin
  inherited;
  GetToolsMenuItem;
end;

destructor TTUOCommon.Destroy;
begin
  if Assigned(FToolsMenuItem) then
    FreeAndNil(FToolsMenuItem) ;
  inherited;
end;

function TTUOCommon.GetRegistryRootKey: String;
var
  IServices: IOTAServices;
begin
  Assert(BorlandIDEServices <> nil);
  IServices := (BorlandIDEServices as IOTAServices);
  Assert(IServices <> nil);
  Result    := IServices.GetBaseRegistryKey;

  Assert(Length(Result) > 0);
  Assert(Result[Length(Result)] <> '\');

  Result:=Result + '\TheUnknownOnes';
end;

function TTUOCommon.GetToolsMenu: TMenuItem;
var
  NTA: INTAServices;
  idx: Integer;
begin
  Result := nil;

  if Supports(BorlandIDEServices, INTAServices, NTA) then
  begin
    for idx := 0 to NTA.MainMenu.Items.Count - 1 do
    begin
      if NTA.MainMenu.Items[idx].Name = 'ToolsMenu' then
      begin
        Result := NTA.MainMenu.Items[idx];
        break;
      end;
    end;
  end;
end;

function TTUOCommon.GetToolsMenuItem: TMenuItem;
var
  ToolsMenu : TMenuItem;
  idx       : Integer;
  bmp       : TBitmap;
begin
  if not Assigned(FToolsMenuItem) then
  begin
    ToolsMenu:=GetToolsMenu;
    FToolsMenuItem:=TMenuItem.Create(nil);
    FToolsMenuItem.Caption:='TUO Tools';
    ToolsMenu.Insert(0,FToolsMenuItem);

    bmp:=TBitmap.Create;
    bmp.LoadFromResourceName(hInstance, 'TOOLS');
    FToolsMenuItem.ImageIndex:=FToolsMenuItem.GetParentMenu.Images.AddMasked(bmp, clFuchsia);
    bmp.Free;
  end;
  Result:=FToolsMenuItem;
end;

initialization
  FTUOCommon:=TTUOCommon.Create;

finalization
  FTUOCommon.Free;

end.
