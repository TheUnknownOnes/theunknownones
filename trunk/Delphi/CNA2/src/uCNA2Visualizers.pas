//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uCNA2Visualizers;

interface

uses
  ComCtrls,
  Classes,
  SysUtils,
  ToolsAPI,
  Menus,
  Controls,
  Graphics,
  ActnList,
  Dialogs,
  ExtCtrls;

type
  TCNA2ShowDialogAction = class(TAction)
  private
    procedure DoOnExecute(Sender : TObject);
  public
    constructor Create(); reintroduce;
    destructor Destroy(); override;
  end;

  Tcna2MainMenuEntry = class
  private
    FMenuItem : TMenuItem;
    function FindToolsMenu : TMenuItem;
  public
    constructor Create();
    destructor Destroy(); override;
  end;

procedure InitVisualizers;
procedure FreeVisualizers;

implementation

uses ImgList, uCNA2ConfigDialog;

var
  iiCNA2 : Integer;
  actShowDialog : TCNA2ShowDialogAction;
  vizMainMenuEntry : Tcna2MainMenuEntry;

procedure InitVisualizers;
var
  il : TImageList;
  ico : TIcon;
  NTA : INTAServices;
begin
  if Supports(BorlandIDEServices, INTAServices, NTA) then
  begin
    il := TImageList.Create(nil);
    ico := TIcon.Create;
    try
      ico.LoadFromResourceName(HInstance, 'CNA2');

      iiCNA2 := il.AddIcon(ico) + NTA.ImageList.Count;

      NTA.AddImages(il);
    finally
      il.Free;
      ico.Free;
    end;
  end;

  actShowDialog := TCNA2ShowDialogAction.Create;

  //Load all visualizers
  vizMainMenuEntry := Tcna2MainMenuEntry.Create;
end;

procedure FreeVisualizers;
begin
  if Assigned(vizMainMenuEntry) then
  begin
    vizMainMenuEntry.Free;
    vizMainMenuEntry := nil;
  end;

  if Assigned(actShowDialog) then
  begin
    actShowDialog.Free;
    actShowDialog := nil;
  end;
end;

{ Tcna2MainMenuEntry }

constructor Tcna2MainMenuEntry.Create;
var
  NTA : INTAServices;
  ToolsMenu : TMenuItem;
begin
  if Supports(BorlandIDEServices, INTAServices, NTA) then
  begin
    ToolsMenu := FindToolsMenu;

    if Assigned(ToolsMenu) then
    begin
      FMenuItem := TMenuItem.Create(NTA.MainMenu);
      FMenuItem.Action := actShowDialog;

      NTA.MenuBeginUpdate;
      try
        ToolsMenu.Insert(3, FMenuItem);
      finally
        NTA.MenuEndUpdate;
      end;
      
    end;
  end;
end;

destructor Tcna2MainMenuEntry.Destroy;
begin
  if Assigned(FMenuItem) then
  begin
    if Assigned(FMenuItem.Parent) then
      FMenuItem.Parent.Remove(FMenuItem);

    FMenuItem.Free;
    FMenuItem := nil;
  end;

  inherited;
end;

function Tcna2MainMenuEntry.FindToolsMenu: TMenuItem;
var
  NTA : INTAServices;
  idx : Integer;
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

{ TCNA2ShowDialogAction }

constructor TCNA2ShowDialogAction.Create;
var
  NTA : INTAServices;
begin
  inherited Create(nil);

  Caption := 'Component Naming Assistent 2 ...';
  Hint := 'Configure CNA2';
  Category := 'TUO';
  ImageIndex := iiCNA2;
  OnExecute := DoOnExecute;

  if Supports(BorlandIDEServices, INTAServices, NTA) then
  begin
    ActionList := NTA.ActionList;
  end;

end;


destructor TCNA2ShowDialogAction.Destroy;
begin
  //ToDo: Remove all references to this Action
  
  inherited;
end;


procedure TCNA2ShowDialogAction.DoOnExecute(Sender: TObject);
begin
  TCNA2ConfigDialog.Execute;
end;

initialization
  vizMainMenuEntry := nil;
  actShowDialog := nil;
  iiCNA2 := -1;

finalization
  FreeVisualizers;

end.
