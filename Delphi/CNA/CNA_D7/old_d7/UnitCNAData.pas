//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit UnitCNAData;

interface

uses
  SysUtils, Classes, Menus, EditIntf, ToolsApi, Dialogs, ExtCtrls,
  DockForm, Windows, Controls, ImgList, Graphics;

type
  TCNADataModule = class(TDataModule)
    PopupMenu1: TPopupMenu;
    miCNAConfig: TMenuItem;
    imlTV: TImageList;
    imlButtons: TImageList;
    imlSplashBMP: TImageList;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    Item : TMenuItem;
  end;

implementation

uses unitCNALangs;

var GlobalImageList : TCustomImageList;

{$R *.dfm}

procedure TCNADataModule.DataModuleCreate(Sender: TObject);
var
  ToolsMenu : TMenuItem;
  Offset    : Integer;
  i         : Integer;
begin
  GlobalImageList:=(BorlandIDEServices as INTAServices).ImageList;
  Offset:=GlobalImageList.Count;
  GlobalImageList.AddImages(imlTV);
  imlTV.Clear;
  ToolsMenu:=nil;

  for i:=0 to (BorlandIDEServices as INTAServices).MainMenu.Items.Count-1 do
  begin
    if (BorlandIDEServices as INTAServices).MainMenu.Items[i].Name='ToolsMenu' then
      ToolsMenu:=(BorlandIDEServices as INTAServices).MainMenu.Items[i];
  end;

  while PopupMenu1.Items.Count>0 do
  begin
    Item := PopupMenu1.Items[0];
    if Item.ImageIndex>=0 then
      Item.ImageIndex:=Item.ImageIndex+Offset;
    PopupMenu1.Items.Delete(0);
    if (Assigned(ToolsMenu)) then
      ToolsMenu.Insert(3, Item);
  end;
end;

end.
