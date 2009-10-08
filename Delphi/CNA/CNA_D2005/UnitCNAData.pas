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
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    BMP : TBitmap;
  end;

implementation

var GlobalImageList : TCustomImageList;

{$R *.dfm}

procedure TCNADataModule.DataModuleCreate(Sender: TObject);
var
  ToolsMenu : TMenuItem;
  Item      : TMenuItem;
  Offset    : Integer;
  i         : Integer;
begin
  bmp:=TBitmap.Create;
  imlSplashBMP.GetBitmap(0,bmp);
  (BorlandIDEServices as IOTAAboutBoxServices).AddPluginInfo(
             'CNA (Component Naming Assistant)',
             'An assistant for helping you to name and initialize the components you drop on the form designer.'+
             #13#10#13#10+
             'Contact: MarcoWarm@gmx.net; chaosben@web.de',
             bmp.Handle,
             false,
             'Freeware without any warranty',
             'by TheUnknownOnes');

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

procedure TCNADataModule.DataModuleDestroy(Sender: TObject);
begin
  BMP.Free;
end;

end.
