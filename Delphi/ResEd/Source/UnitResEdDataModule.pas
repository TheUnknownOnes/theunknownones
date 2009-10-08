//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit UnitResEdDataModule;

interface

uses
  SysUtils, Classes, Menus, EditIntf, ToolsApi, Dialogs, ExtCtrls,
  UnitResEdMain, DockForm, Windows, Controls, ImgList;

type
  TResEdDataModule = class(TDataModule)
    PopupMenu1: TPopupMenu;
    miResEd: TMenuItem;
    imlTV: TImageList;
    imlSplash: TImageList;
    procedure DataModuleDestroy(Sender: TObject);
    procedure miResEdClick(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    class procedure FreeResEdForm; static;
    { Private-Deklarationen }
  public
    class procedure ShowResEdForm;
  end;

implementation

uses UnitResEdExpert;

{$R *.dfm}

procedure CreateResEdForm;
begin
  if not Assigned(MainForm) then
    CreateDockableForm(MainForm, TFormWizardResEd);
end;

class procedure TResEdDataModule.ShowResEdForm;
begin
  CreateResEdForm;
  ShowDockableForm(MainForm);
  TFormWizardResEd(MainForm).TV.SetFocus;
end;

class procedure TResEdDataModule.FreeResEdForm;
begin
  FreeDockableForm(MainForm);
end;

procedure TResEdDataModule.DataModuleCreate(Sender: TObject);
var
  ViewMenu : TMenuItem;
  Item     : TMenuItem;
  Offset   : Integer;
begin
  GlobalImageList:=(BorlandIDEServices as INTAServices).ImageList;
  Offset:=GlobalImageList.Count;
  GlobalImageList.AddImages(imlTV);
  imlTV.Clear;

  inc(IMGIDXResFile,Offset);
  inc(IMGIDXResGroup,Offset);
  inc(IMGIDXResource,Offset);

  ViewMenu := (BorlandIDEServices as INTAServices).MainMenu.Items[3];

  while PopupMenu1.Items.Count>0 do
  begin
    Item := PopupMenu1.Items[0];
    if Item.ImageIndex>=0 then
      Item.ImageIndex:=Item.ImageIndex+Offset;
    PopupMenu1.Items.Delete(0);
    ViewMenu.Insert(3,Item);
  end;

  CreateResEdForm;
end;

procedure TResEdDataModule.miResEdClick(Sender: TObject);
begin
  ShowResEdForm;
end;

procedure TResEdDataModule.DataModuleDestroy(Sender: TObject);
begin
  FreeResEdForm;
end;

end.
