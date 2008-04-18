//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvComponentBase, JvTrayIcon, StdCtrls, Menus, ComCtrls;

type
  Tform_Main = class(TForm)
    gb_Feeds: TGroupBox;
    MainMenu: TMainMenu;
    Feeds1: TMenuItem;
    mi_Addfeed: TMenuItem;
    mi_Modifyfeed: TMenuItem;
    mi_Deletefeed: TMenuItem;
    lv_Feeds: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mi_AddfeedClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  form_Main : Tform_Main;

implementation

uses uData, uEditFeed;

{$R *.dfm}

procedure Tform_Main.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caNone;
  Data.Tray.HideApplication;
end;

procedure Tform_Main.FormCreate(Sender: TObject);
begin
  //Data.Tray.HideApplication;
end;

procedure Tform_Main.mi_AddfeedClick(Sender: TObject);
var
  f : TFeed;
begin
  if Tform_EditFeed.NewFeed(f) then
  begin
    Data.Feeds.Add(f);
  end;
end;

end.
