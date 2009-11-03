unit uMain;

interface

uses
  ActiveX, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ExtCtrls;

type
  TFormMain = class(TForm)
    TrayIcon1: TTrayIcon;
    PopupMenu1: TPopupMenu;
    Quit1: TMenuItem;
    procedure Quit1Click(Sender: TObject);
  public
  end;


var
  FormMain: TFormMain;

implementation

uses
  ObjComAuto;

{$R *.dfm}


procedure TFormMain.Quit1Click(Sender: TObject);
begin
  Self.Close;
end;

end.
