unit uFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uSettings, StdCtrls;

type
  TForm1 = class(TForm)
    btn_SetValue: TButton;
    btn_ValuesExists: TButton;
    btn_GetValue: TButton;
    btn_DeleteValue: TButton;
    Settings1: TSettings;
    Settings2: TSettings;
    SettingsLSFile1: TSettingsLSFile;
    btn_Save: TButton;
    btn_Load: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btn_SetValueClick(Sender: TObject);
    procedure btn_ValuesExistsClick(Sender: TObject);
    procedure btn_GetValueClick(Sender: TObject);
    procedure btn_DeleteValueClick(Sender: TObject);
    procedure btn_SaveClick(Sender: TObject);
    procedure btn_LoadClick(Sender: TObject);
  private

  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn_DeleteValueClick(Sender: TObject);
begin
  Settings1.Delete('/Bli/bl[a|o]{1}/blubber', true);
end;

procedure TForm1.btn_GetValueClick(Sender: TObject);
var
  I : TSettingNameValue;
begin
  if Settings1.GetItem('/Bli/bl[a|o]{1}/blubber', I, true, true, true) then
    MessageDlg(i.Name + ' = ' + VarToStr(i.Value), mtWarning, [mbOK], 0);
end;

procedure TForm1.btn_LoadClick(Sender: TObject);
begin
  if not Settings2.Load then
    MessageDlg('shitty shit', mtWarning, [mbOK], 0);
end;

procedure TForm1.btn_SaveClick(Sender: TObject);
begin
  if not Settings2.Save then
    MessageDlg('shit', mtWarning, [mbOK], 0);
end;

procedure TForm1.btn_SetValueClick(Sender: TObject);
begin
  Settings2.SetValue(Now, '/Bli/bla/blubber');
end;

procedure TForm1.btn_ValuesExistsClick(Sender: TObject);
begin
  if Settings1.ItemsExists('/Bli/bl[a|o]{1}/blubber', true) then
    MessageDlg('jo', mtWarning, [mbOK], 0)
  else
    MessageDlg('neeeeee', mtWarning, [mbOK], 0);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := true;

end;

end.
