unit uFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uSettings, uSettingsStream, uSettingsXML,
  uSettingsComponentLinksDefault;

type
  TForm1 = class(TForm)
    btn_SetValue: TButton;
    btn_ValuesExists: TButton;
    btn_GetValue: TButton;
    btn_DeleteValue: TButton;
    btn_Save: TButton;
    btn_Load: TButton;
    Settings2: TSettingsXMLFile;
    Settings1: TSettingsXMLFile;
    SettingsComponentLinkControl1: TSettingsComponentLinkControl;
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
  v : Variant;
  nv : TSettingNameValues;
  idx : Integer;
begin
  v := Settings1.GetValue('/Bli/bla/blubber', false);
  if not VarIsEmpty(v) then
    MessageDlg(VarToStr(v), mtWarning, [mbOK], 0);

  nv := Settings1.GetSubNameValues(EmptyWideStr, false);
  for idx := Low(nv) to High(nv) do
    MessageDlg(nv[idx].Name + ' = ' + VarToStr(nv[idx].Value), mtWarning, [mbOK], 0);
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
  Settings2.SetValue('/Bli/bla/blubber', Now);
end;

procedure TForm1.btn_ValuesExistsClick(Sender: TObject);
begin
  if Settings1.Exists('/Bli/bl[a|o]{1}/blubber', true) then
    MessageDlg('jo', mtWarning, [mbOK], 0)
  else
    MessageDlg('neeeeee', mtWarning, [mbOK], 0);
end;

procedure TForm1.FormCreate(Sender: TObject);
//var
  //s1, s2, s3 : TSetting;
begin
  ReportMemoryLeaksOnShutdown := true;


  {s1 := TSetting.Create;
  s2 := TSetting.Create(nil);
  s3 := TSetting.Create(s2, 'bla');

  s1.Assign(s2);


  MessageDlg(s2.Index[s2.Index.IndexOfObject(s3)], mtWarning, [mbOK], 0);

  s1.Free;
  s2.Free;}
end;

end.
