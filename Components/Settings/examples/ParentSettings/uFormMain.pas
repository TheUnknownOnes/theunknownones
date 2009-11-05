unit uFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uSettingsXML, uSettingsBase, uSettingsStream;

type
  TForm1 = class(TForm)
    ChildSettings: TSettingsFile;
    ParentSettings: TSettingsXMLFile;
    btn_SetValueParent1: TButton;
    btn_GetValueParent1: TButton;
    btn_DeleteValueParent1: TButton;
    btn_SetValueChild1: TButton;
    btn_GetValueChild1: TButton;
    btn_DeleteValueChild1: TButton;
    rb_ModeAddsMissing: TRadioButton;
    rb_OverridesAll: TRadioButton;
    rb_DontUse: TRadioButton;
    lbl_Value: TLabel;
    ed_Value: TEdit;
    procedure rb_ModeAddsMissingClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btn_SetValueParent1Click(Sender: TObject);
    procedure btn_SetValueChild1Click(Sender: TObject);
    procedure btn_GetValueParent1Click(Sender: TObject);
    procedure btn_GetValueChild1Click(Sender: TObject);
    procedure btn_DeleteValueParent1Click(Sender: TObject);
    procedure btn_DeleteValueChild1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

const
  SettingsPath = '/TestSetting';

implementation

{$R *.dfm}

procedure TForm1.btn_DeleteValueChild1Click(Sender: TObject);
begin
  ChildSettings.Delete(SettingsPath);
end;

procedure TForm1.btn_DeleteValueParent1Click(Sender: TObject);
begin
  ParentSettings.Delete(SettingsPath);
end;

procedure TForm1.btn_GetValueChild1Click(Sender: TObject);
begin
  MessageDlg(ChildSettings.GetValue(SettingsPath, '--Empty--'), mtWarning, [mbOK], 0);
end;

procedure TForm1.btn_GetValueParent1Click(Sender: TObject);
begin
  MessageDlg(ParentSettings.GetValue(SettingsPath, '--Empty--'), mtWarning, [mbOK], 0);
end;

procedure TForm1.btn_SetValueChild1Click(Sender: TObject);
begin
  ChildSettings.SetValue(SettingsPath, ed_Value.Text);
end;

procedure TForm1.btn_SetValueParent1Click(Sender: TObject);
begin
  ParentSettings.SetValue(SettingsPath, ed_Value.Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ed_Value.Text := TimeToStr(Now);
end;

procedure TForm1.rb_ModeAddsMissingClick(Sender: TObject);
begin
  if rb_ModeAddsMissing.Checked then
    ChildSettings.ParentMode := pmAddsMissing
  else
  if rb_OverridesAll.Checked then
    ChildSettings.ParentMode := pmOverridesAll
  else
  if rb_DontUse.Checked then
    ChildSettings.ParentMode := pmDontUse;
end;

end.
