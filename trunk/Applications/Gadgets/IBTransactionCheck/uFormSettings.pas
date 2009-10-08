unit uFormSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uSettingsBase, uSettingsLinksDefault, StdCtrls, ExtCtrls, Mask,
  JvExMask, JvToolEdit, Spin;

type
  Tform_Settings = class(TForm)
    SettingsLinkForm: TSettingsLinkForm;
    Panel1: TPanel;
    btn_OK: TButton;
    btn_Cancel: TButton;
    Label1: TLabel;
    ed_DB: TEdit;
    Label2: TLabel;
    ed_Username: TEdit;
    Label3: TLabel;
    ed_Password: TEdit;
    Label4: TLabel;
    ed_Charset: TEdit;
    Label5: TLabel;
    ed_ClientDLL: TJvFilenameEdit;
    Label6: TLabel;
    ed_Role: TEdit;
    Label7: TLabel;
    ed_MinLen: TSpinEdit;
    Label8: TLabel;
    Label9: TLabel;
    ed_Refresh: TSpinEdit;
    Label10: TLabel;
    procedure FormShow(Sender: TObject);
    procedure btn_OKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private-Deklarationen }
  public
    class function Execute() : Boolean;
  end;

implementation

uses uData, pFIBProps, FIBDatabase;

{$R *.dfm}

procedure Tform_Settings.btn_OKClick(Sender: TObject);
begin
  Data.DB.DBName := ed_DB.Text;
  Data.DB.ConnectParams.UserName := ed_Username.Text;
  Data.DB.ConnectParams.Password := ed_Password.Text;
  Data.DB.ConnectParams.RoleName := ed_Role.Text;
  Data.DB.ConnectParams.CharSet := ed_Charset.Text;
  Data.db.LibraryName := ed_ClientDLL.FileName;
  Data.MinLen := ed_MinLen.Value;
  Data.RefreshInterval := ed_Refresh.Value;

  Data.SaveSettings;
end;

class function Tform_Settings.Execute: Boolean;
var
  form : Tform_Settings;
begin
  form := Tform_Settings.Create(nil);
  try
    Result := IsPositiveResult(form.ShowModal);
  finally
    form.Free;
  end;
    
end;

procedure Tform_Settings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SettingsLinkForm.SaveSettings;
  Action := caHide;
end;

procedure Tform_Settings.FormShow(Sender: TObject);
begin
  SettingsLinkForm.ApplySettings;

  ed_DB.Text := Data.DB.DBName;
  ed_Username.Text := Data.DB.ConnectParams.UserName;
  ed_Password.Text := Data.DB.ConnectParams.Password;
  ed_Role.Text := Data.DB.ConnectParams.RoleName;
  ed_Charset.Text := Data.DB.ConnectParams.CharSet;
  ed_ClientDLL.FileName := Data.DB.LibraryName;
  ed_MinLen.Value := Data.MinLen;
  ed_Refresh.Value := Data.RefreshInterval;
end;

end.
