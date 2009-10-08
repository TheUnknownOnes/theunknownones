unit unit_FormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, pngimage, ExtCtrls, Buttons, Registry;

type
  Tform_Login = class(TForm)
    Image1: TImage;
    gb_Proxy: TGroupBox;
    Label1: TLabel;
    ed_Server: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ed_Port: TEdit;
    ed_UsernameProxy: TEdit;
    ed_PasswordProxy: TEdit;
    gb_DP: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    ed_UsernameDP: TEdit;
    ed_PasswordDP: TEdit;
    pan_Bottom: TPanel;
    btn_Login: TBitBtn;
    btn_Abbrechen: TBitBtn;
    img_RollProxy: TImage;
    pan_Options: TPanel;
    cb_Save: TCheckBox;
    cb_Autostart: TCheckBox;
    cb_AutoLogin: TCheckBox;
    tm_AutoLogin: TTimer;
    procedure tm_AutoLoginTimer(Sender: TObject);
    procedure cb_AutostartClick(Sender: TObject);
    procedure btn_LoginClick(Sender: TObject);
    procedure ed_PortKeyPress(Sender: TObject; var Key: Char);
    procedure btn_AbbrechenClick(Sender: TObject);
    procedure img_RollProxyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  form_Login: Tform_Login;

const
  GB_PROXY_MIN_SIZE=20;
  REG_AUTOSTART_PATH='\Software\Microsoft\Windows\CurrentVersion\Run\';
  REG_AUTOSTART_VALUE='DPShouter';

implementation

uses unit_Data, unit_FormSB;

{$R *.dfm}

procedure Tform_Login.btn_AbbrechenClick(Sender: TObject);
begin
  Close;
end;

procedure Tform_Login.btn_LoginClick(Sender: TObject);
begin
  if cb_Save.Checked then
  begin
    Data.Settings.WriteString('Proxy','Server',ed_Server.Text);
    Data.Settings.WriteString('Proxy','Port',ed_Port.Text);
    Data.Settings.WriteString('Proxy','Username',ed_UsernameProxy.Text);
    Data.Settings.WriteString('Proxy','Password',Data.Crypt(ed_PasswordProxy.Text));

    Data.Settings.WriteString('DP','Username',ed_UsernameDP.Text);
    Data.Settings.WriteString('DP','Password',Data.Crypt(ed_PasswordDP.Text));

    Data.Settings.WriteBool('Misc','SaveLogin',true);
    Data.Settings.WriteBool('Misc','AutoLogin',cb_AutoLogin.Checked);
  end
  else
  begin
    Data.Settings.EraseSection('Proxy');
    Data.Settings.EraseSection('DP');
    Data.Settings.EraseSection('Misc');
    Data.Settings.WriteBool('Misc','SaveLogin',false);
  end;

  Data.Browser.ProxyParams.ProxyServer:=ed_Server.Text;
  Data.Browser.ProxyParams.ProxyPort:=StrtoIntDef(ed_Port.Text,8080);
  Data.Browser.ProxyParams.ProxyUsername:=ed_UsernameProxy.Text;
  Data.Browser.ProxyParams.ProxyPassword:=ed_PasswordProxy.Text;

  if (Data.Login(ed_UsernameDP.Text,ed_PasswordDP.Text)) then
  begin
    Application.CreateForm(Tform_SB, form_SB);
    Self.Left:=0-Self.Width;
    Self.Top:=0-Self.Top;
    Self.Hide;
    if form_SB.ShowModal=mrOk then
      Close
    else
    begin
      form_SB.Release;
      Self.Show;
    end;
  end
  else
    MessageDlg(STR_LoginFailed, mtError, [mbOK], 0);
end;

procedure Tform_Login.cb_AutostartClick(Sender: TObject);
var
  Reg : TRegistry;
begin
  Reg:=TRegistry.Create;
  try
    Reg.RootKey:=HKEY_CURRENT_USER;
    if Reg.OpenKey(REG_AUTOSTART_PATH,false) then
    begin
      if cb_Autostart.Checked then
        Reg.WriteString(REG_AUTOSTART_VALUE,Application.ExeName)
      else
        Reg.DeleteValue(REG_AUTOSTART_VALUE);
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

procedure Tform_Login.ed_PortKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key>#57) or (Key<#48) then Key:=#0;
end;

procedure Tform_Login.FormCreate(Sender: TObject);
var
  Reg : TRegistry;
begin
  Self.Color:=COLOR_Form;
  gb_Proxy.Height:=GB_PROXY_MIN_SIZE;

  cb_Autostart.Checked:=false;
  Reg:=TRegistry.Create;
  try
    Reg.RootKey:=HKEY_CURRENT_USER;
    if Reg.OpenKey(REG_AUTOSTART_PATH,false) then
      cb_Autostart.Checked:=Reg.ValueExists(REG_AUTOSTART_VALUE);
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

procedure Tform_Login.FormShow(Sender: TObject);
begin
  if Data.ShutDown then Close;

  ed_Server.Text:=Data.Settings.ReadString('Proxy','Server','');
  ed_Port.Text:=Data.Settings.ReadString('Proxy','Port','');
  ed_UsernameProxy.Text:=Data.Settings.ReadString('Proxy','Username','');
  ed_PasswordProxy.Text:=Data.Crypt(Data.Settings.ReadString('Proxy','Password',''));

  ed_UsernameDP.Text:=Data.Settings.ReadString('DP','Username','');
  ed_PasswordDP.Text:=Data.Crypt(Data.Settings.ReadString('DP','Password',''));

  cb_Save.Checked:=Data.Settings.ReadBool('Misc','SaveLogin',true);
  cb_AutoLogin.Checked:=Data.Settings.ReadBool('Misc','AutoLogin',false);


  ed_UsernameDP.SetFocus;

  tm_AutoLogin.Enabled:=cb_AutoLogin.Checked;
end;

procedure Tform_Login.img_RollProxyClick(Sender: TObject);
begin
  if gb_Proxy.Height<gb_Proxy.Tag then
    gb_Proxy.Height:=gb_Proxy.Tag
  else
    gb_Proxy.Height:=GB_PROXY_MIN_SIZE;
end;

procedure Tform_Login.tm_AutoLoginTimer(Sender: TObject);
begin
  tm_AutoLogin.Enabled:=false;
  btn_LoginClick(Sender);
end;

end.
