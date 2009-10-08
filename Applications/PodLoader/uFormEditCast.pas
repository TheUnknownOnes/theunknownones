unit uFormEditCast;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uSettingsBase, uSettingsLinksDefault, StdCtrls, ExtCtrls, Mask,
  JvExMask, JvToolEdit, JvExStdCtrls, JvEdit, JvValidateEdit, ImageHLP,
  ClipBrd, uSysTools, ShlObj;

type
  Tform_EditCast = class(TForm)
    SettingsLinkForm: TSettingsLinkForm;
    pan_Bottom: TPanel;
    btn_OK: TButton;
    btn_Cancel: TButton;
    ed_URL: TEdit;
    lbl_URL: TLabel;
    ed_SaveTo: TJvDirectoryEdit;
    lbl_SaveTo: TLabel;
    lbl_CheckInterval: TLabel;
    ed_Interval: TJvValidateEdit;
    lbl_Name: TLabel;
    ed_Name: TEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure btn_OKClick(Sender: TObject);
  private
    FCastID : TGUID;

    procedure LoadSettings;
  public
    { Public-Deklarationen }
  end;

function EditCast(var ACastID : TGUID) : Boolean;

const
  CreateNewCastGUID : TGUID = '{C4A3CBE1-A12C-4DD4-9235-6E47A34E9925}';

implementation

uses uData;

{$R *.dfm}

function EditCast(var ACastID : TGUID) : Boolean;
var
  Form : Tform_EditCast;
begin
  Application.CreateForm(Tform_EditCast, Form);
  try
    Form.FCastID := ACastID;
    Result := IsPositiveResult(Form.ShowModal);

    if Result then
      ACastID := Form.FCastID;

  finally
    Form.Release;
  end;                          
end;

procedure Tform_EditCast.btn_OKClick(Sender: TObject);
var
  CastPath : TSettingName;
begin
  if SameText(ed_Name.Text, EmptyStr) then
    raise Exception.Create('Please specify a name');

  if SameText(ed_URL.Text, EmptyStr) then
    raise Exception.Create('Please specify a URL');

  if SameText(ed_SaveTo.Text, EmptyStr) then
    raise Exception.Create('Please specify a folder where to save the files');

  if (not DirectoryExists(ed_SaveTo.Text)) and
     (not MakeSureDirectoryPathExists(PChar(ed_SaveTo.Text))) then
    raise Exception.Create('Can not create directory');

  if SameGUID(FCastID, CreateNewCastGUID) then
  begin
    CreateGUID(FCastID);
    CastPath := '/Casts/' + GUIDToString(FCastID);
  end;

  Data.Settings.SetValue(CastPath + '/Name', ed_Name.Text);
  Data.Settings.SetValue(CastPath + '/URL', ed_URL.Text);
  Data.Settings.SetValue(CastPath + '/SaveFolder', ed_SaveTo.Text);
  Data.Settings.SetValue(CastPath + '/CheckInterval', ed_Interval.Value);

  ModalResult := mrOk;  
end;

procedure Tform_EditCast.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SettingsLinkForm.SaveSettings;
end;

procedure Tform_EditCast.FormShow(Sender: TObject);
begin
  SettingsLinkForm.ApplySettings;
  LoadSettings;
end;

procedure Tform_EditCast.LoadSettings;
var
  MyMusic : String;
  CastPath : TSettingName;
begin
  MyMusic := GetShellFolder(CSIDL_COMMON_MUSIC);

  CastPath := '/Casts/' + GUIDToString(FCastID);

  ed_Name.Text := Data.Settings.GetValue(CastPath + '/Name', ed_Name.Text);
  ed_URL.Text := Data.Settings.GetValue(CastPath + '/URL', Clipboard.AsText);
  ed_SaveTo.Text := Data.Settings.GetValue(CastPath + '/SaveFolder', MyMusic);
  ed_Interval.Value := Data.Settings.GetValue(CastPath + '/CheckInterval', ed_Interval.Value);
end;

end.
