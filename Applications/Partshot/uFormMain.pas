unit uFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvComponentBase, JvTrayIcon, uFormShooter, StdCtrls, Spin, Mask,
  JvExMask, JvToolEdit, uSettingsBase, uSettingsXML, uSysTools, ShlObj,
  uSettingsLinksDefault, pngimage, ExtCtrls, Menus, JvExControls, JvColorBox,
  JvColorButton;

type
  Tform_Main = class(TForm)
    Tray: TJvTrayIcon;
    gb_ActionAfter: TGroupBox;
    rb_SaveToFile: TRadioButton;
    gb_SaveOptions: TGroupBox;
    Label1: TLabel;
    ed_SaveToFolder: TJvDirectoryEdit;
    Label2: TLabel;
    ed_Format: TEdit;
    Label3: TLabel;
    ed_Counter: TSpinEdit;
    Settings: TSettingsXMLFile;
    SettingsLinkComponent1: TSettingsLinkComponent;
    SettingsLinkComponent2: TSettingsLinkComponent;
    gb_ShotSize: TGroupBox;
    rb_SizeFix: TRadioButton;
    gb_FixSizeOptions: TGroupBox;
    Label4: TLabel;
    ed_FixSizeWidth: TSpinEdit;
    Label5: TLabel;
    ed_FixSizeHeight: TSpinEdit;
    SettingsLinkComponent3: TSettingsLinkComponent;
    SettingsLinkComponent4: TSettingsLinkComponent;
    rb_Ratio: TRadioButton;
    gb_RatioOptions: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    ed_RatioX: TSpinEdit;
    ed_RatioY: TSpinEdit;
    pan_Bottom: TPanel;
    btn_OK: TButton;
    SettingsLinkComponent5: TSettingsLinkComponent;
    SettingsLinkComponent6: TSettingsLinkComponent;
    SettingsLinkComponent7: TSettingsLinkComponent;
    SettingsLinkComponent8: TSettingsLinkComponent;
    gb_Misc: TGroupBox;
    cb_MultiShots: TCheckBox;
    SettingsLinkComponent9: TSettingsLinkComponent;
    pum_tray: TPopupMenu;
    mi_Config: TMenuItem;
    mi_Exit: TMenuItem;
    cl_Shooter: TJvColorButton;
    Label8: TLabel;
    SettingsLinkComponent10: TSettingsLinkComponent;
    Label9: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TrayClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure btn_OKClick(Sender: TObject);
    procedure rb_SaveToFileClick(Sender: TObject);
    procedure rb_SizeFixClick(Sender: TObject);
    procedure mi_ConfigClick(Sender: TObject);
    procedure mi_ExitClick(Sender: TObject);
    procedure cl_ShooterChange(Sender: TObject);
  private
    FShooter : Tform_Shooter;

    procedure LoadSettings;
    procedure SaveSettings;

    procedure StartShooting;
  public
    procedure DoShot;

    function GenNextFileName : String;

    procedure PerformMouseWheel(ADelta : Integer; Shift : TShiftState);
    procedure SetShooterHeightAccordingToRatio; 
  end;

var
  form_Main: Tform_Main;

const
  FileNameFormat = 'Partshot_%.4d';

implementation

{$R *.dfm}

procedure Tform_Main.btn_OKClick(Sender: TObject);
begin
  Tray.HideApplication;
end;

procedure Tform_Main.cl_ShooterChange(Sender: TObject);
begin
  FShooter.Color := cl_Shooter.Color;
end;

procedure Tform_Main.DoShot;
var
  BMP : TBitmap;
  PNG : TPNGObject;
begin
  FShooter.Hide;

  BMP := TBitmap.Create;

  BMP.PixelFormat := pf24bit;
  BMP.Width := FShooter.Width;
  BMP.Height := FShooter.Height;

  BitBlt(BMP.Canvas.Handle,
         0, 0,
         BMP.Width, BMP.Height,
         GetDC(GetDesktopWindow),
         FShooter.Left, FShooter.Top,
         SRCCOPY);

  PNG := TPNGObject.Create();
  PNg.Assign(BMP);
  BMP.Free;

  PNG.SaveToFile(IncludeTrailingPathDelimiter(ed_SaveToFolder.Directory) + GenNextFileName);

  PNG.Free;

  ed_Counter.Value := ed_Counter.Value + 1;

  if cb_MultiShots.Checked then
    StartShooting;
end;

procedure Tform_Main.FormCreate(Sender: TObject);
begin
  ed_SaveToFolder.Directory := GetShellFolder($27);
  ed_Format.Text := FileNameFormat;

  FShooter := Tform_Shooter.Create(Self);

  LoadSettings;
end;

procedure Tform_Main.FormDestroy(Sender: TObject);
begin
  SaveSettings;
end;

function Tform_Main.GenNextFileName: String;
begin
  try
    Result := Format(ed_Format.Text, [ed_Counter.Value]) + '.png'
  except
    ed_Format.Text := FileNameFormat;
    Result := GenNextFileName;
  end;
end;

procedure Tform_Main.LoadSettings;
begin
  Settings.FileName := IncludeTrailingPathDelimiter(GetShellFolder(CSIDL_APPDATA)) + 'Partshot.xml';

  if FileExists(Settings.FileName) then
    Settings.Load;
end;

procedure Tform_Main.mi_ConfigClick(Sender: TObject);
begin
  Tray.ShowApplication;
end;

procedure Tform_Main.mi_ExitClick(Sender: TObject);
begin
  Close;
end;

procedure Tform_Main.PerformMouseWheel(ADelta: Integer; Shift : TShiftState);
var
  Fact : Integer;
begin
  if rb_Ratio.Checked then
  begin
    if ssAlt in Shift then
      Fact := 1
    else
    if ssCtrl in Shift then
      Fact := 10
    else
      Fact := 3;

    FShooter.Width := FShooter.Width + (ADelta div abs(ADelta)) * Fact;
    SetShooterHeightAccordingToRatio;
  end
  else
  if rb_SizeFix.Checked then
  begin
    if ssAlt in Shift then
      Fact := 1
    else
    if ssCtrl in Shift then
      Fact := 10
    else
      Fact := 3;

    if ssShift in Shift then
      FShooter.Width := FShooter.Width + (ADelta div abs(ADelta)) * Fact
    else
      FShooter.Height := FShooter.Height + (ADelta div abs(ADelta)) * Fact;
  end;

  ed_FixSizeWidth.Value := FShooter.Width;
  ed_FixSizeHeight.Value := FShooter.Height;
end;

procedure Tform_Main.rb_SaveToFileClick(Sender: TObject);
begin
  gb_SaveOptions.Enabled := rb_SaveToFile.Checked;
end;

procedure Tform_Main.rb_SizeFixClick(Sender: TObject);
begin
  gb_FixSizeOptions.Enabled := rb_SizeFix.Checked;
  gb_RatioOptions.Enabled := rb_Ratio.Checked;
end;

procedure Tform_Main.SaveSettings;
begin
  Settings.Save;
end;

procedure Tform_Main.SetShooterHeightAccordingToRatio;
begin
  FShooter.Height := Round(FShooter.Width * (ed_RatioY.Value / ed_RatioX.Value));
end;

procedure Tform_Main.StartShooting;
begin
  if rb_SizeFix.Checked then
  begin
    FShooter.Width := ed_FixSizeWidth.Value;
    FShooter.Height := ed_FixSizeHeight.Value;

    FShooter.lbl_Bottom.Caption := 'Use Mousewheel to change height and Mousewheel + Shift to change width (+Ctrl to change faster / +Alt to change slower)';
  end
  else
  if rb_Ratio.Checked then
  begin
    FShooter.lbl_Bottom.Caption := 'Use Mousewheel to change size (+Ctrl to change faster / +Alt to change slower)';
    SetShooterHeightAccordingToRatio;
  end;

  Tray.HideApplication;

  FShooter.Show;
  SetWindowPos(FShooter.Handle, HWND_TOPMOST, FShooter.Left, FShooter.Top, FShooter.Width, FShooter.Height, 0);
  SetActiveWindow(FShooter.Handle);
  Windows.SetFocus(FShooter.Handle);
end;

procedure Tform_Main.TrayClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    StartShooting;
end;

end.
