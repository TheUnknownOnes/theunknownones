unit uFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, uEffectPNGToolbar, ToolWin, ExtCtrls, uBassWaveView,
  Bass, SyncObjs, ImageHlp, uSettingsBase, uSettingsStream, uSysTools, ShlObj,
  uSettingsLinksDefault, JvExComCtrls, JvComCtrls, StdCtrls, JvExControls,
  JvTracker, JvComponentBase, uEffectPNGImage, ActiveX, ComObj, ImgList,
  uImageListProvider, uBaseImageList,
  uPNGImageList;

type
  {$TYPEINFO ON}
  TSRSettings = class
  private
    FMaxLevel: Smallint;
    FMinLevel: Smallint;
    FNoActLevel: Smallint;
  public
    constructor Create();
  published
    property MaxLevel : Smallint read FMaxLevel write FMaxLevel;
    property MinLevel : Smallint read FMinLevel write FMinLevel;
    property NoActLevel : Smallint read FNoActLevel write FNoActLevel;
  end;

  Tform_Main = class(TForm, IDropSource)
    TB: TEffectPNGToolBar;
    btn_MainMenu: TEffectPNGToolButton;
    pum_Main: TPopupMenu;
    mi_Close: TMenuItem;
    ToolButton1: TToolButton;
    btn_Start: TEffectPNGToolButton;
    btn_Cut: TEffectPNGToolButton;
    btn_Stop: TEffectPNGToolButton;
    pan_Wave: TPanel;
    Wave: TBassWaveView;
    tm_Wave: TTimer;
    Settings: TSettingsFile;
    sl_Wave: TSettingsLinkComponent;
    mi_Settings: TMenuItem;
    N1: TMenuItem;
    sl_TimerWave: TSettingsLinkComponent;
    pan_Level: TPanel;
    cb_AutoLevel: TCheckBox;
    tm_Level: TTimer;
    tm_Max: TTimer;
    tm_Min: TTimer;
    sl_Max: TSettingsLinkComponent;
    sl_Min: TSettingsLinkComponent;
    SettingsLinkComponent1: TSettingsLinkComponent;
    lv_Files: TListView;
    pum_Files: TPopupMenu;
    mi_Delete: TMenuItem;
    track_Level: TTrackBar;
    procedure btn_MainMenuClick(Sender: TObject);
    procedure mi_CloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tm_WaveTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btn_StartClick(Sender: TObject);
    procedure btn_CutClick(Sender: TObject);
    procedure btn_StopClick(Sender: TObject);
    procedure mi_SettingsClick(Sender: TObject);
    procedure tm_LevelTimer(Sender: TObject);
    procedure cb_AutoLevelClick(Sender: TObject);
    procedure tm_MaxTimer(Sender: TObject);
    procedure tm_MinTimer(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mi_DeleteClick(Sender: TObject);
    procedure track_LevelChange(Sender: TObject);
    procedure lv_FilesMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FRecDir : String;
    FStreamLock : TCriticalSection;
    FStream : TFileStream;

    FRecChannel : Cardinal;
    FInput : Integer; 
    FRecording: Boolean;
    procedure DockToTop;
    procedure UndockFromTop;
    procedure SetRecording(const Value: Boolean);

    function GenFilename : String;

    procedure LoadSettings;
    procedure SaveSettings;

    procedure SearchInput;

    {$REGION 'IDropSource'}
    function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint): HResult; stdcall;
    function GiveFeedback(dwEffect: Longint): HResult; stdcall;
    {$ENDREGION}
  public
    PeakLevel : Smallint;
    SRSettings : TSRSettings;

    property Recording : Boolean read FRecording write SetRecording;
  end;



  TWaveHeader = packed record
    riff:			array[0..3] of AnsiChar;
    len:			DWord;
    cWavFmt:		array[0..7] of AnsiChar;
    dwHdrLen:		DWord;
    wFormat:		Word;
    wNumChannels:	Word;
    dwSampleRate:	DWord;
    dwBytesPerSec:	DWord;
    wBlockAlign:	Word;
    wBitsPerSample:	Word;
    cData:			array[0..3] of AnsiChar;
    dwDataLen:		DWord;
  end;

var
  form_Main: Tform_Main;

const
  SampleRate = 44100;
  ChannelCount = 1;
  SampleSize = SizeOf(SmallInt);

implementation

uses uFormSettings;

{$R *.dfm}

function RecordingCallback(Handle: HRECORD; buffer: Pointer; length, user: DWord): boolean; stdcall;
var
  V : PSmallInt;
  Peak : SmallInt;
  idx : Integer;
begin
  if form_Main.Recording then
  begin
    form_Main.FStreamLock.Enter;
    try
      form_Main.FStream.WriteBuffer(buffer^, length);
    finally
      form_Main.FStreamLock.Leave;
    end;
  end;

  Peak := 0;
  V := Buffer;

  for idx := 0 to (length div SampleSize) - 1 do
  begin
    if Abs(V^) > Peak then
      Peak := Abs(V^);

    Inc(V);    
  end;

  form_Main.PeakLevel := Round((Peak / MAXSHORT) * 100);

	Result := True;
end;

procedure Tform_Main.btn_CutClick(Sender: TObject);
begin
  Recording := false;
  Recording := true;
end;

procedure Tform_Main.btn_MainMenuClick(Sender: TObject);
begin
  btn_MainMenu.DropdownMenu.Popup(btn_MainMenu.Left, btn_MainMenu.Top + btn_MainMenu.Height);
end;

procedure Tform_Main.btn_StartClick(Sender: TObject);
begin
  Recording := true;
end;

procedure Tform_Main.btn_StopClick(Sender: TObject);
begin
  Recording := false;
end;

procedure Tform_Main.cb_AutoLevelClick(Sender: TObject);
begin
  tm_Max.Enabled := cb_AutoLevel.Checked;
  tm_Min.Enabled := cb_AutoLevel.Checked;
end;

procedure Tform_Main.DockToTop;
var
  r : TRect;
begin
  r := Screen.PrimaryMonitor.WorkareaRect;

  if r.Top <> Screen.PrimaryMonitor.BoundsRect.Top + Height then
  begin
    r.Top := r.Top + Height;
    SystemParametersInfo(SPI_SETWORKAREA, 0, @r, SPIF_SENDCHANGE);
  end;

  r := Screen.PrimaryMonitor.BoundsRect;

  SetBounds(r.Left, r.Top, r.Right - r.Left, Height);
end;

procedure Tform_Main.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;

  Recording := false;

  BASS_ChannelStop(FRecChannel);
  BASS_RecordFree;

  FStreamLock.Free;

  SRSettings.Free;

  Action := caFree;
  UndockFromTop;
end;

procedure Tform_Main.FormCreate(Sender: TObject);
begin
  SRSettings := TSRSettings.Create;

  FRecDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'records\';
  MakeSureDirectoryPathExists(PAnsiChar(Ansistring(FRecDir)));

  LoadSettings;

  FStreamLock := TCriticalSection.Create;

  FStream := nil;
  FRecording := false;

  BASS_RecordInit(-1);
  FRecChannel := BASS_RecordStart(44100, 1, 0, @RecordingCallback, nil);

  SearchInput;

  Wave.BassChannel := FRecChannel;
end;

procedure Tform_Main.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_SPACE then
    btn_Cut.Click;
end;

procedure Tform_Main.FormShow(Sender: TObject);
begin
  DoubleBuffered := true;
  DockToTop;
end;

function Tform_Main.GenFilename: String;
begin
  Result := FRecDir + FormatDateTime('hh_nn_ss.wav', Now);
end;

function Tform_Main.GiveFeedback(dwEffect: Integer): HResult;
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;

procedure Tform_Main.LoadSettings;
begin
  Settings.FileName := IncludeTrailingPathDelimiter(GetShellFolder(CSIDL_APPDATA)) + 'SimpleRecorder.dat';
  if FileExists(Settings.FileName) then
    Settings.Load;

  Settings.ReadObject('/Settings', SRSettings);
end;

procedure Tform_Main.lv_FilesMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  idx : Integer;
  d : IDataObject;
  e : Integer;
  fileList : TStringList;
begin
  if not Assigned(lv_Files.Selected) or (not (ssLeft in Shift)) then
    exit;

  fileList := TStringList.Create;
  try
    for idx := 0 to lv_Files.Items.Count - 1 do
    begin
      if lv_Files.Items[idx].Selected then
        fileList.Add(ExtractFileName(lv_Files.Items[idx].SubItems[0]));
    end;

    d := GetDataObjectFromFileList(FRecDir, fileList);
    DoDragDrop(d, Self, DROPEFFECT_COPY or DROPEFFECT_LINK, e);
  finally
    fileList.Free;
  end;
end;

procedure Tform_Main.mi_CloseClick(Sender: TObject);
begin
  Close;
end;

procedure Tform_Main.mi_DeleteClick(Sender: TObject);
begin
  if Assigned(lv_Files.Selected) then
  begin
    if (MessageBox(0, 'Do you really want to delete the selected files?', 'Delete files', MB_ICONWARNING or MB_YESNO) = idYes) then
    begin
      while Assigned(lv_Files.Selected) do
      begin
        DeleteFileToWasteBin(lv_Files.Selected.SubItems[0]);
        lv_Files.Selected.Delete;
      end;
    end;
  end;
end;

procedure Tform_Main.mi_SettingsClick(Sender: TObject);
begin
  Tform_Settings.Execute;
end;

function Tform_Main.QueryContinueDrag(fEscapePressed: BOOL;
  grfKeyState: Integer): HResult;
begin
  if fEscapePressed then
    Result := DRAGDROP_S_CANCEL
  else
  if (grfKeyState and MK_LBUTTON) = 0 then
    Result := DRAGDROP_S_DROP
  else
    Result := S_OK;
end;

procedure Tform_Main.SaveSettings;
begin
  Settings.WriteObject('/Settings', SRSettings);
  Settings.Save;
end;

procedure Tform_Main.SearchInput;
var
  v : Single;
begin
  FInput := 99;

  while (BASS_RecordGetInputName(FInput) = nil) or
        ((BASS_RecordGetInput(FInput, v) and BASS_INPUT_OFF) = BASS_INPUT_OFF) do
  begin
    Dec(FInput);
  end;
end;

procedure Tform_Main.SetRecording(const Value: Boolean);
var
  wh : TWaveHeader;
  i : Integer;
  Li : TListItem;
begin
  if Value = FRecording then
    exit;

  if not Value then
    FRecording := Value;

  if Value then
  begin
    with wh do
    begin
      riff := 'RIFF';
      len := 36;
      cWavFmt := 'WAVEfmt ';
      dwHdrLen := 16;
      wFormat := 1;
      wNumChannels := ChannelCount;
      dwSampleRate := SampleRate;
      wBlockAlign := ChannelCount * SampleSize;
      dwBytesPerSec := ChannelCount * SampleSize * SampleRate;
      wBitsPerSample := ChannelCount * SampleSize * 8;
      cData := 'data';
      dwDataLen := 0;
    end;

    FStreamLock.Enter;
    try
      FStream := TFileStream.Create(GenFilename, fmCreate or fmOpenWrite or fmShareDenyWrite);
      FStream.WriteBuffer(wh, SizeOf(wh));
    finally
      FStreamLock.Leave;
    end;
  end
  else
  begin
    FStreamLock.Enter;
    try
      FStream.Position := 4;
      i := FStream.Size - 8;
      FStream.Write(i, 4);
      i := i - $24;
      FStream.Position := 40;
      FStream.Write(i, 4);


      Li := lv_Files.Items.Insert(0);
      Li.Caption := ExtractFileName(FStream.FileName);
      Li.SubItems.Add(FStream.FileName);
      lv_Files.Selected := nil;
      lv_Files.Selected := li;
      li.MakeVisible(false);

      FStream.Free;
    finally
      FStreamLock.Leave;
    end;
  end;

  if Value then
    FRecording := Value;

  btn_Start.Enabled := not FRecording;
  btn_Cut.Enabled := FRecording;
  btn_Stop.Enabled := FRecording;
end;

procedure Tform_Main.tm_LevelTimer(Sender: TObject);
var
  v : Single;
begin
  BASS_RecordGetInput(FInput, v);
  track_Level.Position := Round(v * 100);
end;

procedure Tform_Main.tm_MaxTimer(Sender: TObject);
begin
  if PeakLevel > SRSettings.MaxLevel then
    track_Level.Position := track_Level.Position - 1;
end;

procedure Tform_Main.tm_MinTimer(Sender: TObject);
begin
  if (PeakLevel > SRSettings.NoActLevel) and (PeakLevel < SRSettings.MinLevel) then
    track_Level.Position := track_Level.Position + 1;
end;

procedure Tform_Main.tm_WaveTimer(Sender: TObject);
begin
  Wave.Refresh;
end;

procedure Tform_Main.track_LevelChange(Sender: TObject);
begin
  BASS_RecordSetInput(FInput, 0, track_Level.Position / 100);
end;

procedure Tform_Main.UndockFromTop;
var
  r : TRect;
begin
  r := Screen.PrimaryMonitor.WorkareaRect;

  r.Top := r.Top - Height;

  SystemParametersInfo(SPI_SETWORKAREA, 0, @r, SPIF_SENDCHANGE);
end;

{ TSRSettings }

constructor TSRSettings.Create;
begin
  FMaxLevel := 95;
  FMinLevel := 50;
  FNoActLevel := 10;
end;


procedure PatchINT3;
var
  NOP : Byte;
  NTDLL: THandle;
  BytesWritten: DWORD;
  Address: Pointer;
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then Exit;
  NTDLL := GetModuleHandle('NTDLL.DLL');
  if NTDLL = 0 then Exit;
  Address := GetProcAddress(NTDLL, 'DbgBreakPoint');
  if Address = nil then Exit;
  try
    if AnsiChar(Address^) <> #$CC then Exit;

    NOP := $90;
    if WriteProcessMemory(GetCurrentProcess, Address, @NOP, 1, BytesWritten) and
      (BytesWritten = 1) then
      FlushInstructionCache(GetCurrentProcess, Address, 1);
  except
    //Do not panic if you see an EAccessViolation here, it is perfectly harmless!
    on EAccessViolation do ;
    else raise;
  end;
end;

initialization
  // nur wenn ein Debugger vorhanden, den Patch ausführen
  if DebugHook<>0 then
     PatchINT3;

end.
