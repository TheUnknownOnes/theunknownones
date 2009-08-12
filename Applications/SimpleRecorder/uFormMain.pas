unit uFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, uEffectPNGToolbar, ToolWin, ExtCtrls, uBassWaveView,
  Bass, SyncObjs, ImageHlp, uSettingsBase, uSettingsStream, uSysTools, ShlObj,
  uSettingsLinksDefault, JvExComCtrls, JvComCtrls, StdCtrls, JvExControls,
  JvTracker, JvComponentBase, uEffectPNGImage, ActiveX, ComObj;

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
    track_Level: TJvTracker;
    tm_Level: TTimer;
    tm_Max: TTimer;
    tm_Min: TTimer;
    sl_Max: TSettingsLinkComponent;
    sl_Min: TSettingsLinkComponent;
    SettingsLinkComponent1: TSettingsLinkComponent;
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
    procedure track_LevelChangedValue(Sender: TObject; NewValue: Integer);
    procedure cb_AutoLevelClick(Sender: TObject);
    procedure tm_MaxTimer(Sender: TObject);
    procedure tm_MinTimer(Sender: TObject);
  private
    FLastFile : String;
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
  FLastFile := '';

  SRSettings := TSRSettings.Create;
  
  LoadSettings;

  FStreamLock := TCriticalSection.Create;

  FStream := nil;
  FRecording := false;

  BASS_RecordInit(-1);
  FRecChannel := BASS_RecordStart(44100, 1, 0, @RecordingCallback, nil);

  SearchInput;

  Wave.BassChannel := FRecChannel;
end;

procedure Tform_Main.FormShow(Sender: TObject);
begin
  DoubleBuffered := true;
  DockToTop;
end;

function Tform_Main.GenFilename: String;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'records\';
  MakeSureDirectoryPathExists(PAnsiChar(Result));
  Result := Result + FormatDateTime('yyyy_mm_dd_hh_nn_ss.wav', Now);
  FLastFile := Result;
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

procedure Tform_Main.mi_CloseClick(Sender: TObject);
begin
  Close;
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
  FInput := -1;

  repeat
    Inc(FInput);
  until (BASS_RecordGetInputName(FInput) = nil) or
        ((BASS_RecordGetInput(FInput, v) and BASS_INPUT_OFF) = BASS_INPUT_OFF);
        
  Dec(FInput);
end;

procedure Tform_Main.SetRecording(const Value: Boolean);
var
  wh : TWaveHeader;
  i : Integer;
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
  track_Level.Value := Round(v * 100);
end;

procedure Tform_Main.tm_MaxTimer(Sender: TObject);
begin
  if PeakLevel > SRSettings.MaxLevel then
    track_LevelChangedValue(track_Level, track_Level.Value - 1);
end;

procedure Tform_Main.tm_MinTimer(Sender: TObject);
begin
  if (PeakLevel > SRSettings.NoActLevel) and (PeakLevel < SRSettings.MinLevel) then
    track_LevelChangedValue(track_Level, track_Level.Value + 1);
end;

procedure Tform_Main.tm_WaveTimer(Sender: TObject);
begin
  Wave.Refresh;
end;

procedure Tform_Main.track_LevelChangedValue(Sender: TObject;
  NewValue: Integer);
begin
  BASS_RecordSetInput(FInput, 0, NewValue / 100);
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


end.
