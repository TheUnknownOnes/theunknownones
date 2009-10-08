unit uFormSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, JvExComCtrls, JvComCtrls, StdCtrls, ExtCtrls, JvExControls,
  JvScrollMax, JvExExtCtrls, JvExtComponent, Mask, JvExMask, JvSpin;

type
  Tform_Settings = class(TForm)
    ScrollMax: TJvScrollMax;
    bnd_Waveview: TJvScrollMaxBand;
    bnd_AutoLevel: TJvScrollMaxBand;
    GridPanel1: TGridPanel;
    Label1: TLabel;
    track_WaveInterval: TJvTrackBar;
    Label2: TLabel;
    track_WaveZoom: TJvTrackBar;
    GridPanel2: TGridPanel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    track_Max: TJvTrackBar;
    ed_MaxInterval: TJvSpinEdit;
    Label7: TLabel;
    track_Min: TJvTrackBar;
    ed_MinInterval: TJvSpinEdit;
    Label8: TLabel;
    track_NoAct: TJvTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure track_WaveZoomChange(Sender: TObject);
    procedure track_WaveIntervalChange(Sender: TObject);
    procedure ed_MaxIntervalChange(Sender: TObject);
    procedure ed_MinIntervalChange(Sender: TObject);
    procedure track_MaxChange(Sender: TObject);
    procedure track_MinChange(Sender: TObject);
    procedure track_NoActChange(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    class procedure Execute; 
  end;

implementation

uses uFormMain;

{$R *.dfm}

procedure Tform_Settings.ed_MaxIntervalChange(Sender: TObject);
begin
  form_Main.tm_Max.Interval := ed_MaxInterval.AsInteger;
end;

procedure Tform_Settings.ed_MinIntervalChange(Sender: TObject);
begin
  form_Main.tm_Min.Interval := ed_MinInterval.AsInteger;
end;

class procedure Tform_Settings.Execute;
var
  form : Tform_Settings;
begin
  form := Tform_Settings.Create(Application);
  form.Show;
end;

procedure Tform_Settings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  form_Main.mi_Settings.Enabled := true;
end;

procedure Tform_Settings.FormCreate(Sender: TObject);
begin
  form_Main.mi_Settings.Enabled := false;
end;

procedure Tform_Settings.FormShow(Sender: TObject);
begin
  track_WaveZoom.Position := Round(form_Main.Wave.Zoom * 100);
  track_WaveInterval.Position := form_Main.tm_Wave.Interval;

  track_Max.Position := form_Main.SRSettings.MaxLevel;
  track_Min.Position := form_Main.SRSettings.MinLevel;
  track_NoAct.Position := form_Main.SRSettings.NoActLevel;

  ed_MaxInterval.AsInteger := form_Main.tm_Max.Interval;
  ed_MinInterval.AsInteger := form_Main.tm_Min.Interval;  
end;

procedure Tform_Settings.track_MaxChange(Sender: TObject);
begin
  form_Main.SRSettings.MaxLevel := track_Max.Position;
end;

procedure Tform_Settings.track_MinChange(Sender: TObject);
begin
  form_Main.SRSettings.MinLevel := track_Min.Position;
end;

procedure Tform_Settings.track_NoActChange(Sender: TObject);
begin
  form_Main.SRSettings.NoActLevel := track_NoAct.Position;
end;

procedure Tform_Settings.track_WaveIntervalChange(Sender: TObject);
begin
  form_Main.tm_Wave.Interval := track_WaveInterval.Position;
end;

procedure Tform_Settings.track_WaveZoomChange(Sender: TObject);
begin
  form_Main.Wave.Zoom := track_WaveZoom.Position / 100;
end;

end.
