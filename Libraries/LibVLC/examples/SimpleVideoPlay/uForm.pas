unit uForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, uLibVLC, ComCtrls;

type
  TForm5 = class(TForm)
    Panel1: TPanel;
    btn_OpenFile: TButton;
    pan_Video: TPanel;
    dlg_Video: TOpenDialog;
    btn_Play: TButton;
    btn_Pause: TButton;
    btn_Stop: TButton;
    track: TTrackBar;
    tm_RefreshTrack: TTimer;
    procedure btn_OpenFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btn_PlayClick(Sender: TObject);
    procedure btn_PauseClick(Sender: TObject);
    procedure btn_StopClick(Sender: TObject);
    procedure tm_RefreshTrackTimer(Sender: TObject);
    procedure trackChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FLib : ILibVLC;
    FLibInstance : Plibvlc_instance_t;
    FPlayer : Plibvlc_media_player_t;
    FMedia : Plibvlc_media_t;

    procedure CheckVLCException;
  public
    { Public-Deklarationen }
  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}

procedure TForm5.btn_OpenFileClick(Sender: TObject);
begin
  if FLib.libvlc_media_player_is_playing(FPlayer) <> 0 then
    FLib.libvlc_media_player_stop(FPlayer);

  if dlg_Video.Execute then
  begin
    if Assigned(FMedia) then
      FLib.libvlc_media_release(FMedia);
    FMedia := nil;
    track.Max := 1;

    FMedia := FLib.libvlc_media_new_location(FLibInstance, PAnsiChar(AnsiString(dlg_Video.FileName)));
    CheckVLCException;

    FLib.libvlc_media_player_set_media(FPlayer, FMedia);
    CheckVLCException;

    btn_Play.Click;
  end;
end;

procedure TForm5.btn_PauseClick(Sender: TObject);
begin
  FLib.libvlc_media_player_pause(FPlayer);
  CheckVLCException;
end;

procedure TForm5.btn_PlayClick(Sender: TObject);
begin
  FLib.libvlc_media_player_play(FPlayer);
  CheckVLCException;
end;

procedure TForm5.btn_StopClick(Sender: TObject);
begin
  FLib.libvlc_media_player_stop(FPlayer);
  CheckVLCException;
end;

procedure TForm5.CheckVLCException;
var
  msg : PAnsiChar;
begin
  msg := FLib.libvlc_errmsg;

  if Assigned(msg) then
  begin
    try
      raise Exception.Create(msg);
    finally
      FLib.libvlc_clearerr;
    end;
  end;
end;

procedure TForm5.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FLib.libvlc_media_player_is_playing(FPlayer) <> 0 then
    FLib.libvlc_media_player_stop(FPlayer);

  if Assigned(FMedia) then
    FLib.libvlc_media_release(FMedia);

  FLib.libvlc_media_player_release(FPlayer);
  FLib.libvlc_release(FLibInstance);
end;

procedure TForm5.FormCreate(Sender: TObject);
var
  Params : array[0..1] of PAnsiChar;
begin
  Params[0] := PAnsiChar('--plugin-path=./plugins/');
  Params[1] := PAnsiChar('--ignore-config');

  FLib := LoadLibVLC();

  FLibInstance := FLib.libvlc_new(Length(Params), @Params[0]);
  CheckVLCException;

  FPlayer := FLib.libvlc_media_player_new(FLibInstance);
  CheckVLCException;

  FLib.libvlc_media_player_set_hwnd(FPlayer, Pointer(pan_Video.Handle));
  CheckVLCException;
end;

procedure TForm5.tm_RefreshTrackTimer(Sender: TObject);
begin
  if FLib.libvlc_media_player_is_playing(FPlayer) <> 0 then
  begin
    if track.Max = 1 then
    begin
      if FLib.libvlc_media_get_state(FMedia) <> libvlc_NothingSpecial then
        track.Max := Round(FLib.libvlc_media_player_get_length(FPlayer) / 1000)
    end
    else
      track.Position := Round(FLib.libvlc_media_player_get_time(FPlayer) / 1000);
  end;
end;

procedure TForm5.trackChange(Sender: TObject);
begin
  if FLib.libvlc_media_player_is_playing(FPlayer) <> 0 then
  begin
    FLib.libvlc_media_player_set_time(FPlayer, track.Position * 1000)
  end;
end;

end.
