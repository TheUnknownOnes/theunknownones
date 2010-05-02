unit uForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, uLibVLC;

type
  TForm5 = class(TForm)
    Panel1: TPanel;
    btn_OpenFile: TButton;
    pan_Video: TPanel;
    dlg_Video: TOpenDialog;
    btn_Play: TButton;
    btn_Pause: TButton;
    btn_Stop: TButton;
    procedure btn_OpenFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btn_PlayClick(Sender: TObject);
    procedure btn_PauseClick(Sender: TObject);
    procedure btn_StopClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FLib : ILibVLC;
    FLibInstance : Plibvlc_instance_t;
    FPlayer : Plibvlc_media_player_t;
    FMedia : Plibvlc_media_t;
    FException : libvlc_exception_t;

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
  if FLib.libvlc_media_player_is_playing(FPlayer, @FException) <> 0 then
    FLib.libvlc_media_player_stop(FPlayer, @FException);

  if dlg_Video.Execute then
  begin
    if Assigned(FMedia) then
      FLib.libvlc_media_release(FMedia);
    FMedia := nil;

    FMedia := FLib.libvlc_media_new(FLibInstance, PAnsiChar(AnsiString(dlg_Video.FileName)), @FException);
    CheckVLCException;

    FLib.libvlc_media_player_set_media(FPlayer, FMedia, @FException);
    CheckVLCException;

    btn_Play.Click;
  end;
end;

procedure TForm5.btn_PauseClick(Sender: TObject);
begin
  FLib.libvlc_media_player_pause(FPlayer, @FException);
  CheckVLCException;
end;

procedure TForm5.btn_PlayClick(Sender: TObject);
begin
  FLib.libvlc_media_player_play(FPlayer, @FException);
  CheckVLCException;
end;

procedure TForm5.btn_StopClick(Sender: TObject);
begin
  FLib.libvlc_media_player_stop(FPlayer, @FException);
  CheckVLCException;
end;

procedure TForm5.CheckVLCException;
begin
  if FLib.libvlc_exception_raised(@FException) <> 0 then
  begin
    try
      raise Exception.Create(FLib.libvlc_exception_get_message(@FException));
    finally
      FLib.libvlc_exception_clear(@FException);
    end;
  end;
end;

procedure TForm5.FormCreate(Sender: TObject);
var
  Params : array[0..1] of PAnsiChar;
begin
  Params[0] := PAnsiChar('--plugin-path=./plugins/');
  Params[1] := PAnsiChar('--ignore-config');

  FLib := LoadLibVLC();

  FLibInstance := FLib.libvlc_new(Length(Params), @Params[0], @FException);
  CheckVLCException;

  FPlayer := FLib.libvlc_media_player_new(FLibInstance, @FException);
  CheckVLCException;

  FLib.libvlc_media_player_set_hwnd(FPlayer, Pointer(pan_Video.Handle), @FException);
  CheckVLCException;
end;

procedure TForm5.FormDestroy(Sender: TObject);
begin
  if FLib.libvlc_media_player_is_playing(FPlayer, @FException) <> 0 then
    FLib.libvlc_media_player_stop(FPlayer, @FException);

  if Assigned(FMedia) then
    FLib.libvlc_media_release(FMedia);

  FLib.libvlc_media_player_release(FPlayer);
  FLib.libvlc_release(FLibInstance);
end;

end.
