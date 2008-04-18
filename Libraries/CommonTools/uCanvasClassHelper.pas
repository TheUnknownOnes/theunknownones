//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uCanvasClassHelper;

interface

uses
  Graphics, Types;

type
  TCanvasClassHelper = class Helper for TCanvas
  public
    procedure TextOutW(X, Y: Integer; const Text: widestring);
    procedure TextRectW(Rect: TRect; X, Y: Integer; const Text: widestring);
  end;

implementation

uses
  Windows;

procedure TCanvasClassHelper.TextRectW(Rect: TRect; X, Y: Integer; const Text: widestring);
var
  Options: Longint;
begin
  Changing;
  RequiredState([csHandleValid, csFontValid, csBrushValid]);
  Options := ETO_CLIPPED or self.TextFlags;
  if Brush.Style <> bsClear then
    Options := Options or ETO_OPAQUE;
  if ((TextFlags and ETO_RTLREADING) <> 0) and
     (CanvasOrientation = coRightToLeft) then Inc(X, TextWidth(Text) + 1);
  Windows.ExtTextOutW(Handle, X, Y, Options, @Rect, PWideChar(Text),  Length(Text), nil);
  Changed;
end;

procedure TCanvasClassHelper.TextOutW(X, Y: Integer; const Text: widestring);
begin
  Changing;
  RequiredState([csHandleValid, csFontValid, csBrushValid]);
  if CanvasOrientation = coRightToLeft then Inc(X, TextWidth(Text) + 1);
  Windows.ExtTextOutW(Handle, X, Y, TextFlags, nil, PWideChar(Text),
   Length(Text), nil);
  MoveTo(X + TextWidth(Text), Y);
  Changed;
end;

end.
