//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uLogForDebug;

interface

uses
  Classes, Messages, Windows, SysUtils, WM_Helper;

procedure Log(AText : String; AFile : String);

procedure LogRect(APrefix : String; ARect : TRect; AFile : String);
procedure LogPoint(APrefix : String; APoint : TPoint; AFile : String);

procedure LogMessage(AMessage : TMessage; AFile : String); overload;
procedure LogMessage(AMessage : TWMWindowPosChanging; AFile : String); overload;
procedure LogMessage(AMessage : TWMSize; AFile : String); overload;

implementation

procedure LogPoint(APrefix : String; APoint : TPoint; AFile : String);
begin
  Log(Format('%s X:%d Y:%d',[APrefix, Apoint.X, Apoint.Y]), AFile);
end;

procedure LogRect(APrefix : String; ARect : TRect; AFile : String);
begin
  Log(Format('%s Top:%d Left:%d Right:%d Bottom:%d',[APrefix, ARect.Top, ARect.Left, ARect.Right, Arect.Bottom]), AFile);
end;

procedure Log(AText : String; AFile : String);
var
  sl : TStrings;
begin
  sl:=TStringList.Create;
  try
    if FileExists(AFile) then
      sl.LoadFromFile(AFile);
    sl.Add(TimeToStr(Now)+': '+AText);
    sl.SaveToFile(AFile);
  finally
    sl.Free;
  end;
end;

procedure LogMessage(AMessage : TMessage; AFile : String);
begin
  Log(Format('%s (%d , %d)',[TranslateWM(AMessage.Msg),AMessage.WParam,AMessage.LParam]), AFile);
end;

procedure LogMessage(AMessage : TWMWindowPosChanging; AFile : String); overload;
begin
  Log(Format('%s (Top: %d Left: %d Width: %d Height: %d)',[TranslateWM(AMessage.Msg),
                                                             AMessage.WindowPos.x,
                                                             AMessage.WindowPos.y,
                                                             AMessage.WindowPos.cx,
                                                             AMessage.WindowPos.cy]), AFile);
end;

procedure LogMessage(AMessage : TWMSize; AFile : String);
begin
  Log(Format('%s (%d , %d)',[TranslateWM(AMessage.Msg),AMessage.Width,AMessage.Height]), AFile);
end;

end.
