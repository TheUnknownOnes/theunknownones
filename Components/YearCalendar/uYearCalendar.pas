unit uYearCalendar;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, GDIPOBJ, GDIPAPI, dateutils, SysConst;

type
  TYearCalendar = class(TCustomControl)
  private
    FGraphics : TGPGraphics;
    FClientRect : TRect;

    FBrushSolidBlack : TGPSolidBrush;
    FBrushSolidWhite : TGPSolidBrush;
    FBrushSolidMonthTitle : TGPSolidBrush;
    FBrushSolidMonthBack : TGPSolidBrush;
    FBrushSolidWeekBack : TGPSolidBrush;

    FPenLineSolidBlack : TGPPen;
    FPenLineWeekdayUnderline : TGPPen;

    FFontDayTitle : TGPFont;
    FFontDay : TGPFont;
    FFontMonthTitle : TGPFont;

    FYear : Integer;
    procedure SetYear(const Value: Integer);
  protected
    procedure InitGraph(ACanvas : HDC);
    procedure CloseGraph;
    procedure DrawMonth(AYear: Integer; AMonth: Word; ARect : TRect);
    procedure Draw(AYear: Integer; ACanvas : HDC; ARect : TRect);

    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  published
    property Year : Integer read FYear write SetYear;

    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TUO',[TYearCalendar]);
end;

{ TYearCalendar }

constructor TYearCalendar.Create(AOwner: TComponent);
begin
  inherited;
  FYear:=YearOf(now);
  Self.DoubleBuffered:=True;
end;

procedure TYearCalendar.Draw(AYear: Integer; ACanvas : HDC; ARect : TRect);
const
  ROWS = 3;
  COLS = 4;
var
  rect : TRect;
  row : integer;
  idx : integer;
  column : integer;
begin
  FClientRect:=ARect;
  InitGraph(ACanvas);

  idx:=1;
  for row := 0 to ROWS-1 do
    for column := 0 to COLS-1 do
    begin
      rect.Top:=row * ((ARect.Bottom-ARect.Top) div ROWS);
      rect.Left:=column * ((ARect.Right-ARect.Left) div COLS);
      rect.Bottom:=rect.Top + ((ARect.Bottom-ARect.Top) div ROWS);
      rect.Right:=rect.Left + ((ARect.Right-ARect.Left) div COLS);
      DrawMonth(AYear, idx, rect);
      inc(idx);
    end;
  CloseGraph;
end;

procedure TYearCalendar.DrawMonth(AYear: Integer; AMonth: Word; ARect : TRect);
const
  Monatsnamen : array [1..12] of string = (SLongMonthNameJan, SLongMonthNameFeb,
                    SLongMonthNameMar, SLongMonthNameApr, SLongMonthNameMay,
                    SLongMonthNameJun, SLongMonthNameJul, SLongMonthNameAug,
                    SLongMonthNameSep, SLongMonthNameOct, SLongMonthNameNov,
                    SLongMonthNameDec);
  Wochennamen : array [0..6] of string = (SShortDayNameMon, SShortDayNameTue,
                    SShortDayNameWed, SShortDayNameThu, SShortDayNameFri,
                    SShortDayNameSat, SShortDayNameSun);
  XIndentDays : Single = 150;
  YIndentDays : Single = 300;
  WidthDays : Single = 120;
  HeightDays : Single = 140;
var
  TransformX,
  TransformY : single;
  idx : integer;
  day : TDateTime;
  WeekOMon : Word;
  DayOWeek : Word;
  FirstDayIs : Word;
  WeekOYear : Word;
  OldWeek : Word;
begin
  TransformX:=(ARect.Right-ARect.Left) / 1000;
  TransformY:=(ARect.Bottom-ARect.Top) / 1000;

  FGraphics.ScaleTransform(TransformX, TransformY);
  FGraphics.TranslateTransform(ARect.Left, ARect.Top, MatrixOrderAppend);

  FGraphics.FillRectangle(FBrushSolidMonthTitle, 0, 0, 1000, 143);
  FGraphics.FillRectangle(FBrushSolidMonthBack, 0, 143, 1000, 1000);
  FGraphics.FillRectangle(FBrushSolidWeekBack, 0, 143, 125, 1000);
  FGraphics.DrawLine(FPenLineWeekdayUnderline, 150, 286, 1000, 286);

  FGraphics.DrawString(Monatsnamen[AMonth], Length(Monatsnamen[AMonth]),
                       FFontMonthTitle, GDIPAPI.MakePoint(0.1,0), FBrushSolidWhite);

  for idx := 0 to 6 do
  begin
    FGraphics.DrawString(Wochennamen[idx], Length(Wochennamen[idx]),
                         FFontDayTitle, GDIPAPI.MakePoint((idx * WidthDays) + XIndentDays, 190),
                         FBrushSolidBlack);
  end;

  day:=EncodeDateTime(AYear, AMonth, 1, 0, 0, 0, 0);
  FirstDayIs:=DayOfTheWeek(day)-1;
  OldWeek:=0;
  repeat
    WeekOMon:=NthDayOfWeek (day)-1;
    DayOWeek:=DayOfTheWeek(day)-1;
    if DayOWeek<FirstDayIs then
      Inc(WeekOMon);

    WeekOYear:=WeekOf(day);
    if WeekOYear <> OldWeek then
    begin
      FGraphics.DrawString(IntToStr(WeekOYear), Length(IntToStr(WeekOYear)),
                           FFontDayTitle,
                           GDIPAPI.MakePoint(20,(WeekOMon * HeightDays) + YIndentDays),
                           FBrushSolidBlack);
      OldWeek:=WeekOYear;
    end;

    FGraphics.DrawString(IntToStr(DayOfTheMonth(day)), Length(IntToStr(DayOfTheMonth(day))),
                         FFontDay,
                         GDIPAPI.MakePoint((DayOWeek * WidthDays) + XIndentDays,
                                           (WeekOMon * HeightDays) + YIndentDays),
                         FBrushSolidBlack);

    day:=IncDay(day);
  until MonthOf(day)<>AMonth;


  FGraphics.ResetTransform;
end;

procedure TYearCalendar.InitGraph(ACanvas : HDC);
begin
  FGraphics:=TGPGraphics.Create(ACanvas);
  FGraphics.SetSmoothingMode(CompositingQualityHighQuality);

  FBrushSolidBlack:=TGPSolidBrush.Create(MakeColor(0,0,0));
  FBrushSolidWhite:=TGPSolidBrush.Create(MakeColor(255,255,255));
  FBrushSolidMonthTitle:=TGPSolidBrush.Create(MakeColor(128,128,128));
  FBrushSolidMonthBack:=TGPSolidBrush.Create(MakeColor(255,255,255));
  FBrushSolidWeekBack:=TGPSolidBrush.Create(MakeColor(250, 250, 250));
  FPenLineSolidBlack:=TGPPen.Create(FBrushSolidBlack, 1);
  FPenLineWeekdayUnderline:=TGPPen.Create(MakeColor(250,250,250));

  FFontDayTitle:=TGPFont.Create('Tahoma', 40, FontStyleBold);
  FFontDay:=TGPFont.Create('Tahoma', 40, FontStyleRegular);
  FFontMonthTitle:=TGPFont.Create('Tahoma', 80, FontStyleBold);
end;

procedure TYearCalendar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  Invalidate;
end;

procedure TYearCalendar.Paint;
begin
  inherited;
  Draw(FYear, Self.Canvas.Handle, self.ClientRect);
end;

procedure TYearCalendar.SetYear(const Value: Integer);
begin
  if Value=0 then
    FYear:=YearOf(now)
  else
    FYear := Value;
  Invalidate;
end;

procedure TYearCalendar.CloseGraph;
begin
  FFontDay.Free;
  FFontDayTitle.Free;
  FFontMonthTitle.Free;
  FBrushSolidMonthTitle.Free;
  FBrushSolidWeekBack.Free;
  FBrushSolidMonthBack.Free;
  FBrushSolidWhite.Free;
  FBrushSolidBlack.Free;
  FPenLineSolidBlack.Free;
  FPenLineWeekdayUnderline.Free;
  FGraphics.Free;
end;

end.
