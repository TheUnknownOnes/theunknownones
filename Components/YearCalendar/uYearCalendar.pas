unit uYearCalendar;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Controls, Forms,
  Dialogs, ExtCtrls, GDIPOBJ, GDIPAPI, dateutils, SysConst, Graphics;

type
  TYearCalenderDayFillStyle = (dfsFull, dfsLowerRight);

  TYearCalendarDay = class(TCollectionItem)
  private
    FDate: TDate;
    FColor: TColor;
    FText: String;
    FFillStyle: TYearCalenderDayFillStyle;
    FData: TObject;
  published
    property Date : TDate read FDate write FDate;
    property Color : TColor read FColor write FColor;
    property Text : String read FText write FText;
    property FillStyle : TYearCalenderDayFillStyle read FFillStyle write FFillStyle;
    property Data : TObject read FData write FData;

    constructor Create(Collection: TCollection); override;
  end;

  TYearCalendarDays = class(TCollection)
  public
    function Add: TYearCalendarDay;
    function QueryByMonth(AYear, AMonth: Integer): TYearCalendarDays;
    function QueryByDay(ADate : TDate): TYearCalendarDays;
  end;

  THoverDayEvent = procedure(Sender: TObject; ADay : TDate) of object;

  TYearCalendar = class(TCustomControl)
  private
    FGraphics : TGPGraphics;
    FClientRect : TRect;

    FBrushSolidBlack : TGPSolidBrush;
    FBrushSolidWhite : TGPSolidBrush;
    FBrushSolidVariable : TGPSolidBrush;
    FBrushSolidMonthTitle : TGPSolidBrush;
    FBrushSolidMonthBack : TGPSolidBrush;
    FBrushSolidWeekBack : TGPSolidBrush;

    FBrushTranslucentHoverDay : TGPSolidBrush;

    FPenLineSolidBlack : TGPPen;
    FPenLineSolidBlue  : TGPPen;
    FPenLineWeekdayUnderline : TGPPen;

    FFontDayTitle : TGPFont;
    FFontDay : TGPFont;
    FFontTxt : TGPFont;
    FFontMonthTitle : TGPFont;

    FYear : Integer;
    FSelectedDay : TDate;
    FHoveredDay : TDate;
    FDays: TYearCalendarDays;

    FOnHoverDay : THoverDayEvent;
    FHotTrack: Boolean;

    procedure SetYear(const Value: Integer);
    procedure SetDays(const Value: TYearCalendarDays);
  protected
    procedure InitGraph(ACanvas : HDC);
    procedure CloseGraph;
    procedure DrawMonth(AYear: Integer; AMonth: Word; ARect : TRect);
    procedure Draw(AYear: Integer; ACanvas : HDC; ARect : TRect);

    procedure DoHoverDay(ADay : TDate);

    procedure Paint; override;
    procedure Click; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    property SelectedDate : TDate read FSelectedDay;
    
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Year : Integer read FYear write SetYear;
    property Days : TYearCalendarDays read FDays write SetDays;
    property HotTrack : Boolean read FHotTrack write FHotTrack;

    property Align;
    property Anchors;

    property OnHoverDay : THoverDayEvent read FOnHoverDay write FOnHoverDay;
    property OnClick;
  end;

procedure Register;

implementation

uses Types, Math;

procedure Register;
begin
  RegisterComponents('TUO',[TYearCalendar]);
end;

{ TYearCalendar }

constructor TYearCalendar.Create(AOwner: TComponent);
begin
  inherited;
  FYear:=YearOf(now);
  FDays:=TYearCalendarDays.Create(TYearCalendarDay);
  Self.DoubleBuffered:=True;
end;

destructor TYearCalendar.Destroy;
begin
  FDays.Free;
  inherited;
end;

procedure TYearCalendar.DoHoverDay(ADay : TDate);
begin
  if Assigned(FOnHoverDay) then
    FOnHoverDay(self, ADay);
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
  SpaceDays = 700;
var
  HeightDays : Integer;
  TransformX,
  TransformY : Single;
  MousePosX,
  MousePosy  : Single;
  MousePos   : TPoint;
  idx : integer;
  day : TDateTime;
  WeekOMon : Word;
  DayOWeek : Word;
  FirstDayIs : Word;
  WeekOYear : Word;
  OldWeek : Word;
  DateTriangle : array[0..2] of TGPPointF;

  Dates : TYearCalendarDays;
  rgbval : Integer;

  function MouseInDay(x, y : single): Boolean;
  begin
    Result:=(MousePosX >= x) and (MousePosY >= y) and
            (MousePosX <= x + WidthDays) and
            (MousePosY <= y + HeightDays);
  end;

  function MyWeeksBetween(ANow, AThen : TDate): Word;
  var
    v1,
    v2 : Word;
  begin
    v1:=(1+WeeksBetween(ANow, AThen));
    v2:=(1+WeekOfTheYear(ANow)-WeekOfTheYear(AThen));
    if v1>6 then
      v1:=5;
    if v2>6 then
      v2:=5;
    Result:=Max(v1, v2);
  end;

begin
  HeightDays:=SpaceDays
                 div
              MyWeeksBetween(IncMonth(EncodeDate(AYear, AMonth, 1))-1,
                                 EncodeDate(AYear, AMonth, 1));
                                 
  Dates:=Self.FDays.QueryByMonth(AYear, AMonth);

  TransformX := (ARect.Right-ARect.Left) / 1000;
  TransformY := (ARect.Bottom-ARect.Top) / 1000;

  MousePosX := -1;
  MousePosY := -1;
  MousePos := ScreenToClient(Mouse.CursorPos);

  if (MousePos.X >= ARect.Left) and (MousePos.X <= ARect.Right) then
    MousePosX := (MousePos.X - ARect.Left) / TransformX;

  if (MousePos.Y >= ARect.Top) and (MousePos.Y <= ARect.Bottom) then
    MousePosY := ( MousePos.Y - ARect.Top) / TransformY;

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

    if MouseInDay((DayOWeek * WidthDays) + XIndentDays,
                  (WeekOMon * HeightDays) + YIndentDays) then
    begin
      FGraphics.FillRectangle(FBrushTranslucentHoverDay, (DayOWeek * WidthDays) + XIndentDays,
                  (WeekOMon * HeightDays) + YIndentDays, WidthDays, HeightDays);
      if FHoveredDay<>day then
      begin
        FHoveredDay:=day;
        DoHoverDay(FHoveredDay);
      end;
    end;

    for idx := 0 to Dates.Count - 1 do
    begin 
      rgbval:= ColorToRGB(TYearCalendarDay(Dates.Items[idx]).Color);
      FBrushSolidVariable.SetColor(MakeColor(GetBlue(rgbval),
                                             GetGreen(rgbval),
                                             GetRed(rgbval)));

      if SameDate(day,TYearCalendarDay(Dates.Items[idx]).Date) then
      begin
        case TYearCalendarDay(Dates.Items[idx]).FillStyle of
          dfsFull :
              FGraphics.FillRectangle(FBrushSolidVariable,
                                      (DayOWeek * WidthDays) + XIndentDays,
                                      (WeekOMon * HeightDays) + YIndentDays,
                                      WidthDays,
                                      HeightDays);
          dfsLowerRight :
              begin
                DateTriangle[0].X:=(DayOWeek * WidthDays) + XIndentDays + WidthDays;
                DateTriangle[0].Y:=(WeekOMon * HeightDays) + YIndentDays;
                DateTriangle[1].X:=(DayOWeek * WidthDays) + XIndentDays;
                DateTriangle[1].Y:=(WeekOMon * HeightDays) + YIndentDays + HeightDays;
                DateTriangle[2].X:=(DayOWeek * WidthDays) + XIndentDays + WidthDays;
                DateTriangle[2].Y:=(WeekOMon * HeightDays) + YIndentDays + HeightDays;

                FGraphics.FillPolygon(FBrushSolidVariable, PGPPointF(@DateTriangle[0]), 3);
              end;
        end;            

        FGraphics.DrawString(TYearCalendarDay(Dates.Items[idx]).Text,
                             Length(TYearCalendarDay(Dates.Items[idx]).Text),
                             FFontTxt,
                             GDIPAPI.MakePoint((DayOWeek * WidthDays) + XIndentDays + (WidthDays / 2),
                                               (WeekOMon * HeightDays) + YIndentDays + (HeightDays / 2)),
                                               FBrushSolidBlack);

        Dates.Delete(idx);
        break;
      end;
    end;

    if day=FSelectedDay then
      FGraphics.DrawRectangle(FPenLineSolidBlue, (DayOWeek * WidthDays) + XIndentDays,
                  (WeekOMon * HeightDays) + YIndentDays, WidthDays, HeightDays);

    FGraphics.DrawString(IntToStr(DayOfTheMonth(day)), Length(IntToStr(DayOfTheMonth(day))),
                         FFontDay,
                         GDIPAPI.MakePoint((DayOWeek * WidthDays) + XIndentDays,
                                           (WeekOMon * HeightDays) + YIndentDays),
                         FBrushSolidBlack);

    day:=IncDay(day);
  until MonthOf(day)<>AMonth;

  FGraphics.ResetTransform;
  Dates.Free;
end;

procedure TYearCalendar.InitGraph(ACanvas : HDC);
begin
  FGraphics:=TGPGraphics.Create(ACanvas);
  FGraphics.SetCompositingQuality(CompositingQualityHighSpeed);
  FGraphics.SetSmoothingMode(SmoothingModeHighSpeed);

  FBrushSolidBlack:=TGPSolidBrush.Create(MakeColor(0,0,0));
  FBrushSolidWhite:=TGPSolidBrush.Create(MakeColor(255,255,255));
  FBrushSolidVariable:=TGPSolidBrush.Create(MakeColor(0,0,0));
  FBrushSolidMonthTitle:=TGPSolidBrush.Create(MakeColor(128,128,128));
  FBrushSolidMonthBack:=TGPSolidBrush.Create(MakeColor(255,255,255));
  FBrushSolidWeekBack:=TGPSolidBrush.Create(MakeColor(250, 250, 250));

  FBrushTranslucentHoverDay:=TGPSolidBrush.Create(MakeColor(128, 200, 200, 255));

  FPenLineSolidBlack:=TGPPen.Create(FBrushSolidBlack, 1);
  FPenLineSolidBlue:=TGPPen.Create(MakeColor(0,0,255));
  FPenLineWeekdayUnderline:=TGPPen.Create(MakeColor(250,250,250));

  FFontDayTitle:=TGPFont.Create('Tahoma', 40, FontStyleBold);
  FFontTxt:=TGPFont.Create('Tahoma', 30, FontStyleItalic);
  FFontDay:=TGPFont.Create('Tahoma', 40, FontStyleRegular);
  FFontMonthTitle:=TGPFont.Create('Tahoma', 80, FontStyleBold);
end;

procedure TYearCalendar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FHotTrack then
    Invalidate;
end;

procedure TYearCalendar.Paint;
begin
  inherited;
  Draw(FYear, Self.Canvas.Handle, self.ClientRect);
end;

procedure TYearCalendar.SetDays(const Value: TYearCalendarDays);
begin
  FDays.Assign(Value);
end;

procedure TYearCalendar.SetYear(const Value: Integer);
begin
  if Value=0 then
    FYear:=YearOf(now)
  else
    FYear := Value;
  Invalidate;
end;

procedure TYearCalendar.Click;
begin
  Invalidate;
  FSelectedDay:=FHoveredDay;

  if not Focused then
    Windows.SetFocus(Handle);

  inherited;

  Application.ProcessMessages;

  Invalidate;
end;

procedure TYearCalendar.CloseGraph;
begin
  FFontDay.Free;
  FFontDayTitle.Free;
  FFontTxt.Free;
  FFontMonthTitle.Free;
  FBrushSolidVariable.Free;
  FBrushSolidMonthTitle.Free;
  FBrushSolidWeekBack.Free;
  FBrushSolidMonthBack.Free;
  FBrushSolidWhite.Free;
  FBrushTranslucentHoverDay.Free;
  FBrushSolidBlack.Free;
  FPenLineSolidBlack.Free;
  FPenLineSolidBlue.Free;
  FPenLineWeekdayUnderline.Free;
  FGraphics.Free;
end;

{ TYearCalendarDays }

function TYearCalendarDays.Add: TYearCalendarDay;
begin
  Result:=TYearCalendarDay(inherited Add);
end;

function TYearCalendarDays.QueryByDay(ADate: TDate): TYearCalendarDays;
var
  idx : Integer;
begin
  Result:=TYearCalendarDays.Create(ItemClass);

  for idx := 0 to self.Count - 1 do
    if IsSameDay(TYearCalendarDay(self.Items[idx]).Date, ADate) then
      with result.Add do
      begin
        Date:=TYearCalendarDay(self.Items[idx]).Date;
        Color:=TYearCalendarDay(self.Items[idx]).Color;
        Text:=TYearCalendarDay(self.Items[idx]).Text;
        FillStyle:=TYearCalendarDay(self.Items[idx]).FillStyle;
        Data:=TYearCalendarDay(self.Items[idx]).Data;
      end;
end;

function TYearCalendarDays.QueryByMonth(AYear,
  AMonth: Integer): TYearCalendarDays;
var
  idx : Integer;
begin
  Result:=TYearCalendarDays.Create(ItemClass);

  for idx := 0 to self.Count - 1 do
    if (YearOf(TYearCalendarDay(self.Items[idx]).Date)=AYear) and
       (MonthOf(TYearCalendarDay(self.Items[idx]).Date)=AMonth) then
      with result.Add do
      begin
        Date:=TYearCalendarDay(self.Items[idx]).Date;
        Color:=TYearCalendarDay(self.Items[idx]).Color;
        Text:=TYearCalendarDay(self.Items[idx]).Text;
        FillStyle:=TYearCalendarDay(self.Items[idx]).FillStyle;
        Data:=TYearCalendarDay(self.Items[idx]).Data;
      end;
end;

{ TYearCalendarDay }

constructor TYearCalendarDay.Create(Collection: TCollection);
begin
  inherited;
  FColor:=clLime;
  FText:='';
end;

end.
