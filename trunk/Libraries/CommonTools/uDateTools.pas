//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uDateTools;

interface

uses
  Windows, Classes, DateUtils, SysUtils, comctrls;

type
  ICalendarWeek = Interface
  ['{F2B41A7E-3D55-4541-8C79-A79425DEEC78}']
    procedure SetWeek(AWeek: Byte);
    function GetWeek: Byte;
    procedure SetYear(AYear: Word);
    function GetYear: Word;

    procedure Verify;

    function ToString : String;
    function ToInteger: Integer;
    function WeekStart : TDateTime;
    function WeekEnd : TDateTime;
    property Week: Byte read GetWeek write SetWeek;
    property Year: Word read GetYear write SetYear;
    property AsString: String read ToString;
  End;

  TCalendarWeek = class(TInterfacedObject,ICalendarWeek)
  private
    FWeek : Byte;
    FYear : Word;
  public
    {$REGION 'Constructors'}
    constructor FromString(AString : String; AVerifyInput : Boolean = False);
    constructor FromInteger(AInteger: Integer; AVerifyInput : Boolean = False);
    constructor FromDateTime(ADateTime: TDateTime);
    constructor FromWeekYear(AWeek: Byte; AYear: Word; AVerifyInput : Boolean = False);
    {$ENDREGION}

    {$REGION 'ICalendarWeek'}
    procedure SetWeek(AWeek: Byte);
    function GetWeek: Byte;
    procedure SetYear(AYear: Word);
    function GetYear: Word;

    procedure Verify;

    function ToString : String;
    function ToInteger: Integer;
    function WeekStart : TDateTime;
    function WeekEnd : TDateTime;
    {$ENDREGION}
  end;

      {
  TCalendarWeek = record
  private
    FValid : Boolean;
    function FirstDayOfWeek : Byte;
  public
    Week : Byte;
    Year : Word;

    function WeekStart : TDateTime;
    function WeekEnd : TDateTime;

    //Typecasts
    class operator Implicit(ADate : TDateTime) : TCalendarWeek; overload;
    //class operator Implicit(ACalendarWeek : TCalendarWeek) : TCalendarWeek; overload;
    class operator Implicit(AInteger : Integer) : TCalendarWeek; overload;
    class operator Implicit(AString : String) : TCalendarWeek; overload;
    class operator Implicit(ACalendarWeek : TCalendarWeek) : String; overload;
    class operator Explicit(ACalendarWeek : TCalendarWeek) : String; overload;
    class operator Explicit(ACalendarWeek : TCalendarWeek) : Integer; overload;
    class operator Explicit(ACalendarWeek : TCalendarWeek) : TDateTime; overload;

    //+
    class operator Add(ACalendarWeek : TCalendarWeek; AWeeks : Integer) : TCalendarWeek;
    //class operator Inc(AWeeks : Integer) : TCalendarWeek;

    //-
    class operator Subtract(ACalendarWeek : TCalendarWeek; AWeeks : Integer) : TCalendarWeek;
    //class operator Dec(AWeeks : Integer) : TCalendarWeek;

    //=
    class operator Equal(ACalendarWeek1, ACalendarWeek2 : TCalendarWeek) : Boolean; overload;
    class operator Equal(ACalendarWeek : TCalendarWeek; ADate : TDateTime) : Boolean; overload;

    //< > <= >=
    class operator GreaterThan(ACalendarWeek1, ACalendarWeek2 : TCalendarWeek) : Boolean; overload;
    class operator GreaterThan(ACalendarWeek : TCalendarWeek; ADate : TDateTime) : Boolean; overload;
    class operator GreaterThanOrEqual(ACalendarWeek1, ACalendarWeek2 : TCalendarWeek) : Boolean; overload;
    class operator GreaterThanOrEqual(ACalendarWeek : TCalendarWeek; ADate : TDateTime) : Boolean; overload;
    class operator LessThan(ACalendarWeek1, ACalendarWeek2 : TCalendarWeek) : Boolean; overload;
    class operator LessThan(ACalendarWeek : TCalendarWeek; ADate : TDateTime) : Boolean; overload;
    class operator LessThanOrEqual(ACalendarWeek1, ACalendarWeek2 : TCalendarWeek) : Boolean; overload;
    class operator LessThanOrEqual(ACalendarWeek : TCalendarWeek; ADate : TDateTime) : Boolean; overload;
  end;  }

const
  CW_YEARLEN=4;
  CW_WEEKLEN=2;

implementation

{ TKalenderWoche }
  {
class operator TCalendarWeek.Implicit(ACalendarWeek : TCalendarWeek) : String; 
var
  Fmt : String;
begin
  Fmt:='%.'+InttoStr(CW_YEARLEN)+'d/%.'+InttoStr(CW_WEEKLEN)+'d';
  Result:=Format(FMT, [ACalendarWeek.Year, ACalendarWeek.Week]);
end;

class operator TCalendarWeek.Implicit(ADate: TDateTime): TCalendarWeek;
begin
  Result.Year:=YearOf(ADate);
  Result.Week:=WeekOf(ADate);
end;

class operator TCalendarWeek.Explicit(ACalendarWeek : TCalendarWeek) : TDateTime;
begin
  Result:=ACalendarWeek.WeekStart;
end;

class operator TCalendarWeek.Explicit(ACalendarWeek : TCalendarWeek) : String; 
begin
  Result:=ACalendarWeek;
end;

class operator TCalendarWeek.Explicit(ACalendarWeek : TCalendarWeek) : Integer;
var
  Fmt : String;
begin
  Fmt:='%.'+InttoStr(CW_YEARLEN)+'d%.'+InttoStr(CW_WEEKLEN)+'d';
  Result:=StrToInt(Format(FMT, [ACalendarWeek.Year, ACalendarWeek.Week]));
end;

class operator TCalendarWeek.Add(ACalendarWeek : TCalendarWeek; AWeeks : Integer): TCalendarWeek;
begin
  Result:=ACalendarWeek.WeekStart+(AWeeks*DaysPerWeek);
end;

class operator TCalendarWeek.Equal(ACalendarWeek: TCalendarWeek;
  ADate: TDateTime): Boolean;
begin
  Result:=(ADate>=ACalendarWeek.WeekStart) and
          (ADate<=ACalendarWeek.WeekEnd);
end;

class operator TCalendarWeek.Equal(ACalendarWeek1,
  ACalendarWeek2: TCalendarWeek): Boolean;
begin
  Result:=(ACalendarWeek1.Week=ACalendarWeek2.Week) and
          (ACalendarWeek1.Year=ACalendarWeek2.Year);
end;

function TCalendarWeek.FirstDayOfWeek: byte;
var
  Buffer : array[0..1] of Char;

begin
  if GetLocaleInfo(LOCALE_USER_DEFAULT,
                   LOCALE_IFIRSTDAYOFWEEK,
                   Buffer,
                   SizeOf(Buffer))=SizeOf(Buffer) then
    Result:=Ord(Buffer[0])-48+1
  else
    Result:=1;
end;

class operator TCalendarWeek.GreaterThan(ACalendarWeek: TCalendarWeek;
  ADate: TDateTime): Boolean;
begin
  Result:=ACalendarWeek.WeekStart>ADate;
end;

class operator TCalendarWeek.GreaterThanOrEqual(ACalendarWeek1,
  ACalendarWeek2: TCalendarWeek): Boolean;
begin
  Result:=(ACalendarWeek1=ACalendarWeek2) or
          (ACalendarWeek1>ACalendarWeek2);
end;

class operator TCalendarWeek.GreaterThanOrEqual(ACalendarWeek: TCalendarWeek;
  ADate: TDateTime): Boolean;
begin
  Result:=(ACalendarWeek=ADate) or
          (ACalendarWeek>ADate);
end;

class operator TCalendarWeek.GreaterThan(ACalendarWeek1,
  ACalendarWeek2: TCalendarWeek): Boolean;
begin
  Result:=(ACalendarWeek1.Year>ACalendarWeek2.Year) or
          ( (ACalendarWeek1.Year=ACalendarWeek2.Year) and
            (ACalendarWeek1.Week>ACalendarWeek2.Week)
          );
end;


class operator TCalendarWeek.LessThan(ACalendarWeek1,
  ACalendarWeek2: TCalendarWeek): Boolean;
begin
  Result:=(ACalendarWeek1.Year<ACalendarWeek2.Week) or
          ( (ACalendarWeek1.Year=ACalendarWeek2.Year) and
            (ACalendarWeek1.Week<ACalendarWeek2.Week)
          );
end;

class operator TCalendarWeek.LessThan(ACalendarWeek: TCalendarWeek;
  ADate: TDateTime): Boolean;
begin
  Result:=ACalendarWeek.WeekEnd<ADate;
end;

class operator TCalendarWeek.LessThanOrEqual(ACalendarWeek1,
  ACalendarWeek2: TCalendarWeek): Boolean;
begin
  Result:=(ACalendarWeek1=ACalendarWeek2) or
          (ACalendarWeek1<ACalendarWeek2);
end;

class operator TCalendarWeek.LessThanOrEqual(ACalendarWeek: TCalendarWeek;
  ADate: TDateTime): Boolean;
begin
  Result:=(ACalendarWeek=ADate) or
          (ACalendarWeek<ADate);
end;

class operator TCalendarWeek.Subtract(ACalendarWeek: TCalendarWeek;
  AWeeks: Integer): TCalendarWeek;
begin
  Result:=ACalendarWeek.WeekStart-(AWeeks*DaysPerWeek);
end;

function TCalendarWeek.WeekStart: TDateTime;
begin
  Result:=StartOfAWeek(Year, Week);
end;

function TCalendarWeek.WeekEnd: TDateTime;
begin
  Result:=EndOfAWeek(Year, Week);
end;

class operator TCalendarWeek.Implicit(AInteger: Integer): TCalendarWeek;
var
  PartYear : String[CW_YEARLEN];
  PartWeek : String[CW_WEEKLEN];
  IntVal : String[CW_YEARLEN+CW_WEEKLEN];
begin
  IntVal:=IntToStr(AInteger);

  PartYear:=Copy(IntVal, 1, CW_YEARLEN);
  PartWeek:=Copy(IntVal, CW_YEARLEN+1, CW_WEEKLEN);

  Result.Week:=StrToIntDef(PartWeek,1);
  Result.Year:=StrToIntDef(PartYear,1899);
end;

class operator TCalendarWeek.Implicit(AString : String) : TCalendarWeek;
var
  PartYear : String[CW_YEARLEN];
  PartWeek : String[CW_WEEKLEN];
begin
  PartYear:=Copy(AString, 1, CW_YEARLEN);
  PartWeek:=Copy(AString, Length(AString)-CW_WEEKLEN+1, CW_WEEKLEN);

  Result.Week:=StrToInt(PartWeek);
  Result.Year:=StrToInt(PartYear);
end;   }

{ TCalendarWeek }

constructor TCalendarWeek.FromDateTime(ADateTime: TDateTime);
begin
  inherited Create;
  FWeek:=WeekOfTheYear(ADateTime, FYear);
end;

constructor TCalendarWeek.FromInteger(AInteger: Integer; AVerifyInput : Boolean = False);
var
  PartYear : String[CW_YEARLEN];
  PartWeek : String[CW_WEEKLEN];
  IntVal : String[CW_YEARLEN+CW_WEEKLEN];
begin
  inherited Create;

  IntVal:=IntToStr(AInteger);

  PartYear:=Copy(IntVal, 1, CW_YEARLEN);
  PartWeek:=Copy(IntVal, CW_YEARLEN+1, CW_WEEKLEN);

  FWeek:=StrToIntDef(PartWeek,1);
  FYear:=StrToIntDef(PartYear,1899);

  if AVerifyInput then
    Verify;
end;

constructor TCalendarWeek.FromString(AString : String; AVerifyInput : Boolean = False);
var
  PartYear : String[CW_YEARLEN];
  PartWeek : String[CW_WEEKLEN];
begin
  inherited Create;

  PartYear:=Copy(AString, 1, CW_YEARLEN);
  PartWeek:=Copy(AString, Length(AString)-CW_WEEKLEN+1, CW_WEEKLEN);

  FWeek:=StrToInt(PartWeek);
  FYear:=StrToInt(PartYear);

  if AVerifyInput then
    Verify;
end;

constructor TCalendarWeek.FromWeekYear(AWeek: Byte; AYear: Word; AVerifyInput : Boolean = False);
begin
  inherited Create;
  FWeek:=AWeek;
  FYear:=AYear;

  if AVerifyInput then
    Verify;
end;

function TCalendarWeek.GetWeek: Byte;
begin
  Result:=FWeek;
end;

function TCalendarWeek.GetYear: Word;
begin
  Result:=FYear;
end;

procedure TCalendarWeek.SetWeek(AWeek: Byte);
begin
  FWeek:=AWeek;
end;

procedure TCalendarWeek.SetYear(AYear: Word);
begin
  FYear:=AYear;
end;

function TCalendarWeek.ToInteger: Integer;
var
  Fmt : String;
begin
  Fmt:='%.'+InttoStr(CW_YEARLEN)+'d%.'+InttoStr(CW_WEEKLEN)+'d';
  Result:=StrToInt(Format(FMT, [FYear, FWeek]));
end;

function TCalendarWeek.ToString: String;
var
  Fmt : String;
begin
  if FYear=0 then
    Result:=''
  else
  begin
    Fmt:='%.'+InttoStr(CW_YEARLEN)+'d/%.'+InttoStr(CW_WEEKLEN)+'d';
    Result:=Format(FMT, [FYear, FWeek]);
  end;
end;

procedure TCalendarWeek.Verify;
begin
  StartOfAWeek(FYear, FWeek);
end;

function TCalendarWeek.WeekEnd: TDateTime;
begin
  Result:=EndOfAWeek(FYear, FWeek);
end;

function TCalendarWeek.WeekStart: TDateTime;
begin
  Result:=StartOfAWeek(FYear, FWeek);
end;

end.
