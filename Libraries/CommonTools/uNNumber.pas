//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uNNumber;

interface

uses
  SysUtils, StrUtils, Math;

type
  TnnDigits = array of Char;
  
  TNNumber = record
  private
    FBase : Byte;
    FDigits : TnnDigits;
      { - holds all allowed digits of the used number system
        - use "Digits" or "SetSystemXXX" to initialize it }

    procedure SetDigits(ADigits : TnnDigits);
    function GetDigits: TnnDigits;

    function GetValueOfDigit(ADigit : Char) : Byte;
    procedure FromString(AString : String);

    procedure SystemNeeded;
  public
    Value : Int64;
      { - represents the current value of the number }

    property Digits : TnnDigits read GetDigits write SetDigits;
      { - get/set the digits which are valid in this number system }

    procedure SetSystemBin;
      { - activates the binary system }
    procedure SetSystemOct;
      { - activates the octal system }
    procedure SetSystemDec;
      { - activates the decimal system }
    procedure SetSystemHex;
      { - activates the hexadecimal system }

    procedure SetStdDigits(ABase : Byte);
      { - sets the standard digits 0..9 A..Z a..z}

    function ToString(ALen : Word = 0) : String;
      { - converts the current value to a string of the current system }

    class operator Implicit(ANumber : TNNumber) : String;
    class operator Implicit(AString : String) : TNNumber;
    class operator Implicit(AInt : Int64) : TNNumber;

    class operator Equal(ANumber1, ANumber2 : TNNumber) : Boolean;
    class operator Equal(ANumber : TNNumber; AInt : Int64) : Boolean;

    class operator NotEqual(ANumber1, ANumber2 : TNNumber) : Boolean;
    class operator NotEqual(ANumber : TNNumber; AInt : Int64) : Boolean;

    class operator GreaterThan(ANumber1, ANumber2 : TNNumber) : Boolean;
    class operator GreaterThan(ANumber : TNNumber; AInt : Int64) : Boolean;

    class operator LessThan(ANumber1, ANumber2 : TNNumber) : Boolean;
    class operator LessThan(ANumber : TNNumber; AInt : Int64) : Boolean;

    class operator GreaterThanOrEqual(ANumber1, ANumber2 : TNNumber) : Boolean;
    class operator GreaterThanOrEqual(ANumber : TNNumber; AInt : Int64) : Boolean;

    class operator LessThanOrEqual(ANumber1, ANumber2 : TNNumber) : Boolean;
    class operator LessThanOrEqual(ANumber : TNNumber; AInt : Int64) : Boolean;

    class operator Add(ANumber1, ANumber2 : TNNumber) : TNNumber;
    class operator Add(ANumber : TNNumber; AInt : Int64) : TNNumber;

    class operator Subtract(ANumber1, ANumber2 : TNNumber) : TNNumber;
    class operator Subtract(ANumber : TNNumber; AInt : Int64) : TNNumber;
  end;

implementation

{ TNNumber }

class operator TNNumber.Equal(ANumber1, ANumber2: TNNumber): Boolean;
begin
  Result:=ANumber1.Value=ANumber2.Value;
end;

class operator TNNumber.Add(ANumber1, ANumber2: TNNumber): TNNumber;
begin
  Result.Value:=ANumber1.Value+ANumber2.Value;
  Result.Digits:=ANumber1.Digits;
end;

class operator TNNumber.Add(ANumber: TNNumber; AInt: Int64): TNNumber;
begin
  Result.Value:=ANumber.Value+AInt;
  Result.Digits:=ANumber.Digits;
end;

class operator TNNumber.Equal(ANumber: TNNumber; AInt: Int64): Boolean;
begin
  Result:=ANumber.Value=AInt;
end;

procedure TNNumber.FromString(AString: String);
var
  idx : Integer;
  NewValue : Int64;
  NegativeValue : Boolean;
  StringLen : Integer;
begin
  SystemNeeded;

  NewValue:=0;

  NegativeValue:=AString[1]='-';
  if NegativeValue then
    Delete(AString,1,1);

  StringLen:=Length(AString);

  for idx:=1 to StringLen do
    Inc(NewValue, GetValueOfDigit(AString[idx])*Round(IntPower(FBase, StringLen-idx)));

  if NegativeValue then
    Value:=NewValue*-1
  else
    Value:=NewValue;
end;

function TNNumber.GetDigits: TnnDigits;
begin
  Result:=FDigits;
end;

function TNNumber.GetValueOfDigit(ADigit: Char): Byte;
var
  idx : Byte;
  Found : Boolean;
begin
  SystemNeeded;

  Found:=false;
  Result:=0;

  for idx:=0 to Length(FDigits) do
  begin
    if FDigits[idx]=ADigit then
    begin
      Result:=idx;
      Found:=true;
      break;
    end;
  end;

  if not Found then
    raise Exception.Create('Invalid digit (Not found in this system)');
end;

class operator TNNumber.GreaterThan(ANumber1, ANumber2: TNNumber): Boolean;
begin
  Result:=ANumber1.Value>ANumber2.Value;
end;

class operator TNNumber.GreaterThan(ANumber: TNNumber; AInt: Int64): Boolean;
begin
  Result:=ANumber.Value>AInt;
end;

class operator TNNumber.GreaterThanOrEqual(ANumber1,
  ANumber2: TNNumber): Boolean;
begin
  Result:=(ANumber1>ANumber2) or
          (ANumber1=ANumber2);
end;

class operator TNNumber.GreaterThanOrEqual(ANumber: TNNumber;
  AInt: Int64): Boolean;
begin
  Result:=(ANumber>AInt) or
          (ANumber=AInt);
end;

class operator TNNumber.Implicit(AString: String): TNNumber;
begin
  Result.FromString(AString);
end;

class operator TNNumber.Implicit(ANumber: TNNumber): String;
begin
  Result:=ANumber.ToString;
end;

procedure TNNumber.SetDigits(ADigits: TnnDigits);
begin
  FDigits:=ADigits;
  FBase:=Length(FDigits);
end;

procedure TNNumber.SetStdDigits(ABase: Byte);
var
  idx : Integer;
  ValueBase : Byte;
  Dig : TnnDigits;
begin
  SetLength(Dig, ABase);
  FBase:=ABase;

  for idx:=0 to ABase do
  begin
    case idx of
      0:  ValueBase:=Ord('0');
      10: ValueBase:=Ord('A')-idx;
      36: ValueBase:=Ord('a')-idx;
    end;

    Dig[idx]:=Chr(ValueBase+idx);
  end;

  Digits:=Dig;
end;

procedure TNNumber.SetSystemBin;
begin
  SetStdDigits(2);
end;

procedure TNNumber.SetSystemDec;
begin
  SetStdDigits(10);
end;

procedure TNNumber.SetSystemHex;
begin
  SetStdDigits(16);
end;

procedure TNNumber.SetSystemOct;
begin
  SetStdDigits(8);
end;

class operator TNNumber.Subtract(ANumber1, ANumber2: TNNumber): TNNumber;
begin
  Result:=ANumber1.Value-ANumber2.Value;
  Result.Digits:=ANumber1.Digits;
end;

class operator TNNumber.Subtract(ANumber: TNNumber; AInt: Int64): TNNumber;
begin
  Result:=ANumber.Value-AInt;
  Result.Digits:=ANumber.Digits;
end;

procedure TNNumber.SystemNeeded;
begin
  if Length(FDigits)=0  then
    raise Exception.Create('No number system defined!');
end;

function TNNumber.ToString(ALen : Word): String;
var
  CurValue : Int64;
  Rest : Byte;
begin
  SystemNeeded;

  CurValue:=Abs(Value);

  Result:='';

  repeat
    Rest:=CurValue mod FBase;

    Result:=FDigits[Rest]+Result;

    CurValue:=CurValue div FBase;
  until CurValue=0;

  if ALen>0 then
    Result:=DupeString(FDigits[0], ALen-Length(Result))+Result;

  if Value<0 then
    Result:='-'+Result;
end;

class operator TNNumber.Implicit(AInt: Int64): TNNumber;
begin
  Result.Value:=AInt;
end;

class operator TNNumber.LessThan(ANumber1, ANumber2: TNNumber): Boolean;
begin
  Result:=ANumber1.Value<ANumber2.Value;
end;

class operator TNNumber.LessThan(ANumber: TNNumber; AInt: Int64): Boolean;
begin
  Result:=ANumber.Value<AInt;
end;

class operator TNNumber.LessThanOrEqual(ANumber1, ANumber2: TNNumber): Boolean;
begin
  Result:=(ANumber1<ANumber2) or
          (ANumber1=ANumber2);
end;

class operator TNNumber.LessThanOrEqual(ANumber: TNNumber;
  AInt: Int64): Boolean;
begin
  Result:=(ANumber<AInt) or
          (ANumber=AInt);
end;

class operator TNNumber.NotEqual(ANumber1, ANumber2: TNNumber): Boolean;
begin
  Result:=ANumber1.Value<>ANumber2.Value;
end;

class operator TNNumber.NotEqual(ANumber: TNNumber; AInt: Int64): Boolean;
begin
  Result:=ANumber.Value<>AInt;
end;

end.
