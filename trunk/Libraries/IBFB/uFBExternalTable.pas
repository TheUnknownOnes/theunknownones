//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************


{Warning: This code doesnt work fully correct.
 If you wanna use it, only uses the varchar/char routines.
 Other routines may write an unreadable file due to missing
 knowledge about the correct way to pad 0-bytes.
}

unit uFBExternalTable;

interface

uses
  Classes, SysUtils, DateUtils, Windows;

type
  TfbetRecord = class
  protected
    FBufferSize : Cardinal;
    FBuffer : PByte;
    FPos : PByte;

    function GetPosition: Cardinal;
    procedure CheckPos(ANextSize : Cardinal; AIsChar : Boolean = false);

    procedure GetFBTimestamp(ADateTime : TDatetime; out ADate : Integer; out ATime : Cardinal);
  public
    constructor Create(ABufferSize : Cardinal); reintroduce;
    destructor Destroy; override;

    procedure Reset;

    procedure WriteSmallint(AValue : Smallint);
    procedure WriteInteger(AValue : Integer);
    procedure WriteBigint(AValue : Int64);
    procedure WriteChar(AValue : AnsiString; AFieldSize : Integer);
    procedure WriteVarchar(AValue : AnsiString; AFieldSize : Integer);
    procedure WriteDate(AValue : TDate);
    procedure WriteTime(AValue : TTime);
    procedure WriteTimestamp(AValue : TDateTime);
    procedure WriteFloat(AValue : Single);
    procedure WriteDouble(AValue : Double);

    property Position : Cardinal read GetPosition;
    property Data : PByte read FBuffer;
    property DataSize : Cardinal read GetPosition;
  end;

implementation

{ TfbetRecord }

procedure TfbetRecord.CheckPos(ANextSize: Cardinal; AIsChar: Boolean);
//This funtion is a copy from Henrique Netzka (http://www.ibphoenix.com/main.nfs?a=ibphoenix&s=1191401661:63&page=ibp_native_external)
var
  LPos : Cardinal;
begin
  LPos := Position;

  if (LPos and 3) <> 0 then
  begin
    if ((LPos + ANextSize) and 3) <> 0 then
    begin
      if ANextSize >= 4 then
      begin
        if (not AIsChar) and ((ANextSize = 4) or ((ANextSize and 7) = 0)) then
        begin
          while (LPos and 3) <> 0 do
          begin
            Inc(FPos);
            Inc(LPos);
          end;
        end
        else
        begin
          while (LPos and 1) <> 0 do
          begin
            Inc(FPos);
            Inc(LPos);
          end;
        end;
      end;
    end;
  end;

end;

constructor TfbetRecord.Create(ABufferSize: Cardinal);
begin
  FBufferSize := ABufferSize;
  GetMem(FBuffer, FBufferSize);
  Reset;
end;

destructor TfbetRecord.Destroy;
begin
  FreeMem(FBuffer, FBufferSize);
  inherited;
end;

procedure TfbetRecord.GetFBTimestamp(ADateTime: TDatetime; out ADate : Integer;
  out ATime: Cardinal);
const
  BaseDate = -15018; //17.11.1858
begin
  ADate := Trunc(ADateTime - BaseDate);
  ATime := MilliSecondOfTheDay(ADateTime) * 10;
end;

function TfbetRecord.GetPosition: Cardinal;
begin
  Result := Cardinal(FPos) - Cardinal(FBuffer);
end;

procedure TfbetRecord.Reset;
begin
  FillMemory(FBuffer, FBufferSize, 0);
  FPos := FBuffer;
end;

procedure TfbetRecord.WriteBigInt(AValue: Int64);
begin
  CheckPos(8);
  CopyMemory(FPos, @AValue, 8);
  Inc(FPos, 8);
end;

procedure TfbetRecord.WriteChar(AValue: AnsiString; AFieldSize : Integer);
var
  l : Integer;
begin
  l := Length(AValue);

  if AFieldSize < l then
  begin
    l := AFieldSize;
    SetLength(AValue, AFieldSize)
  end;

  CheckPos(l, true);
  FillChar(FPos^, AFieldSize, #32);
  CopyMemory(FPos, @AValue[1], l);
  Inc(FPos, AFieldSize);
end;

procedure TfbetRecord.WriteDate(AValue: TDate);
var
  tsd : Integer;
  tst : Cardinal;
begin
  GetFBTimestamp(AValue, tsd, tst);
  WriteInteger(tsd);
end;

procedure TfbetRecord.WriteDouble(AValue: Double);
begin
  CheckPos(8);
  CopyMemory(FPos, @AValue, 8);
  Inc(FPos, 8)
end;

procedure TfbetRecord.WriteFloat(AValue: Single);
var
  buffer : array[0..1] of Cardinal;
begin
  CheckPos(8);
  FillMemory(@buffer, 8, 0);
  CopyMemory(@buffer[0], @AValue, 4);
  CopyMemory(FPos, @buffer, 8);
  Inc(FPos, 8)
end;

procedure TfbetRecord.WriteInteger(AValue: Integer);
begin
  CheckPos(4);

  CopyMemory(FPos, @AValue, 4);
  Inc(FPos, 4);
end;

procedure TfbetRecord.WriteSmallInt(AValue: Smallint);
begin
  CheckPos(2);
  CopyMemory(FPos, @AValue, 2);
  Inc(FPos, 2);
end;

procedure TfbetRecord.WriteTime(AValue: TTime);
var
  tsd : Integer;
  tst : Cardinal;
begin
  GetFBTimestamp(AValue, tsd, tst);
  WriteInteger(tst);
end;

procedure TfbetRecord.WriteTimestamp(AValue: TDateTime);
var
  tsd : Integer;
  tst : Cardinal;
begin
  GetFBTimestamp(AValue, tsd, tst);
  CheckPos(8);
  CopyMemory(FPos, @tsd, 4);
  Inc(FPos, 4);
  CopyMemory(FPos, @tst, 4);
  Inc(FPos, 4);
end;

procedure TfbetRecord.WriteVarchar(AValue: AnsiString; AFieldSize : Integer);
var
  l : Integer;
  lw : array[0..1] of Byte;
begin
  l := Length(AValue);

  if AFieldSize < l then
  begin
    l := AFieldSize;
    SetLength(AValue, AFieldSize);
  end;

  CheckPos(l + 2);

  lw[0] := Lo(l);
  lw[1] := Hi(l);
  CopyMemory(FPos, @lw, 2);
  Inc(FPos, 2);

  CopyMemory(FPos, @AValue[1], l);
  Inc(FPos, AFieldSize);
end;

end.
