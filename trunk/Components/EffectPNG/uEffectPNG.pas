//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uEffectPNG;

interface

uses
  SysUtils, Classes, Controls, Windows, Messages, Graphics, PNGImage, Math,
  ComCtrls, Dialogs, StdCtrls;

type

  TPNGEffect = (peNothing, peGrayScale, peSepia, peColorize, peInvert, peGamma{, peTest});
  TEffectFrom = (efTop, efBottom, efLeft, efRight);
  TMirror = (mNothing, mHorizontal, mVertical, mCrossOver);
  TGradient  = array[0..255] of TRGBTriple;
  TPercent = Byte;
  TColorLine = array of TRGBTriple;
  PColorLine = ^TColorLine;

  TPNGEffects = class(TPersistent)
  private
    FUpdateCount : Cardinal;
    FGradient : TGradient;
    FAlpha: TPercent;
    FEffect: TPNGEffect;
    FOnChange: TNotifyEvent;
    FSepiaDepth: TPercent;
    FGrayPercent: TPercent;
    FColorizeColor: TColor;
    FEffectFrom: TEffectFrom;
    FEffectPercent: TPercent;
    FMirror: TMirror;
    FGammaValue: Single;
    procedure SetAlpha(const Value: TPercent);
    procedure SetEffect(const Value: TPNGEffect);

    procedure BuildGradient(AMiddleColor : TColor; var AGradient : TGradient);
    procedure DoMirror(const APNG : TPNGObject);
    procedure DoChange();
    procedure DoGammaChange(const APixel : PRGBTriple);
    procedure SetSepiaDepth(const Value: TPercent);
    procedure SetGrayPercent(const Value: TPercent);
    procedure SetColorizeColor(const Value: TColor);
    procedure Convert(const APixel: PRGBTriple);
    procedure SetEffectFrom(const Value: TEffectFrom);
    procedure SetEffectPercent(const Value: TPercent);
    procedure SetMirror(const Value: TMirror);
    procedure SetGammaValue(const Value: Single);

  public
    constructor Create();
    destructor Destroy(); override;

    procedure ApplyEffects(const APNG : TPNGObject);
    procedure Assign(Source : TPersistent); override;

    procedure BeginUpdate;
    procedure EndUpdate;

  published
    property OnChange : TNotifyEvent read FOnChange write FOnChange;

    property Effect : TPNGEffect read FEffect write SetEffect default peNothing;

    property Alpha : TPercent read FAlpha write SetAlpha default 100;
    property GrayPercent : TPercent read FGrayPercent write SetGrayPercent default 100;
    property EffectPercent : TPercent read FEffectPercent write SetEffectPercent default 100;
    property EffectFrom : TEffectFrom read FEffectFrom write SetEffectFrom default efBottom;
    property SepiaDepth : TPercent read FSepiaDepth write SetSepiaDepth default 50;
    property ColorizeColor : TColor read FColorizeColor write SetColorizeColor default clBlue;
    property Mirror : TMirror read FMirror write SetMirror default mNothing;
    property GammaValue : Single read FGammaValue write SetGammaValue;
  end;


implementation


procedure TPNGEffects.ApplyEffects(const APNG: TPNGObject);
var
  AlphaLine : pByteArray;
  ColorLine : PColorLine;
  idxCol,
  idxLine : Cardinal;
  Multi : Single;
  DoThisLine : Boolean;
  DoThisCol : array of Boolean;
begin
  if FMirror<>mNothing then
    DoMirror(APNG);

  multi:=FAlpha/100;

  SetLength(DoThisCol,APNG.Width);

  for idxCol:=0 to APNG.Width-1 do
  begin
    case FEffectFrom of
      efLeft: DoThisCol[idxCol]:=(idxCol)<(APNG.Width*(FEffectPercent/100));
      efRight: DoThisCol[idxCol]:=(APNG.Width-idxCol)<(APNG.Width*(FEffectPercent/100));
      else DoThisCol[idxCol]:=true;
    end;
  end;

  for idxLine:=0 to APNG.Height-1 do
  begin
    AlphaLine:=APNG.AlphaScanline[idxLine];

    ColorLine:=APNG.Scanline[idxLine];

    case FEffectFrom of
      efTop: DoThisLine:=(idxLine)<(APNG.Height*(FEffectPercent/100));
      efBottom: DoThisLine:=(APNG.Height-idxLine)<(APNG.Height*(FEffectPercent/100));
      else DoThisLine:=true;
    end;

    for idxCol:=0 to APNG.Width-1 do
    begin

      if Assigned(AlphaLine) then
      begin
        AlphaLine^[idxCol]:=round((AlphaLine^[idxCol])*multi);
      end;

      if Assigned(ColorLine) and DoThisLine and  DoThisCol[idxCol] then
      begin
        Convert(PRGBTriple(Integer(ColorLine)+(idxCol*3)));
      end;
    end;
  end;   

  SetLength(DoThisCol,0);

end;

procedure TPNGEffects.Assign(Source: TPersistent);
begin
  if Source is TPNGEffects then
  begin
    Effect:=TPNGEffects(Source).Effect;

    Alpha:=TPNGEffects(Source).Alpha;
    GrayPercent:=TPNGEffects(Source).GrayPercent;
    SepiaDepth:=TPNGEffects(Source).SepiaDepth;
    ColorizeColor:=TPNGEffects(Source).ColorizeColor;
    EffectFrom:=TPNGEffects(Source).EffectFrom;
    EffectPercent:=TPNGEffects(Source).EffectPercent;
    Mirror:=TPNGEffects(Source).Mirror;
    GammaValue:=TPNGEffects(Source).GammaValue;
  end;
end;

procedure TPNGEffects.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TPNGEffects.BuildGradient(AMiddleColor : TColor; var AGradient : TGradient);
var
  idx : Byte;
  RStep,
  GStep,
  BStep : Single;
  RVal,
  GVal,
  BVal : Byte;
  RGBColor : Cardinal;
begin
  RGBColor:=ColorToRGB(AMiddleColor);

  RVal:=GetRValue(RGBColor);
  GVal:=GetGValue(RGBColor);
  BVal:=GetBValue(RGBColor);

  RStep:=RVal/128;
  GStep:=GVal/128;
  BStep:=BVal/128;

  for idx:=0 to 127 do
  begin
    AGradient[idx].rgbtRed:=Round(RVal-((127-idx)*RStep));
    AGradient[idx].rgbtGreen:=Round(GVal-((127-idx)*GStep));
    AGradient[idx].rgbtBlue:=Round(BVal-((127-idx)*BStep));
  end;

  RStep:=(255-RVal)/128;
  GStep:=(255-GVal)/128;
  BStep:=(255-BVal)/128;

  for idx:=128 to 255 do
  begin
    AGradient[idx].rgbtRed:=Round(RVal+((idx-127)*RStep));
    AGradient[idx].rgbtGreen:=Round(GVal+((idx-127)*GStep));
    AGradient[idx].rgbtBlue:=Round(BVal+((idx-127)*BStep));
  end;
end;

procedure TPNGEffects.Convert(const APixel: PRGBTriple);
var
  GrayCol,
  NewRed,
  NewGreen : Byte;
  ColDiff : Single;
begin
  if FEffect in [peGrayScale, peSepia] then
  begin
    GrayCol:=(APixel.rgbtBlue+APixel.rgbtGreen+APixel.rgbtRed) div 3;

    if FEffect=peGrayScale then
    begin
      ColDiff:=(GrayCol-APixel.rgbtBlue)*((100-FGrayPercent)/100);
      APixel.rgbtBlue:=round(GrayCol-ColDiff);
      ColDiff:=(GrayCol-APixel.rgbtGreen)*((100-FGrayPercent)/100);
      APixel.rgbtGreen:=round(GrayCol-ColDiff);
      ColDiff:=(GrayCol-APixel.rgbtRed)*((100-FGrayPercent)/100);
      APixel.rgbtRed:=round(GrayCol-ColDiff);
    end
    else
    begin
      APixel.rgbtBlue:=GrayCol;
      APixel.rgbtGreen:=GrayCol;
      APixel.rgbtRed:=GrayCol;
    end;

    if FEffect=peSepia then
    begin
      NewRed:=APixel.rgbtRed+(FSepiaDepth*2);
      NewGreen:=Apixel.rgbtGreen+FSepiaDepth;

      if NewRed<=((FSepiaDepth*2)-1) then
        NewRed:=255;

      if NewGreen<=(FSepiaDepth-1) then
        NewGreen:=255;

      APixel.rgbtGreen:=NewGreen;
      APixel.rgbtRed:=NewRed;
    end;
  end;

  if FEffect=peColorize then
  begin
    GrayCol:=(APixel.rgbtBlue+APixel.rgbtGreen+APixel.rgbtRed) div 3;

    CopyMemory(APixel,Pointer(Integer(@FGradient)+(GrayCol*3)),3);
  end;

  if FEffect=peInvert then
  begin
    APixel.rgbtRed:=APixel.rgbtRed xor $FF;
    APixel.rgbtGreen:=APixel.rgbtGreen xor $FF;
    APixel.rgbtBlue:=APixel.rgbtBlue xor $FF;
  end;

  if FEffect=peGamma then
  begin
    DoGammaChange(APixel);
  end;

end;

constructor TPNGEffects.Create;
begin
  FAlpha:=100;
  FSepiaDepth:=50;
  FGrayPercent:=100;
  FEffect:=peNothing;
  FColorizeColor:=clBlue;
  BuildGradient(FColorizeColor,FGradient);
  FEffectPercent:=100;
  FGammaValue:=1;
  
  FUpdateCount:=0;
  FEffectFrom:=efBottom;
  FMirror:=mNothing;

  inherited Create();
end;

destructor TPNGEffects.Destroy;
begin

  inherited;
end;

procedure TPNGEffects.DoChange;
begin
  if Assigned(FOnChange) and (FUpdateCount=0) then
    FOnChange(Self);
end;

procedure TPNGEffects.DoGammaChange(const APixel: PRGBTriple);
begin
  APixel.rgbtRed:=round(255*Power(APixel.rgbtRed/255,FGammaValue));
  APixel.rgbtGreen:=round(255*Power(APixel.rgbtGreen/255,FGammaValue));
  APixel.rgbtBlue:=round(255*Power(APixel.rgbtBlue/255,FGammaValue));
end;

procedure TPNGEffects.DoMirror(const APNG: TPNGObject);
var
  idxRow,
  idxCol : Cardinal;
  TempAlpha : Byte;
  TempPixel : PRGBTriple;
  AlphaLine,
  TempAlphaLine : pByteArray;
  ColorLine,
  TempColorLine : PColorLine;
  RunToRow,
  RunToCol : Cardinal;
  Dummy : Byte;
begin
  if FMirror=mNothing then exit;

  Dummy:=1;

  TempPixel:=New(PRGBTriple);

  if FMirror=mHorizontal then
  begin
    RunToRow:=APNG.Height-1;
    RunToCol:=APNG.Width div 2;
    if APNG.Width mod 2 = 0 then Dec(RunToCol);

    for idxRow:=0 to RunToRow do
    begin
      AlphaLine:=APNG.AlphaScanline[idxRow];
      ColorLine:=APNG.Scanline[idxRow];
      
      for idxCol:=0 to RunToCol do
      begin
        TempAlpha:=AlphaLine[idxCol];
        CopyMemory(TempPixel,Pointer(Integer(ColorLine)+idxCol*3),3);

        AlphaLine[idxCol]:=AlphaLine[APNG.Width-Dummy-idxCol];
        CopyMemory(Pointer(Integer(ColorLine)+idxCol*3),Pointer(Integer(ColorLine)+(APNG.Width-Dummy-idxCol)*3),3);
        
        AlphaLine[APNG.Width-Dummy-idxCol]:=TempAlpha;
        CopyMemory(Pointer(Integer(ColorLine)+(APNG.Width-Dummy-idxCol)*3),TempPixel,3);
      end;
    end;
  end;

  if FMirror=mVertical then
  begin
    RunToRow:=APNG.Height div 2;
    RunToCol:=APNG.Width-1;
    if APNG.Height mod 2 = 0 then Dec(RunToRow);

    for idxRow:=0 to RunToRow do
    begin
      AlphaLine:=APNG.AlphaScanline[idxRow];
      ColorLine:=APNG.Scanline[idxRow];

      TempAlphaLine:=APNG.AlphaScanline[APNG.Height-Dummy-idxRow];
      TempColorLine:=APNG.Scanline[APNG.Height-Dummy-idxRow];

      for idxCol:=0 to RunToCol do
      begin
        TempAlpha:=AlphaLine[idxCol];
        CopyMemory(TempPixel,Pointer(Integer(ColorLine)+idxCol*3),3);

        AlphaLine[idxCol]:=TempAlphaLine[idxCol];
        CopyMemory(Pointer(Integer(ColorLine)+idxCol*3),Pointer(Integer(TempColorLine)+idxCol*3),3);

        TempAlphaLine[idxCol]:=TempAlpha;
        CopyMemory(Pointer(Integer(TempColorLine)+idxCol*3),TempPixel,3);
      end;
    end;
  end;

  if FMirror=mCrossOver then
  begin
    RunToRow:=APNG.Height-1;
    RunToCol:=APNG.Width div 2;
    if APNG.Width mod 2 = 0 then
      Dec(RunToCol)
    else
      Dummy:=0;


    for idxRow:=0 to RunToRow do
    begin
      AlphaLine:=APNG.AlphaScanline[idxRow];
      ColorLine:=APNG.Scanline[idxRow];

      TempAlphaLine:=APNG.AlphaScanline[APNG.Height-1-idxRow];
      TempColorLine:=APNG.Scanline[APNG.Height-1-idxRow];

      for idxCol:=0 to RunToCol do
      begin
        TempAlpha:=AlphaLine[idxCol];
        CopyMemory(TempPixel,Pointer(Integer(ColorLine)+idxCol*3),3);

        AlphaLine[idxCol]:=TempAlphaLine[APNG.Width-Dummy-idxCol];
        CopyMemory(Pointer(Integer(ColorLine)+idxCol*3),Pointer(Integer(TempColorLine)+(APNG.Width-Dummy-idxCol)*3),3);

        TempAlphaLine[APNG.Width-Dummy-idxCol]:=TempAlpha;
        CopyMemory(Pointer(Integer(TempColorLine)+(APNG.Width-Dummy-idxCol)*3),TempPixel,3);
      end;
    end;
  end;

  Dispose(TempPixel);

end;

procedure TPNGEffects.EndUpdate;
begin
  Dec(FUpdateCount);
  DoChange;
end;

procedure TPNGEffects.SetAlpha(const Value: TPercent);
begin
  if Value>100 then
    FAlpha := 100
  else FAlpha:=Value;
  DoChange;
end;

procedure TPNGEffects.SetColorizeColor(const Value: TColor);
begin
  FColorizeColor := Value;
  BuildGradient(Value,FGradient);
  DoChange;
end;

procedure TPNGEffects.SetEffect(const Value: TPNGEffect);
begin
  FEffect := Value;
  DoChange;
end;

procedure TPNGEffects.SetEffectFrom(const Value: TEffectFrom);
begin
  FEffectFrom := Value;
  DoChange;
end;

procedure TPNGEffects.SetEffectPercent(const Value: TPercent);
begin
  if Value>100 then
    FEffectPercent := 100
  else FEffectPercent:=Value;
  DoChange
end;

procedure TPNGEffects.SetGammaValue(const Value: Single);
begin
  FGammaValue := Value;
  DoChange;
end;

procedure TPNGEffects.SetGrayPercent(const Value: TPercent);
begin
  if Value>100 then
    FGrayPercent := 100
  else FGrayPercent:=Value;
  DoChange;
end;

procedure TPNGEffects.SetMirror(const Value: TMirror);
begin
  FMirror := Value;
  DoChange;
end;

procedure TPNGEffects.SetSepiaDepth(const Value: TPercent);
begin
  if Value>100 then
    FSepiaDepth := 100
  else FSepiaDepth:=Value;
  DoChange;
end;


end.
