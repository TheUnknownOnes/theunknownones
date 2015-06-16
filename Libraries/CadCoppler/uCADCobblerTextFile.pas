//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************

unit uCADCobblerTextFile;

interface

uses
  Classes, StrUtils, SysUtils, Types, Generics.Collections;

type
  ECCException = class(Exception)
  end;

  EInvalidFileVersion = class(ECCException)
  end;

  ECorruptFile = class(ECCException)
  end;

  TCCCoord = record
    X, Y : Double;
  end;

  TCCCoordList = class(TList<TCCCoord>)
  public
  end;

  TCCPieceElement = class
  public
    constructor Create(); virtual; abstract;
  end;

  TCCPieceElementList = class(TObjectList<TCCPieceElement>)
  end;

  TCCPieceCoordListElement = class(TCCPieceElement)
  protected
    FCoords : TCCCoordList;
  public
    constructor Create(); override;
    destructor Destroy; override;

    property Coords : TCCCoordList read FCoords;
  end;

  TCCPieceLIElement = class(TCCPieceCoordListElement)
  end;

  TCCPieceBOElement = class(TCCPieceCoordListElement)
  end;

  TCCPieceSTElement = class(TCCPieceCoordListElement)
  protected
    FRadius : Double;
  public
    constructor Create(); override;

    property Radius : Double read FRadius;
  end;

  TCCPiece = class
  protected
    FName : String;
    FSize : String;
    FComment : String;
    FElements : TCCPieceElementList;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    property Name : String read FName;
    property Size : String read FSize;
    property Comment : String read FComment;
    property Elements : TCCPieceElementList read FElements;
  end;

  TCustomCCPieceConverter = class
  protected
    FPiece : TCCPiece;

    procedure SearchMinMax(out AMin, AMax : TCCCoord); virtual;
  public
    constructor Create(); virtual; abstract;

    procedure Convert(const APiece : TCCPiece); virtual;
  end;

  TCCPiecesList = class(TObjectList<TCCPiece>)
  end;

  TCustomCCTextFile = class
  protected
    FVersion : Integer;
    FName : String;
    FDate : TDate;
    FComment : String;
    FPieceNames : TStringList;
    FSizeNames : TStringList;
    FPieces : TCCPiecesList;

    FParserNextPiece,
    FParserNextSize : Integer; //Index in FPieceNames and FSizeNames
    FParserCurrentPiece : TCCPiece;
    FParserCurrentElement : TCCPieceElement;

    property Version : Integer read FVersion;
    property Name : String read FName;
    property Date : TDate read FDate;
    property Comment : String read FComment;
    property PieceNames : TStringList read FPieceNames;
    property SizeNames : TStringList read FSizeNames;
    property Pieces : TCCPiecesList read FPieces;

    procedure Parse(const ARawText : String); virtual;
    procedure ParseLine(const ALine : String); virtual;
    procedure ParseData(const ADataID, AData : String); virtual;
    procedure HandleVN(const AData : String); virtual;
    procedure HandleNA(const AData : String); virtual;
    procedure HandleDA(const AData : String); virtual;
    procedure HandleCM(const AData : String); virtual;
    procedure HandlePN(const AData : String); virtual;
    procedure HandleSN(const AData : String); virtual;
    procedure HandleBE(const AData : String); virtual;
    procedure HandleEN(const AData : String); virtual;
    procedure HandleLI(const AData : String); virtual;
    procedure HandleBO(const AData : String); virtual;
    procedure HandleST(const AData : String); virtual;
    procedure HandleRA(const AData : String); virtual;
    procedure HandleCO(const AData : String); virtual;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    procedure Clear; virtual;

    procedure LoadFromStream(AStream : TStream); overload; virtual;
    procedure LoadFromStream(AStream : TStream; AEncoding : TEncoding); overload; virtual;
    procedure LoadFromFile(AFilename : TFileName); overload; virtual;
    procedure LoadFromFile(AFilename : TFileName; AEncoding : TEncoding); overload; virtual;
  end;

  TCCTextFile = class(TCustomCCTextFile)
  public
    property Version;
    property Name;
    property Date;
    property Comment;
    property PieceNames;
    property SizeNames;
    property Pieces;
  end;

implementation

{ TCustomCCTextFile }

procedure TCustomCCTextFile.Clear;
begin
  FVersion := 0;
  FName := EmptyStr;
  FDate := 0;
  FComment := EmptyStr;
  FPieceNames.Clear;
  FSizeNames.Clear;
  FPieces.Clear;
end;

constructor TCustomCCTextFile.Create;
begin
  FPieceNames := TStringList.Create;
  FSizeNames := TStringList.Create;
  FPieces := TCCPiecesList.Create;
end;

destructor TCustomCCTextFile.Destroy;
begin
  FPieceNames.Free;
  FSizeNames.Free;
  FPieces.Free;

  inherited;
end;

procedure TCustomCCTextFile.HandleBE(const AData: String);
begin
  if Assigned(FParserCurrentPiece) then
    raise ECorruptFile.CreateFmt('File misses end-marker for piece "%s/%s"', [FParserCurrentPiece.FName, FParserCurrentPiece.Size]);

  if FParserNextPiece >= FPieceNames.Count then
    raise ECorruptFile.CreateFmt('File contains more data then pieces declared (%d)', [FPieceNames.Count]);

  if FParserNextSize >= FSizeNames.Count then
    raise ECorruptFile.CreateFmt('File contains more data then sizes declared (%d)', [FSizeNames.Count]);

  FParserCurrentPiece := TCCPiece.Create;
  FParserCurrentPiece.FName := FPieceNames[FParserNextPiece];
  FParserCurrentPiece.FSize := FSizeNames[FParserNextSize];
  FPieces.Add(FParserCurrentPiece);

  FParserCurrentElement := nil;
end;

procedure TCustomCCTextFile.HandleBO(const AData: String);
begin
  if not Assigned(FParserCurrentPiece) then
    raise ECorruptFile.Create('File contains element-data without coresponding piece');

  FParserCurrentElement := TCCPieceBOElement.Create;
  FParserCurrentPiece.FElements.Add(FParserCurrentElement);
end;

procedure TCustomCCTextFile.HandleCM(const AData: String);
begin
  if not Assigned(FParserCurrentPiece) then
    FComment := AData
  else
    FParserCurrentPiece.FComment := AData;
end;

procedure TCustomCCTextFile.HandleCO(const AData: String);
var
  p : Integer;
  coord : TCCCoord;
  s : String;
begin
  if FParserCurrentElement is TCCPieceCoordListElement then
  begin
    s := StringReplace(AData, '.', ',', [rfReplaceAll]);
    p := Pos(#32, s);
    if p > 0 then
    begin
      coord.X := StrToFloat(Copy(s, 1, p - 1));
      coord.Y := StrToFloat(Copy(s, p + 1, Length(s) - p - 1));
      TCCPieceCoordListElement(FParserCurrentElement).Coords.Add(coord);
    end;
  end;
end;

procedure TCustomCCTextFile.HandleDA(const AData: String);
begin
  FDate := EncodeDate(StrToIntDef(Copy(AData, 7, 4), 0),
                      StrToIntDef(Copy(AData, 4, 2), 0),
                      StrToIntDef(Copy(AData, 1, 2), 0));
end;

procedure TCustomCCTextFile.HandleEN(const AData: String);
begin
  Inc(FParserNextSize);

  if FParserNextSize = FSizeNames.Count then
  begin
    Inc(FParserNextPiece);
    FParserNextSize := 0;
  end;

  FParserCurrentPiece := nil;
end;

procedure TCustomCCTextFile.HandleLI(const AData: String);
begin
  if not Assigned(FParserCurrentPiece) then
    raise ECorruptFile.Create('File contains element-data without coresponding piece');

  FParserCurrentElement := TCCPieceLIElement.Create;
  FParserCurrentPiece.FElements.Add(FParserCurrentElement);
end;

procedure TCustomCCTextFile.HandleNA(const AData: String);
begin
  FName := AData;
end;

procedure TCustomCCTextFile.HandlePN(const AData: String);
begin
  FPieceNames.Add(AData);
end;

procedure TCustomCCTextFile.HandleRA(const AData: String);
begin
  if not Assigned(FParserCurrentElement) then
    raise ECorruptFile.Create('File contains radius-data without coresponding element');

  if FParserCurrentElement is TCCPieceSTElement then
    TCCPieceSTElement(FParserCurrentElement).FRadius := StrToFloat(AData)
  else
    raise ECorruptFile.CreateFmt('Current Element %s doesnt support radius-data', [FParserCurrentElement.ClassName]);
end;

procedure TCustomCCTextFile.HandleSN(const AData: String);
begin
  FSizeNames.Add(AData);
end;

procedure TCustomCCTextFile.HandleST(const AData: String);
begin
  if not Assigned(FParserCurrentPiece) then
    raise ECorruptFile.Create('File contains element-data without coresponding piece');

  FParserCurrentElement := TCCPieceSTElement.Create;
  FParserCurrentPiece.FElements.Add(FParserCurrentElement);
end;

procedure TCustomCCTextFile.HandleVN(const AData: String);
begin
  FVersion := StrToIntDef(AData, -1);
  if FVersion = -1 then
    raise EInvalidFileVersion.Create('Invalid fileversion (' + AData + ')');
end;

procedure TCustomCCTextFile.LoadFromFile(AFilename: TFileName);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCustomCCTextFile.LoadFromStream(AStream: TStream);
begin
  LoadFromStream(AStream, nil);
end;

procedure TCustomCCTextFile.LoadFromFile(AFilename: TFileName;
  AEncoding: TEncoding);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream, AEncoding);
  finally
    Stream.Free;
  end;
end;

procedure TCustomCCTextFile.LoadFromStream(AStream: TStream;
  AEncoding: TEncoding);
var
  Size: Integer;
  Buffer: TBytes;
  RawText : String;
begin
  Clear;
  Size := AStream.Size - AStream.Position;
  SetLength(Buffer, Size);
  AStream.Read(Buffer[0], Size);

  Size := TEncoding.GetBufferEncoding(Buffer, AEncoding);
  RawText := AEncoding.GetString(Buffer, Size, Length(Buffer) - Size);
  Parse(RawText);
end;

procedure TCustomCCTextFile.Parse(const ARawText: String);
//code stolen from Classes.TStrings.SetTextStr
var
  P, Start : PChar;
  S : String;
begin
  FParserNextPiece := 0;
  FParserNextSize := 0;
  FParserCurrentPiece := nil;

  P := Pointer(ARawText);
  if Assigned(P) then
  begin
    while P^ <> #0 do
    begin
      Start := P;
      while not CharInSet(P^, [#0, #10, #13]) do Inc(P);
      SetString(S, Start, P - Start);
      ParseLine(S);
      if P^ = #13 then Inc(P);
      if P^ = #10 then Inc(P);
    end;
  end;

  if FPieces.Count <> FPieceNames.Count * FSizeNames.Count then
    raise ECorruptFile.CreateFmt('Number of pieces read (%d) doenst match number of declared pieces (%d) and sizes(%d)',
                                 [FPieces.Count, FPieceNames.Count, FSizeNames.Count]);
end;

procedure TCustomCCTextFile.ParseData(const ADataID, AData: String);
begin
  if ADataID = 'VN' then HandleVN(AData)
  else
  if ADataID = 'NA' then HandleNA(AData)
  else
  if ADataID = 'DA' then HandleDA(AData)
  else
  if ADataID = 'CM' then HandleCM(AData)
  else
  if ADataID = 'PN' then HandlePN(AData)
  else
  if ADataID = 'SN' then HandleSN(AData)
  else
  if ADataID = 'BE' then HandleBE(AData)
  else
  if ADataID = 'EN' then HandleEN(AData)
  else
  if ADataID = 'LI' then HandleLI(AData)
  else
  if ADataID = 'BO' then HandleBO(AData)
  else
  if ADataID = 'ST' then HandleST(AData)
  else
  if ADataID = 'CO' then HandleCO(AData)

end;

procedure TCustomCCTextFile.ParseLine(const ALine: String);
var
  P : PChar;
  DataID,
  Data : String;
begin
  P := Pointer(ALine);
  if Assigned(P) then
  begin
    if (P^ = '#') and (Length(ALine) >= 3) then
    begin
      DataID := Copy(ALine, 2, 2);

      if Length(ALine) > 4 then
        Data := Copy(ALine, 5, Length(ALine) - 4)
      else
        Data := EmptyStr;

      ParseData(DataID, Data);
    end;
  end;
end;

{ TCCPiece }

constructor TCCPiece.Create;
begin
  FElements := TCCPieceElementList.Create;
end;

destructor TCCPiece.Destroy;
begin
  FElements.Free;
  inherited;
end;

{ TCCPieceCoordListElement }

constructor TCCPieceCoordListElement.Create;
begin
  FCoords := TCCCoordList.Create;
end;

destructor TCCPieceCoordListElement.Destroy;
begin
  FCoords.Free;
  inherited;
end;

{ TCCPieceSTElement }

constructor TCCPieceSTElement.Create;
begin
  inherited;

  FRadius := 0;
end;

{ TCustomCCPieceConverter }

procedure TCustomCCPieceConverter.Convert(const APiece: TCCPiece);
begin
  FPiece := APiece;
end;

procedure TCustomCCPieceConverter.SearchMinMax(out AMin, AMax: TCCCoord);
var
  e : TCCPieceElement;
  c : TCCPieceCoordListElement absolute e;
  coord : TCCCoord;
begin
  AMin.X := 0;
  AMin.Y := 0;
  AMax.X := 0;
  AMax.Y := 0;

  for e in FPiece.Elements do
  begin
    if c is TCCPieceCoordListElement then
    begin
      for coord in c.Coords do
      begin
        if coord.X < AMin.X then AMin.X := coord.X;
        if coord.Y < AMin.Y then AMin.Y := coord.Y;
        if coord.X > AMax.X then AMax.X := coord.X;
        if coord.Y > AMax.Y then AMax.Y := coord.Y;
      end;
    end;
  end;
end;

end.
