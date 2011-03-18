unit uPS_CSV;

interface

uses
  uPSCompiler,
  uPSRuntime,
  Classes,
  SysUtils,
  TextStream,
  UIB,
  Windows;

procedure PS_Register_CSV_C(ACompiler : TPSPascalCompiler);
procedure PS_Register_CSV_R(AExec : TPSExec; ARCi : TPSRuntimeClassImporter);

implementation

type
  TCSVReader = class
  private
    FFields : array of String;
    FEOF : Boolean;

    FTextStream : TTextStream;
    FDelimiter: Char;
    FQuoteChar: Char;
    function GetActive: Boolean;
    function GetFieldCount: Integer;
  public
    constructor Create();
    destructor Destroy; override;

    procedure Open(AFilename : String);
    procedure Close;
    procedure Next;

    function Field(AIndex : Integer) : String;

    property Active : Boolean read GetActive;
    property EOF : Boolean read FEOF;
    property Delimiter : Char read FDelimiter write FDelimiter;
    property QuoteChar : Char read FQuoteChar write FQuoteChar;
    property FieldCount : Integer read GetFieldCount;
  end;

procedure _WriteCSV(AQuery : TUIBQuery; AFilename : String; ADelimiter, AQuote : Char);
var
  ts : TTextStream;
  idx : Integer;
  s : String;
begin
  ts := TTextStream.Create(AFilename, saWriteWithoutBOM);
  try
    while not AQuery.Eof do
    begin
      for idx := 0 to AQuery.Fields.FieldCount - 1 do
      begin
        s := AQuery.Fields.AsString[idx];

        if AQuote <> #0 then
          s := AQuote + s + AQuote;

        if idx > 0 then
          s := ADelimiter + s;

        ts.WritePart(s);
      end;
      ts.WriteLine('');

      AQuery.Next;
    end;
  finally
    ts.Free;
  end;
end;

procedure TCSVReader_Delimiter_R(Self: TCSVReader; var T: Char); begin T := Self.Delimiter; end;
procedure TCSVReader_Delimiter_W(Self: TCSVReader; T: Char); begin Self.Delimiter := T; end;
procedure TCSVReader_QuoteChar_R(Self: TCSVReader; var T: Char); begin T := Self.QuoteChar; end;
procedure TCSVReader_QuoteChar_W(Self: TCSVReader; T: Char); begin Self.QuoteChar := T; end;
procedure TCSVReader_Active_R(Self: TCSVReader; var T: Boolean); begin T := Self.Active; end;
procedure TCSVReader_EOF_R(Self: TCSVReader; var T: Boolean); begin T := Self.EOF; end;
procedure TCSVReader_FieldCount_R(Self: TCSVReader; var T: Integer); begin T := Self.FieldCount; end;

procedure PS_Register_CSV_C(ACompiler : TPSPascalCompiler);
begin
  ACompiler.AddDelphiFunction('procedure WriteCSV(AQuery : TUIBQuery; AFilename : String; ADelimiter, AQuoteChar : Char);');

  with ACompiler.AddClass(ACompiler.FindClass('TObject'), TCSVReader) do
  begin
    RegisterMethod('constructor Create();');

    RegisterMethod('procedure Open(AFilename : String);');
    RegisterMethod('procedure Close;');
    RegisterMethod('procedure Next;');
    RegisterMethod('function Field(AIndex : Integer) : String;');

    RegisterProperty('Active', 'Boolean', iptR);
    RegisterProperty('Delimiter', 'Char', iptRW);
    RegisterProperty('QuoteChar', 'Char', iptRW);
    RegisterProperty('EOF', 'Boolean', iptR);
    RegisterProperty('FieldCount', 'Integer', iptR);
  end;
end;

procedure PS_Register_CSV_R(AExec : TPSExec; ARCi : TPSRuntimeClassImporter);
begin
  AExec.RegisterDelphiFunction(@_WriteCSV, 'WriteCSV', cdRegister);

  with ARCi.Add(TCSVReader) do
  begin
    RegisterConstructor(@TCSVReader.Create, 'Create');

    RegisterMethod(@TCSVReader.Open, 'Open');
    RegisterMethod(@TCSVReader.Close, 'Close');
    RegisterMethod(@TCSVReader.Next, 'Next');
    RegisterMethod(@TCSVReader.Field, 'Field');

    RegisterPropertyHelper(@TCSVReader_Active_R, nil, 'Active');
    RegisterPropertyHelper(@TCSVReader_Delimiter_R, @TCSVReader_Delimiter_W, 'Delimiter');
    RegisterPropertyHelper(@TCSVReader_QuoteChar_R, @TCSVReader_QuoteChar_W, 'QuoteChar');
    RegisterPropertyHelper(@TCSVReader_EOF_R, nil, 'EOF');
    RegisterPropertyHelper(@TCSVReader_FieldCount_R, nil, 'FieldCount');
  end;
end;

{ TCSVReader }

procedure TCSVReader.Close;
begin
  if Active then
    FreeAndNil(FTextStream);

  SetLength(FFields, 0);
end;

constructor TCSVReader.Create;
begin
  inherited;

  FTextStream := nil;
  FDelimiter := ',';
  FQuoteChar := '"';
end;

destructor TCSVReader.Destroy;
begin
  Close;

  inherited;
end;

function TCSVReader.Field(AIndex: Integer): String;
begin
  Result := FFields[AIndex];
end;

function TCSVReader.GetActive: Boolean;
begin
  Result := Assigned(FTextStream);
end;

function TCSVReader.GetFieldCount: Integer;
begin
  Result := Length(FFields);
end;

procedure TCSVReader.Next;
var
  idx : Integer;
  line : TStringList;
begin
  if not Active then exit;

  FEOF := FTextStream.EoF;

  line := TStringList.Create;
  try
    line.QuoteChar := FQuoteChar;
    line.Delimiter := FDelimiter;
    line.StrictDelimiter := true;

    line.DelimitedText := FTextStream.ReadLine;

    SetLength(FFields, line.Count);

    for idx := 0 to line.Count - 1 do
    begin
      FFields[idx] := line[idx];
    end;
  finally
    line.Free;
  end;
end;

procedure TCSVReader.Open(AFilename: String);
begin
  if Active then
    Close;

  FTextStream := TTextStream.Create(AFilename, saRead);
  FEOF := false;

  Next;
end;

end.
