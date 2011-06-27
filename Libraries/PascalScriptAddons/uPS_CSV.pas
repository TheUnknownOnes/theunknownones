unit uPS_CSV;

interface

uses
  uPSCompiler,
  uPSRuntime,
  Classes,
  SysUtils,
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

    FFile : TStreamReader;
    FDelimiter: Char;
    FQuoteChar: Char;
    FEncoding: TEncoding;
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
    property Encoding : TEncoding read FEncoding write FEncoding;
  end;

//The following code was stolen from http://www.delphipraxis.net/25844-csv-hilfsfunktionen.html

//  ParseCSVLine Orginal by Perry Way
//  redesign by Andreas Schmidt
procedure ParseCSVLine(const ALine: string; AFields: TStrings; delim, quote:char);
var
   iState:  cardinal;
   i:       cardinal;
   iLength: cardinal;
   sField:  string;
   aChar: Char;
begin
   Assert(Assigned(AFields));
   AFields.Clear;

  // determine length of input //
  iLength := Length(ALine);
  // exit if empty string //
  if iLength = 0 then
    Exit;
  // initialize State Machine //
  iState := 0;
  sField := '';

  // state machine //
  for i := 1 to iLength do
  begin
    aChar := ALine[i];
    case iState of
//--------------------------------------------------------------//
      0: // unknown //
      begin
        sField := '';
        if aChar = quote then // start of embedded quotes or commas //
           iState := 2
        else if aChar=delim then // empty field //
            AFields.Add(sField)
        else
          begin // start of regular field //
            sField := ALine[i];
            iState := 1;
          end;
      end;
//--------------------------------------------------------------//
      1: // continuation of regular field //
      begin
        if aChar = delim then // end of regular field //
          begin
            AFields.Add(sField);
            // if end of input, then we know there remains a "null" field //
            if (i = iLength) then
            begin
              AFields.Add('');
            end // (i = iLength) //
            else
            begin
              iState := 0;
            end;
          end
          else // concatenate current char //
          begin
            sField := sField + aChar;
            if (i = iLength) then // EOL //
              AFields.Add(sField);
          end;
      end;
//--------------------------------------------------------------//
      2: // continuation of embedded quotes or commas //
      begin
        if aChar = quote then // end of embedded comma field or beginning of embedded quote
          begin
            if i < iLength then // NotTheEndPos //
            begin
              if ALine[i+1] = delim then
              begin // end of embedded comma field //
                iState := 1
              end
              else
              begin
                iState := 3;
              end;
            end
            else
            begin // end of field since end of line //
              AFields.Add(sField);
            end;
          end
          else // concatenate current char //
          begin
            sField := sField + aChar;
          end;
      end;
//--------------------------------------------------------------//
      3: // beginning of embedded quote //
      begin
        if aChar = quote then
          begin
            sField := sField + aChar;
            iState := 2;
          end;
      end;
//--------------------------------------------------------------//
    end; // case iState //
  end;
end;

function CharsInString(const s, search : string):Boolean;
var
   i,j : Integer;
begin
   for i := 1 to Length(s) do
   begin
      for j:= 1 to Length(search) do
         if s[i] = search[j] then
         begin
            Result := True;
            Exit;
         end;
   end;
   Result := False;
end;

//End of stolen code :)

procedure _WriteCSV(AQuery : TUIBQuery; AFilename : String; ADelimiter, AQuote : Char; AEncoding : TEncoding);
var
  dest : TFileStream;
  idx : Integer;
  s : String;
  buffer : TBytes;
begin
  dest := TFileStream.Create(AFilename, fmOpenReadWrite or fmCreate);
  try
    while not AQuery.Eof do
    begin
      for idx := 0 to AQuery.Fields.FieldCount - 1 do
      begin
        s := AQuery.Fields.AsString[idx];

        if AQuote <> #0 then
        begin
          if CharsInString(s, ' ' + ADelimiter + AQuote) then
            s := AQuote + s + AQuote;
        end;

        if idx > 0 then
          s := ADelimiter + s;

        buffer := AEncoding.GetBytes(s);
        dest.Write(buffer[0], Length(Buffer));
      end;

      buffer := AEncoding.GetBytes(#13#10);
      dest.Write(buffer[0], Length(Buffer));

      AQuery.Next;
    end;
  finally
    dest.Free;
  end;
end;

procedure TCSVReader_Delimiter_R(Self: TCSVReader; var T: Char); begin T := Self.Delimiter; end;
procedure TCSVReader_Delimiter_W(Self: TCSVReader; T: Char); begin Self.Delimiter := T; end;
procedure TCSVReader_QuoteChar_R(Self: TCSVReader; var T: Char); begin T := Self.QuoteChar; end;
procedure TCSVReader_QuoteChar_W(Self: TCSVReader; T: Char); begin Self.QuoteChar := T; end;
procedure TCSVReader_Active_R(Self: TCSVReader; var T: Boolean); begin T := Self.Active; end;
procedure TCSVReader_EOF_R(Self: TCSVReader; var T: Boolean); begin T := Self.EOF; end;
procedure TCSVReader_FieldCount_R(Self: TCSVReader; var T: Integer); begin T := Self.FieldCount; end;
procedure TCSVReader_Encoding_R(Self: TCSVReader; var T: TEncoding); begin T := Self.Encoding; end;
procedure TCSVReader_Encoding_W(Self: TCSVReader; T: TEncoding); begin Self.Encoding := T; end;

procedure PS_Register_CSV_C(ACompiler : TPSPascalCompiler);
begin
  ACompiler.AddDelphiFunction('procedure WriteCSV(AQuery : TUIBQuery; AFilename : String; ADelimiter, AQuoteChar : Char; AEncoding : TEncoding);');

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
    RegisterProperty('Encoding', 'TEncoding', iptRW);
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
    RegisterPropertyHelper(@TCSVReader_Encoding_R, @TCSVReader_Encoding_W, 'Encoding');
  end;
end;

{ TCSVReader }

procedure TCSVReader.Close;
begin
  if Active then
    FreeAndNil(FFile);

  SetLength(FFields, 0);
end;

constructor TCSVReader.Create;
begin
  inherited;

  FFile := nil;
  FDelimiter := ',';
  FQuoteChar := '"';
  FEncoding := TEncoding.Default;
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
  Result := Assigned(FFile);
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

  line := TStringList.Create;
  try
    ParseCSVLine(FFile.ReadLine, line, FDelimiter, FQuoteChar);

    FEOF := FFile.EndOfStream;

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

  FFile := TStreamReader.Create(AFilename, FEncoding);
  FEOF := false;

  Next;
end;

end.
