unit uExporterDestinationCSV;

interface

uses
  uExporter, Classes, SysUtils;

type
  {$M+}

  TExporterDestinationCSVBase = class(TExporterDestinationBase)
  private
    FSepChar: Char;
    FIncFNames: Boolean;
    FQuotChar: Char;
    FLineSep: String;
  protected
    function QuoteValue(AValue: Variant): WideString;

    function Execute(ASource: TExporterSourceBase): Boolean; override;
    function Execute1D(ASource: TExporterSource1DBase): Boolean;
    function Execute2D(ASource: TExporterSource2DBase): Boolean;
    function Execute3D(ASource: TExporterSource3DBase): Boolean;
    procedure DoWriteLine(AFile, ALine: WideString); virtual; abstract;
    procedure DoFinalTasks(AFile: WideString); virtual; abstract;
  public
    constructor Create;

    property IncludeFieldNames: Boolean read FIncFNames write FIncFNames;
    property SeperatorChar: Char read FSepChar write FSepChar;
    property QuotationChar: Char read FQuotChar write FQuotChar;
    property LineSeperator: String read FLineSep write FLineSep;
  end;

  TExporterDestinationCSVFile = class(TExporterDestinationCSVBase)
  private
    FFileList : TStringList;
    FFilePath: TFileName;
    FFileExt: TFileName;
    FBaseFileName: TFilename;
    function GetFilename: String;
    procedure SetFilename(const Value: String);
    procedure SetFilePath(const Value: TFileName);
  protected
    function GetFileStream(AFile: WideString): TFileStream;
    procedure DoWriteLine(AFile, ALine: WideString); override;
    procedure DoFinalTasks(AFile: WideString); override;
  public
    constructor Create(AFileName: String);
    destructor Destroy; override;
  published
    property FilePath: TFileName read FFilePath write SetFilePath;
    property BaseFileName: TFilename read FBaseFileName write FBaseFileName;
    property FileExtension: TFileName read FFileExt write FFileExt;
    property Filename : String read GetFilename write SetFilename;
  end;

implementation

uses
  Variants;

{ TExporterDestinationCSVBase }

constructor TExporterDestinationCSVBase.Create;
begin
  inherited;
  FLineSep:=#13#10;
  FQuotChar:='"';
  FSepChar:=',';
  FIncFNames:=True;
end;

function TExporterDestinationCSVBase.Execute(
  ASource: TExporterSourceBase): Boolean;
begin
  if ASource is TExporterSource3DBase then
    Execute3D(TExporterSource3DBase(ASource))
  else
  if ASource is TExporterSource2DBase then
    Execute2D(TExporterSource2DBase(ASource))
  else
  if ASource is TExporterSource1DBase then
    Execute1D(TExporterSource1DBase(ASource))
  else
    raise Exception.Create('Quelle enthält eine ungültige Anzahl an Dimensionen');
end;

function TExporterDestinationCSVBase.Execute1D(
  ASource: TExporterSource1DBase): Boolean;
begin
  ASource.NavigateRow(ndBeginning);

  if IncludeFieldNames then
    Self.DoWriteLine('', ASource.GetCaption(0));

  repeat
    Self.DoWriteLine('', QuoteValue(ASource.GetValue));
  until ASource.NavigateRow(ndNext)<>nrOK;

  DoFinalTasks('');
end;

function TExporterDestinationCSVBase.Execute2D(
  ASource: TExporterSource2DBase): Boolean;
var
  Line : WideString;
  sep : WideString;
begin    
  if SeperatorChar=#0 then
    sep:=''
  else
    sep:=SeperatorChar;

  ASource.NavigateRow(ndBeginning);
  ASource.NavigateColumn(ndBeginning);

  if IncludeFieldNames then
  begin
    Line:=QuoteValue(ASource.GetCaption(1));
    while ASource.NavigateColumn(ndNext)=nrOk do
      Line:=Line+sep+QuoteValue(ASource.GetCaption(1));

    self.DoWriteLine('', Line);
  end;

  repeat
    ASource.NavigateColumn(ndBeginning);
    Line:=QuoteValue(ASource.GetValue);
    while ASource.NavigateColumn(ndNext)=nrOk do
      Line:=Line+sep+QuoteValue(ASource.GetValue);

    Self.DoWriteLine('', Line);
  until ASource.NavigateRow(ndNext)<>nrOK;

  DoFinalTasks('');
end;

function TExporterDestinationCSVBase.Execute3D(
  ASource: TExporterSource3DBase): Boolean;
var
  Line : WideString;
  sep : WideString;
  PageName : WideString;
begin
  if SeperatorChar=#0 then
    sep:=''
  else
    sep:=SeperatorChar;

  ASource.NavigatePage(ndBeginning);
  repeat
    PageName:=ASource.GetCaption(0);

    ASource.NavigateRow(ndBeginning);
    ASource.NavigateColumn(ndBeginning);

    if IncludeFieldNames then
    begin
      Line:=QuoteValue(ASource.GetCaption(2));
      while ASource.NavigateColumn(ndNext)=nrOk do
        Line:=Line+sep+QuoteValue(ASource.GetCaption(2));

      self.DoWriteLine('', Line);
    end;

    repeat
      ASource.NavigateColumn(ndBeginning);
      Line:=QuoteValue(ASource.GetValue);
      while ASource.NavigateColumn(ndNext)=nrOk do
        Line:=Line+sep+QuoteValue(ASource.GetValue);

      Self.DoWriteLine(PageName, Line);
    until ASource.NavigateRow(ndNext)<>nrOK;

    DoFinalTasks(PageName);
  until ASource.NavigatePage(ndNext)<>nrOK;
end;

function TExporterDestinationCSVBase.QuoteValue(AValue: Variant): WideString;
begin
  Result:=VarToWideStrDef(AValue, '');
  if QuotationChar<>#0 then
    Result:=QuotationChar+Result+QuotationChar;
end;

{ TExporterDestinationCSVFile }

constructor TExporterDestinationCSVFile.Create(AFileName: String);
begin
  inherited Create;
  FFileList:=TStringList.Create;
  Filename := AFileName;
end;

destructor TExporterDestinationCSVFile.Destroy;
begin
  while FFileList.Count>0 do
  begin
    FFileList.Objects[0].Free;
    FFileList.Delete(0);
  end;
  
  FFileList.Free;
  inherited;
end;

procedure TExporterDestinationCSVFile.DoFinalTasks(AFile: WideString);
var
  fs : TFileStream;
  idx : integer;
begin
  fs:=GetFileStream(AFile);
  idx:=FFileList.IndexOfObject(fs);
  if idx>=0 then
  begin
    FFileList.Delete(idx);
    fs.Free;
  end;
end;

procedure TExporterDestinationCSVFile.DoWriteLine(AFile, ALine: WideString);
var
  txt : String;
begin
  txt:=ALine+FLineSep;

  GetFileStream(AFile).WriteBuffer(Pchar(txt)^, Length(txt));
end;

function TExporterDestinationCSVFile.GetFilename: String;
begin
  Result := FFilePath + FBaseFileName + FFileExt;
end;

function TExporterDestinationCSVFile.GetFileStream(
  AFile: WideString): TFileStream;
var
  idx : Integer;
  FNam: string;
begin
  idx:=FFileList.IndexOf(AFile);
  if idx<0 then
  begin
    FNam:=FFilePath+FBaseFileName+AFile+FFileExt;
    
    Result:=TFileStream.Create(FNam, fmCreate);
    FFileList.AddObject(AFile, Result);
  end
  else
    Result:=TFileStream(FFileList.Objects[idx]);
end;


procedure TExporterDestinationCSVFile.SetFilename(const Value: String);
begin
  FFilePath:=IncludeTrailingPathDelimiter(ExtractFilePath(Value));
  FBaseFileName:=ChangeFileExt(ExtractFileName(Value),'');
  FFileExt:=ExtractFileExt(Value);
end;

procedure TExporterDestinationCSVFile.SetFilePath(const Value: TFileName);
begin
  FFilePath := IncludeTrailingPathDelimiter(Value);
end;

end.
