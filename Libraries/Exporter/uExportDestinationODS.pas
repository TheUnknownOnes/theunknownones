unit uExportDestinationODS;

interface

uses
  uExporter,
  SysUtils,
  StrUtils,
  Variants,
  Windows,
  Classes,
  DateUtils,
  uXMLTools,
  PDSevenZip;

{$M+}

type
  TExporterDestinationODS = class(TExporterDestinationBase)
  private
    FFilename : String;
  protected
    function Execute(ASource: TExporterSourceBase): Boolean; override;
    function Execute1D(ASource: TExporterSource1DBase): Boolean;
    function Execute2D(ASource: TExporterSource2DBase): Boolean;
    function Execute3D(ASource: TExporterSource3DBase): Boolean;

    function GenTagsDocumentHead : String;
    function GenTagsDocumentFoot : String;
    function GenTagTableStart(AName : String) : String;
    function GenTagTableEnd() : String;
    function GenTagRowStart() : String;
    function GenTagRowEnd() : String;
    function GenTagCell(AData: Variant) : WideString;
    function GenManifest : WideString;

    procedure WriteFile(AContent : TStream);
  published
    property Filename : String read FFilename write FFilename;
  end;

implementation

var
  ODSFormatSettings : TFormatSettings;

{ TExporterDestinationODS }

function TExporterDestinationODS.Execute(ASource: TExporterSourceBase): Boolean;
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

function TExporterDestinationODS.Execute1D(
  ASource: TExporterSource1DBase): Boolean;
var
  content : Widestring;
  utf8 : String;
  fs : TFileStream;
  MS_Content: TMemoryStream;

  procedure WriteContent;
  begin
    utf8 := UTF8Encode(content);
    MS_Content.Write(PChar(UTF8)^, length(utf8));
    content := EmptyStr;
  end;
  
begin
  MS_Content := TMemoryStream.Create;
  try
    content := GenTagsDocumentHead + GenTagTableStart('Tabelle1');

    ASource.NavigateRow(ndBeginning);

    repeat
      content := content + GenTagRowStart + GenTagCell(ASource.GetValue) + GenTagRowEnd;
      WriteContent;
    until ASource.NavigateRow(ndNext) <> nrOK;

    content := content + GenTagTableEnd + GenTagsDocumentFoot;

    WriteContent;

    WriteFile(MS_Content);

  finally
    MS_Content.Free;
  end;
end;

function TExporterDestinationODS.Execute2D(
  ASource: TExporterSource2DBase): Boolean;
var
  content : Widestring;
  utf8 : String;
  fs : TFileStream;
  MS_Content: TMemoryStream;

  procedure WriteContent;
  begin
    utf8 := UTF8Encode(content);
    MS_Content.Write(PChar(UTF8)^, length(utf8));
    content := EmptyStr;
  end;

begin
  MS_Content := TMemoryStream.Create;
  try
    content := GenTagsDocumentHead + GenTagTableStart('Tabelle1');

    ASource.NavigateRow(ndBeginning);
    ASource.NavigateColumn(ndBeginning);

    content := content + GenTagRowStart;
    repeat
      content := content + GenTagCell(ASource.GetCaption(1))
    until ASource.NavigateColumn(ndNext) <> nrOK;
    content := content + GenTagRowEnd;

    WriteContent;

    repeat
      content := content + GenTagRowStart;

      ASource.NavigateColumn(ndBeginning);
      repeat
        content := content + GenTagCell(ASource.GetValue);
      until ASource.NavigateColumn(ndNext) <> nrOK;

      content := content + GenTagRowEnd;
      WriteContent;

    until ASource.NavigateRow(ndNext) <> nrOK;

    content := content + GenTagTableEnd + GenTagsDocumentFoot;

    WriteContent;

    WriteFile(MS_Content);

  finally
    MS_Content.Free;
  end;
end;

function TExporterDestinationODS.Execute3D(
  ASource: TExporterSource3DBase): Boolean;
var
  content : Widestring;
  utf8 : String;
  fs : TFileStream;
  MS_Content: TMemoryStream;

  procedure WriteContent;
  begin
    utf8 := UTF8Encode(content);
    MS_Content.Write(PChar(UTF8)^, length(utf8));
    content := EmptyStr;
  end;

begin
  MS_Content := TMemoryStream.Create;
  try
    content := GenTagsDocumentHead;

    ASource.NavigatePage(ndBeginning);
    repeat
      GenTagTableStart(ASource.GetCaption(2));

      ASource.NavigateRow(ndBeginning);
      ASource.NavigateColumn(ndBeginning);

      content := content + GenTagRowStart;
      repeat
        content := content + GenTagCell(ASource.GetCaption(1))
      until ASource.NavigateColumn(ndNext) <> nrOK;
      content := content + GenTagRowEnd;

      WriteContent;

      repeat
        content := content + GenTagRowStart;

        ASource.NavigateColumn(ndBeginning);
        repeat
          content := content + GenTagCell(ASource.GetValue);
        until ASource.NavigateColumn(ndNext) <> nrOK;

        content := content + GenTagRowEnd;
        WriteContent;

      until ASource.NavigateRow(ndNext) <> nrOK;

      content := content + GenTagTableEnd;
      WriteContent;
      
    until ASource.NavigatePage(ndNext) <> nrOK;

    content := content + GenTagsDocumentFoot;

    WriteContent;

    WriteFile(MS_Content);

  finally
    MS_Content.Free;
  end;
end;

function TExporterDestinationODS.GenManifest: WideString;
begin
  Result := '<?xml version="1.0" encoding="UTF-8"?> ';
  Result := Result + '<manifest:manifest xmlns:manifest="urn:oasis:names:tc:opendocument:xmlns:manifest:1.0">';
    Result := Result + '<manifest:file-entry manifest:media-type="application/vnd.oasis.opendocument.spreadsheet" manifest:version="1.2" manifest:full-path="/"/>';
  Result := Result + '</manifest:manifest>';
end;

function TExporterDestinationODS.GenTagCell(AData: Variant): WideString;
var
  valuetype : String;
  valuetag : String;
  text : String;
  style : String;
begin
  text := '';
  valuetag := '';
  style := '';

  if not VarIsNull(AData) then
  begin
    case VarType(AData) of
      varShortInt,
      varByte,
      varWord,
      varLongWord,
      varInt64,
      varSmallInt,
      varInteger: valuetype := 'int';
      varSingle,
      varDouble: valuetype := 'float';
      varCurrency: valuetype := 'currency';
      varDate:
      begin
        valuetype := 'date';
        style := 'StyleCellWithDate';
      end;
      varBoolean:
      begin
        if AData then
          AData := 1
        else
          AData := 0;

        valuetype := 'int';
      end;
      varString,
      varOleStr: valuetype := 'string';
      else
      begin
        try
          AData := Double(AData);
          valuetype := 'float';
        except
          try
            AData := Integer(AData);
            valuetype := 'int';
          except
            valuetype := 'string';
          end;
        end;
      end;
    end;
  end;

  if valuetype = 'int' then
  begin
    valuetag := 'office:value=' + Format('"%d"', [Integer(AData)], ODSFormatSettings);
    valuetype := 'float'
  end
  else
  if valuetype = 'float' then
  begin
    valuetag := 'office:value=' + Format('"%f"', [Double(AData)], ODSFormatSettings);
  end
  else
  if valuetype = 'currency' then
  begin
    valuetag := 'office:value=' + Format('"%f"', [Double(AData)], ODSFormatSettings);
  end
  else
  if valuetype = 'date' then
  begin
    valuetag := 'office:date-value="' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', TDateTime(AData)) + '"';
    
    if TimeOf(AData) = 0 then
      style := 'StyleCellWithDate'
    else
      style := 'StyleCellWithDateTime';
  end
  else
  if valuetype = 'string' then
  begin
    text := '<text:p>' + ReplaceSpecialCharsWithEntities(VarToStr(AData)) + '</text:p>';
  end;

  if style <> '' then
    style := 'table:style-name="' + style + '"';
  
  Result := Format('<table:table-cell %s office:value-type="%s" %s>%s</table:table-cell>',
                   [style, valuetype, valuetag, text]);
end;

function TExporterDestinationODS.GenTagRowEnd: String;
begin
  Result := '</table:table-row>';
end;

function TExporterDestinationODS.GenTagRowStart: String;
begin
  Result := '<table:table-row>';
end;

function TExporterDestinationODS.GenTagsDocumentFoot: String;
begin
  Result := '</office:spreadsheet></office:body></office:document-content>';
end;

function TExporterDestinationODS.GenTagsDocumentHead: String;
var
  fs : TFormatSettings;

  procedure Add(AText : String);
  begin
    Result := Result + AText;
  end;
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, fs);

  Result := '<?xml version="1.0" encoding="UTF-8"?>';

  Add('<office:document-content office:version="1.2" ');
    Add('xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" ');
    Add('xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0" ');
    Add('xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0" ');
    Add('xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0" ');
    Add('xmlns:number="urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0" ');
  Add('>');

    Add('<office:automatic-styles>');

      Add('<number:date-style style:name="StyleDateTime" number:automatic-order="true" number:format-source="language">');
        Add('<number:day/>' +
            '<number:text>' + fs.DateSeparator +'</number:text>' +
            '<number:month/>' +
            '<number:text>' + fs.DateSeparator +'</number:text>' +
            '<number:year/>' +
            '<number:text> </number:text>' +
            '<number:hours number:style="long"/>' +
            '<number:text>' + fs.TimeSeparator +'</number:text>' +
            '<number:minutes number:style="long"/>' +
            '<number:text>' + fs.TimeSeparator +'</number:text>' +
            '<number:seconds number:style="long"/>');
      Add('</number:date-style>');

      Add('<number:date-style style:name="StyleDate" number:automatic-order="true" number:format-source="language">');
        Add('<number:day/>' +
            '<number:text>' + fs.DateSeparator +'</number:text>' +
            '<number:month/>' +
            '<number:text>' + fs.DateSeparator +'</number:text>' +
            '<number:year/>');
      Add('</number:date-style>');

      Add('<style:style style:name="StyleCellWithDateTime" style:family="table-cell" style:parent-style-name="Default" style:data-style-name="StyleDateTime"/>');
      Add('<style:style style:name="StyleCellWithDate" style:family="table-cell" style:parent-style-name="Default" style:data-style-name="StyleDate"/>');

    Add('</office:automatic-styles>');

    Add('<office:body><office:spreadsheet>');
end;

function TExporterDestinationODS.GenTagTableEnd: String;
begin
  Result := '</table:table>';
end;

function TExporterDestinationODS.GenTagTableStart(AName: String): String;
begin
  Result := Format('<table:table table:name="%s">', [AName]);
end;

procedure TExporterDestinationODS.WriteFile(AContent: TStream);
var
  MS_Manifest : TMemoryStream;
  s : String;
  arch : I7zOutArchive;
begin
  MS_Manifest := TMemoryStream.Create;
  try
    s := UTF8Encode(GenManifest);
    MS_Manifest.Write(PChar(s)^, Length(s));

    arch := CreateOutArchive(CLSID_CFormatZip);
    arch.AddStream(AContent, soReference, faArchive, CurrentFileTime, CurrentFileTime, 'content.xml', false, false);
    arch.AddStream(MS_Manifest,  soReference, faArchive, CurrentFileTime, CurrentFileTime, 'META-INF\manifest.xml', false, false);
    arch.SaveToFile(FFilename);
  finally
    MS_Manifest.Free;
  end;
end;

initialization
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, ODSFormatSettings);
  ODSFormatSettings.ThousandSeparator := #0;
  ODSFormatSettings.DecimalSeparator := '.';

end.
