//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uXMLTools;

interface

uses
  Classes, SysUtils, MSXML2, MSXML, IniFiles, WideStrings, ComObj, uSysTools;

type
  TXMLIni = class(TCustomIniFile)
  private
    FDocument : IXMLDOMDocument;
    FRootKey : IXMLDOMNode;

    function GetSection(ASectionName : WideString) : IXMLDOMNode;
    function GetKey(ASectionName, AKeyName : WideString) : IXMLDOMNode;
    function GetValue(const ASection, AKey : WideString) : WideString;
    procedure SetValue(const ASection, AKey, AValue : WideString);

    procedure UpdateFile; virtual;
    property FileName;
  public
    constructor Create(const ADocument : IXMLDOMDocument; ARootKey : WideString); reintroduce; overload;
    constructor Create(const ANode : IXMLDOMNode); reintroduce; overload;
    function ReadString(const Section, Ident, Default: string): string; override;
    procedure WriteString(const Section, Ident, Value: String); override;
    function ReadInteger(const Section, Ident: string; Default: Longint): Longint; override;
    procedure WriteInteger(const Section, Ident: string; Value: Longint); override;
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; override;
    procedure WriteBool(const Section, Ident: string; Value: Boolean); override;
    function ReadBinaryStream(const Section, Name: string; Value: TStream): Integer; override;
    function ReadDate(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function ReadFloat(const Section, Name: string; Default: Double): Double; override;
    function ReadTime(const Section, Name: string; Default: TDateTime): TDateTime; override;
    procedure WriteBinaryStream(const Section, Name: string; Value: TStream); override;
    procedure WriteDate(const Section, Name: string; Value: TDateTime); override;
    procedure WriteDateTime(const Section, Name: string; Value: TDateTime); override;
    procedure WriteFloat(const Section, Name: string; Value: Double); override;
    procedure WriteTime(const Section, Name: string; Value: TDateTime); override;
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); overload; override;
    procedure ReadSections(const Section: string; Strings: TStrings); overload; override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    procedure EraseSection(const Section: string); override;
    procedure DeleteKey(const Section, Ident: String); override;
    function ValueExists(const Section, Ident: string): Boolean; override;
  end;

  function XGetXMLNodeByPath(const ADocument : IXMLDOMDocument; APath : WideString) : IXMLDOMNode;
  procedure XAddAttribute(const ANode : IXMLDOMNode; AName, AValue : WideString); overload;
  procedure XAddAttribute(const ANode : IXMLDOMNode; AName: WideString; AValue : OleVariant); overload;
  procedure XSetAttribute(const ANode : IXMLDOMNode; AName, AValue : WideString); overload;
  procedure XSetAttribute(const ANode : IXMLDOMNode; AName: WideString; AValue : OleVariant); overload;
  function XAddChildOrGetIfExists(const AParent : IXMLDOMNode; AChildTag : WideString) : IXMLDOMNode;
  function XReadAttribute(const ANode : IXMLDOMNode; AAtrributeName : WideString; var AAttribute : IXMLDOMNode) : Boolean;
  function XGetText(const ANode : IXMLDOMNode) : WideString;

  function CreateMSXMLV1Document : IXMLDOMDocument;
  function CreateMSXMLV2Document : IXMLDOMDocument2;

  function ReplaceSpecialCharsWithEntities(AXML : String) : String;
  function ReplaceEntitiesWithSpecialChars(AXML : String) : String;

implementation

function ReplaceSpecialCharsWithEntities(AXML : String) : String;
begin
  Result := MultipleStringReplace(AXML, ['"',
                                         '&',
                                         '''',
                                         '<',
                                         '>'],
                                        ['&quot;',
                                         '&amp;',
                                         '&apos;',
                                         '&lt;',
                                         '&gt;'],
                                         [rfReplaceAll]);
end;

function ReplaceEntitiesWithSpecialChars(AXML : String) : String;
begin
  Result := MultipleStringReplace(AXML, ['&quot;',
                                         '&amp;',
                                         '&apos;',
                                         '&lt;',
                                         '&gt;'],
                                        ['"',
                                         '&',
                                         '''',
                                         '<',
                                         '>'],
                                        [rfReplaceAll]);
end;

function CreateMSXMLV1Document : IXMLDOMDocument;
begin
  Result:=CreateOleObject('Microsoft.XMLDOM') as IXMLDOMDocument;
end;

function CreateMSXMLV2Document : IXMLDOMDocument2;
begin
  Result:=CreateOleObject('MSXML2.DOMDocument.4.0') as IXMLDOMDocument2;
end;

function XGetText(const ANode : IXMLDOMNode) : WideString;
var
  idx : Integer;
begin
  if ANode.nodeType = NODE_TEXT then
    Result := ANode.text
  else
  if ANode.nodeType = NODE_ELEMENT then
  begin
    for idx := 0 to ANode.childNodes.length  - 1 do
    begin
      if ANode.childNodes.item[idx].nodeType = NODE_TEXT then
        Result := XGetText(ANode.childNodes.item[idx]);
    end;
  end;
end;

function XAddChildOrGetIfExists(const AParent : IXMLDOMNode; AChildTag : WideString) : IXMLDOMNode;
begin
  Result:=AParent.selectSingleNode(AChildTag);
  if not Assigned(Result) then
  begin
    Result:=AParent.ownerDocument.createElement(AChildTag);
    Result:=AParent.appendChild(Result);
  end;
end;

function XGetXMLNodeByPath(const ADocument : IXMLDOMDocument; APath : WideString) : IXMLDOMNode;
var
  sl : TWideStrings;
  idx : Integer;
  CurNode,
  NewNode : IXMLDOMNode;
begin
  sl:=TWideStringList.Create;
  sl.Delimiter:='.';
  sl.DelimitedText:=APath;

  CurNode:=ADocument.documentElement;
  for idx:=0 to sl.Count-1 do
  begin
    NewNode:=CurNode.selectSingleNode(sl[idx]);
    if not Assigned(NewNode) then
    begin
      NewNode:=ADocument.createElement(sl[idx]);
      NewNode:=CurNode.appendChild(NewNode);
    end;
    CurNode:=NewNode;
  end;

  Result:=CurNode;

  sl.Free;
end;

procedure XAddAttribute(const ANode : IXMLDOMNode; AName, AValue : WideString); overload;
var
  XAttr : IXMLDOMAttribute;
begin
  XAttr:=ANode.ownerDocument.createAttribute(AName);
  XAttr.text:=AValue;
  ANode.attributes.setNamedItem(XAttr);
end;

procedure XAddAttribute(const ANode : IXMLDOMNode; AName: WideString; AValue : OleVariant); overload;
var
  XAttr : IXMLDOMAttribute;
begin
  XAttr:=ANode.ownerDocument.createAttribute(AName);
  XAttr.nodeValue:=AValue;
  ANode.attributes.setNamedItem(XAttr);
end;

procedure XSetAttribute(const ANode : IXMLDOMNode; AName, AValue : WideString); overload;
var
  XAttr : IXMLDOMAttribute;
begin
  XAttr := ANode.Attributes.getNamedItem(AName) as IXMLDOMAttribute;
  if not Assigned(XAttr) then
    XAddAttribute(ANode, AName, AValue)
  else
    XAttr.text := AValue;
end;

procedure XSetAttribute(const ANode : IXMLDOMNode; AName: WideString; AValue : OleVariant); overload;
var
  XAttr : IXMLDOMAttribute;
begin
  XAttr := ANode.Attributes.getNamedItem(AName) as IXMLDOMAttribute;
  if not Assigned(XAttr) then
    XAddAttribute(ANode, AName, AValue)
  else
    XAttr.text := AValue;
end;

function XReadAttribute(const ANode : IXMLDOMNode; AAtrributeName : WideString; var AAttribute : IXMLDOMNode) : Boolean;
var
  Attr : IXMLDOMNode;
begin
  Attr:=ANode.attributes.getNamedItem(AAtrributeName);

  Result:=Assigned(Attr);
  if Result then
    AAttribute:=Attr;
end;

{ TXMLIni }

const
  XMLINI_Name='Name';
  XMLINI_Section = 'Section';
  XMLINI_Key = 'Key';

constructor TXMLIni.Create(const ADocument: IXMLDOMDocument;
  ARootKey: WideString);
begin
  FDocument:=ADocument;
  FRootKey:=XGetXMLNodeByPath(FDocument,ARootKey);
end;

constructor TXMLIni.Create(const ANode: IXMLDOMNode);
begin
  FDocument:=ANode.ownerDocument;
  FRootKey:=ANode;
end;

procedure TXMLIni.DeleteKey(const Section, Ident: String);
var
  Key : IXMLDOMNode;
begin
  Key:=GetKey(Section,Ident);
  Key.parentNode.removeChild(Key);
end;

procedure TXMLIni.EraseSection(const Section: string);
var
  Sect : IXMLDOMNode;
begin
  Sect:=GetSection(Section);
  Sect.parentNode.removeChild(Sect);
end;

function TXMLIni.GetKey(ASectionName, AKeyName: WideString): IXMLDOMNode;
var
  Section : IXMLDOMNode;
begin
  Section:=GetSection(ASectionName);
  Result:=Section.selectSingleNode(XMLINI_Key+'[@'+XMLINI_Name+'='''+AKeyName+''']');

  if not Assigned(Result) then
  begin
    Result:=FDocument.createElement(XMLINI_Key);
    Result:=Section.appendChild(Result);
    XAddAttribute(Result,XMLINI_Name,AKeyName);
  end;
end;

function TXMLIni.GetSection(ASectionName: WideString): IXMLDOMNode;
begin
  Result:=FRootKey.selectSingleNode(XMLINI_Section+'[@'+XMLINI_Name+'='''+ASectionName+''']');

  if not Assigned(Result) then
  begin
    Result:=FDocument.createElement(XMLINI_Section);
    Result:=FRootKey.appendChild(Result);
    XAddAttribute(Result,XMLINI_Name,ASectionName);
  end;
end;

function TXMLIni.GetValue(const ASection, AKey: WideString): WideString;
var
  Key : IXMLDOMNode;
begin
  Key:=GetKey(ASection,AKey);
  Result:=Key.text;
end;

function TXMLIni.ReadBinaryStream(const Section, Name: string;
  Value: TStream): Integer;
begin
  Result:=inherited ReadBinaryStream(Section,Name, Value);
end;

function TXMLIni.ReadBool(const Section, Ident: string;
  Default: Boolean): Boolean;
begin
  Result:=inherited ReadBool(Section, Ident, Default);
end;

function TXMLIni.ReadDate(const Section, Name: string;
  Default: TDateTime): TDateTime;
begin
  Result:=inherited ReadDate(Section, Name, Default);
end;

function TXMLIni.ReadDateTime(const Section, Name: string;
  Default: TDateTime): TDateTime;
begin
  Result:=inherited ReadDateTime(Section, Name, Default);
end;

function TXMLIni.ReadFloat(const Section, Name: string;
  Default: Double): Double;
begin
  Result:=inherited ReadFloat(Section, Name, Default);
end;

function TXMLIni.ReadInteger(const Section, Ident: string;
  Default: Integer): Longint;
begin
  Result:=inherited ReadInteger(Section, Ident, Default);
end;

procedure TXMLIni.ReadSection(const Section: string; Strings: TStrings);
var
  Sect : IXMLDOMNode;
  Keys : IXMLDOMNodeList;
  idx : Integer;
begin
  Sect:=GetSection(Section);
  Keys:=Sect.selectNodes(XMLINI_Key);

  Strings.Capacity:=Keys.length;

  Strings.BeginUpdate;
  try
    for idx:=0 to Keys.length-1 do
      Strings.Add(Keys.item[idx].attributes.getNamedItem(XMLINI_Name).text);
  finally
    Strings.EndUpdate;
  end;
end;

procedure TXMLIni.ReadSections(Strings: TStrings);
var
  Sections : IXMLDOMNodeList;
  idx : Integer;
begin
  Sections:=FRootKey.selectNodes(XMLINI_Section);

  Strings.Capacity:=Sections.length;

  Strings.BeginUpdate;
  try
    for idx:=0 to Sections.length-1 do
      Strings.Add(Sections.item[idx].attributes.getNamedItem(XMLINI_Name).text);
  finally
    Strings.EndUpdate;
  end;
end;

procedure TXMLIni.ReadSections(const Section: string; Strings: TStrings);
begin
  inherited;
end;

procedure TXMLIni.ReadSectionValues(const Section: string; Strings: TStrings);
var
  Sect : IXMLDOMNode;
  Keys : IXMLDOMNodeList;
  idx : INteger;
begin
  Sect:=GetSection(Section);
  Keys:=sect.selectNodes(XMLINI_Key);

  Strings.Capacity:=Keys.length;

  Strings.BeginUpdate;
  try
    for idx:=0 to Keys.length-1 do
      Strings.Add(Keys.item[idx].attributes.getNamedItem(XMLINI_Name).text + '='+
                  Keys.item[idx].text);
  finally
    Strings.EndUpdate;
  end;
end;

function TXMLIni.ReadString(const Section, Ident, Default: string): string;
var
  Key : IXMLDOMNode;
begin
  if ValueExists(Section, Ident) then
    Result:=GetValue(Section,Ident)
  else
    Result:=Default;

end;

function TXMLIni.ReadTime(const Section, Name: string;
  Default: TDateTime): TDateTime;
begin
  Result:=inherited ReadTime(Section, Name, Default);
end;


procedure TXMLIni.SetValue(const ASection, AKey, AValue: WideString);
var
  Key : IXMLDOMNode;
begin
  Key:=GetKey(ASection,AKey);
  Key.text:=AValue;
end;

procedure TXMLIni.UpdateFile;
begin
  
end;

function TXMLIni.ValueExists(const Section, Ident: string): Boolean;
var
  Key : IXMLDOMNode;
begin
  Key:=FRootKey.selectSingleNode(XMLINI_Section+'[@'+XMLINI_Name+'='''+Section+''']/'+
                                 XMLINI_Key+'[@'+XMLINI_Name+'='''+Ident+''']');
  Result:=Assigned(Key);
end;

procedure TXMLIni.WriteBinaryStream(const Section, Name: string;
  Value: TStream);
begin
  inherited;
end;

procedure TXMLIni.WriteBool(const Section, Ident: string; Value: Boolean);
begin
  inherited;
end;

procedure TXMLIni.WriteDate(const Section, Name: string; Value: TDateTime);
begin
  inherited;
end;

procedure TXMLIni.WriteDateTime(const Section, Name: string; Value: TDateTime);
begin
  inherited;
end;

procedure TXMLIni.WriteFloat(const Section, Name: string; Value: Double);
begin
  inherited;
end;

procedure TXMLIni.WriteInteger(const Section, Ident: string; Value: Integer);
begin
  inherited;
end;

procedure TXMLIni.WriteString(const Section, Ident, Value: String);
begin
  SetValue(Section, Ident, Value);  
end;

procedure TXMLIni.WriteTime(const Section, Name: string; Value: TDateTime);
begin
  inherited;
end;

end.
