
{************************************************************************************}
{                                                                                    }
{                                  XML-Datenbindung                                  }
{                                                                                    }
{         Generiert am: 09.01.2006 08:03:19                                          }
{       Generiert von: D:\DelphiComponents\ResEd\ResourceUtils\resx.xdb              }
{   Einstellungen gespeichert in: D:\DelphiComponents\ResEd\ResourceUtils\resx.xdb   }
{                                                                                    }
{************************************************************************************}

unit resx;

interface

uses xmldom, XMLDoc, XMLIntf;

type

{ Forward-Deklarationen }

  IXMLRoot = interface;
  IXMLData = interface;
  IXMLDataList = interface;
  IXMLResheader = interface;
  IXMLResheaderList = interface;

{ IXMLRoot }

  IXMLRoot = interface(IXMLNode)
    ['{F6D47210-A882-4C14-8DCB-EE83B19BB433}']
    { Eigenschaftszugriff }
    function Get_Data: IXMLDataList;
    function Get_Resheader: IXMLResheaderList;
    { Methoden & Eigenschaften }
    property Data: IXMLDataList read Get_Data;
    property Resheader: IXMLResheaderList read Get_Resheader;
  end;

{ IXMLData }

  IXMLData = interface(IXMLNode)
    ['{CEF4111D-FA70-47E6-9FC0-96C219125D3E}']
    { Eigenschaftszugriff }
    function Get_Name: WideString;
    function Get_Type_: WideString;
    function Get_Mimetype: WideString;
    function Get_Value: WideString;
    function Get_Comment: WideString;
    procedure Set_Name(Value: WideString);
    procedure Set_Type_(Value: WideString);
    procedure Set_Mimetype(Value: WideString);
    procedure Set_Value(Value: WideString);
    procedure Set_Comment(Value: WideString);
    { Methoden & Eigenschaften }
    property Name: WideString read Get_Name write Set_Name;
    property Type_: WideString read Get_Type_ write Set_Type_;
    property Mimetype: WideString read Get_Mimetype write Set_Mimetype;
    property Value: WideString read Get_Value write Set_Value;
    property Comment: WideString read Get_Comment write Set_Comment;
  end;

{ IXMLDataList }

  IXMLDataList = interface(IXMLNodeCollection)
    ['{1EEF0C06-95D0-413D-A4B6-B7475A767604}']
    { Methoden & Eigenschaften }
    function Add: IXMLData;
    function Insert(const Index: Integer): IXMLData;
    function Get_Item(Index: Integer): IXMLData;
    property Items[Index: Integer]: IXMLData read Get_Item; default;
  end;

{ IXMLResheader }

  IXMLResheader = interface(IXMLNode)
    ['{08B774F5-C6D9-4EAE-AEC6-91B675990026}']
    { Eigenschaftszugriff }
    function Get_Name: WideString;
    function Get_Value: WideString;
    procedure Set_Name(Value: WideString);
    procedure Set_Value(Value: WideString);
    { Methoden & Eigenschaften }
    property Name: WideString read Get_Name write Set_Name;
    property Value: WideString read Get_Value write Set_Value;
  end;

{ IXMLResheaderList }

  IXMLResheaderList = interface(IXMLNodeCollection)
    ['{BF321250-E085-456C-9849-AD653039237F}']
    { Methoden & Eigenschaften }
    function Add: IXMLResheader;
    function Insert(const Index: Integer): IXMLResheader;
    function Get_Item(Index: Integer): IXMLResheader;
    property Items[Index: Integer]: IXMLResheader read Get_Item; default;
  end;

{ Forward-Deklarationen }

  TXMLRoot = class;
  TXMLData = class;
  TXMLDataList = class;
  TXMLResheader = class;
  TXMLResheaderList = class;

{ TXMLRoot }

  TXMLRoot = class(TXMLNode, IXMLRoot)
  private
    FData: IXMLDataList;
    FResheader: IXMLResheaderList;
  protected
    { IXMLRoot }
    function Get_Data: IXMLDataList;
    function Get_Resheader: IXMLResheaderList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLData }

  TXMLData = class(TXMLNode, IXMLData)
  protected
    { IXMLData }
    function Get_Name: WideString;
    function Get_Type_: WideString;
    function Get_Mimetype: WideString;
    function Get_Value: WideString;
    function Get_Comment: WideString;
    procedure Set_Name(Value: WideString);
    procedure Set_Type_(Value: WideString);
    procedure Set_Mimetype(Value: WideString);
    procedure Set_Value(Value: WideString);
    procedure Set_Comment(Value: WideString);
  end;

{ TXMLDataList }

  TXMLDataList = class(TXMLNodeCollection, IXMLDataList)
  protected
    { IXMLDataList }
    function Add: IXMLData;
    function Insert(const Index: Integer): IXMLData;
    function Get_Item(Index: Integer): IXMLData;
  end;

{ TXMLResheader }

  TXMLResheader = class(TXMLNode, IXMLResheader)
  protected
    { IXMLResheader }
    function Get_Name: WideString;
    function Get_Value: WideString;
    procedure Set_Name(Value: WideString);
    procedure Set_Value(Value: WideString);
  end;

{ TXMLResheaderList }

  TXMLResheaderList = class(TXMLNodeCollection, IXMLResheaderList)
  protected
    { IXMLResheaderList }
    function Add: IXMLResheader;
    function Insert(const Index: Integer): IXMLResheader;
    function Get_Item(Index: Integer): IXMLResheader;
  end;

{ Globale Funktionen }

function GetResX(Doc: IXMLDocument): IXMLRoot;
function LoadResX(const FileName: WideString): IXMLRoot;
function NewResX: IXMLRoot;

const
  TargetNamespace = '';

implementation

{ Globale Funktionen }

function GetResX(Doc: IXMLDocument): IXMLRoot;
begin
  Result := Doc.GetDocBinding('root', TXMLRoot, TargetNamespace) as IXMLRoot;
end;

function LoadResX(const FileName: WideString): IXMLRoot;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('root', TXMLRoot, TargetNamespace) as IXMLRoot;
end;

function NewResX: IXMLRoot;
begin
  Result := NewXMLDocument.GetDocBinding('root', TXMLRoot, TargetNamespace) as IXMLRoot;
end;

{ TXMLRoot }

procedure TXMLRoot.AfterConstruction;
begin
  RegisterChildNode('data', TXMLData);
  RegisterChildNode('resheader', TXMLResheader);
  FData := CreateCollection(TXMLDataList, IXMLData, 'data') as IXMLDataList;
  FResheader := CreateCollection(TXMLResheaderList, IXMLResheader, 'resheader') as IXMLResheaderList;
  inherited;
end;

function TXMLRoot.Get_Data: IXMLDataList;
begin
  Result := FData;
end;

function TXMLRoot.Get_Resheader: IXMLResheaderList;
begin
  Result := FResheader;
end;

{ TXMLData }

function TXMLData.Get_Name: WideString;
begin
  Result := AttributeNodes['name'].Text;
end;

procedure TXMLData.Set_Name(Value: WideString);
begin
  SetAttribute('name', Value);
end;

function TXMLData.Get_Type_: WideString;
begin
  Result := AttributeNodes['type'].Text;
end;

procedure TXMLData.Set_Type_(Value: WideString);
begin
  SetAttribute('type', Value);
end;

function TXMLData.Get_Mimetype: WideString;
begin
  Result := AttributeNodes['mimetype'].Text;
end;

procedure TXMLData.Set_Mimetype(Value: WideString);
begin
  SetAttribute('mimetype', Value);
end;

function TXMLData.Get_Value: WideString;
begin
  Result := ChildNodes['value'].Text;
end;

procedure TXMLData.Set_Value(Value: WideString);
begin
  ChildNodes['value'].NodeValue := Value;
end;

function TXMLData.Get_Comment: WideString;
begin
  Result := ChildNodes['comment'].Text;
end;

procedure TXMLData.Set_Comment(Value: WideString);
begin
  ChildNodes['comment'].NodeValue := Value;
end;

{ TXMLDataList }

function TXMLDataList.Add: IXMLData;
begin
  Result := AddItem(-1) as IXMLData;
end;

function TXMLDataList.Insert(const Index: Integer): IXMLData;
begin
  Result := AddItem(Index) as IXMLData;
end;
function TXMLDataList.Get_Item(Index: Integer): IXMLData;
begin
  Result := List[Index] as IXMLData;
end;

{ TXMLResheader }

function TXMLResheader.Get_Name: WideString;
begin
  Result := AttributeNodes['name'].Text;
end;

procedure TXMLResheader.Set_Name(Value: WideString);
begin
  SetAttribute('name', Value);
end;

function TXMLResheader.Get_Value: WideString;
begin
  Result := ChildNodes['value'].Text;
end;

procedure TXMLResheader.Set_Value(Value: WideString);
begin
  ChildNodes['value'].NodeValue := Value;
end;

{ TXMLResheaderList }

function TXMLResheaderList.Add: IXMLResheader;
begin
  Result := AddItem(-1) as IXMLResheader;
end;

function TXMLResheaderList.Insert(const Index: Integer): IXMLResheader;
begin
  Result := AddItem(Index) as IXMLResheader;
end;
function TXMLResheaderList.Get_Item(Index: Integer): IXMLResheader;
begin
  Result := List[Index] as IXMLResheader;
end;

end.