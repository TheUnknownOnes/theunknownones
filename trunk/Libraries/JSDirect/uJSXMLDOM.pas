unit uJSXMLDOM;

interface

uses
  xmldom,
  uJSDirect, SysUtils;

type
  TjsDOMObject = class;
  TjsDOMObjectClass = class of TjsDOMObject;

  IjsElement = interface
  ['{5B6E31D8-D9F0-4D5F-B036-351D81BD8761}']
    function get_JSVar: String;
    function get_GUID: String;
    function get_ClassOfImplementingObject: TjsDOMObjectClass;

    procedure SetPropertyValue(AProperty : String; AValue : Variant; AValueIsJSCode: Boolean = False);

    function GetPropertyValue(AProperty : String) : String; overload;
    procedure GetPropertyValue(AProperty : String; out AValue : String; ADefault : String = ''; ACheckNullUndefined : Boolean = true); overload;
    procedure GetPropertyValue(AProperty : String; out AValue : WideString; ADefault : String = ''; ACheckNullUndefined : Boolean = true); overload;
    procedure GetPropertyValue(AProperty : String; out AValue : OleVariant; ADefault : String = ''; ACheckNullUndefined : Boolean = true); overload;
    procedure GetPropertyValue(AProperty : String; out AValue : Smallint; ADefault : Integer = 0); overload;
    procedure GetPropertyValue(AProperty : String; out AValue : Integer; ADefault : Integer = 0); overload;
    procedure GetPropertyValue(AProperty : String; out AValue : Int64; ADefault : Integer = 0); overload;
    procedure GetPropertyValue(AProperty : String; out AValue : Double; ADefault : Double = 0); overload;
    procedure GetPropertyValue(AProperty : String; out AValue : Single; ADefault : Double = 0); overload;
    procedure GetPropertyValue(AProperty : String; out AValue : Boolean; ADefault : Boolean = false); overload;
    procedure GetPropertyValue(AProperty : String; out AValue : WordBool; ADefault : Boolean = false); overload;
  end;

  TjsDOMObject = class(TjsdBaseObject, IInterface, IDispatch, IjsElement)
  private
    FRefCount: Integer;
  protected
    {$REGION 'IjsElement'}
    function get_JSVar: String;
    function get_GUID: String;
    function get_ClassOfImplementingObject: TjsDOMObjectClass;
    {$ENDREGION}

    {$REGION 'IInterface'}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    {$ENDREGION}

    {$REGION 'IDispatch'}
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    {$ENDREGION}
  public
    class function GetDOMObjectJsName(ANode: IDispatch): String;
    class function IsPositiveMethodResult(AResult: String): Boolean;

    //Interface specific jsDirect Functionality
    procedure GetPropertyValueIntf(AProperty : String; out AValue; AObjectClass: TjsDOMObjectClass); overload;
    function ExecMethod(AMethod : String; AObjectClass: TjsDOMObjectClass) : IInterface; overload;

    constructor Create(AApplication : TjsdApplication; ACreateCommand : String); reintroduce;
  end;

  TjsDOMImplementation = class(TjsDOMObject,IDOMImplementation)
  private
    function hasFeature(const feature, version: DOMString): WordBool;
    function createDocumentType(const qualifiedName, publicId,           { DOM Level 2 }
      systemId: DOMString): IDOMDocumentType; safecall;
    function createDocument(const namespaceURI, qualifiedName: DOMString;{ DOM Level 2 }
      doctype: IDOMDocumentType): IDOMDocument; safecall;
  end;

  TjsDOMNodeList = class(TjsDOMObject,IDOMNodeList)
  private
    function get_item(index: Integer): IDOMNode; safecall;
    function get_length: Integer; safecall;
  end;

  TjsDOMNamedNodeMap = class(TjsDOMObject,IDOMNamedNodeMap)
  private
    function get_item(index: Integer): IDOMNode; safecall;
    function get_length: Integer; safecall;
    function getNamedItem(const name: DOMString): IDOMNode; safecall;
    function getNamedItemNS(const namespaceURI, localName: DOMString): IDOMNode; safecall;
    function removeNamedItem(const name: DOMString): IDOMNode; safecall;
    function removeNamedItemNS(const namespaceURI, localName: DOMString): IDOMNode; safecall;
    function setNamedItem(const arg: IDOMNode): IDOMNode; safecall;
    function setNamedItemNS(const arg: IDOMNode): IDOMNode; safecall;
  end;

  TjsDOMNode = class(TjsDOMObject,IDOMNode)
  private
    function appendChild(const newChild: IDOMNode): IDOMNode; safecall;
    function cloneNode(deep: WordBool): IDOMNode; safecall;
    function get_attributes: IDOMNamedNodeMap; safecall;
    function get_childNodes: IDOMNodeList; safecall;
    function get_firstChild: IDOMNode; safecall;
    function get_lastChild: IDOMNode; safecall;
    function get_localName: DOMString; safecall;
    function get_namespaceURI: DOMString; safecall;
    function get_nextSibling: IDOMNode; safecall;
    function get_nodeName: DOMString; safecall;
    function get_nodeType: DOMNodeType; safecall;
    function get_nodeValue: DOMString; safecall;
    function get_ownerDocument: IDOMDocument; safecall;
    function get_parentNode: IDOMNode; safecall;
    function get_prefix: DOMString; safecall;
    function get_previousSibling: IDOMNode; safecall;
    function hasChildNodes: WordBool; safecall;
    function insertBefore(const newChild, refChild: IDOMNode): IDOMNode; safecall;
    function removeChild(const childNode: IDOMNode): IDOMNode; safecall;
    function replaceChild(const newChild, oldChild: IDOMNode): IDOMNode; safecall;
    function supports(const feature, version: DOMString): WordBool;
    procedure normalize; safecall;
    procedure set_nodeValue(value: DOMString); safecall;
  end;

  TjsDOMCharacterData = class(TjsDOMNode,IDOMCharacterData)
  private
    function get_data: DOMString; safecall;
    function get_length: Integer; safecall;
    function substringData(offset, count: Integer): DOMString; safecall;
    procedure appendData(const data: DOMString); safecall;
    procedure deleteData(offset, count: Integer); safecall;
    procedure insertData(offset: Integer; const data: DOMString); safecall;
    procedure replaceData(offset, count: Integer; const data: DOMString); safecall;
    procedure set_data(const data: DOMString); safecall;
  end;

  TjsDOMAttr = class(TjsDOMNode,IDOMAttr)
  private
    function get_name: DOMString; safecall;
    function get_ownerElement: IDOMElement; safecall;
    function get_specified: WordBool; safecall;
    function get_value: DOMString; safecall;
    procedure set_value(const attributeValue: DOMString); safecall;
  end;

  TjsDOMElement = class(TjsDOMNode,IDOMElement)
  private
    function get_tagName: DOMString; safecall;
    function getAttribute(const name: DOMString): DOMString; safecall;
    function getAttributeNode(const name: DOMString): IDOMAttr; safecall;
    function getAttributeNodeNS(const namespaceURI, localName: DOMString): IDOMAttr; safecall;
    function getAttributeNS(const namespaceURI, localName: DOMString): DOMString; safecall;
    function getElementsByTagName(const name: DOMString): IDOMNodeList; safecall;
    function getElementsByTagNameNS(const namespaceURI, localName: DOMString): IDOMNodeList; safecall;
    function hasAttribute(const name: DOMString): WordBool; safecall;
    function hasAttributeNS(const namespaceURI, localName: DOMString): WordBool; safecall;
    function removeAttributeNode(const oldAttr: IDOMAttr): IDOMAttr; safecall;
    function setAttributeNode(const newAttr: IDOMAttr): IDOMAttr; safecall;
    function setAttributeNodeNS(const newAttr: IDOMAttr): IDOMAttr; safecall;
    procedure removeAttribute(const name: DOMString); safecall;
    procedure removeAttributeNS(const namespaceURI, localName: DOMString); safecall;
    procedure setAttribute(const name, value: DOMString); safecall;
    procedure setAttributeNS(const namespaceURI, qulifiedName, value: DOMString); safecall;
  end;

  TjsDOMText = class(TjsDOMCharacterData,IDOMText)
  private
    function splitText(offset: Integer): IDOMText; safecall;
  end;

  TjsDOMComment = class(TjsDOMCharacterData,IDOMComment)
  private
  end;

  TjsDOMCDATASection = class(TjsDOMText,IDOMCDATASection)
  private
  end;

  TjsDOMDocumentType = class(TjsDOMNode,IDOMDocumentType)
  private
    function get_entities: IDOMNamedNodeMap; safecall;
    function get_internalSubset: DOMString; safecall;
    function get_name: DOMString; safecall;
    function get_notations: IDOMNamedNodeMap; safecall;
    function get_publicId: DOMString; safecall;
    function get_systemId: DOMString; safecall;
  end;

  TjsDOMNotation = class(TjsDOMNode,IDOMNotation)
  private
    function get_publicId: DOMString; safecall;
    function get_systemId: DOMString; safecall;
  end;

  TjsDOMEntity = class(TjsDOMNode,IDOMEntity)
  private
    function get_notationName: DOMString; safecall;
    function get_publicId: DOMString; safecall;
    function get_systemId: DOMString; safecall;
  end;

  TjsDOMEntityReference = class(TjsDOMNode,IDOMEntityReference)
  private
  end;

  TjsDOMProcessingInstruction = class(TjsDOMNode,IDOMProcessingInstruction)
  private
    function get_data: DOMString; safecall;
    function get_target: DOMString; safecall;
    procedure set_data(const value: DOMString); safecall;
  end;

  TjsDOMDocumentFragment = class(TjsDOMNode,IDOMDocumentFragment)
  private
  end;

  TjsDOMDocument = class(TjsDOMNode,IDOMDocument)
  private
    function createAttribute(const name: DOMString): IDOMAttr; safecall;
    function createAttributeNS(const namespaceURI, { DOM Level 2 } qualifiedName: DOMString): IDOMAttr; safecall;
    function createCDATASection(const data: DOMString): IDOMCDATASection; safecall;
    function createComment(const data: DOMString): IDOMComment; safecall;
    function createDocumentFragment: IDOMDocumentFragment; safecall;
    function createElement(const tagName: DOMString): IDOMElement; safecall;
    function createElementNS(const namespaceURI, { DOM Level 2 } qualifiedName: DOMString): IDOMElement; safecall;
    function createEntityReference(const name: DOMString): IDOMEntityReference; safecall;
    function createProcessingInstruction(const target, data: DOMString): IDOMProcessingInstruction; safecall;
    function createTextNode(const data: DOMString): IDOMText; safecall;
    function get_doctype: IDOMDocumentType; safecall;
    function get_documentElement: IDOMElement; safecall;
    function get_domImplementation: IDOMImplementation; safecall;
    function getElementById(const elementId: DOMString): IDOMElement; safecall;
    function getElementsByTagName(const tagName: DOMString): IDOMNodeList; safecall;
    function getElementsByTagNameNS(const namespaceURI, { DOM Level 2 } localName: DOMString): IDOMNodeList; safecall;
    function importNode(importedNode: IDOMNode; deep: WordBool): IDOMNode; safecall;
    procedure set_documentElement(const Element: IDOMElement); safecall;
  end;

  TjsDOMNodeEx = class(TjsDOMNode,IDOMNodeEx)
  private
    function get_text: DOMString; safecall;
    function get_xml: DOMString; safecall;
    procedure set_text(const Value: DOMString); safecall;
    procedure transformNode(const stylesheet: IDOMNode; const output: IDOMDocument); overload; safecall;
    procedure transformNode(const stylesheet: IDOMNode; var output: WideString); overload; safecall;
  end;

  TjsDOMNodeSelect = class(TjsDOMObject,IDOMNodeSelect)
  private
    function selectNode(const nodePath: WideString): IDOMNode; safecall;
    function selectNodes(const nodePath: WideString): IDOMNodeList; safecall;
  end;

  TjsDOMXSLProcessor = class(TjsDOMObject,IDOMXSLProcessor)
  private
    function Get_input: OleVariant; safecall;
    function Get_output: OleVariant; safecall;
    function Get_stylesheet: IDOMNode; safecall;
    function transform: WordBool; safecall;
    procedure reset; safecall;
    procedure Set_input(const value: OleVariant); safecall;
    procedure Set_output(const value: OleVariant); safecall;
    procedure setParameter(const Name: DOMString; Value: OleVariant; const namespaceURI: DOMString); safecall;
  end;

  function ToJSCodeEx(AValue : Variant) : String;

implementation

uses
  Windows, uJSHelper, Variants;

function ToJSCodeEx(AValue : Variant) : String;
var
  elem : IjsElement;
begin
  if (VarType(AValue)=varUnknown) and Supports(AValue, IjsElement, elem)  then
    Result:=elem.get_JSVar
  else
    Result:=ToJSCode(AValue);
end;

{ TjsDOMObject }

constructor TjsDOMObject.Create(AApplication: TjsdApplication;
  ACreateCommand: String);
begin
  inherited Create(AApplication);

  if FApplication.Exec(_JSVar + '=' + ACreateCommand, True) = 'null' then
    Abort;
end;

function TjsDOMObject.ExecMethod(AMethod: String;
  AObjectClass: TjsDOMObjectClass): IInterface;
var
  obj : TjsDOMObject;
begin
  if Copy(AMethod, Length(AMethod), 1) <> ')' then
    AMethod := AMethod + '()';

  obj:=AObjectClass.Create(FApplication, _JSVar + '.' + AMethod);
  if not Obj.GetInterface(IInterface, Result) then
    Result:=nil;
end;

class function TjsDOMObject.GetDOMObjectJsName(ANode: IDispatch): String;
var
  el : IjsElement;
begin
  if Assigned(ANode) and SysUtils.Supports(ANode, IjsElement, el) then
    Result:=el.get_JSVar
  else
    Result:='';
end;

function TjsDOMObject.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount,
  LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result:=E_NOTIMPL;
end;

procedure TjsDOMObject.GetPropertyValueIntf(AProperty: String; out AValue; AObjectClass: TjsDOMObjectClass);
begin
  AObjectClass.Create(FApplication, _JSVar + '.' + AProperty).GetInterface(IInterface, AValue);
end;

function TjsDOMObject.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result:=E_NOTIMPL;
end;

function TjsDOMObject.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result:=E_NOTIMPL;
end;

function TjsDOMObject.get_ClassOfImplementingObject: TjsDOMObjectClass;
begin
  Result:=TjsDOMObjectClass(Self.ClassType);
end;

function TjsDOMObject.get_GUID: String;
begin
  Result:=_GUID;
end;

function TjsDOMObject.get_JSVar: String;
begin
  Result:=_JSVar;
end;

function TjsDOMObject.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
begin
  Result:=E_NOTIMPL;
end;

class function TjsDOMObject.IsPositiveMethodResult(AResult: String): Boolean;
begin
  Result:=StrToBool(AResult);
end;

function TjsDOMObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TjsDOMObject._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TjsDOMObject._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

{ TjsDOMNodeList }

function TjsDOMNodeList.get_item(index: Integer): IDOMNode; safecall;
begin
  Result:=ExecMethod('item('+ToJSCodeEx(index)+')', TjsDOMNode) as IDOMNode;
end;

function TjsDOMNodeList.get_length: Integer; safecall;
begin
  GetPropertyValue('length', Result)
end;

{ TjsDOMNamedNodeMap }

function TjsDOMNamedNodeMap.get_item(index: Integer): IDOMNode; safecall;
begin
  Result:=ExecMethod('item('+ToJSCodeEx(index)+')', TjsDOMNode) as IDOMNode;
end;

function TjsDOMNamedNodeMap.get_length: Integer; safecall;
begin
  GetPropertyValue('length', Result)
end;

function TjsDOMNamedNodeMap.getNamedItem(const name: DOMString): IDOMNode; safecall;
begin
  Result:=ExecMethod('getNamedItem('+ToJSCodeEx(name)+')', TjsDOMNode) as IDOMNode;
end;

function TjsDOMNamedNodeMap.getNamedItemNS(const namespaceURI, localName: DOMString): IDOMNode; safecall;
begin
  Result:=ExecMethod('getNamedItemNS('+ToJSCodeEx(namespaceURI)+','+ToJSCodeEx(localName)+')', TjsDOMNode) as IDOMNode;
end;

function TjsDOMNamedNodeMap.removeNamedItem(const name: DOMString): IDOMNode; safecall;
begin
  Result:=ExecMethod('removeNamedItem('+ToJSCodeEx(name)+')', TjsDOMNode) as IDOMNode;
end;

function TjsDOMNamedNodeMap.removeNamedItemNS(const namespaceURI, localName: DOMString): IDOMNode; safecall;
begin
  Result:=ExecMethod('removeNamedItemNS('+ToJSCodeEx(namespaceURI)+','+ToJSCodeEx(localName)+')', TjsDOMNode) as IDOMNode;
end;

function TjsDOMNamedNodeMap.setNamedItem(const arg: IDOMNode): IDOMNode; safecall;
begin
  Result:=ExecMethod('setNamedItem('+ToJSCodeEx(arg)+')', TjsDOMNode) as IDOMNode;
end;

function TjsDOMNamedNodeMap.setNamedItemNS(const arg: IDOMNode): IDOMNode; safecall;
begin
  Result:=ExecMethod('setNamedItemNS('+ToJSCodeEx(arg)+')', TjsDOMNode) as IDOMNode;
end;

{ TjsDOMNode }

function TjsDOMNode.appendChild(const newChild: IDOMNode): IDOMNode; safecall;

begin
  Result:=ExecMethod('appendChild('+ToJSCodeEx(newChild)+')', TjsDOMNode) as IDOMNode;
end;

function TjsDOMNode.cloneNode(deep: WordBool): IDOMNode; safecall;
begin
  Result:=ExecMethod('cloneNode('+ToJSCodeEx(deep)+')', self.get_ClassOfImplementingObject) as IDOMNode;
end;

function TjsDOMNode.get_attributes: IDOMNamedNodeMap; safecall;
begin
  GetPropertyValueIntf('attributes', Result, TjsDOMNamedNodeMap);
end;

function TjsDOMNode.get_childNodes: IDOMNodeList; safecall;
begin
  GetPropertyValueIntf('childNodes', Result, TjsDOMNodeList);
end;

function TjsDOMNode.get_firstChild: IDOMNode; safecall;
begin
  GetPropertyValueIntf('firstChild', Result, TjsDOMNode);
end;

function TjsDOMNode.get_lastChild: IDOMNode; safecall;
begin
  GetPropertyValueIntf('lastChild', Result, TjsDOMNode)
end;

function TjsDOMNode.get_localName: DOMString; safecall;
begin
  GetPropertyValue('localName', Result)
end;

function TjsDOMNode.get_namespaceURI: DOMString; safecall;
begin
  GetPropertyValue('namespaceURI', Result)
end;

function TjsDOMNode.get_nextSibling: IDOMNode; safecall;
begin
  GetPropertyValueIntf('nextSibling', Result, TjsDOMNode);
end;

function TjsDOMNode.get_nodeName: DOMString; safecall;
begin
  GetPropertyValue('nodeName', Result)
end;

function TjsDOMNode.get_nodeType: DOMNodeType; safecall;
begin
  GetPropertyValue('nodeType', Result);
end;

function TjsDOMNode.get_nodeValue: DOMString; safecall;
begin
  GetPropertyValue('nodeValue', Result)
end;

function TjsDOMNode.get_ownerDocument: IDOMDocument; safecall;
begin
  GetPropertyValueIntf('ownerDocument', Result, TjsDOMDocument);
end;

function TjsDOMNode.get_parentNode: IDOMNode; safecall;
begin
  GetPropertyValueIntf('parentNode', Result, TjsDOMNode);
end;

function TjsDOMNode.get_prefix: DOMString; safecall;
begin
  GetPropertyValue('prefix', Result)
end;

function TjsDOMNode.get_previousSibling: IDOMNode; safecall;
begin
  GetPropertyValueIntf('previousSibling', Result, TjsDOMNode)
end;

function TjsDOMNode.hasChildNodes: WordBool; safecall;
begin
  Result:=IsPositiveMethodResult(ExecMethod('hasChildNodes('+')', True));
end;

function TjsDOMNode.insertBefore(const newChild, refChild: IDOMNode): IDOMNode; safecall;
begin
  Result:=ExecMethod('insertBefore('+ToJSCodeEx(newChild)+','+ToJSCodeEx(refChild)+')', TjsDOMNode) as IDOMNode;
end;

function TjsDOMNode.removeChild(const childNode: IDOMNode): IDOMNode; safecall;
begin
  Result:=ExecMethod('removeChild('+ToJSCodeEx(childNode)+')', TjsDOMNode) as IDOMNode;
end;

function TjsDOMNode.replaceChild(const newChild, oldChild: IDOMNode): IDOMNode; safecall;
begin
  Result:=ExecMethod('replaceChild('+ToJSCodeEx(newChild)+','+ToJSCodeEx(oldChild)+')', TjsDOMNode) as IDOMNode;
end;

function TjsDOMNode.supports(const feature, version: DOMString): WordBool;
begin
  Result:=IsPositiveMethodResult(ExecMethod('supports('+ToJSCodeEx(feature)+','+ToJSCodeEx(version)+')', True));
end;

procedure TjsDOMNode.normalize; safecall;
begin
  ExecMethod('normalize('+')');
end;

procedure TjsDOMNode.set_nodeValue(value: DOMString); safecall;
begin
  SetPropertyValue('nodeValue', value);
end;

{ TjsDOMCharacterData }

function TjsDOMCharacterData.get_data: DOMString; safecall;
begin
  GetPropertyValue('data', Result)
end;

function TjsDOMCharacterData.get_length: Integer; safecall;
begin
  GetPropertyValue('length', Result)
end;

function TjsDOMCharacterData.substringData(offset, count: Integer): DOMString; safecall;
begin
  Result:=ExecMethod('substringData('+ToJSCodeEx(offset)+','+ToJSCodeEx(count)+')', True);
end;

procedure TjsDOMCharacterData.appendData(const data: DOMString); safecall;
begin
  ExecMethod('appendData('+ToJSCodeEx(data)+')');
end;

procedure TjsDOMCharacterData.deleteData(offset, count: Integer); safecall;
begin
  ExecMethod('deleteData('+ToJSCodeEx(offset)+','+ToJSCodeEx(count)+')');
end;

procedure TjsDOMCharacterData.insertData(offset: Integer; const data: DOMString); safecall;
begin
  ExecMethod('insertData('+ToJSCodeEx(offset)+','+ToJSCodeEx(data)+')');
end;

procedure TjsDOMCharacterData.replaceData(offset, count: Integer; const data: DOMString); safecall;
begin
  ExecMethod('replaceData('+ToJSCodeEx(offset)+','+ToJSCodeEx(count)+','+ToJSCodeEx(data)+')');
end;

procedure TjsDOMCharacterData.set_data(const data: DOMString); safecall;
begin
  SetPropertyValue('data', data);
end;

{ TjsDOMAttr }

function TjsDOMAttr.get_name: DOMString; safecall;
begin
  GetPropertyValue('name', Result)
end;

function TjsDOMAttr.get_ownerElement: IDOMElement; safecall;
begin
  GetPropertyValueIntf('ownerElement', Result, TjsDOMElement)
end;

function TjsDOMAttr.get_specified: WordBool; safecall;
begin
  GetPropertyValue('specified', Result)
end;

function TjsDOMAttr.get_value: DOMString; safecall;
begin
  GetPropertyValue('value', Result)
end;

procedure TjsDOMAttr.set_value(const attributeValue: DOMString); safecall;
begin
  SetPropertyValue('value', attributeValue);
end;

{ TjsDOMElement }

function TjsDOMElement.get_tagName: DOMString; safecall;
begin
  GetPropertyValue('tagName', Result)
end;

function TjsDOMElement.getAttribute(const name: DOMString): DOMString; safecall;
begin
  Result:=ExecMethod('getAttribute('+ToJSCodeEx(name)+')', True);
end;

function TjsDOMElement.getAttributeNode(const name: DOMString): IDOMAttr; safecall;
begin
  Result:=ExecMethod('getAttributeNode('+ToJSCodeEx(name)+')', TjsDOMAttr) as IDOMAttr;
end;

function TjsDOMElement.getAttributeNodeNS(const namespaceURI, localName: DOMString): IDOMAttr; safecall;
begin
  Result:=ExecMethod('getAttributeNodeNS('+ToJSCodeEx(namespaceURI)+','+ToJSCodeEx(localName)+')', TjsDOMAttr) as IDOMAttr;
end;

function TjsDOMElement.getAttributeNS(const namespaceURI, localName: DOMString): DOMString; safecall;
begin
  Result:=ExecMethod('getAttributeNS('+ToJSCodeEx(namespaceURI)+','+ToJSCodeEx(localName)+')', True);
end;

function TjsDOMElement.getElementsByTagName(const name: DOMString): IDOMNodeList; safecall;
begin
  Result:=ExecMethod('getElementsByTagName('+ToJSCodeEx(name)+')', TjsDOMNodeList) as IDOMNodeList;
end;

function TjsDOMElement.getElementsByTagNameNS(const namespaceURI, localName: DOMString): IDOMNodeList; safecall;
begin
  Result:=ExecMethod('getElementsByTagNameNS('+ToJSCodeEx(namespaceURI)+','+ToJSCodeEx(localName)+')', TjsDOMNodeList) as IDOMNodeList;
end;

function TjsDOMElement.hasAttribute(const name: DOMString): WordBool; safecall;
begin
  Result:=IsPositiveMethodResult(ExecMethod('hasAttribute('+ToJSCodeEx(name)+')', True));
end;

function TjsDOMElement.hasAttributeNS(const namespaceURI, localName: DOMString): WordBool; safecall;
begin
  Result:=IsPositiveMethodResult(ExecMethod('hasAttributeNS('+ToJSCodeEx(namespaceURI)+','+ToJSCodeEx(localName)+')', True));
end;

function TjsDOMElement.removeAttributeNode(const oldAttr: IDOMAttr): IDOMAttr; safecall;
begin
  Result:=ExecMethod('removeAttributeNode('+ToJSCodeEx(oldAttr)+')', TjsDOMAttr) as IDOMAttr;
end;

function TjsDOMElement.setAttributeNode(const newAttr: IDOMAttr): IDOMAttr; safecall;
begin
  Result:=ExecMethod('setAttributeNode('+ToJSCodeEx(newAttr)+')', TjsDOMAttr) as IDOMAttr;
end;

function TjsDOMElement.setAttributeNodeNS(const newAttr: IDOMAttr): IDOMAttr; safecall;
begin
  Result:=ExecMethod('setAttributeNodeNS('+ToJSCodeEx(newAttr)+')', TjsDOMAttr) as IDOMAttr;
end;

procedure TjsDOMElement.removeAttribute(const name: DOMString); safecall;
begin
  ExecMethod('removeAttribute('+ToJSCodeEx(name)+')');
end;

procedure TjsDOMElement.removeAttributeNS(const namespaceURI, localName: DOMString); safecall;
begin
  ExecMethod('removeAttributeNS('+ToJSCodeEx(namespaceURI)+','+ToJSCodeEx(localName)+')');
end;

procedure TjsDOMElement.setAttribute(const name, value: DOMString); safecall;
begin
  ExecMethod('setAttribute('+ToJSCodeEx(name)+','+ToJSCodeEx(value)+')');
end;

procedure TjsDOMElement.setAttributeNS(const namespaceURI, qulifiedName, value: DOMString); safecall;
begin
  ExecMethod('setAttributeNS('+ToJSCodeEx(namespaceURI)+','+ToJSCodeEx(qulifiedName)+','+ToJSCodeEx(value)+')');
end;

{ TjsDOMText }

function TjsDOMText.splitText(offset: Integer): IDOMText; safecall;
begin
  Result:=ExecMethod('splitText('+ToJSCodeEx(offset)+')', TjsDOMText) as IDOMText;
end;

{ TjsDOMDocumentType }

function TjsDOMDocumentType.get_entities: IDOMNamedNodeMap; safecall;
begin
  GetPropertyValueIntf('entities', Result, TjsDOMNamedNodeMap)
end;

function TjsDOMDocumentType.get_internalSubset: DOMString; safecall;
begin
  GetPropertyValue('internalSubset', Result)
end;

function TjsDOMDocumentType.get_name: DOMString; safecall;
begin
  GetPropertyValue('name', Result)
end;

function TjsDOMDocumentType.get_notations: IDOMNamedNodeMap; safecall;
begin
  GetPropertyValueIntf('notations', Result, TjsDOMNamedNodeMap)
end;

function TjsDOMDocumentType.get_publicId: DOMString; safecall;
begin
  GetPropertyValue('publicId', Result)
end;

function TjsDOMDocumentType.get_systemId: DOMString; safecall;
begin
  GetPropertyValue('systemId', Result)
end;

{ TjsDOMNotation }

function TjsDOMNotation.get_publicId: DOMString; safecall;
begin
  GetPropertyValue('publicId', Result)
end;

function TjsDOMNotation.get_systemId: DOMString; safecall;
begin
  GetPropertyValue('systemId', Result)
end;

{ TjsDOMEntity }

function TjsDOMEntity.get_notationName: DOMString; safecall;
begin
  GetPropertyValue('notationName', Result)
end;

function TjsDOMEntity.get_publicId: DOMString; safecall;
begin
  GetPropertyValue('publicId', Result)
end;

function TjsDOMEntity.get_systemId: DOMString; safecall;
begin
  GetPropertyValue('systemId', Result)
end;

{ TjsDOMProcessingInstruction }

function TjsDOMProcessingInstruction.get_data: DOMString; safecall;
begin
  GetPropertyValue('data', Result)
end;

function TjsDOMProcessingInstruction.get_target: DOMString; safecall;
begin
  GetPropertyValue('target', Result)
end;

procedure TjsDOMProcessingInstruction.set_data(const value: DOMString); safecall;
begin
  SetPropertyValue('data', value);
end;

{ TjsDOMDocument }

function TjsDOMDocument.createAttribute(const name: DOMString): IDOMAttr; safecall;
begin
  Result:=ExecMethod('createAttribute('+ToJSCodeEx(name)+')', TjsDOMAttr) as IDOMAttr;
end;

function TjsDOMDocument.createAttributeNS(const namespaceURI, qualifiedName: DOMString): IDOMAttr; safecall;
begin
  Result:=ExecMethod('createAttributeNS('+ToJSCodeEx(namespaceURI)+','+ToJSCodeEx(qualifiedName)+')', TjsDOMAttr) as IDOMAttr;
end;

function TjsDOMDocument.createCDATASection(const data: DOMString): IDOMCDATASection; safecall;
begin
  Result:=ExecMethod('createCDATASection('+ToJSCodeEx(data)+')', TjsDOMCDATASection) as IDOMCDATASection;
end;

function TjsDOMDocument.createComment(const data: DOMString): IDOMComment; safecall;
begin
  Result:=ExecMethod('createComment('+ToJSCodeEx(data)+')', TjsDOMComment) as IDOMComment;
end;

function TjsDOMDocument.createDocumentFragment: IDOMDocumentFragment; safecall;
begin
  Result:=ExecMethod('createDocumentFragment('+')', TjsDOMDocumentFragment) as IDOMDocumentFragment;
end;

function TjsDOMDocument.createElement(const tagName: DOMString): IDOMElement; safecall;
begin
  Result:=ExecMethod('createElement('+ToJSCodeEx(tagName)+')', TjsDOMElement) as IDOMElement;
end;

function TjsDOMDocument.createElementNS(const namespaceURI, qualifiedName: DOMString): IDOMElement; safecall;
begin
  Result:=ExecMethod('createElementNS('+ToJSCodeEx(namespaceURI)+','+ToJSCodeEx(qualifiedName)+')', TjsDOMElement) as IDOMElement;
end;

function TjsDOMDocument.createEntityReference(const name: DOMString): IDOMEntityReference; safecall;
begin
  Result:=ExecMethod('createEntityReference('+ToJSCodeEx(name)+')', TjsDOMEntityReference) as IDOMEntityReference;
end;

function TjsDOMDocument.createProcessingInstruction(const target, data: DOMString): IDOMProcessingInstruction; safecall;
begin
  Result:=ExecMethod('createProcessingInstruction('+ToJSCodeEx(target)+','+ToJSCodeEx(data)+')', TjsDOMProcessingInstruction) as IDOMProcessingInstruction;
end;

function TjsDOMDocument.createTextNode(const data: DOMString): IDOMText; safecall;
begin
  Result:=ExecMethod('createTextNode('+ToJSCodeEx(data)+')', TjsDOMText) as IDOMText;
end;

function TjsDOMDocument.get_doctype: IDOMDocumentType; safecall;
begin
  GetPropertyValueIntf('doctype', Result, TjsDOMDocumentType)
end;

function TjsDOMDocument.get_documentElement: IDOMElement; safecall;
begin
  GetPropertyValueIntf('documentElement', Result, TjsDOMElement)
end;

function TjsDOMDocument.get_domImplementation: IDOMImplementation; safecall;
begin
  GetPropertyValueIntf('implementation', Result, TjsDOMImplementation);
end;

function TjsDOMDocument.getElementById(const elementId: DOMString): IDOMElement; safecall;
begin
  Result:=ExecMethod('getElementById('+ToJSCodeEx(elementId)+')', TjsDOMElement) as IDOMElement;
end;

function TjsDOMDocument.getElementsByTagName(const tagName: DOMString): IDOMNodeList; safecall;
begin
  Result:=ExecMethod('getElementsByTagName('+ToJSCodeEx(tagName)+')', TjsDOMNodeList) as IDOMNodeList;
end;

function TjsDOMDocument.getElementsByTagNameNS(const namespaceURI, localName: DOMString): IDOMNodeList; safecall;
begin
  Result:=ExecMethod('getElementsByTagNameNS('+ToJSCodeEx(namespaceURI)+','+ToJSCodeEx(localName)+')', TjsDOMNodeList) as IDOMNodeList;
end;

function TjsDOMDocument.importNode(importedNode: IDOMNode; deep: WordBool): IDOMNode; safecall;
begin
  Result:=ExecMethod('importNode('+ToJSCodeEx(importedNode)+','+ToJSCodeEx(deep)+')', TjsDOMNode) as IDOMNode;
end;

procedure TjsDOMDocument.set_documentElement(const Element: IDOMElement); safecall;
begin
  SetPropertyValue('documentElement', Element);
end;

{ TjsDOMNodeEx }

function TjsDOMNodeEx.get_text: DOMString; safecall;
begin
  GetPropertyValue('text', Result)
end;

function TjsDOMNodeEx.get_xml: DOMString; safecall;
begin
  GetPropertyValue('xml', Result)
end;

procedure TjsDOMNodeEx.set_text(const Value: DOMString); safecall;
begin
  SetPropertyValue('text', Value);
end;

procedure TjsDOMNodeEx.transformNode(const stylesheet: IDOMNode; const output: IDOMDocument); safecall;
begin
  ExecMethod('transformNode('+ToJSCodeEx(stylesheet)+','+ToJSCodeEx(output)+')');
end;

procedure TjsDOMNodeEx.transformNode(const stylesheet: IDOMNode; var output: WideString); safecall;
begin
  ExecMethod('transformNode('+ToJSCodeEx(stylesheet)+','+ToJSCodeEx(output)+')');
end;

{ TjsDOMNodeSelect }

function TjsDOMNodeSelect.selectNode(const nodePath: WideString): IDOMNode; safecall;
begin
  Result:=ExecMethod('selectNode('+ToJSCodeEx(nodePath)+')', TjsDOMNode) as IDOMNode;
end;

function TjsDOMNodeSelect.selectNodes(const nodePath: WideString): IDOMNodeList; safecall;
begin
  Result:=ExecMethod('selectNodes('+ToJSCodeEx(nodePath)+')', TjsDOMNodeList) as IDOMNodeList;
end;

{ TjsDOMXSLProcessor }

function TjsDOMXSLProcessor.Get_input: OleVariant; safecall;
begin
  GetPropertyValue('input', Result)
end;

function TjsDOMXSLProcessor.Get_output: OleVariant; safecall;
begin
  GetPropertyValue('output', Result)
end;

function TjsDOMXSLProcessor.Get_stylesheet: IDOMNode; safecall;
begin
  GetPropertyValueIntf('stylesheet', Result, TjsDOMNode)
end;

function TjsDOMXSLProcessor.transform: WordBool; safecall;
begin
  Result:=IsPositiveMethodResult(ExecMethod('transform('+')', True));
end;

procedure TjsDOMXSLProcessor.reset; safecall;
begin
  ExecMethod('reset('+')');
end;

procedure TjsDOMXSLProcessor.Set_input(const value: OleVariant); safecall;
begin
  SetPropertyValue('input', value);
end;

procedure TjsDOMXSLProcessor.Set_output(const value: OleVariant); safecall;
begin
  SetPropertyValue('output', value);
end;

procedure TjsDOMXSLProcessor.setParameter(const Name: DOMString; Value: OleVariant; const namespaceURI: DOMString); safecall;
begin
  ExecMethod('setParameter('+ToJSCodeEx(Name)+','+ToJSCodeEx(Value)+','+ToJSCodeEx(namespaceURI)+')');
end;

{ TjsDOMImplementation }

function TjsDOMImplementation.createDocument(const namespaceURI,
  qualifiedName: DOMString; doctype: IDOMDocumentType): IDOMDocument;
begin
  Result:=ExecMethod('createDocument('+ToJSCodeEx(namespaceURI)+','+ToJSCodeEx(qualifiedName)+','+ToJSCodeEx(doctype)+')', TjsDOMDocument) as IDOMDocument;
end;

function TjsDOMImplementation.createDocumentType(const qualifiedName, publicId,
  systemId: DOMString): IDOMDocumentType;
begin
  Result:=ExecMethod('createDocument('+ToJSCodeEx(qualifiedName)+','+ToJSCodeEx(publicId)+','+ToJSCodeEx(systemId)+')', TjsDOMDocumentType) as IDOMDocumentType;
end;

function TjsDOMImplementation.hasFeature(const feature,
  version: DOMString): WordBool;
begin
  Result:=IsPositiveMethodResult(ExecMethod('hasFeature('+ToJSCodeEx(feature)+','+ToJSCodeEx(version)+')', True));
end;

end.
