unit uJSDOM;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  uJSHelper,
  uJSDirect,
  mshtml,
  TypInfo,
  Windows,
  Variants;

type
  IjsElement = interface
  ['{5B6E31D8-D9F0-4D5F-B036-351D81BD8761}']
    function get_JSVar: String;
    function get_GUID: String;
  end;

  TjsDOMObject = class(TjsdObjectEx, IDispatch, IjsElement)
  private
    FRefCount: Integer;
  protected
    {$REGION 'IjsElement'}
    function get_JSVar: String;
    function get_GUID: String;
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
  
    constructor Create(AApplication : TjsdApplication; ACreateCommand : String); override;
  end;
//
//  TjsHTMLElementCollection = class(TjsDOMObject, IHTMLElementCollection, IHTMLElementCollection2, IHTMLElementCollection3)
//
//  end;

  TjsHTMLAttributeCollection = class(TjsDOMObject, IHTMLAttributeCollection, IHTMLAttributeCollection2)
  protected
    {$REGION 'IHTMLAttributeCollection'}     
    function Get_length: Integer; safecall;
    function Get__newEnum: IUnknown; safecall;
    function item(const name: OleVariant): IDispatch; safecall;          
    {$ENDREGION}     
    
    {$REGION 'IHTMLAttributeCollection'}     
    function getNamedItem(const bstrName: WideString): IHTMLDOMAttribute; safecall;
    function setNamedItem(const ppNode: IHTMLDOMAttribute): IHTMLDOMAttribute; safecall;
    function removeNamedItem(const bstrName: WideString): IHTMLDOMAttribute; safecall;
    {$ENDREGION}   
  end;

  TjsHTMLDOMNode = class(TjsDOMObject, IHTMLDOMNode, IHTMLDOMNode2)
  protected
    {$REGION 'IHTMLDOMNode'}
    function Get_nodeType: Integer; safecall;
    function Get_parentNode: IHTMLDOMNode; safecall;
    function hasChildNodes: WordBool; safecall;
    function Get_childNodes: IDispatch; safecall;
    function Get_attributes: IDispatch; safecall;
    function insertBefore(const newChild: IHTMLDOMNode; refChild: OleVariant): IHTMLDOMNode; safecall;
    function removeChild(const oldChild: IHTMLDOMNode): IHTMLDOMNode; safecall;
    function replaceChild(const newChild: IHTMLDOMNode; const oldChild: IHTMLDOMNode): IHTMLDOMNode; safecall;
    function cloneNode(fDeep: WordBool): IHTMLDOMNode; safecall;
    function removeNode(fDeep: WordBool): IHTMLDOMNode; safecall;
    function swapNode(const otherNode: IHTMLDOMNode): IHTMLDOMNode; safecall;
    function replaceNode(const replacement: IHTMLDOMNode): IHTMLDOMNode; safecall;
    function appendChild(const newChild: IHTMLDOMNode): IHTMLDOMNode; safecall;
    function Get_nodeName: WideString; safecall;
    procedure Set_nodeValue(p: OleVariant); safecall;
    function Get_nodeValue: OleVariant; safecall;
    function Get_firstChild: IHTMLDOMNode; safecall;
    function Get_lastChild: IHTMLDOMNode; safecall;
    function Get_previousSibling: IHTMLDOMNode; safecall;
    function Get_nextSibling: IHTMLDOMNode; safecall;
    {$ENDREGION}

    {$REGION 'IHTMLDOMNode2'}
    function Get_ownerDocument: IDispatch; safecall;
    {$ENDREGION}
  end;

  TjsHTMLDOMAttribute = class(TjsHTMLDOMNode, IHTMLDOMAttribute)
  protected
    {$REGION 'IHTMLDOMAttribute'}
    function Get_nodeName: WideString; safecall;
    procedure Set_nodeValue(p: OleVariant); safecall;
    function Get_nodeValue: OleVariant; safecall;
    function Get_specified: WordBool; safecall;
    {$ENDREGION}

    {$REGION 'IHTMLDOMAttribute2'}


    {$ENDREGION}
  end;

  TjsDOMDocument = class(TjsHTMLDOMNode, IHTMLDocument)
  protected
    function Get_Script: IDispatch; safecall;
  end;

implementation

{ TjsDOMObject }

constructor TjsDOMObject.Create(AApplication: TjsdApplication;
  ACreateCommand: String);
begin
  inherited;
  if StrToBool(FApplication.Exec('!'+_JSVar,True)) then
    Abort;
end;

function TjsDOMObject.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount,
  LocaleID: Integer; DispIDs: Pointer): HResult;
begin 
  Result:=E_NOTIMPL;
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

{ TjsHTMLDOMNode }

function TjsHTMLDOMNode.appendChild(const newChild: IHTMLDOMNode): IHTMLDOMNode;
begin
  Result:=TjsHTMLDOMNode.Create(FApplication, _JSVar+'.appendChild('+Self.GetDOMObjectJsName(newChild)+')');
end;

function TjsHTMLDOMNode.cloneNode(fDeep: WordBool): IHTMLDOMNode;
begin
  Result:=TjsHTMLDOMNode.Create(FApplication, _JSVar+'.cloneNode('+LowerCase(BoolToStr(fDeep))+')');
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

function TjsHTMLDOMNode.Get_attributes: IDispatch;
begin
  Result:=TjsHTMLDOMAttribute.Create(FApplication, _JSVar+'.attributes');
end;

function TjsHTMLDOMNode.Get_childNodes: IDispatch;
begin

end;

function TjsHTMLDOMNode.Get_firstChild: IHTMLDOMNode;
begin

end;

function TjsHTMLDOMNode.Get_lastChild: IHTMLDOMNode;
begin

end;

function TjsHTMLDOMNode.Get_nextSibling: IHTMLDOMNode;
begin

end;

function TjsHTMLDOMNode.Get_nodeName: WideString;
begin

end;

function TjsHTMLDOMNode.Get_nodeType: Integer;
begin

end;

function TjsHTMLDOMNode.Get_nodeValue: OleVariant;
begin

end;

function TjsHTMLDOMNode.Get_ownerDocument: IDispatch;
begin

end;

function TjsHTMLDOMNode.Get_parentNode: IHTMLDOMNode;
begin

end;

function TjsHTMLDOMNode.Get_previousSibling: IHTMLDOMNode;
begin

end;

function TjsHTMLDOMNode.hasChildNodes: WordBool;
begin

end;

function TjsHTMLDOMNode.insertBefore(const newChild: IHTMLDOMNode;
  refChild: OleVariant): IHTMLDOMNode;
begin

end;

function TjsHTMLDOMNode.removeChild(const oldChild: IHTMLDOMNode): IHTMLDOMNode;
begin

end;

function TjsHTMLDOMNode.removeNode(fDeep: WordBool): IHTMLDOMNode;
begin

end;

function TjsHTMLDOMNode.replaceChild(const newChild,
  oldChild: IHTMLDOMNode): IHTMLDOMNode;
begin

end;

function TjsHTMLDOMNode.replaceNode(
  const replacement: IHTMLDOMNode): IHTMLDOMNode;
begin

end;

procedure TjsHTMLDOMNode.Set_nodeValue(p: OleVariant);
begin

end;

function TjsHTMLDOMNode.swapNode(const otherNode: IHTMLDOMNode): IHTMLDOMNode;
begin

end;

{ TjsHTMLDOMAttribute }

function TjsHTMLDOMAttribute.Get_nodeName: WideString;
begin
  GetPropertyValue('nodeName', Result);
end;

function TjsHTMLDOMAttribute.Get_nodeValue: OleVariant;
begin
  GetPropertyValue('nodeValue', Result);
end;

function TjsHTMLDOMAttribute.Get_specified: WordBool;
begin
  GetPropertyValue('specified', Result);
end;

procedure TjsHTMLDOMAttribute.Set_nodeValue(p: OleVariant);
begin

end;

{ TjsDOMDocument }

function TjsDOMDocument.Get_Script: IDispatch;
begin

end;

{ TjsHTMLAttributeCollection }

function TjsHTMLAttributeCollection.getNamedItem(
  const bstrName: WideString): IHTMLDOMAttribute;
begin
  Result:=TjsHTMLDOMAttribute.Create(FApplication, _JSVar+'.getNamedItem("'+bstrName+'")');
end;

function TjsHTMLAttributeCollection.Get_length: Integer;
begin
  GetPropertyValue('length', Result);
end;

function TjsHTMLAttributeCollection.Get__newEnum: IUnknown;
begin
  Result:=nil;
end;

function TjsHTMLAttributeCollection.item(const name: OleVariant): IDispatch;
begin
  Result:=TjsHTMLDOMAttribute.Create(FApplication, _JSVar+'.item("'+name+'")');
end;

function TjsHTMLAttributeCollection.removeNamedItem(
  const bstrName: WideString): IHTMLDOMAttribute;
begin
  Result:=TjsHTMLDOMAttribute.Create(FApplication, _JSVar+'.removeNamedItem("'+bstrName+'")');
end;

function TjsHTMLAttributeCollection.setNamedItem(
  const ppNode: IHTMLDOMAttribute): IHTMLDOMAttribute;
begin
  Result:=TjsHTMLDOMAttribute.Create(FApplication, _JSVar+'.setNamedItem('+GetDOMObjectJsName(ppNode)+')');

end;

end.
