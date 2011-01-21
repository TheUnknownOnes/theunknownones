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
  Variants,
  ActiveX;

type
  IjsElement = interface
  ['{5B6E31D8-D9F0-4D5F-B036-351D81BD8761}']
    function get_JSVar: String;
    function get_GUID: String;
  end;

  TjsDOMObject = class(TjsdBaseObject, IDispatch, IjsElement)
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
    class function IsPositiveMethodResult(AResult: String): Boolean;

    constructor Create(AApplication : TjsdApplication; ACreateCommand : String); reintroduce;
  end;

  TjsHTMLElementCollection = class(TjsDOMObject, IHTMLElementCollection, IHTMLElementCollection3)
  protected
    {$REGION 'IHTMLElementCollection'}
    function toString: WideString; reintroduce; safecall;
    procedure Set_length(p: Integer); safecall;
    function Get_length: Integer; safecall;
    function Get__newEnum: IUnknown; safecall;
    function item(name: OleVariant; index: OleVariant): IDispatch; safecall;
    function tags(tagName: OleVariant): IDispatch; safecall;
    {$ENDREGION}

    {$REGION 'IHTMLElementCollection3'}
    function namedItem(const name: WideString): IDispatch; safecall;
    {$ENDREGION}
  end;

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

  TjsHTMLDOMAttribute = class(TjsHTMLDOMNode, IHTMLDOMAttribute, IHTMLDOMAttribute2)
  protected
    {$REGION 'IHTMLDOMAttribute'}
    function Get_nodeName: WideString; safecall;
    procedure Set_nodeValue(p: OleVariant); safecall;
    function Get_nodeValue: OleVariant; safecall;
    function Get_specified: WordBool; safecall;
    {$ENDREGION}

    {$REGION 'IHTMLDOMAttribute2'}
    function Get_name: WideString; safecall;
    procedure Set_value(const p: WideString); safecall;
    function Get_value: WideString; safecall;
    function Get_expando: WordBool; safecall;
    function cloneNode(fDeep: WordBool): IHTMLDOMAttribute; safecall;
    {$ENDREGION}
  end;

  TjsHTMLElement = class(TjsHTMLDOMNode, IHTMLElement)
  protected
    {$REGION 'IHTMLElement'}
    procedure setAttribute(const strAttributeName: WideString; AttributeValue: OleVariant;
                           lFlags: Integer); safecall;
    function getAttribute(const strAttributeName: WideString; lFlags: Integer): OleVariant; safecall;
    function removeAttribute(const strAttributeName: WideString; lFlags: Integer): WordBool; safecall;
    procedure Set_className(const p: WideString); safecall;
    function Get_className: WideString; safecall;
    procedure Set_id(const p: WideString); safecall;
    function Get_id: WideString; safecall;
    function Get_tagName: WideString; safecall;
    function Get_parentElement: IHTMLElement; safecall;
    function Get_style: IHTMLStyle; safecall;
    procedure Set_onhelp(p: OleVariant); safecall;
    function Get_onhelp: OleVariant; safecall;
    procedure Set_onclick(p: OleVariant); safecall;
    function Get_onclick: OleVariant; safecall;
    procedure Set_ondblclick(p: OleVariant); safecall;
    function Get_ondblclick: OleVariant; safecall;
    procedure Set_onkeydown(p: OleVariant); safecall;
    function Get_onkeydown: OleVariant; safecall;
    procedure Set_onkeyup(p: OleVariant); safecall;
    function Get_onkeyup: OleVariant; safecall;
    procedure Set_onkeypress(p: OleVariant); safecall;
    function Get_onkeypress: OleVariant; safecall;
    procedure Set_onmouseout(p: OleVariant); safecall;
    function Get_onmouseout: OleVariant; safecall;
    procedure Set_onmouseover(p: OleVariant); safecall;
    function Get_onmouseover: OleVariant; safecall;
    procedure Set_onmousemove(p: OleVariant); safecall;
    function Get_onmousemove: OleVariant; safecall;
    procedure Set_onmousedown(p: OleVariant); safecall;
    function Get_onmousedown: OleVariant; safecall;
    procedure Set_onmouseup(p: OleVariant); safecall;
    function Get_onmouseup: OleVariant; safecall;
    function Get_document: IDispatch; safecall;
    procedure Set_title(const p: WideString); safecall;
    function Get_title: WideString; safecall;
    procedure Set_language(const p: WideString); safecall;
    function Get_language: WideString; safecall;
    procedure Set_onselectstart(p: OleVariant); safecall;
    function Get_onselectstart: OleVariant; safecall;
    procedure scrollIntoView(varargStart: OleVariant); safecall;
    function contains(const pChild: IHTMLElement): WordBool; safecall;
    function Get_sourceIndex: Integer; safecall;
    function Get_recordNumber: OleVariant; safecall;
    procedure Set_lang(const p: WideString); safecall;
    function Get_lang: WideString; safecall;
    function Get_offsetLeft: Integer; safecall;
    function Get_offsetTop: Integer; safecall;
    function Get_offsetWidth: Integer; safecall;
    function Get_offsetHeight: Integer; safecall;
    function Get_offsetParent: IHTMLElement; safecall;
    procedure Set_innerHTML(const p: WideString); safecall;
    function Get_innerHTML: WideString; safecall;
    procedure Set_innerText(const p: WideString); safecall;
    function Get_innerText: WideString; safecall;
    procedure Set_outerHTML(const p: WideString); safecall;
    function Get_outerHTML: WideString; safecall;
    procedure Set_outerText(const p: WideString); safecall;
    function Get_outerText: WideString; safecall;
    procedure insertAdjacentHTML(const where: WideString; const html: WideString); safecall;
    procedure insertAdjacentText(const where: WideString; const text: WideString); safecall;
    function Get_parentTextEdit: IHTMLElement; safecall;
    function Get_isTextEdit: WordBool; safecall;
    procedure click; safecall;
    function Get_filters: IHTMLFiltersCollection; safecall;
    procedure Set_ondragstart(p: OleVariant); safecall;
    function Get_ondragstart: OleVariant; safecall;
    function toString: WideString; safecall;
    procedure Set_onbeforeupdate(p: OleVariant); safecall;
    function Get_onbeforeupdate: OleVariant; safecall;
    procedure Set_onafterupdate(p: OleVariant); safecall;
    function Get_onafterupdate: OleVariant; safecall;
    procedure Set_onerrorupdate(p: OleVariant); safecall;
    function Get_onerrorupdate: OleVariant; safecall;
    procedure Set_onrowexit(p: OleVariant); safecall;
    function Get_onrowexit: OleVariant; safecall;
    procedure Set_onrowenter(p: OleVariant); safecall;
    function Get_onrowenter: OleVariant; safecall;
    procedure Set_ondatasetchanged(p: OleVariant); safecall;
    function Get_ondatasetchanged: OleVariant; safecall;
    procedure Set_ondataavailable(p: OleVariant); safecall;
    function Get_ondataavailable: OleVariant; safecall;
    procedure Set_ondatasetcomplete(p: OleVariant); safecall;
    function Get_ondatasetcomplete: OleVariant; safecall;
    procedure Set_onfilterchange(p: OleVariant); safecall;
    function Get_onfilterchange: OleVariant; safecall;
    function Get_children: IDispatch; safecall;
    function Get_all: IDispatch; safecall;
    {$ENDREGION}

  end;

  TjsHTMLDocument = class(TjsHTMLDOMNode, IHTMLDocument2)
  protected
    function Get_Script: IDispatch; safecall;

    {$REGION 'IHTMLDocument2'}
    function Get_all: IHTMLElementCollection; safecall;
    function Get_body: IHTMLElement; safecall;
    function Get_activeElement: IHTMLElement; safecall;
    function Get_images: IHTMLElementCollection; safecall;
    function Get_applets: IHTMLElementCollection; safecall;
    function Get_links: IHTMLElementCollection; safecall;
    function Get_forms: IHTMLElementCollection; safecall;
    function Get_anchors: IHTMLElementCollection; safecall;
    procedure Set_title(const p: WideString); safecall;
    function Get_title: WideString; safecall;
    function Get_scripts: IHTMLElementCollection; safecall;
    procedure Set_designMode(const p: WideString); safecall;
    function Get_designMode: WideString; safecall;
    function Get_selection: IHTMLSelectionObject; safecall;
    function Get_readyState: WideString; safecall;
    function Get_frames: IHTMLFramesCollection2; safecall;
    function Get_embeds: IHTMLElementCollection; safecall;
    function Get_plugins: IHTMLElementCollection; safecall;
    procedure Set_alinkColor(p: OleVariant); safecall;
    function Get_alinkColor: OleVariant; safecall;
    procedure Set_bgColor(p: OleVariant); safecall;
    function Get_bgColor: OleVariant; safecall;
    procedure Set_fgColor(p: OleVariant); safecall;
    function Get_fgColor: OleVariant; safecall;
    procedure Set_linkColor(p: OleVariant); safecall;
    function Get_linkColor: OleVariant; safecall;
    procedure Set_vlinkColor(p: OleVariant); safecall;
    function Get_vlinkColor: OleVariant; safecall;
    function Get_referrer: WideString; safecall;
    function Get_location: IHTMLLocation; safecall;
    function Get_lastModified: WideString; safecall;
    procedure Set_url(const p: WideString); safecall;
    function Get_url: WideString; safecall;
    procedure Set_domain(const p: WideString); safecall;
    function Get_domain: WideString; safecall;
    procedure Set_cookie(const p: WideString); safecall;
    function Get_cookie: WideString; safecall;
    procedure Set_expando(p: WordBool); safecall;
    function Get_expando: WordBool; safecall;
    procedure Set_charset(const p: WideString); safecall;
    function Get_charset: WideString; safecall;
    procedure Set_defaultCharset(const p: WideString); safecall;
    function Get_defaultCharset: WideString; safecall;
    function Get_mimeType: WideString; safecall;
    function Get_fileSize: WideString; safecall;
    function Get_fileCreatedDate: WideString; safecall;
    function Get_fileModifiedDate: WideString; safecall;
    function Get_fileUpdatedDate: WideString; safecall;
    function Get_security: WideString; safecall;
    function Get_protocol: WideString; safecall;
    function Get_nameProp: WideString; safecall;
    procedure write(psarray: PSafeArray); safecall;
    procedure writeln(psarray: PSafeArray); safecall;
    function open(const url: WideString; name: OleVariant; features: OleVariant; replace: OleVariant): IDispatch; safecall;
    procedure close; safecall;
    procedure clear; safecall;
    function queryCommandSupported(const cmdID: WideString): WordBool; safecall;
    function queryCommandEnabled(const cmdID: WideString): WordBool; safecall;
    function queryCommandState(const cmdID: WideString): WordBool; safecall;
    function queryCommandIndeterm(const cmdID: WideString): WordBool; safecall;
    function queryCommandText(const cmdID: WideString): WideString; safecall;
    function queryCommandValue(const cmdID: WideString): OleVariant; safecall;
    function execCommand(const cmdID: WideString; showUI: WordBool; value: OleVariant): WordBool; safecall;
    function execCommandShowHelp(const cmdID: WideString): WordBool; safecall;
    function createElement(const eTag: WideString): IHTMLElement; safecall;
    procedure Set_onhelp(p: OleVariant); safecall;
    function Get_onhelp: OleVariant; safecall;
    procedure Set_onclick(p: OleVariant); safecall;
    function Get_onclick: OleVariant; safecall;
    procedure Set_ondblclick(p: OleVariant); safecall;
    function Get_ondblclick: OleVariant; safecall;
    procedure Set_onkeyup(p: OleVariant); safecall;
    function Get_onkeyup: OleVariant; safecall;
    procedure Set_onkeydown(p: OleVariant); safecall;
    function Get_onkeydown: OleVariant; safecall;
    procedure Set_onkeypress(p: OleVariant); safecall;
    function Get_onkeypress: OleVariant; safecall;
    procedure Set_onmouseup(p: OleVariant); safecall;
    function Get_onmouseup: OleVariant; safecall;
    procedure Set_onmousedown(p: OleVariant); safecall;
    function Get_onmousedown: OleVariant; safecall;
    procedure Set_onmousemove(p: OleVariant); safecall;
    function Get_onmousemove: OleVariant; safecall;
    procedure Set_onmouseout(p: OleVariant); safecall;
    function Get_onmouseout: OleVariant; safecall;
    procedure Set_onmouseover(p: OleVariant); safecall;
    function Get_onmouseover: OleVariant; safecall;
    procedure Set_onreadystatechange(p: OleVariant); safecall;
    function Get_onreadystatechange: OleVariant; safecall;
    procedure Set_onafterupdate(p: OleVariant); safecall;
    function Get_onafterupdate: OleVariant; safecall;
    procedure Set_onrowexit(p: OleVariant); safecall;
    function Get_onrowexit: OleVariant; safecall;
    procedure Set_onrowenter(p: OleVariant); safecall;
    function Get_onrowenter: OleVariant; safecall;
    procedure Set_ondragstart(p: OleVariant); safecall;
    function Get_ondragstart: OleVariant; safecall;
    procedure Set_onselectstart(p: OleVariant); safecall;
    function Get_onselectstart: OleVariant; safecall;
    function elementFromPoint(x: Integer; y: Integer): IHTMLElement; safecall;
    function Get_parentWindow: IHTMLWindow2; safecall;
    function Get_styleSheets: IHTMLStyleSheetsCollection; safecall;
    procedure Set_onbeforeupdate(p: OleVariant); safecall;
    function Get_onbeforeupdate: OleVariant; safecall;
    procedure Set_onerrorupdate(p: OleVariant); safecall;
    function Get_onerrorupdate: OleVariant; safecall;
    function toString: WideString; safecall;
    function createStyleSheet(const bstrHref: WideString; lIndex: Integer): IHTMLStyleSheet; safecall;
    {$ENDREGION}
  end;

  TjsHTMLFramesCollection = class(TjsDOMObject, IHTMLFramesCollection2)
  protected
    function item(const pvarIndex: OleVariant): OleVariant; safecall;
    function Get_length: Integer; safecall;
  end;

  TjsHTMLWindow = class(TjsHTMLFramesCollection, IHTMLWindow2)
  protected
    function Get_frames: IHTMLFramesCollection2; safecall;
    procedure Set_defaultStatus(const p: WideString); safecall;
    function Get_defaultStatus: WideString; safecall;
    procedure Set_status(const p: WideString); safecall;
    function Get_status: WideString; safecall;
    function setTimeout(const expression: WideString; msec: Integer; var language: OleVariant): Integer; safecall;
    procedure clearTimeout(timerID: Integer); safecall;
    procedure alert(const message: WideString); safecall;
    function confirm(const message: WideString): WordBool; safecall;
    function prompt(const message: WideString; const defstr: WideString): OleVariant; safecall;
    function Get_Image: IHTMLImageElementFactory; safecall;
    function Get_location: IHTMLLocation; safecall;
    function Get_history: IOmHistory; safecall;
    procedure close; safecall;
    procedure Set_opener(p: OleVariant); safecall;
    function Get_opener: OleVariant; safecall;
    function Get_navigator: IOmNavigator; safecall;
    procedure Set_name(const p: WideString); safecall;
    function Get_name: WideString; safecall;
    function Get_parent: IHTMLWindow2; safecall;
    function open(const url: WideString; const name: WideString; const features: WideString;
                  replace: WordBool): IHTMLWindow2; safecall;
    function Get_self: IHTMLWindow2; safecall;
    function Get_top: IHTMLWindow2; safecall;
    function Get_window: IHTMLWindow2; safecall;
    procedure navigate(const url: WideString); safecall;
    procedure Set_onfocus(p: OleVariant); safecall;
    function Get_onfocus: OleVariant; safecall;
    procedure Set_onblur(p: OleVariant); safecall;
    function Get_onblur: OleVariant; safecall;
    procedure Set_onload(p: OleVariant); safecall;
    function Get_onload: OleVariant; safecall;
    procedure Set_onbeforeunload(p: OleVariant); safecall;
    function Get_onbeforeunload: OleVariant; safecall;
    procedure Set_onunload(p: OleVariant); safecall;
    function Get_onunload: OleVariant; safecall;
    procedure Set_onhelp(p: OleVariant); safecall;
    function Get_onhelp: OleVariant; safecall;
    procedure Set_onerror(p: OleVariant); safecall;
    function Get_onerror: OleVariant; safecall;
    procedure Set_onresize(p: OleVariant); safecall;
    function Get_onresize: OleVariant; safecall;
    procedure Set_onscroll(p: OleVariant); safecall;
    function Get_onscroll: OleVariant; safecall;
    function Get_document: IHTMLDocument2; safecall;
    function Get_event: IHTMLEventObj; safecall;
    function Get__newEnum: IUnknown; safecall;
    function showModalDialog(const dialog: WideString; var varArgIn: OleVariant;
                             var varOptions: OleVariant): OleVariant; safecall;
    procedure showHelp(const helpURL: WideString; helpArg: OleVariant; const features: WideString); safecall;
    function Get_screen: IHTMLScreen; safecall;
    function Get_Option: IHTMLOptionElementFactory; safecall;
    procedure focus; safecall;
    function Get_closed: WordBool; safecall;
    procedure blur; safecall;
    procedure scroll(x: Integer; y: Integer); safecall;
    function Get_clientInformation: IOmNavigator; safecall;
    function setInterval(const expression: WideString; msec: Integer; var language: OleVariant): Integer; safecall;
    procedure clearInterval(timerID: Integer); safecall;
    procedure Set_offscreenBuffering(p: OleVariant); safecall;
    function Get_offscreenBuffering: OleVariant; safecall;
    function execScript(const code: WideString; const language: WideString): OleVariant; safecall;
    function toString: WideString; safecall;
    procedure scrollBy(x: Integer; y: Integer); safecall;
    procedure scrollTo(x: Integer; y: Integer); safecall;
    procedure moveTo(x: Integer; y: Integer); safecall;
    procedure moveBy(x: Integer; y: Integer); safecall;
    procedure resizeTo(x: Integer; y: Integer); safecall;
    procedure resizeBy(x: Integer; y: Integer); safecall;
    function Get_external: IDispatch; safecall;
  end;

  TjsOMNavigator = class(TjsDOMObject, IOmNavigator)
  protected
    function Get_appCodeName: WideString; safecall;
    function Get_appName: WideString; safecall;
    function Get_appVersion: WideString; safecall;
    function Get_userAgent: WideString; safecall;
    function javaEnabled: WordBool; safecall;
    function taintEnabled: WordBool; safecall;
    function Get_mimeTypes: IHTMLMimeTypesCollection; safecall;
    function Get_plugins: IHTMLPluginsCollection; safecall;
    function Get_cookieEnabled: WordBool; safecall;
    function Get_opsProfile: IHTMLOpsProfile; safecall;
    function toString: WideString; safecall;
    function Get_cpuClass: WideString; safecall;
    function Get_systemLanguage: WideString; safecall;
    function Get_browserLanguage: WideString; safecall;
    function Get_userLanguage: WideString; safecall;
    function Get_platform: WideString; safecall;
    function Get_appMinorVersion: WideString; safecall;
    function Get_connectionSpeed: Integer; safecall;
    function Get_onLine: WordBool; safecall;
    function Get_userProfile: IHTMLOpsProfile; safecall;
  end;

implementation

{ TjsDOMObject }


constructor TjsDOMObject.Create(AApplication: TjsdApplication;
  ACreateCommand: String);
begin
  inherited Create(AApplication);

  if FApplication.Exec(_JSVar + '=' + ACreateCommand, True) = 'null' then
    Abort;
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

{ TjsHTMLDOMNode }

function TjsHTMLDOMNode.appendChild(const newChild: IHTMLDOMNode): IHTMLDOMNode;
begin
  Result:=TjsHTMLDOMNode.Create(FApplication, _JSVar+'.appendChild('+Self.GetDOMObjectJsName(newChild)+')');
end;

function TjsHTMLDOMNode.cloneNode(fDeep: WordBool): IHTMLDOMNode;
begin
  Result:=TjsHTMLDOMNode.Create(FApplication, _JSVar+'.cloneNode('+LowerCase(BoolToStr(fDeep))+')');
end;

function TjsHTMLDOMNode.Get_attributes: IDispatch;
begin
  Result:=TjsHTMLDOMAttribute.Create(FApplication, _JSVar+'.attributes');
end;

function TjsHTMLDOMNode.Get_childNodes: IDispatch;
begin
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.childNodes');
end;

function TjsHTMLDOMNode.Get_firstChild: IHTMLDOMNode;
begin
  Result:=TjsHTMLDOMNode.Create(FApplication, _JSVar+'.firstChild');
end;

function TjsHTMLDOMNode.Get_lastChild: IHTMLDOMNode;
begin
  Result:=TjsHTMLDOMNode.Create(FApplication, _JSVar+'.lastChild');
end;

function TjsHTMLDOMNode.Get_nextSibling: IHTMLDOMNode;
begin
  Result:=TjsHTMLDOMNode.Create(FApplication, _JSVar+'.nextSibling');
end;

function TjsHTMLDOMNode.Get_nodeName: WideString;
begin
  GetPropertyValue('nodeName', Result);
end;

function TjsHTMLDOMNode.Get_nodeType: Integer;
begin
  GetPropertyValue('nodeType', Result);
end;

function TjsHTMLDOMNode.Get_nodeValue: OleVariant;
begin
  GetPropertyValue('nodeValue', Result);
end;

function TjsHTMLDOMNode.Get_ownerDocument: IDispatch;
begin
  Result:=TjsHTMLDocument.Create(FApplication, _JSVar+'.ownerDocument');
end;

function TjsHTMLDOMNode.Get_parentNode: IHTMLDOMNode;
begin
  Result:=TjsHTMLDOMNode.Create(FApplication, _JSVar+'.parentNode');
end;

function TjsHTMLDOMNode.Get_previousSibling: IHTMLDOMNode;
begin
  Result:=TjsHTMLDOMNode.Create(FApplication, _JSVar+'.previousSibling');
end;

function TjsHTMLDOMNode.hasChildNodes: WordBool;
begin
  GetPropertyValue('hasChildNodes', Result);
end;

function TjsHTMLDOMNode.insertBefore(const newChild: IHTMLDOMNode;
  refChild: OleVariant): IHTMLDOMNode;
begin
  Result:=TjsHTMLDOMNode.Create(FApplication, _JSVar+'.insertBefore('+GetDOMObjectJsName(newChild)+', '+GetDOMObjectJsName(refChild)+')');
end;

function TjsHTMLDOMNode.removeChild(const oldChild: IHTMLDOMNode): IHTMLDOMNode;
begin
  Result:=TjsHTMLDOMNode.Create(FApplication, _JSVar+'.removeChild('+GetDOMObjectJsName(oldChild)+')');
end;

function TjsHTMLDOMNode.removeNode(fDeep: WordBool): IHTMLDOMNode;
begin
  Result:=TjsHTMLDOMNode.Create(FApplication, _JSVar+'.removeNode('+LowerCase(BoolToStr(fDeep))+')');
end;

function TjsHTMLDOMNode.replaceChild(const newChild,
  oldChild: IHTMLDOMNode): IHTMLDOMNode;
begin
  Result:=TjsHTMLDOMNode.Create(FApplication, _JSVar+'.replaceChild('+GetDOMObjectJsName(newChild)+','+GetDOMObjectJsName(oldChild)+')');
end;

function TjsHTMLDOMNode.replaceNode(
  const replacement: IHTMLDOMNode): IHTMLDOMNode;
begin
  Result:=TjsHTMLDOMNode.Create(FApplication, _JSVar+'.replaceNode('+GetDOMObjectJsName(replacement)+')');
end;

procedure TjsHTMLDOMNode.Set_nodeValue(p: OleVariant);
begin
  SetPropertyValue('nodeValue', p);
end;

function TjsHTMLDOMNode.swapNode(const otherNode: IHTMLDOMNode): IHTMLDOMNode;
begin
  Result:=TjsHTMLDOMNode.Create(FApplication, _JSVar+'.swapNode('+GetDOMObjectJsName(otherNode)+')');
end;

{ TjsHTMLDOMAttribute }

function TjsHTMLDOMAttribute.cloneNode(fDeep: WordBool): IHTMLDOMAttribute;
begin
  Result:=TjsHTMLDOMAttribute.Create(FApplication, _JSVar+'.cloneNode('+LowerCase(BoolToStr(fDeep))+')');
end;

function TjsHTMLDOMAttribute.Get_expando: WordBool;
begin
  GetPropertyValue('expando', Result);
end;

function TjsHTMLDOMAttribute.Get_name: WideString;
begin
  GetPropertyValue('name', Result);
end;

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

function TjsHTMLDOMAttribute.Get_value: WideString;
begin
  GetPropertyValue('value', Result);
end;

procedure TjsHTMLDOMAttribute.Set_nodeValue(p: OleVariant);
begin
  SetPropertyValue('nodeValue', p);
end;

procedure TjsHTMLDOMAttribute.Set_value(const p: WideString);
begin
  SetPropertyValue('value', p);
end;

{ TjsHTMLDocument }

procedure TjsHTMLDocument.clear;
begin
  ExecMethod('clear');
end;

procedure TjsHTMLDocument.close;
begin
  ExecMethod('close');
end;

function TjsHTMLDocument.createElement(const eTag: WideString): IHTMLElement;
begin
  Result:=TjsHTMLElement.Create(FApplication, _JSVar+'.createElement("'+eTag+'")');
end;

function TjsHTMLDocument.createStyleSheet(const bstrHref: WideString;
  lIndex: Integer): IHTMLStyleSheet;
begin

end;

function TjsHTMLDocument.elementFromPoint(x, y: Integer): IHTMLElement;
begin
  Result:=TjsHTMLElement.Create(FApplication, _JSVar+'.elementFromPoint('+IntToStr(x)+','+IntToStr(y)+')');
end;

function TjsHTMLDocument.execCommand(const cmdID: WideString; showUI: WordBool;
  value: OleVariant): WordBool;
begin
  Result:=IsPositiveMethodResult(
    ExecMethod('execCommand('+ToJSString(cmdID)+','+
                            LowerCase(BoolToStr(showUI))+','+
                            ToJSCode(value)+')', True));
end;

function TjsHTMLDocument.execCommandShowHelp(const cmdID: WideString): WordBool;
begin
  Result:=IsPositiveMethodResult(
    ExecMethod('execCommandShowHelp('+ToJSString(cmdID)+')', True));
end;

function TjsHTMLDocument.Get_activeElement: IHTMLElement;
begin
  Result:=TjsHTMLElement.Create(FApplication, _JSVar+'.activeElement');
end;

function TjsHTMLDocument.Get_alinkColor: OleVariant;
begin
  GetPropertyValue('alinkColor', Result);
end;

function TjsHTMLDocument.Get_all: IHTMLElementCollection;
begin
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.all');
end;

function TjsHTMLDocument.Get_anchors: IHTMLElementCollection;
begin
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.anchors');
end;

function TjsHTMLDocument.Get_applets: IHTMLElementCollection;
begin
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.applets');
end;

function TjsHTMLDocument.Get_bgColor: OleVariant;
begin
  GetPropertyValue('bgColor', Result);
end;

function TjsHTMLDocument.Get_body: IHTMLElement;
begin
  Result:=TjsHTMLElement.Create(FApplication, _JSVar+'.body');
end;

function TjsHTMLDocument.Get_charset: WideString;
begin
  GetPropertyValue('charset', Result);
end;

function TjsHTMLDocument.Get_cookie: WideString;
begin
  GetPropertyValue('cookie', Result);
end;

function TjsHTMLDocument.Get_defaultCharset: WideString;
begin
  GetPropertyValue('defaultCharset', Result);
end;

function TjsHTMLDocument.Get_designMode: WideString;
begin
  GetPropertyValue('designMode', Result);
end;

function TjsHTMLDocument.Get_domain: WideString;
begin
  GetPropertyValue('domain', Result);
end;

function TjsHTMLDocument.Get_embeds: IHTMLElementCollection;
begin
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.embeds');
end;

function TjsHTMLDocument.Get_expando: WordBool;
begin
  GetPropertyValue('expando', Result);
end;

function TjsHTMLDocument.Get_fgColor: OleVariant;
begin
  GetPropertyValue('fgColor', Result);
end;

function TjsHTMLDocument.Get_fileCreatedDate: WideString;
begin
  GetPropertyValue('fileCreatedDate', Result);
end;

function TjsHTMLDocument.Get_fileModifiedDate: WideString;
begin
  GetPropertyValue('fileModifiedDate', Result);
end;

function TjsHTMLDocument.Get_fileSize: WideString;
begin
  GetPropertyValue('fileSize', Result);
end;

function TjsHTMLDocument.Get_fileUpdatedDate: WideString;
begin
  GetPropertyValue('fileUpdateDate', Result);
end;

function TjsHTMLDocument.Get_forms: IHTMLElementCollection;
begin
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.forms');
end;

function TjsHTMLDocument.Get_frames: IHTMLFramesCollection2;
begin
  Result:=TjsHTMLFramesCollection.Create(FApplication, _JSVar+'.frames');
end;

function TjsHTMLDocument.Get_images: IHTMLElementCollection;
begin
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.images');
end;

function TjsHTMLDocument.Get_lastModified: WideString;
begin
  GetPropertyValue('lastModified', Result);
end;

function TjsHTMLDocument.Get_linkColor: OleVariant;
begin
  GetPropertyValue('linkColor', Result);
end;

function TjsHTMLDocument.Get_links: IHTMLElementCollection;
begin
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.links');
end;

function TjsHTMLDocument.Get_location: IHTMLLocation;
begin

end;

function TjsHTMLDocument.Get_mimeType: WideString;
begin
  GetPropertyValue('mimeType', Result);
end;

function TjsHTMLDocument.Get_nameProp: WideString;
begin
  GetPropertyValue('nameProp', Result);
end;

function TjsHTMLDocument.Get_onafterupdate: OleVariant;
begin
  GetPropertyValue('onafterupdate', Result);
end;

function TjsHTMLDocument.Get_onbeforeupdate: OleVariant;
begin
  GetPropertyValue('onbeforeupdate', Result);
end;

function TjsHTMLDocument.Get_onclick: OleVariant;
begin
  GetPropertyValue('onclick', Result);
end;

function TjsHTMLDocument.Get_ondblclick: OleVariant;
begin
  GetPropertyValue('ondblclick', Result);
end;

function TjsHTMLDocument.Get_ondragstart: OleVariant;
begin
  GetPropertyValue('ondragstart', Result);
end;

function TjsHTMLDocument.Get_onerrorupdate: OleVariant;
begin
  GetPropertyValue('onerrorupdate', Result);
end;

function TjsHTMLDocument.Get_onhelp: OleVariant;
begin
  GetPropertyValue('onhelp', Result);
end;

function TjsHTMLDocument.Get_onkeydown: OleVariant;
begin
  GetPropertyValue('onkeydown', Result);
end;

function TjsHTMLDocument.Get_onkeypress: OleVariant;
begin
  GetPropertyValue('onkeypress', Result);
end;

function TjsHTMLDocument.Get_onkeyup: OleVariant;
begin
  GetPropertyValue('onkeyup', Result);
end;

function TjsHTMLDocument.Get_onmousedown: OleVariant;
begin
  GetPropertyValue('onmousedown', Result);
end;

function TjsHTMLDocument.Get_onmousemove: OleVariant;
begin
  GetPropertyValue('onmousemove', Result);
end;

function TjsHTMLDocument.Get_onmouseout: OleVariant;
begin
  GetPropertyValue('onmouseout', Result);
end;

function TjsHTMLDocument.Get_onmouseover: OleVariant;
begin
  GetPropertyValue('onmouseover', Result);
end;

function TjsHTMLDocument.Get_onmouseup: OleVariant;
begin
  GetPropertyValue('onmouseup', Result);
end;

function TjsHTMLDocument.Get_onreadystatechange: OleVariant;
begin
  GetPropertyValue('onreadystatechange', Result);
end;

function TjsHTMLDocument.Get_onrowenter: OleVariant;
begin
  GetPropertyValue('onrowenter', Result);
end;

function TjsHTMLDocument.Get_onrowexit: OleVariant;
begin
  GetPropertyValue('onrowexit', Result);
end;

function TjsHTMLDocument.Get_onselectstart: OleVariant;
begin
  GetPropertyValue('onselectstart', Result);
end;

function TjsHTMLDocument.Get_parentWindow: IHTMLWindow2;
begin

end;

function TjsHTMLDocument.Get_plugins: IHTMLElementCollection;
begin
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.plugins');
end;

function TjsHTMLDocument.Get_protocol: WideString;
begin
  GetPropertyValue('protocol', Result);
end;

function TjsHTMLDocument.Get_readyState: WideString;
begin
  GetPropertyValue('readyState', Result);
end;

function TjsHTMLDocument.Get_referrer: WideString;
begin
  GetPropertyValue('referrer', Result);
end;

function TjsHTMLDocument.Get_Script: IDispatch;
begin
  Result:=nil;
end;

function TjsHTMLDocument.Get_scripts: IHTMLElementCollection;
begin
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.scripts');
end;

function TjsHTMLDocument.Get_security: WideString;
begin
  GetPropertyValue('security', Result);
end;

function TjsHTMLDocument.Get_selection: IHTMLSelectionObject;
begin
end;

function TjsHTMLDocument.Get_styleSheets: IHTMLStyleSheetsCollection;
begin

end;

function TjsHTMLDocument.Get_title: WideString;
begin

end;

function TjsHTMLDocument.Get_url: WideString;
begin

end;

function TjsHTMLDocument.Get_vlinkColor: OleVariant;
begin

end;

function TjsHTMLDocument.open(const url: WideString; name, features,
  replace: OleVariant): IDispatch;
begin

end;

function TjsHTMLDocument.queryCommandEnabled(const cmdID: WideString): WordBool;
begin

end;

function TjsHTMLDocument.queryCommandIndeterm(const cmdID: WideString): WordBool;
begin

end;

function TjsHTMLDocument.queryCommandState(const cmdID: WideString): WordBool;
begin

end;

function TjsHTMLDocument.queryCommandSupported(
  const cmdID: WideString): WordBool;
begin

end;

function TjsHTMLDocument.queryCommandText(const cmdID: WideString): WideString;
begin

end;

function TjsHTMLDocument.queryCommandValue(const cmdID: WideString): OleVariant;
begin

end;

procedure TjsHTMLDocument.Set_alinkColor(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_bgColor(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_charset(const p: WideString);
begin

end;

procedure TjsHTMLDocument.Set_cookie(const p: WideString);
begin

end;

procedure TjsHTMLDocument.Set_defaultCharset(const p: WideString);
begin

end;

procedure TjsHTMLDocument.Set_designMode(const p: WideString);
begin

end;

procedure TjsHTMLDocument.Set_domain(const p: WideString);
begin

end;

procedure TjsHTMLDocument.Set_expando(p: WordBool);
begin

end;

procedure TjsHTMLDocument.Set_fgColor(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_linkColor(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_onafterupdate(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_onbeforeupdate(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_onclick(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_ondblclick(p: OleVariant);
begin
  FApplication.Exec(_JSVar+'.ondblclick = '+p);
end;

procedure TjsHTMLDocument.Set_ondragstart(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_onerrorupdate(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_onhelp(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_onkeydown(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_onkeypress(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_onkeyup(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_onmousedown(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_onmousemove(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_onmouseout(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_onmouseover(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_onmouseup(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_onreadystatechange(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_onrowenter(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_onrowexit(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_onselectstart(p: OleVariant);
begin

end;

procedure TjsHTMLDocument.Set_title(const p: WideString);
begin

end;

procedure TjsHTMLDocument.Set_url(const p: WideString);
begin

end;

procedure TjsHTMLDocument.Set_vlinkColor(p: OleVariant);
begin

end;

function TjsHTMLDocument.toString: WideString;
begin

end;

procedure TjsHTMLDocument.write(psarray: PSafeArray);
begin

end;

procedure TjsHTMLDocument.writeln(psarray: PSafeArray);
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

{ TjsHTMLElementCollection }

function TjsHTMLElementCollection.Get_length: Integer;
begin
  GetPropertyValue('length', Result);
end;

function TjsHTMLElementCollection.Get__newEnum: IUnknown;
begin
  Result:=nil;
end;

function TjsHTMLElementCollection.item(name, index: OleVariant): IDispatch;
begin
  Result:=TjsHTMLElement.Create(FApplication, _JSVar+'.item('+ToJSCode(name)+', '+ToJSString(index)+')');
end;

function TjsHTMLElementCollection.namedItem(const name: WideString): IDispatch;
begin
  result:=TjsHTMLElement.Create(FApplication, _JSVar+'.namedItem("'+name+'")');
end;

procedure TjsHTMLElementCollection.Set_length(p: Integer);
begin
  SetPropertyValue('length', p);
end;

function TjsHTMLElementCollection.tags(tagName: OleVariant): IDispatch;
begin
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.getElementsByTagName('+ToJSString(tagName)+')');
end;

function TjsHTMLElementCollection.toString: WideString;
begin
  result:=ExecMethod('toString()', True);
end;

{ TjsHTMLElement }

procedure TjsHTMLElement.click;
begin
  ExecMethod('click');
end;

function TjsHTMLElement.contains(const pChild: IHTMLElement): WordBool;
begin

end;

function TjsHTMLElement.getAttribute(const strAttributeName: WideString;
  lFlags: Integer): OleVariant;
begin

end;

function TjsHTMLElement.Get_all: IDispatch;
begin

end;

function TjsHTMLElement.Get_children: IDispatch;
begin

end;

function TjsHTMLElement.Get_className: WideString;
begin

end;

function TjsHTMLElement.Get_document: IDispatch;
begin
  Result:=TjsHTMLDocument.Create(FApplication, _JSVar+'.document');
end;

function TjsHTMLElement.Get_filters: IHTMLFiltersCollection;
begin

end;

function TjsHTMLElement.Get_id: WideString;
begin

end;

function TjsHTMLElement.Get_innerHTML: WideString;
begin

end;

function TjsHTMLElement.Get_innerText: WideString;
begin

end;

function TjsHTMLElement.Get_isTextEdit: WordBool;
begin

end;

function TjsHTMLElement.Get_lang: WideString;
begin

end;

function TjsHTMLElement.Get_language: WideString;
begin

end;

function TjsHTMLElement.Get_offsetHeight: Integer;
begin

end;

function TjsHTMLElement.Get_offsetLeft: Integer;
begin

end;

function TjsHTMLElement.Get_offsetParent: IHTMLElement;
begin

end;

function TjsHTMLElement.Get_offsetTop: Integer;
begin

end;

function TjsHTMLElement.Get_offsetWidth: Integer;
begin

end;

function TjsHTMLElement.Get_onafterupdate: OleVariant;
begin

end;

function TjsHTMLElement.Get_onbeforeupdate: OleVariant;
begin

end;

function TjsHTMLElement.Get_onclick: OleVariant;
begin

end;

function TjsHTMLElement.Get_ondataavailable: OleVariant;
begin

end;

function TjsHTMLElement.Get_ondatasetchanged: OleVariant;
begin

end;

function TjsHTMLElement.Get_ondatasetcomplete: OleVariant;
begin

end;

function TjsHTMLElement.Get_ondblclick: OleVariant;
begin

end;

function TjsHTMLElement.Get_ondragstart: OleVariant;
begin

end;

function TjsHTMLElement.Get_onerrorupdate: OleVariant;
begin

end;

function TjsHTMLElement.Get_onfilterchange: OleVariant;
begin

end;

function TjsHTMLElement.Get_onhelp: OleVariant;
begin

end;

function TjsHTMLElement.Get_onkeydown: OleVariant;
begin

end;

function TjsHTMLElement.Get_onkeypress: OleVariant;
begin

end;

function TjsHTMLElement.Get_onkeyup: OleVariant;
begin

end;

function TjsHTMLElement.Get_onmousedown: OleVariant;
begin

end;

function TjsHTMLElement.Get_onmousemove: OleVariant;
begin

end;

function TjsHTMLElement.Get_onmouseout: OleVariant;
begin

end;

function TjsHTMLElement.Get_onmouseover: OleVariant;
begin

end;

function TjsHTMLElement.Get_onmouseup: OleVariant;
begin

end;

function TjsHTMLElement.Get_onrowenter: OleVariant;
begin

end;

function TjsHTMLElement.Get_onrowexit: OleVariant;
begin

end;

function TjsHTMLElement.Get_onselectstart: OleVariant;
begin

end;

function TjsHTMLElement.Get_outerHTML: WideString;
begin

end;

function TjsHTMLElement.Get_outerText: WideString;
begin

end;

function TjsHTMLElement.Get_parentElement: IHTMLElement;
begin

end;

function TjsHTMLElement.Get_parentTextEdit: IHTMLElement;
begin

end;

function TjsHTMLElement.Get_recordNumber: OleVariant;
begin

end;

function TjsHTMLElement.Get_sourceIndex: Integer;
begin

end;

function TjsHTMLElement.Get_style: IHTMLStyle;
begin

end;

function TjsHTMLElement.Get_tagName: WideString;
begin
  GetPropertyValue('tagName', Result);
end;

function TjsHTMLElement.Get_title: WideString;
begin

end;

procedure TjsHTMLElement.insertAdjacentHTML(const where, html: WideString);
begin

end;

procedure TjsHTMLElement.insertAdjacentText(const where, text: WideString);
begin

end;

function TjsHTMLElement.removeAttribute(const strAttributeName: WideString;
  lFlags: Integer): WordBool;
begin

end;

procedure TjsHTMLElement.scrollIntoView(varargStart: OleVariant);
begin

end;

procedure TjsHTMLElement.setAttribute(const strAttributeName: WideString;
  AttributeValue: OleVariant; lFlags: Integer);
begin

end;

procedure TjsHTMLElement.Set_className(const p: WideString);
begin

end;

procedure TjsHTMLElement.Set_id(const p: WideString);
begin

end;

procedure TjsHTMLElement.Set_innerHTML(const p: WideString);
begin

end;

procedure TjsHTMLElement.Set_innerText(const p: WideString);
begin

end;

procedure TjsHTMLElement.Set_lang(const p: WideString);
begin

end;

procedure TjsHTMLElement.Set_language(const p: WideString);
begin

end;

procedure TjsHTMLElement.Set_onafterupdate(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_onbeforeupdate(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_onclick(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_ondataavailable(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_ondatasetchanged(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_ondatasetcomplete(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_ondblclick(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_ondragstart(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_onerrorupdate(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_onfilterchange(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_onhelp(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_onkeydown(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_onkeypress(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_onkeyup(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_onmousedown(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_onmousemove(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_onmouseout(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_onmouseover(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_onmouseup(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_onrowenter(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_onrowexit(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_onselectstart(p: OleVariant);
begin

end;

procedure TjsHTMLElement.Set_outerHTML(const p: WideString);
begin

end;

procedure TjsHTMLElement.Set_outerText(const p: WideString);
begin

end;

procedure TjsHTMLElement.Set_title(const p: WideString);
begin

end;

function TjsHTMLElement.toString: WideString;
begin

end;

{ TjsHTMLFramesCollection }

function TjsHTMLFramesCollection.Get_length: Integer;
begin
  GetPropertyValue('length', Result);
end;

function TjsHTMLFramesCollection.item(const pvarIndex: OleVariant): OleVariant;
begin
  Result:=null;
  //todo: fix this!
end;

{ TjsHTMLWindow }

procedure TjsHTMLWindow.alert(const message: WideString);
begin
  ExecMethod('alert('+ToJSString(message)+')', True);
end;

procedure TjsHTMLWindow.blur;
begin
  ExecMethod('blur');
end;

procedure TjsHTMLWindow.clearInterval(timerID: Integer);
begin

end;

procedure TjsHTMLWindow.clearTimeout(timerID: Integer);
begin

end;

procedure TjsHTMLWindow.close;
begin
  ExecMethod('close');
end;

function TjsHTMLWindow.confirm(const message: WideString): WordBool;
begin
  Result:=IsPositiveMethodResult(ExecMethod('confirm('+ToJSString(message)+')',True));
end;

function TjsHTMLWindow.execScript(const code, language: WideString): OleVariant;
begin
  ExecMethod('execScript('+ToJSString(code)+','+ToJSString(language)+')');
end;

procedure TjsHTMLWindow.focus;
begin
  ExecMethod('focus');
end;

function TjsHTMLWindow.Get_clientInformation: IOmNavigator;
begin
  Result:=TjsOMNavigator.Create(FApplication, _JSVar+'.clientInformation');
end;

function TjsHTMLWindow.Get_closed: WordBool;
begin
  GetPropertyValue('closed', Result);
end;

function TjsHTMLWindow.Get_defaultStatus: WideString;
begin
  GetPropertyValue('defaultStatus', Result);
end;

function TjsHTMLWindow.Get_document: IHTMLDocument2;
begin
  Result:=TjsHTMLDocument.Create(FApplication, _JSVar+'.document');
end;

function TjsHTMLWindow.Get_event: IHTMLEventObj;
begin
  //todo: IHTMLEventObj
end;

function TjsHTMLWindow.Get_external: IDispatch;
begin
  //todo: window get external?
end;

function TjsHTMLWindow.Get_frames: IHTMLFramesCollection2;
begin
  Result:=TjsHTMLFramesCollection.Create(FApplication, _JSVar+'.frames');
end;

function TjsHTMLWindow.Get_history: IOmHistory;
begin

end;

function TjsHTMLWindow.Get_Image: IHTMLImageElementFactory;
begin

end;

function TjsHTMLWindow.Get_location: IHTMLLocation;
begin

end;

function TjsHTMLWindow.Get_name: WideString;
begin
  GetPropertyValue('name', Result);
end;

function TjsHTMLWindow.Get_navigator: IOmNavigator;
begin

end;

function TjsHTMLWindow.Get_offscreenBuffering: OleVariant;
begin
  GetPropertyValue('offscreenBuffering', Result);
end;

function TjsHTMLWindow.Get_onbeforeunload: OleVariant;
begin
  GetPropertyValue('onbeforeunload', Result);
end;

function TjsHTMLWindow.Get_onblur: OleVariant;
begin
  GetPropertyValue('onblur', Result);
end;

function TjsHTMLWindow.Get_onerror: OleVariant;
begin
  GetPropertyValue('onerror', Result);
end;

function TjsHTMLWindow.Get_onfocus: OleVariant;
begin
  GetPropertyValue('onfocus', Result);

end;

function TjsHTMLWindow.Get_onhelp: OleVariant;
begin
  GetPropertyValue('onhelp', Result);
end;

function TjsHTMLWindow.Get_onload: OleVariant;
begin
  GetPropertyValue('onload', Result);
end;

function TjsHTMLWindow.Get_onresize: OleVariant;
begin
  GetPropertyValue('onresize', Result);
end;

function TjsHTMLWindow.Get_onscroll: OleVariant;
begin
  GetPropertyValue('onscroll', Result);
end;

function TjsHTMLWindow.Get_onunload: OleVariant;
begin
  GetPropertyValue('onunload', Result);
end;

function TjsHTMLWindow.Get_opener: OleVariant;
begin
  GetPropertyValue('opener', Result);
end;

function TjsHTMLWindow.Get_Option: IHTMLOptionElementFactory;
begin
  //todo: IHTMLOptionElementFactory;
end;

function TjsHTMLWindow.Get_parent: IHTMLWindow2;
begin
  Result:=TjsHTMLWindow.Create(FApplication, _JSVar+'.parent');
end;

function TjsHTMLWindow.Get_screen: IHTMLScreen;
begin
  //Todo: IHTMLScreen
end;

function TjsHTMLWindow.Get_self: IHTMLWindow2;
begin
  Result:=self; //;-) minimize network traffic
end;

function TjsHTMLWindow.Get_status: WideString;
begin
  GetPropertyValue('status', Result);
end;

function TjsHTMLWindow.Get_top: IHTMLWindow2;
begin
  Result:=TjsHTMLWindow.Create(FApplication, _JSVar+'.top');
end;

function TjsHTMLWindow.Get_window: IHTMLWindow2;
begin
  Result:=TjsHTMLWindow.Create(FApplication, _JSVar+'.window');
end;

function TjsHTMLWindow.Get__newEnum: IUnknown;
begin
  Result:=Nil;
end;

procedure TjsHTMLWindow.moveBy(x, y: Integer);
begin

end;

procedure TjsHTMLWindow.moveTo(x, y: Integer);
begin

end;

procedure TjsHTMLWindow.navigate(const url: WideString);
begin

end;

function TjsHTMLWindow.open(const url, name, features: WideString;
  replace: WordBool): IHTMLWindow2;
begin

end;

function TjsHTMLWindow.prompt(const message, defstr: WideString): OleVariant;
begin

end;

procedure TjsHTMLWindow.resizeBy(x, y: Integer);
begin

end;

procedure TjsHTMLWindow.resizeTo(x, y: Integer);
begin

end;

procedure TjsHTMLWindow.scroll(x, y: Integer);
begin

end;

procedure TjsHTMLWindow.scrollBy(x, y: Integer);
begin

end;

procedure TjsHTMLWindow.scrollTo(x, y: Integer);
begin

end;

function TjsHTMLWindow.setInterval(const expression: WideString; msec: Integer;
  var language: OleVariant): Integer;
begin

end;

function TjsHTMLWindow.setTimeout(const expression: WideString; msec: Integer;
  var language: OleVariant): Integer;
begin

end;

procedure TjsHTMLWindow.Set_defaultStatus(const p: WideString);
begin

end;

procedure TjsHTMLWindow.Set_name(const p: WideString);
begin

end;

procedure TjsHTMLWindow.Set_offscreenBuffering(p: OleVariant);
begin

end;

procedure TjsHTMLWindow.Set_onbeforeunload(p: OleVariant);
begin

end;

procedure TjsHTMLWindow.Set_onblur(p: OleVariant);
begin

end;

procedure TjsHTMLWindow.Set_onerror(p: OleVariant);
begin

end;

procedure TjsHTMLWindow.Set_onfocus(p: OleVariant);
begin

end;

procedure TjsHTMLWindow.Set_onhelp(p: OleVariant);
begin

end;

procedure TjsHTMLWindow.Set_onload(p: OleVariant);
begin

end;

procedure TjsHTMLWindow.Set_onresize(p: OleVariant);
begin

end;

procedure TjsHTMLWindow.Set_onscroll(p: OleVariant);
begin

end;

procedure TjsHTMLWindow.Set_onunload(p: OleVariant);
begin

end;

procedure TjsHTMLWindow.Set_opener(p: OleVariant);
begin

end;

procedure TjsHTMLWindow.Set_status(const p: WideString);
begin

end;

procedure TjsHTMLWindow.showHelp(const helpURL: WideString; helpArg: OleVariant;
  const features: WideString);
begin

end;

function TjsHTMLWindow.showModalDialog(const dialog: WideString; var varArgIn,
  varOptions: OleVariant): OleVariant;
begin

end;

function TjsHTMLWindow.toString: WideString;
begin

end;

{ TjsOMNavigator }

function TjsOMNavigator.Get_appCodeName: WideString;
begin
  GetPropertyValue('appCodeName', Result);
end;

function TjsOMNavigator.Get_appMinorVersion: WideString;
begin
  GetPropertyValue('appMinorVersion', Result);
end;

function TjsOMNavigator.Get_appName: WideString;
begin
  GetPropertyValue('appName', Result);
end;

function TjsOMNavigator.Get_appVersion: WideString;
begin
  GetPropertyValue('appVersion', Result);
end;

function TjsOMNavigator.Get_browserLanguage: WideString;
begin
  GetPropertyValue('browserLanguage', Result);
end;

function TjsOMNavigator.Get_connectionSpeed: Integer;
begin
  GetPropertyValue('connectionSpeed', Result);
end;

function TjsOMNavigator.Get_cookieEnabled: WordBool;
begin
  GetPropertyValue('cookieEnabled', Result);
end;

function TjsOMNavigator.Get_cpuClass: WideString;
begin
  GetPropertyValue('cpuClass', Result);
end;

function TjsOMNavigator.Get_mimeTypes: IHTMLMimeTypesCollection;
begin
  //todo: Mimetypes collection
end;

function TjsOMNavigator.Get_onLine: WordBool;
begin
  GetPropertyValue('onLine', Result);
end;

function TjsOMNavigator.Get_opsProfile: IHTMLOpsProfile;
begin
  //todo: OpsProfile
end;

function TjsOMNavigator.Get_platform: WideString;
begin
  GetPropertyValue('platform', Result);
end;

function TjsOMNavigator.Get_plugins: IHTMLPluginsCollection;
begin
  //todo: htmlpluginscolllection
end;

function TjsOMNavigator.Get_systemLanguage: WideString;
begin
  GetPropertyValue('systemLanguage', Result);
end;

function TjsOMNavigator.Get_userAgent: WideString;
begin
  GetPropertyValue('userAgent', Result);
end;

function TjsOMNavigator.Get_userLanguage: WideString;
begin
  GetPropertyValue('userLanguage', Result);
end;

function TjsOMNavigator.Get_userProfile: IHTMLOpsProfile;
begin
  //todo: opsProfile
end;

function TjsOMNavigator.javaEnabled: WordBool;
begin
  GetPropertyValue('javaEnabled', Result);
end;

function TjsOMNavigator.taintEnabled: WordBool;
begin
  GetPropertyValue('taintEnabled', Result);
end;

function TjsOMNavigator.toString: WideString;
begin
  Result:=ExecMethod('toString', True);
end;

end.
