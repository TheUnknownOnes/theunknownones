unit uJSDOM;

(*
 * This is a "simple demonstration" what you can do with jsDirect.
 * ujsDOM is a wrapper for "all" native built in JavaScript objects.
 * As you can guess these are not ALL! objects ... but we opted for the most
 * important ones (in our opinion)
 * The classes were built largely automatically ... so there may be this or that
 * method or property that does not work. Also we chose mshtml as a base.
 * THIS IS COMPLETELY WRONG :-D we know that, since IE does not support WebSocket.
 * If you have a complete interface list of the Firefox/Chromium DOM ... just
 * give us a hint or implement it yourself.
 *)

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

  IjsDOMCollection = interface
  ['{B8FBDF2B-1362-42F9-9889-1C034E3A6D51}']
    function Get_length: Integer; safecall;
    function Get__newEnum: IUnknown; safecall;
    function item(name: OleVariant; index: OleVariant): IDispatch; overload; safecall;
    function item(const name: OleVariant): IDispatch; overload; safecall;
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

  TjsDOMObjectClass = class of TjsDOMObject;

  TjsDOMCollection = class(TjsDOMObject, IjsDOMCollection)
  private
    FElementClass: TjsDOMObjectClass;
  protected
    function Get_length: Integer; safecall;
    function Get__newEnum: IUnknown; safecall;
    function item(name: OleVariant; index: OleVariant): IDispatch; overload; safecall;
    function item(const name: OleVariant): IDispatch; overload; safecall;
  public
    constructor Create(AApplication : TjsdApplication; ACreateCommand : String; AElementClass: TjsDOMObjectClass); reintroduce;
  end;

  TjsHTMLElementCollection = class(TjsDOMCollection, IHTMLElementCollection, IHTMLElementCollection3)
  protected
    {$REGION 'IHTMLElementCollection'}
    function toString: WideString; reintroduce; safecall;
    procedure Set_length(p: Integer); safecall;
    function tags(tagName: OleVariant): IDispatch; safecall;
    {$ENDREGION}

    {$REGION 'IHTMLElementCollection3'}
    function namedItem(const name: WideString): IDispatch; safecall;
    {$ENDREGION}
  end;

  TjsHTMLAttributeCollection = class(TjsDOMCollection, IHTMLAttributeCollection, IHTMLAttributeCollection2)
  protected
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

  TjsHTMLElement = class(TjsHTMLDOMNode,IHTMLElement,IHTMLElement2,IHTMLElement3,IHTMLElement4)
  private
    function addBehavior(const bstrUrl: WideString; var pvarFactory: OleVariant): Integer; safecall;
    function applyElement(const apply: IHTMLElement; const where: WideString): IHTMLElement; safecall;
    function attachEvent(const event: WideString; const pdisp: IDispatch): WordBool; safecall;
    function componentFromPoint(x: Integer; y: Integer): WideString; safecall;
    function contains(const pChild: IHTMLElement): WordBool; safecall;
    function createControlRange: IDispatch; safecall;
    function dragDrop: WordBool; safecall;
    function FireEvent(const bstrEventName: WideString; var pvarEventObject: OleVariant): WordBool; safecall;
    function Get_accessKey: WideString; safecall;
    function Get_all: IDispatch; safecall;
    function Get_behaviorUrns: IDispatch; safecall;
    function Get_canHaveChildren: WordBool; safecall;
    function Get_canHaveHTML: WordBool; safecall;
    function Get_children: IDispatch; safecall;
    function Get_className: WideString; safecall;
    function Get_clientHeight: Integer; safecall;
    function Get_clientLeft: Integer; safecall;
    function Get_clientTop: Integer; safecall;
    function Get_clientWidth: Integer; safecall;
    function Get_contentEditable: WideString; safecall;
    function Get_currentStyle: IHTMLCurrentStyle; safecall;
    function Get_dir: WideString; safecall;
    function Get_disabled: WordBool; safecall;
    function Get_document: IDispatch; safecall;
    function Get_filters: IHTMLFiltersCollection; safecall;
    function Get_glyphMode: Integer; safecall;
    function Get_hideFocus: WordBool; safecall;
    function Get_id: WideString; safecall;
    function Get_inflateBlock: WordBool; safecall;
    function Get_innerHTML: WideString; safecall;
    function Get_innerText: WideString; safecall;
    function Get_isContentEditable: WordBool; safecall;
    function Get_isDisabled: WordBool; safecall;
    function Get_isMultiLine: WordBool; safecall;
    function Get_isTextEdit: WordBool; safecall;
    function Get_lang: WideString; safecall;
    function Get_language: WideString; safecall;
    function Get_offsetHeight: Integer; safecall;
    function Get_offsetLeft: Integer; safecall;
    function Get_offsetParent: IHTMLElement; safecall;
    function Get_offsetTop: Integer; safecall;
    function Get_offsetWidth: Integer; safecall;
    function Get_onactivate: OleVariant; safecall;
    function Get_onafterupdate: OleVariant; safecall;
    function Get_onbeforeactivate: OleVariant; safecall;
    function Get_onbeforecopy: OleVariant; safecall;
    function Get_onbeforecut: OleVariant; safecall;
    function Get_onbeforedeactivate: OleVariant; safecall;
    function Get_onbeforeeditfocus: OleVariant; safecall;
    function Get_onbeforepaste: OleVariant; safecall;
    function Get_onbeforeupdate: OleVariant; safecall;
    function Get_onblur: OleVariant; safecall;
    function Get_oncellchange: OleVariant; safecall;
    function Get_onclick: OleVariant; safecall;
    function Get_oncontextmenu: OleVariant; safecall;
    function Get_oncontrolselect: OleVariant; safecall;
    function Get_oncopy: OleVariant; safecall;
    function Get_oncut: OleVariant; safecall;
    function Get_ondataavailable: OleVariant; safecall;
    function Get_ondatasetchanged: OleVariant; safecall;
    function Get_ondatasetcomplete: OleVariant; safecall;
    function Get_ondblclick: OleVariant; safecall;
    function Get_ondeactivate: OleVariant; safecall;
    function Get_ondrag: OleVariant; safecall;
    function Get_ondragend: OleVariant; safecall;
    function Get_ondragenter: OleVariant; safecall;
    function Get_ondragleave: OleVariant; safecall;
    function Get_ondragover: OleVariant; safecall;
    function Get_ondragstart: OleVariant; safecall;
    function Get_ondrop: OleVariant; safecall;
    function Get_onerrorupdate: OleVariant; safecall;
    function Get_onfilterchange: OleVariant; safecall;
    function Get_onfocus: OleVariant; safecall;
    function Get_onfocusin: OleVariant; safecall;
    function Get_onfocusout: OleVariant; safecall;
    function Get_onhelp: OleVariant; safecall;
    function Get_onkeydown: OleVariant; safecall;
    function Get_onkeypress: OleVariant; safecall;
    function Get_onkeyup: OleVariant; safecall;
    function Get_onlayoutcomplete: OleVariant; safecall;
    function Get_onlosecapture: OleVariant; safecall;
    function Get_onmousedown: OleVariant; safecall;
    function Get_onmouseenter: OleVariant; safecall;
    function Get_onmouseleave: OleVariant; safecall;
    function Get_onmousemove: OleVariant; safecall;
    function Get_onmouseout: OleVariant; safecall;
    function Get_onmouseover: OleVariant; safecall;
    function Get_onmouseup: OleVariant; safecall;
    function Get_onmousewheel: OleVariant; safecall;
    function Get_onmove: OleVariant; safecall;
    function Get_onmoveend: OleVariant; safecall;
    function Get_onmovestart: OleVariant; safecall;
    function Get_onpage: OleVariant; safecall;
    function Get_onpaste: OleVariant; safecall;
    function Get_onpropertychange: OleVariant; safecall;
    function Get_onreadystatechange: OleVariant; safecall;
    function Get_onresize: OleVariant; safecall;
    function Get_onresizeend: OleVariant; safecall;
    function Get_onresizestart: OleVariant; safecall;
    function Get_onrowenter: OleVariant; safecall;
    function Get_onrowexit: OleVariant; safecall;
    function Get_onrowsdelete: OleVariant; safecall;
    function Get_onrowsinserted: OleVariant; safecall;
    function Get_onscroll: OleVariant; safecall;
    function Get_onselectstart: OleVariant; safecall;
    function Get_outerHTML: WideString; safecall;
    function Get_outerText: WideString; safecall;
    function Get_parentElement: IHTMLElement; safecall;
    function Get_parentTextEdit: IHTMLElement; safecall;
    function Get_readyState: OleVariant; safecall;
    function Get_readyStateValue: Integer; safecall;
    function Get_recordNumber: OleVariant; safecall;
    function Get_runtimeStyle: IHTMLStyle; safecall;
    function Get_scopeName: WideString; safecall;
    function Get_scrollHeight: Integer; safecall;
    function Get_scrollLeft: Integer; safecall;
    function Get_scrollTop: Integer; safecall;
    function Get_scrollWidth: Integer; safecall;
    function Get_sourceIndex: Integer; safecall;
    function Get_style: IHTMLStyle; safecall;
    function Get_tabIndex: Smallint; safecall;
    function Get_tagName: WideString; safecall;
    function Get_tagUrn: WideString; safecall;
    function Get_title: WideString; safecall;
    function getAdjacentText(const where: WideString): WideString; safecall;
    function getAttribute(const strAttributeName: WideString; lFlags: Integer): OleVariant; safecall;
    function getAttributeNode(const bstrName: WideString): IHTMLDOMAttribute; safecall;
    function getBoundingClientRect: IHTMLRect; safecall;
    function getClientRects: IHTMLRectCollection; safecall;
    function getElementsByTagName(const v: WideString): IHTMLElementCollection; safecall;
    function getExpression(const propname: WideString): OleVariant; safecall;
    function insertAdjacentElement(const where: WideString; const insertedElement: IHTMLElement): IHTMLElement; safecall;
    function removeAttribute(const strAttributeName: WideString; lFlags: Integer): WordBool; safecall;
    function removeAttributeNode(const pattr: IHTMLDOMAttribute): IHTMLDOMAttribute; safecall;
    function removeBehavior(cookie: Integer): WordBool; safecall;
    function removeExpression(const propname: WideString): WordBool; safecall;
    function replaceAdjacentText(const where: WideString; const newText: WideString): WideString; safecall;
    function setAttributeNode(const pattr: IHTMLDOMAttribute): IHTMLDOMAttribute; safecall;
    function toString: WideString; safecall;
    procedure addFilter(const pUnk: IUnknown); safecall;
    procedure blur; safecall;
    procedure clearAttributes; safecall;
    procedure click; safecall;
    procedure detachEvent(const event: WideString; const pdisp: IDispatch); safecall;
    procedure doScroll(component: OleVariant); safecall;
    procedure focus; safecall;
    procedure insertAdjacentHTML(const where: WideString; const html: WideString); safecall;
    procedure insertAdjacentText(const where: WideString; const text: WideString); safecall;
    procedure mergeAttributes(const mergeThis: IHTMLElement); overload; safecall;
    procedure mergeAttributes(const mergeThis: IHTMLElement; var pvarFlags: OleVariant); overload; safecall;
    procedure normalize; safecall;
    procedure releaseCapture; safecall;
    procedure removeFilter(const pUnk: IUnknown); safecall;
    procedure scrollIntoView(varargStart: OleVariant); safecall;
    procedure Set_accessKey(const p: WideString); safecall;
    procedure Set_className(const p: WideString); safecall;
    procedure Set_contentEditable(const p: WideString); safecall;
    procedure Set_dir(const p: WideString); safecall;
    procedure Set_disabled(p: WordBool); safecall;
    procedure Set_hideFocus(p: WordBool); safecall;
    procedure Set_id(const p: WideString); safecall;
    procedure Set_inflateBlock(p: WordBool); safecall;
    procedure Set_innerHTML(const p: WideString); safecall;
    procedure Set_innerText(const p: WideString); safecall;
    procedure Set_lang(const p: WideString); safecall;
    procedure Set_language(const p: WideString); safecall;
    procedure Set_onactivate(p: OleVariant); safecall;
    procedure Set_onafterupdate(p: OleVariant); safecall;
    procedure Set_onbeforeactivate(p: OleVariant); safecall;
    procedure Set_onbeforecopy(p: OleVariant); safecall;
    procedure Set_onbeforecut(p: OleVariant); safecall;
    procedure Set_onbeforedeactivate(p: OleVariant); safecall;
    procedure Set_onbeforeeditfocus(p: OleVariant); safecall;
    procedure Set_onbeforepaste(p: OleVariant); safecall;
    procedure Set_onbeforeupdate(p: OleVariant); safecall;
    procedure Set_onblur(p: OleVariant); safecall;
    procedure Set_oncellchange(p: OleVariant); safecall;
    procedure Set_onclick(p: OleVariant); safecall;
    procedure Set_oncontextmenu(p: OleVariant); safecall;
    procedure Set_oncontrolselect(p: OleVariant); safecall;
    procedure Set_oncopy(p: OleVariant); safecall;
    procedure Set_oncut(p: OleVariant); safecall;
    procedure Set_ondataavailable(p: OleVariant); safecall;
    procedure Set_ondatasetchanged(p: OleVariant); safecall;
    procedure Set_ondatasetcomplete(p: OleVariant); safecall;
    procedure Set_ondblclick(p: OleVariant); safecall;
    procedure Set_ondeactivate(p: OleVariant); safecall;
    procedure Set_ondrag(p: OleVariant); safecall;
    procedure Set_ondragend(p: OleVariant); safecall;
    procedure Set_ondragenter(p: OleVariant); safecall;
    procedure Set_ondragleave(p: OleVariant); safecall;
    procedure Set_ondragover(p: OleVariant); safecall;
    procedure Set_ondragstart(p: OleVariant); safecall;
    procedure Set_ondrop(p: OleVariant); safecall;
    procedure Set_onerrorupdate(p: OleVariant); safecall;
    procedure Set_onfilterchange(p: OleVariant); safecall;
    procedure Set_onfocus(p: OleVariant); safecall;
    procedure Set_onfocusin(p: OleVariant); safecall;
    procedure Set_onfocusout(p: OleVariant); safecall;
    procedure Set_onhelp(p: OleVariant); safecall;
    procedure Set_onkeydown(p: OleVariant); safecall;
    procedure Set_onkeypress(p: OleVariant); safecall;
    procedure Set_onkeyup(p: OleVariant); safecall;
    procedure Set_onlayoutcomplete(p: OleVariant); safecall;
    procedure Set_onlosecapture(p: OleVariant); safecall;
    procedure Set_onmousedown(p: OleVariant); safecall;
    procedure Set_onmouseenter(p: OleVariant); safecall;
    procedure Set_onmouseleave(p: OleVariant); safecall;
    procedure Set_onmousemove(p: OleVariant); safecall;
    procedure Set_onmouseout(p: OleVariant); safecall;
    procedure Set_onmouseover(p: OleVariant); safecall;
    procedure Set_onmouseup(p: OleVariant); safecall;
    procedure Set_onmousewheel(p: OleVariant); safecall;
    procedure Set_onmove(p: OleVariant); safecall;
    procedure Set_onmoveend(p: OleVariant); safecall;
    procedure Set_onmovestart(p: OleVariant); safecall;
    procedure Set_onpage(p: OleVariant); safecall;
    procedure Set_onpaste(p: OleVariant); safecall;
    procedure Set_onpropertychange(p: OleVariant); safecall;
    procedure Set_onreadystatechange(p: OleVariant); safecall;
    procedure Set_onresize(p: OleVariant); safecall;
    procedure Set_onresizeend(p: OleVariant); safecall;
    procedure Set_onresizestart(p: OleVariant); safecall;
    procedure Set_onrowenter(p: OleVariant); safecall;
    procedure Set_onrowexit(p: OleVariant); safecall;
    procedure Set_onrowsdelete(p: OleVariant); safecall;
    procedure Set_onrowsinserted(p: OleVariant); safecall;
    procedure Set_onscroll(p: OleVariant); safecall;
    procedure Set_onselectstart(p: OleVariant); safecall;
    procedure Set_outerHTML(const p: WideString); safecall;
    procedure Set_outerText(const p: WideString); safecall;
    procedure Set_scrollLeft(p: Integer); safecall;
    procedure Set_scrollTop(p: Integer); safecall;
    procedure Set_tabIndex(p: Smallint); safecall;
    procedure Set_tagUrn(const p: WideString); safecall;
    procedure Set_title(const p: WideString); safecall;
    procedure setActive; safecall;
    procedure setAttribute(const strAttributeName: WideString; AttributeValue: OleVariant;  lFlags: Integer); safecall;
    procedure setCapture(containerCapture: WordBool); safecall;
    procedure setExpression(const propname: WideString; const expression: WideString;  const language: WideString); safecall;
  end;

  TjsHTMLFormElement = class(TjsHTMLElement,IHTMLFormElement,IHTMLFormElement2,IHTMLFormElement3)
  private
    function Get__newEnum: IUnknown; safecall;
    function Get_acceptCharset: WideString; safecall;
    function Get_action: WideString; safecall;
    function Get_dir: WideString; safecall;
    function Get_elements: IDispatch; safecall;
    function Get_encoding: WideString; safecall;
    function Get_length: Integer; safecall;
    function Get_method: WideString; safecall;
    function Get_name: WideString; safecall;
    function Get_onreset: OleVariant; safecall;
    function Get_onsubmit: OleVariant; safecall;
    function Get_target: WideString; safecall;
    function item(name: OleVariant; index: OleVariant): IDispatch; safecall;
    function namedItem(const name: WideString): IDispatch; safecall;
    function tags(tagName: OleVariant): IDispatch; safecall;
    function urns(urn: OleVariant): IDispatch; safecall;
    procedure reset; safecall;
    procedure Set_acceptCharset(const p: WideString); safecall;
    procedure Set_action(const p: WideString); safecall;
    procedure Set_dir(const p: WideString); safecall;
    procedure Set_encoding(const p: WideString); safecall;
    procedure Set_length(p: Integer); safecall;
    procedure Set_method(const p: WideString); safecall;
    procedure Set_name(const p: WideString); safecall;
    procedure Set_onreset(p: OleVariant); safecall;
    procedure Set_onsubmit(p: OleVariant); safecall;
    procedure Set_target(const p: WideString); safecall;
    procedure submit; safecall;
  end;

  TjsHTMLRect = class(TjsDOMObject,IHTMLRect)
  private
    function Get_bottom: Integer; safecall;
    function Get_left: Integer; safecall;
    function Get_right: Integer; safecall;
    function Get_top: Integer; safecall;
    procedure Set_bottom(p: Integer); safecall;
    procedure Set_left(p: Integer); safecall;
    procedure Set_right(p: Integer); safecall;
    procedure Set_top(p: Integer); safecall;
  end;

  TjsHTMLRectCollection = class(TjsDOMCollection,IHTMLRectCollection)
  private
    function item(const pvarIndex: OleVariant): OleVariant; safecall;
  end;

  TjsHTMLStyle = class(TjsDOMObject,IHTMLStyle,IHTMLStyle2,IHTMLStyle3,IHTMLStyle4)
  private
    function Get_accelerator: WideString; safecall;
    function Get_background: WideString; safecall;
    function Get_backgroundAttachment: WideString; safecall;
    function Get_backgroundColor: OleVariant; safecall;
    function Get_backgroundImage: WideString; safecall;
    function Get_backgroundPosition: WideString; safecall;
    function Get_backgroundPositionX: OleVariant; safecall;
    function Get_backgroundPositionY: OleVariant; safecall;
    function Get_backgroundRepeat: WideString; safecall;
    function Get_behavior: WideString; safecall;
    function Get_border: WideString; safecall;
    function Get_borderBottom: WideString; safecall;
    function Get_borderBottomColor: OleVariant; safecall;
    function Get_borderBottomStyle: WideString; safecall;
    function Get_borderBottomWidth: OleVariant; safecall;
    function Get_borderCollapse: WideString; safecall;
    function Get_borderColor: WideString; safecall;
    function Get_borderLeft: WideString; safecall;
    function Get_borderLeftColor: OleVariant; safecall;
    function Get_borderLeftStyle: WideString; safecall;
    function Get_borderLeftWidth: OleVariant; safecall;
    function Get_borderRight: WideString; safecall;
    function Get_borderRightColor: OleVariant; safecall;
    function Get_borderRightStyle: WideString; safecall;
    function Get_borderRightWidth: OleVariant; safecall;
    function Get_borderStyle: WideString; safecall;
    function Get_borderTop: WideString; safecall;
    function Get_borderTopColor: OleVariant; safecall;
    function Get_borderTopStyle: WideString; safecall;
    function Get_borderTopWidth: OleVariant; safecall;
    function Get_borderWidth: WideString; safecall;
    function Get_bottom: OleVariant; safecall;
    function Get_clear: WideString; safecall;
    function Get_clip: WideString; safecall;
    function Get_color: OleVariant; safecall;
    function Get_cssText: WideString; safecall;
    function Get_cursor: WideString; safecall;
    function Get_direction: WideString; safecall;
    function Get_display: WideString; safecall;
    function Get_filter: WideString; safecall;
    function Get_font: WideString; safecall;
    function Get_fontFamily: WideString; safecall;
    function Get_fontSize: OleVariant; safecall;
    function Get_fontStyle: WideString; safecall;
    function Get_fontVariant: WideString; safecall;
    function Get_fontWeight: WideString; safecall;
    function Get_height: OleVariant; safecall;
    function Get_imeMode: WideString; safecall;
    function Get_layoutFlow: WideString; safecall;
    function Get_layoutGrid: WideString; safecall;
    function Get_layoutGridChar: OleVariant; safecall;
    function Get_layoutGridLine: OleVariant; safecall;
    function Get_layoutGridMode: WideString; safecall;
    function Get_layoutGridType: WideString; safecall;
    function Get_left: OleVariant; safecall;
    function Get_letterSpacing: OleVariant; safecall;
    function Get_lineBreak: WideString; safecall;
    function Get_lineHeight: OleVariant; safecall;
    function Get_listStyle: WideString; safecall;
    function Get_listStyleImage: WideString; safecall;
    function Get_listStylePosition: WideString; safecall;
    function Get_listStyleType: WideString; safecall;
    function Get_margin: WideString; safecall;
    function Get_marginBottom: OleVariant; safecall;
    function Get_marginLeft: OleVariant; safecall;
    function Get_marginRight: OleVariant; safecall;
    function Get_marginTop: OleVariant; safecall;
    function Get_minHeight: OleVariant; safecall;
    function Get_overflow: WideString; safecall;
    function Get_overflowX: WideString; safecall;
    function Get_overflowY: WideString; safecall;
    function Get_padding: WideString; safecall;
    function Get_paddingBottom: OleVariant; safecall;
    function Get_paddingLeft: OleVariant; safecall;
    function Get_paddingRight: OleVariant; safecall;
    function Get_paddingTop: OleVariant; safecall;
    function Get_pageBreakAfter: WideString; safecall;
    function Get_pageBreakBefore: WideString; safecall;
    function Get_pixelBottom: Integer; safecall;
    function Get_pixelHeight: Integer; safecall;
    function Get_pixelLeft: Integer; safecall;
    function Get_pixelRight: Integer; safecall;
    function Get_pixelTop: Integer; safecall;
    function Get_pixelWidth: Integer; safecall;
    function Get_posBottom: Single; safecall;
    function Get_posHeight: Single; safecall;
    function Get_position: WideString; safecall;
    function Get_posLeft: Single; safecall;
    function Get_posRight: Single; safecall;
    function Get_posTop: Single; safecall;
    function Get_posWidth: Single; safecall;
    function Get_right: OleVariant; safecall;
    function Get_rubyAlign: WideString; safecall;
    function Get_rubyOverhang: WideString; safecall;
    function Get_rubyPosition: WideString; safecall;
    function Get_scrollbar3dLightColor: OleVariant; safecall;
    function Get_scrollbarArrowColor: OleVariant; safecall;
    function Get_scrollbarBaseColor: OleVariant; safecall;
    function Get_scrollbarDarkShadowColor: OleVariant; safecall;
    function Get_scrollbarFaceColor: OleVariant; safecall;
    function Get_scrollbarHighlightColor: OleVariant; safecall;
    function Get_scrollbarShadowColor: OleVariant; safecall;
    function Get_scrollbarTrackColor: OleVariant; safecall;
    function Get_styleFloat: WideString; safecall;
    function Get_tableLayout: WideString; safecall;
    function Get_textAlign: WideString; safecall;
    function Get_textAlignLast: WideString; safecall;
    function Get_textAutospace: WideString; safecall;
    function Get_textDecoration: WideString; safecall;
    function Get_textDecorationBlink: WordBool; safecall;
    function Get_textDecorationLineThrough: WordBool; safecall;
    function Get_textDecorationNone: WordBool; safecall;
    function Get_textDecorationOverline: WordBool; safecall;
    function Get_textDecorationUnderline: WordBool; safecall;
    function Get_textIndent: OleVariant; safecall;
    function Get_textJustify: WideString; safecall;
    function Get_textJustifyTrim: WideString; safecall;
    function Get_textKashida: OleVariant; safecall;
    function Get_textKashidaSpace: OleVariant; safecall;
    function Get_textOverflow: WideString; safecall;
    function Get_textTransform: WideString; safecall;
    function Get_textUnderlinePosition: WideString; safecall;
    function Get_top: OleVariant; safecall;
    function Get_unicodeBidi: WideString; safecall;
    function Get_verticalAlign: OleVariant; safecall;
    function Get_visibility: WideString; safecall;
    function Get_whiteSpace: WideString; safecall;
    function Get_width: OleVariant; safecall;
    function Get_wordBreak: WideString; safecall;
    function Get_wordSpacing: OleVariant; safecall;
    function Get_wordWrap: WideString; safecall;
    function Get_writingMode: WideString; safecall;
    function Get_zIndex: OleVariant; safecall;
    function Get_zoom: OleVariant; safecall;
    function getAttribute(const strAttributeName: WideString; lFlags: Integer): OleVariant; safecall;
    function getExpression(const propname: WideString): OleVariant; safecall;
    function removeAttribute(const strAttributeName: WideString; lFlags: Integer): WordBool; safecall;
    function removeExpression(const propname: WideString): WordBool; safecall;
    function toString: WideString; safecall;
    procedure Set_accelerator(const p: WideString); safecall;
    procedure Set_background(const p: WideString); safecall;
    procedure Set_backgroundAttachment(const p: WideString); safecall;
    procedure Set_backgroundColor(p: OleVariant); safecall;
    procedure Set_backgroundImage(const p: WideString); safecall;
    procedure Set_backgroundPosition(const p: WideString); safecall;
    procedure Set_backgroundPositionX(p: OleVariant); safecall;
    procedure Set_backgroundPositionY(p: OleVariant); safecall;
    procedure Set_backgroundRepeat(const p: WideString); safecall;
    procedure Set_behavior(const p: WideString); safecall;
    procedure Set_border(const p: WideString); safecall;
    procedure Set_borderBottom(const p: WideString); safecall;
    procedure Set_borderBottomColor(p: OleVariant); safecall;
    procedure Set_borderBottomStyle(const p: WideString); safecall;
    procedure Set_borderBottomWidth(p: OleVariant); safecall;
    procedure Set_borderCollapse(const p: WideString); safecall;
    procedure Set_borderColor(const p: WideString); safecall;
    procedure Set_borderLeft(const p: WideString); safecall;
    procedure Set_borderLeftColor(p: OleVariant); safecall;
    procedure Set_borderLeftStyle(const p: WideString); safecall;
    procedure Set_borderLeftWidth(p: OleVariant); safecall;
    procedure Set_borderRight(const p: WideString); safecall;
    procedure Set_borderRightColor(p: OleVariant); safecall;
    procedure Set_borderRightStyle(const p: WideString); safecall;
    procedure Set_borderRightWidth(p: OleVariant); safecall;
    procedure Set_borderStyle(const p: WideString); safecall;
    procedure Set_borderTop(const p: WideString); safecall;
    procedure Set_borderTopColor(p: OleVariant); safecall;
    procedure Set_borderTopStyle(const p: WideString); safecall;
    procedure Set_borderTopWidth(p: OleVariant); safecall;
    procedure Set_borderWidth(const p: WideString); safecall;
    procedure Set_bottom(p: OleVariant); safecall;
    procedure Set_clear(const p: WideString); safecall;
    procedure Set_clip(const p: WideString); safecall;
    procedure Set_color(p: OleVariant); safecall;
    procedure Set_cssText(const p: WideString); safecall;
    procedure Set_cursor(const p: WideString); safecall;
    procedure Set_direction(const p: WideString); safecall;
    procedure Set_display(const p: WideString); safecall;
    procedure Set_filter(const p: WideString); safecall;
    procedure Set_font(const p: WideString); safecall;
    procedure Set_fontFamily(const p: WideString); safecall;
    procedure Set_fontSize(p: OleVariant); safecall;
    procedure Set_fontStyle(const p: WideString); safecall;
    procedure Set_fontVariant(const p: WideString); safecall;
    procedure Set_fontWeight(const p: WideString); safecall;
    procedure Set_height(p: OleVariant); safecall;
    procedure Set_imeMode(const p: WideString); safecall;
    procedure Set_layoutFlow(const p: WideString); safecall;
    procedure Set_layoutGrid(const p: WideString); safecall;
    procedure Set_layoutGridChar(p: OleVariant); safecall;
    procedure Set_layoutGridLine(p: OleVariant); safecall;
    procedure Set_layoutGridMode(const p: WideString); safecall;
    procedure Set_layoutGridType(const p: WideString); safecall;
    procedure Set_left(p: OleVariant); safecall;
    procedure Set_letterSpacing(p: OleVariant); safecall;
    procedure Set_lineBreak(const p: WideString); safecall;
    procedure Set_lineHeight(p: OleVariant); safecall;
    procedure Set_listStyle(const p: WideString); safecall;
    procedure Set_listStyleImage(const p: WideString); safecall;
    procedure Set_listStylePosition(const p: WideString); safecall;
    procedure Set_listStyleType(const p: WideString); safecall;
    procedure Set_margin(const p: WideString); safecall;
    procedure Set_marginBottom(p: OleVariant); safecall;
    procedure Set_marginLeft(p: OleVariant); safecall;
    procedure Set_marginRight(p: OleVariant); safecall;
    procedure Set_marginTop(p: OleVariant); safecall;
    procedure Set_minHeight(p: OleVariant); safecall;
    procedure Set_overflow(const p: WideString); safecall;
    procedure Set_overflowX(const p: WideString); safecall;
    procedure Set_overflowY(const p: WideString); safecall;
    procedure Set_padding(const p: WideString); safecall;
    procedure Set_paddingBottom(p: OleVariant); safecall;
    procedure Set_paddingLeft(p: OleVariant); safecall;
    procedure Set_paddingRight(p: OleVariant); safecall;
    procedure Set_paddingTop(p: OleVariant); safecall;
    procedure Set_pageBreakAfter(const p: WideString); safecall;
    procedure Set_pageBreakBefore(const p: WideString); safecall;
    procedure Set_pixelBottom(p: Integer); safecall;
    procedure Set_pixelHeight(p: Integer); safecall;
    procedure Set_pixelLeft(p: Integer); safecall;
    procedure Set_pixelRight(p: Integer); safecall;
    procedure Set_pixelTop(p: Integer); safecall;
    procedure Set_pixelWidth(p: Integer); safecall;
    procedure Set_posBottom(p: Single); safecall;
    procedure Set_posHeight(p: Single); safecall;
    procedure Set_position(const p: WideString); safecall;
    procedure Set_posLeft(p: Single); safecall;
    procedure Set_posRight(p: Single); safecall;
    procedure Set_posTop(p: Single); safecall;
    procedure Set_posWidth(p: Single); safecall;
    procedure Set_right(p: OleVariant); safecall;
    procedure Set_rubyAlign(const p: WideString); safecall;
    procedure Set_rubyOverhang(const p: WideString); safecall;
    procedure Set_rubyPosition(const p: WideString); safecall;
    procedure Set_scrollbar3dLightColor(p: OleVariant); safecall;
    procedure Set_scrollbarArrowColor(p: OleVariant); safecall;
    procedure Set_scrollbarBaseColor(p: OleVariant); safecall;
    procedure Set_scrollbarDarkShadowColor(p: OleVariant); safecall;
    procedure Set_scrollbarFaceColor(p: OleVariant); safecall;
    procedure Set_scrollbarHighlightColor(p: OleVariant); safecall;
    procedure Set_scrollbarShadowColor(p: OleVariant); safecall;
    procedure Set_scrollbarTrackColor(p: OleVariant); safecall;
    procedure Set_styleFloat(const p: WideString); safecall;
    procedure Set_tableLayout(const p: WideString); safecall;
    procedure Set_textAlign(const p: WideString); safecall;
    procedure Set_textAlignLast(const p: WideString); safecall;
    procedure Set_textAutospace(const p: WideString); safecall;
    procedure Set_textDecoration(const p: WideString); safecall;
    procedure Set_textDecorationBlink(p: WordBool); safecall;
    procedure Set_textDecorationLineThrough(p: WordBool); safecall;
    procedure Set_textDecorationNone(p: WordBool); safecall;
    procedure Set_textDecorationOverline(p: WordBool); safecall;
    procedure Set_textDecorationUnderline(p: WordBool); safecall;
    procedure Set_textIndent(p: OleVariant); safecall;
    procedure Set_textJustify(const p: WideString); safecall;
    procedure Set_textJustifyTrim(const p: WideString); safecall;
    procedure Set_textKashida(p: OleVariant); safecall;
    procedure Set_textKashidaSpace(p: OleVariant); safecall;
    procedure Set_textOverflow(const p: WideString); safecall;
    procedure Set_textTransform(const p: WideString); safecall;
    procedure Set_textUnderlinePosition(const p: WideString); safecall;
    procedure Set_top(p: OleVariant); safecall;
    procedure Set_unicodeBidi(const p: WideString); safecall;
    procedure Set_verticalAlign(p: OleVariant); safecall;
    procedure Set_visibility(const p: WideString); safecall;
    procedure Set_whiteSpace(const p: WideString); safecall;
    procedure Set_width(p: OleVariant); safecall;
    procedure Set_wordBreak(const p: WideString); safecall;
    procedure Set_wordSpacing(p: OleVariant); safecall;
    procedure Set_wordWrap(const p: WideString); safecall;
    procedure Set_writingMode(const p: WideString); safecall;
    procedure Set_zIndex(p: OleVariant); safecall;
    procedure Set_zoom(p: OleVariant); safecall;
    procedure setAttribute(const strAttributeName: WideString; AttributeValue: OleVariant;  lFlags: Integer); safecall;
    procedure setExpression(const propname: WideString; const expression: WideString;  const language: WideString); safecall;
  end;

  TjsHTMLStyleSheet = class(TjsDOMObject,IHTMLStyleSheet,IHTMLStyleSheet2)
  private
    function addImport(const bstrUrl: WideString; lIndex: Integer): Integer; safecall;
    function addPageRule(const bstrSelector: WideString; const bstrStyle: WideString;  lIndex: Integer): Integer; safecall;
    function addRule(const bstrSelector: WideString; const bstrStyle: WideString; lIndex: Integer): Integer; safecall;
    function Get_cssText: WideString; safecall;
    function Get_disabled: WordBool; safecall;
    function Get_href: WideString; safecall;
    function Get_id: WideString; safecall;
    function Get_imports: IHTMLStyleSheetsCollection; safecall;
    function Get_media: WideString; safecall;
    function Get_owningElement: IHTMLElement; safecall;
    function Get_pages: IHTMLStyleSheetPagesCollection; safecall;
    function Get_parentStyleSheet: IHTMLStyleSheet; safecall;
    function Get_readOnly: WordBool; safecall;
    function Get_rules: IHTMLStyleSheetRulesCollection; safecall;
    function Get_title: WideString; safecall;
    function Get_type_: WideString; safecall;
    procedure removeImport(lIndex: Integer); safecall;
    procedure removeRule(lIndex: Integer); safecall;
    procedure Set_cssText(const p: WideString); safecall;
    procedure Set_disabled(p: WordBool); safecall;
    procedure Set_href(const p: WideString); safecall;
    procedure Set_media(const p: WideString); safecall;
    procedure Set_title(const p: WideString); safecall;
  end;

  TjsHTMLStyleSheetsCollection = class(TjsDOMCollection,IHTMLStyleSheetsCollection)
  protected
    function item(const pvarIndex: OleVariant): OleVariant; safecall;
  end;

  TjsHTMLStyleSheetPage = class(TjsDOMObject,IHTMLStyleSheetPage)
  private
    function Get_pseudoClass: WideString; safecall;
    function Get_selector: WideString; safecall;
  end;

  TjsHTMLStyleSheetPagesCollection = class(TjsDOMCollection,IHTMLStyleSheetPagesCollection)
  private
    function item(index: Integer): IHTMLStyleSheetPage; safecall;
  end;

  TjsHTMLRuleStyle = class(TjsDOMObject,IHTMLRuleStyle,IHTMLRuleStyle2,IHTMLRuleStyle3,IHTMLRuleStyle4)
  private
    function Get_accelerator: WideString; safecall;
    function Get_background: WideString; safecall;
    function Get_backgroundAttachment: WideString; safecall;
    function Get_backgroundColor: OleVariant; safecall;
    function Get_backgroundImage: WideString; safecall;
    function Get_backgroundPosition: WideString; safecall;
    function Get_backgroundPositionX: OleVariant; safecall;
    function Get_backgroundPositionY: OleVariant; safecall;
    function Get_backgroundRepeat: WideString; safecall;
    function Get_behavior: WideString; safecall;
    function Get_border: WideString; safecall;
    function Get_borderBottom: WideString; safecall;
    function Get_borderBottomColor: OleVariant; safecall;
    function Get_borderBottomStyle: WideString; safecall;
    function Get_borderBottomWidth: OleVariant; safecall;
    function Get_borderCollapse: WideString; safecall;
    function Get_borderColor: WideString; safecall;
    function Get_borderLeft: WideString; safecall;
    function Get_borderLeftColor: OleVariant; safecall;
    function Get_borderLeftStyle: WideString; safecall;
    function Get_borderLeftWidth: OleVariant; safecall;
    function Get_borderRight: WideString; safecall;
    function Get_borderRightColor: OleVariant; safecall;
    function Get_borderRightStyle: WideString; safecall;
    function Get_borderRightWidth: OleVariant; safecall;
    function Get_borderStyle: WideString; safecall;
    function Get_borderTop: WideString; safecall;
    function Get_borderTopColor: OleVariant; safecall;
    function Get_borderTopStyle: WideString; safecall;
    function Get_borderTopWidth: OleVariant; safecall;
    function Get_borderWidth: WideString; safecall;
    function Get_bottom: OleVariant; safecall;
    function Get_clear: WideString; safecall;
    function Get_clip: WideString; safecall;
    function Get_color: OleVariant; safecall;
    function Get_cssText: WideString; safecall;
    function Get_cursor: WideString; safecall;
    function Get_direction: WideString; safecall;
    function Get_display: WideString; safecall;
    function Get_filter: WideString; safecall;
    function Get_font: WideString; safecall;
    function Get_fontFamily: WideString; safecall;
    function Get_fontSize: OleVariant; safecall;
    function Get_fontStyle: WideString; safecall;
    function Get_fontVariant: WideString; safecall;
    function Get_fontWeight: WideString; safecall;
    function Get_height: OleVariant; safecall;
    function Get_imeMode: WideString; safecall;
    function Get_layoutFlow: WideString; safecall;
    function Get_layoutGrid: WideString; safecall;
    function Get_layoutGridChar: OleVariant; safecall;
    function Get_layoutGridLine: OleVariant; safecall;
    function Get_layoutGridMode: WideString; safecall;
    function Get_layoutGridType: WideString; safecall;
    function Get_left: OleVariant; safecall;
    function Get_letterSpacing: OleVariant; safecall;
    function Get_lineBreak: WideString; safecall;
    function Get_lineHeight: OleVariant; safecall;
    function Get_listStyle: WideString; safecall;
    function Get_listStyleImage: WideString; safecall;
    function Get_listStylePosition: WideString; safecall;
    function Get_listStyleType: WideString; safecall;
    function Get_margin: WideString; safecall;
    function Get_marginBottom: OleVariant; safecall;
    function Get_marginLeft: OleVariant; safecall;
    function Get_marginRight: OleVariant; safecall;
    function Get_marginTop: OleVariant; safecall;
    function Get_minHeight: OleVariant; safecall;
    function Get_overflow: WideString; safecall;
    function Get_overflowX: WideString; safecall;
    function Get_overflowY: WideString; safecall;
    function Get_padding: WideString; safecall;
    function Get_paddingBottom: OleVariant; safecall;
    function Get_paddingLeft: OleVariant; safecall;
    function Get_paddingRight: OleVariant; safecall;
    function Get_paddingTop: OleVariant; safecall;
    function Get_pageBreakAfter: WideString; safecall;
    function Get_pageBreakBefore: WideString; safecall;
    function Get_pixelBottom: Integer; safecall;
    function Get_pixelRight: Integer; safecall;
    function Get_posBottom: Single; safecall;
    function Get_position: WideString; safecall;
    function Get_posRight: Single; safecall;
    function Get_right: OleVariant; safecall;
    function Get_rubyAlign: WideString; safecall;
    function Get_rubyOverhang: WideString; safecall;
    function Get_rubyPosition: WideString; safecall;
    function Get_scrollbar3dLightColor: OleVariant; safecall;
    function Get_scrollbarArrowColor: OleVariant; safecall;
    function Get_scrollbarBaseColor: OleVariant; safecall;
    function Get_scrollbarDarkShadowColor: OleVariant; safecall;
    function Get_scrollbarFaceColor: OleVariant; safecall;
    function Get_scrollbarHighlightColor: OleVariant; safecall;
    function Get_scrollbarShadowColor: OleVariant; safecall;
    function Get_scrollbarTrackColor: OleVariant; safecall;
    function Get_styleFloat: WideString; safecall;
    function Get_tableLayout: WideString; safecall;
    function Get_textAlign: WideString; safecall;
    function Get_textAlignLast: WideString; safecall;
    function Get_textAutospace: WideString; safecall;
    function Get_textDecoration: WideString; safecall;
    function Get_textDecorationBlink: WordBool; safecall;
    function Get_textDecorationLineThrough: WordBool; safecall;
    function Get_textDecorationNone: WordBool; safecall;
    function Get_textDecorationOverline: WordBool; safecall;
    function Get_textDecorationUnderline: WordBool; safecall;
    function Get_textIndent: OleVariant; safecall;
    function Get_textJustify: WideString; safecall;
    function Get_textJustifyTrim: WideString; safecall;
    function Get_textKashida: OleVariant; safecall;
    function Get_textKashidaSpace: OleVariant; safecall;
    function Get_textOverflow: WideString; safecall;
    function Get_textTransform: WideString; safecall;
    function Get_textUnderlinePosition: WideString; safecall;
    function Get_top: OleVariant; safecall;
    function Get_unicodeBidi: WideString; safecall;
    function Get_verticalAlign: OleVariant; safecall;
    function Get_visibility: WideString; safecall;
    function Get_whiteSpace: WideString; safecall;
    function Get_width: OleVariant; safecall;
    function Get_wordBreak: WideString; safecall;
    function Get_wordSpacing: OleVariant; safecall;
    function Get_wordWrap: WideString; safecall;
    function Get_writingMode: WideString; safecall;
    function Get_zIndex: OleVariant; safecall;
    function Get_zoom: OleVariant; safecall;
    function getAttribute(const strAttributeName: WideString; lFlags: Integer): OleVariant; safecall;
    function removeAttribute(const strAttributeName: WideString; lFlags: Integer): WordBool; safecall;
    procedure Set_accelerator(const p: WideString); safecall;
    procedure Set_background(const p: WideString); safecall;
    procedure Set_backgroundAttachment(const p: WideString); safecall;
    procedure Set_backgroundColor(p: OleVariant); safecall;
    procedure Set_backgroundImage(const p: WideString); safecall;
    procedure Set_backgroundPosition(const p: WideString); safecall;
    procedure Set_backgroundPositionX(p: OleVariant); safecall;
    procedure Set_backgroundPositionY(p: OleVariant); safecall;
    procedure Set_backgroundRepeat(const p: WideString); safecall;
    procedure Set_behavior(const p: WideString); safecall;
    procedure Set_border(const p: WideString); safecall;
    procedure Set_borderBottom(const p: WideString); safecall;
    procedure Set_borderBottomColor(p: OleVariant); safecall;
    procedure Set_borderBottomStyle(const p: WideString); safecall;
    procedure Set_borderBottomWidth(p: OleVariant); safecall;
    procedure Set_borderCollapse(const p: WideString); safecall;
    procedure Set_borderColor(const p: WideString); safecall;
    procedure Set_borderLeft(const p: WideString); safecall;
    procedure Set_borderLeftColor(p: OleVariant); safecall;
    procedure Set_borderLeftStyle(const p: WideString); safecall;
    procedure Set_borderLeftWidth(p: OleVariant); safecall;
    procedure Set_borderRight(const p: WideString); safecall;
    procedure Set_borderRightColor(p: OleVariant); safecall;
    procedure Set_borderRightStyle(const p: WideString); safecall;
    procedure Set_borderRightWidth(p: OleVariant); safecall;
    procedure Set_borderStyle(const p: WideString); safecall;
    procedure Set_borderTop(const p: WideString); safecall;
    procedure Set_borderTopColor(p: OleVariant); safecall;
    procedure Set_borderTopStyle(const p: WideString); safecall;
    procedure Set_borderTopWidth(p: OleVariant); safecall;
    procedure Set_borderWidth(const p: WideString); safecall;
    procedure Set_bottom(p: OleVariant); safecall;
    procedure Set_clear(const p: WideString); safecall;
    procedure Set_clip(const p: WideString); safecall;
    procedure Set_color(p: OleVariant); safecall;
    procedure Set_cssText(const p: WideString); safecall;
    procedure Set_cursor(const p: WideString); safecall;
    procedure Set_direction(const p: WideString); safecall;
    procedure Set_display(const p: WideString); safecall;
    procedure Set_filter(const p: WideString); safecall;
    procedure Set_font(const p: WideString); safecall;
    procedure Set_fontFamily(const p: WideString); safecall;
    procedure Set_fontSize(p: OleVariant); safecall;
    procedure Set_fontStyle(const p: WideString); safecall;
    procedure Set_fontVariant(const p: WideString); safecall;
    procedure Set_fontWeight(const p: WideString); safecall;
    procedure Set_height(p: OleVariant); safecall;
    procedure Set_imeMode(const p: WideString); safecall;
    procedure Set_layoutFlow(const p: WideString); safecall;
    procedure Set_layoutGrid(const p: WideString); safecall;
    procedure Set_layoutGridChar(p: OleVariant); safecall;
    procedure Set_layoutGridLine(p: OleVariant); safecall;
    procedure Set_layoutGridMode(const p: WideString); safecall;
    procedure Set_layoutGridType(const p: WideString); safecall;
    procedure Set_left(p: OleVariant); safecall;
    procedure Set_letterSpacing(p: OleVariant); safecall;
    procedure Set_lineBreak(const p: WideString); safecall;
    procedure Set_lineHeight(p: OleVariant); safecall;
    procedure Set_listStyle(const p: WideString); safecall;
    procedure Set_listStyleImage(const p: WideString); safecall;
    procedure Set_listStylePosition(const p: WideString); safecall;
    procedure Set_listStyleType(const p: WideString); safecall;
    procedure Set_margin(const p: WideString); safecall;
    procedure Set_marginBottom(p: OleVariant); safecall;
    procedure Set_marginLeft(p: OleVariant); safecall;
    procedure Set_marginRight(p: OleVariant); safecall;
    procedure Set_marginTop(p: OleVariant); safecall;
    procedure Set_minHeight(p: OleVariant); safecall;
    procedure Set_overflow(const p: WideString); safecall;
    procedure Set_overflowX(const p: WideString); safecall;
    procedure Set_overflowY(const p: WideString); safecall;
    procedure Set_padding(const p: WideString); safecall;
    procedure Set_paddingBottom(p: OleVariant); safecall;
    procedure Set_paddingLeft(p: OleVariant); safecall;
    procedure Set_paddingRight(p: OleVariant); safecall;
    procedure Set_paddingTop(p: OleVariant); safecall;
    procedure Set_pageBreakAfter(const p: WideString); safecall;
    procedure Set_pageBreakBefore(const p: WideString); safecall;
    procedure Set_pixelBottom(p: Integer); safecall;
    procedure Set_pixelRight(p: Integer); safecall;
    procedure Set_posBottom(p: Single); safecall;
    procedure Set_position(const p: WideString); safecall;
    procedure Set_posRight(p: Single); safecall;
    procedure Set_right(p: OleVariant); safecall;
    procedure Set_rubyAlign(const p: WideString); safecall;
    procedure Set_rubyOverhang(const p: WideString); safecall;
    procedure Set_rubyPosition(const p: WideString); safecall;
    procedure Set_scrollbar3dLightColor(p: OleVariant); safecall;
    procedure Set_scrollbarArrowColor(p: OleVariant); safecall;
    procedure Set_scrollbarBaseColor(p: OleVariant); safecall;
    procedure Set_scrollbarDarkShadowColor(p: OleVariant); safecall;
    procedure Set_scrollbarFaceColor(p: OleVariant); safecall;
    procedure Set_scrollbarHighlightColor(p: OleVariant); safecall;
    procedure Set_scrollbarShadowColor(p: OleVariant); safecall;
    procedure Set_scrollbarTrackColor(p: OleVariant); safecall;
    procedure Set_styleFloat(const p: WideString); safecall;
    procedure Set_tableLayout(const p: WideString); safecall;
    procedure Set_textAlign(const p: WideString); safecall;
    procedure Set_textAlignLast(const p: WideString); safecall;
    procedure Set_textAutospace(const p: WideString); safecall;
    procedure Set_textDecoration(const p: WideString); safecall;
    procedure Set_textDecorationBlink(p: WordBool); safecall;
    procedure Set_textDecorationLineThrough(p: WordBool); safecall;
    procedure Set_textDecorationNone(p: WordBool); safecall;
    procedure Set_textDecorationOverline(p: WordBool); safecall;
    procedure Set_textDecorationUnderline(p: WordBool); safecall;
    procedure Set_textIndent(p: OleVariant); safecall;
    procedure Set_textJustify(const p: WideString); safecall;
    procedure Set_textJustifyTrim(const p: WideString); safecall;
    procedure Set_textKashida(p: OleVariant); safecall;
    procedure Set_textKashidaSpace(p: OleVariant); safecall;
    procedure Set_textOverflow(const p: WideString); safecall;
    procedure Set_textTransform(const p: WideString); safecall;
    procedure Set_textUnderlinePosition(const p: WideString); safecall;
    procedure Set_top(p: OleVariant); safecall;
    procedure Set_unicodeBidi(const p: WideString); safecall;
    procedure Set_verticalAlign(p: OleVariant); safecall;
    procedure Set_visibility(const p: WideString); safecall;
    procedure Set_whiteSpace(const p: WideString); safecall;
    procedure Set_width(p: OleVariant); safecall;
    procedure Set_wordBreak(const p: WideString); safecall;
    procedure Set_wordSpacing(p: OleVariant); safecall;
    procedure Set_wordWrap(const p: WideString); safecall;
    procedure Set_writingMode(const p: WideString); safecall;
    procedure Set_zIndex(p: OleVariant); safecall;
    procedure Set_zoom(p: OleVariant); safecall;
    procedure setAttribute(const strAttributeName: WideString; AttributeValue: OleVariant;  lFlags: Integer); safecall;
  end;

  TjsHTMLStyleSheetRule = class(TjsDOMObject,IHTMLStyleSheetRule)
  private
    function Get_readOnly: WordBool; safecall;
    function Get_selectorText: WideString; safecall;
    function Get_style: IHTMLRuleStyle; safecall;
    procedure Set_selectorText(const p: WideString); safecall;
  end;

  TjsHTMLStyleSheetRulesCollection = class(TjsDOMCollection,IHTMLStyleSheetRulesCollection)
  private
    function item(index: Integer): IHTMLStyleSheetRule; safecall;
  end;

  TjsOmHistory = class(TjsDOMObject,IOmHistory)
  private
    function Get_length: Smallint; safecall;
    procedure back(var pvargdistance: OleVariant); safecall;
    procedure forward(var pvargdistance: OleVariant); safecall;
    procedure go(var pvargdistance: OleVariant); safecall;
  end;

  TjsHTMLImgElement = class(TjsDOMObject,IHTMLImgElement,IHTMLImgElement2)
  private
    function Get_align: WideString; safecall;
    function Get_alt: WideString; safecall;
    function Get_border: OleVariant; safecall;
    function Get_complete: WordBool; safecall;
    function Get_dynsrc: WideString; safecall;
    function Get_fileCreatedDate: WideString; safecall;
    function Get_fileModifiedDate: WideString; safecall;
    function Get_fileSize: WideString; safecall;
    function Get_fileUpdatedDate: WideString; safecall;
    function Get_height: Integer; safecall;
    function Get_href: WideString; safecall;
    function Get_hspace: Integer; safecall;
    function Get_isMap: WordBool; safecall;
    function Get_longDesc: WideString; safecall;
    function Get_loop: OleVariant; safecall;
    function Get_lowsrc: WideString; safecall;
    function Get_mimeType: WideString; safecall;
    function Get_name: WideString; safecall;
    function Get_nameProp: WideString; safecall;
    function Get_onabort: OleVariant; safecall;
    function Get_onerror: OleVariant; safecall;
    function Get_onload: OleVariant; safecall;
    function Get_protocol: WideString; safecall;
    function Get_readyState: WideString; safecall;
    function Get_src: WideString; safecall;
    function Get_Start: WideString; safecall;
    function Get_useMap: WideString; safecall;
    function Get_vrml: WideString; safecall;
    function Get_vspace: Integer; safecall;
    function Get_width: Integer; safecall;
    procedure Set_align(const p: WideString); safecall;
    procedure Set_alt(const p: WideString); safecall;
    procedure Set_border(p: OleVariant); safecall;
    procedure Set_dynsrc(const p: WideString); safecall;
    procedure Set_height(p: Integer); safecall;
    procedure Set_hspace(p: Integer); safecall;
    procedure Set_isMap(p: WordBool); safecall;
    procedure Set_longDesc(const p: WideString); safecall;
    procedure Set_loop(p: OleVariant); safecall;
    procedure Set_lowsrc(const p: WideString); safecall;
    procedure Set_name(const p: WideString); safecall;
    procedure Set_onabort(p: OleVariant); safecall;
    procedure Set_onerror(p: OleVariant); safecall;
    procedure Set_onload(p: OleVariant); safecall;
    procedure Set_src(const p: WideString); safecall;
    procedure Set_Start(const p: WideString); safecall;
    procedure Set_useMap(const p: WideString); safecall;
    procedure Set_vrml(const p: WideString); safecall;
    procedure Set_vspace(p: Integer); safecall;
    procedure Set_width(p: Integer); safecall;
  end;


  TjsHTMLImageElementFactory = class(TjsDOMObject,IHTMLImageElementFactory)
  private
    function create(width: OleVariant; height: OleVariant): IHTMLImgElement; overload; safecall;
  end;

  TjsHTMLLocation = class(TjsDOMObject,IHTMLLocation)
  private
    function Get_hash: WideString; safecall;
    function Get_host: WideString; safecall;
    function Get_hostname: WideString; safecall;
    function Get_href: WideString; safecall;
    function Get_pathname: WideString; safecall;
    function Get_port: WideString; safecall;
    function Get_protocol: WideString; safecall;
    function Get_search: WideString; safecall;
    function toString: WideString; safecall;
    procedure assign(const bstr: WideString); safecall;
    procedure reload(flag: WordBool); safecall;
    procedure replace(const bstr: WideString); safecall;
    procedure Set_hash(const p: WideString); safecall;
    procedure Set_host(const p: WideString); safecall;
    procedure Set_hostname(const p: WideString); safecall;
    procedure Set_href(const p: WideString); safecall;
    procedure Set_pathname(const p: WideString); safecall;
    procedure Set_port(const p: WideString); safecall;
    procedure Set_protocol(const p: WideString); safecall;
    procedure Set_search(const p: WideString); safecall;
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

  TjsHTMLOptionElement = class(TjsDOMObject,IHTMLOptionElement,IHTMLOptionElement3)
  private
    function Get_defaultSelected: WordBool; safecall;
    function Get_form: IHTMLFormElement; safecall;
    function Get_index: Integer; safecall;
    function Get_label_: WideString; safecall;
    function Get_selected: WordBool; safecall;
    function Get_text: WideString; safecall;
    function Get_value: WideString; safecall;
    procedure Set_defaultSelected(p: WordBool); safecall;
    procedure Set_index(p: Integer); safecall;
    procedure Set_label_(const p: WideString); safecall;
    procedure Set_selected(p: WordBool); safecall;
    procedure Set_text(const p: WideString); safecall;
    procedure Set_value(const p: WideString); safecall;
  end;

  TjsHTMLOptionElementFactory = class(TjsDOMObject,IHTMLOptionElementFactory)
  private
    function create(text: OleVariant; value: OleVariant; defaultSelected: OleVariant;  selected: OleVariant): IHTMLOptionElement; overload; safecall;
  end;

  TjsHTMLFramesCollection = class(TjsDOMCollection, IHTMLFramesCollection2)
  protected
    function item(const pvarIndex: OleVariant): OleVariant; safecall;
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
  public
    constructor Create(AApplication : TjsdApplication; ACreateCommand : String); reintroduce;
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

  TjsHTMLScreen = class(TjsDOMObject,IHTMLScreen,IHTMLScreen2)
  private
    function Get_availHeight: Integer; safecall;
    function Get_availWidth: Integer; safecall;
    function Get_bufferDepth: Integer; safecall;
    function Get_colorDepth: Integer; safecall;
    function Get_deviceXDPI: Integer; safecall;
    function Get_deviceYDPI: Integer; safecall;
    function Get_fontSmoothingEnabled: WordBool; safecall;
    function Get_height: Integer; safecall;
    function Get_logicalXDPI: Integer; safecall;
    function Get_logicalYDPI: Integer; safecall;
    function Get_updateInterval: Integer; safecall;
    function Get_width: Integer; safecall;
    procedure Set_bufferDepth(p: Integer); safecall;
    procedure Set_updateInterval(p: Integer); safecall;
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
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.childNodes', TjsHTMLElement);
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
  Result:=tjsHTMLStyleSheet.Create(FApplication, _JSVar+'.createStyleSheet('+
    ToJSCode(bstrHref)+','+ToJSCode(lIndex)+')');
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
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.all', TjsHTMLElement);
end;

function TjsHTMLDocument.Get_anchors: IHTMLElementCollection;
begin
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.anchors', TjsHTMLElement);
end;

function TjsHTMLDocument.Get_applets: IHTMLElementCollection;
begin
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.applets', TjsHTMLElement);
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
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.embeds', TjsHTMLElement);
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
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.forms', TjsHTMLElement);
end;

function TjsHTMLDocument.Get_frames: IHTMLFramesCollection2;
begin
  Result:=TjsHTMLFramesCollection.Create(FApplication, _JSVar+'.frames', TjsHTMLWindow);
end;

function TjsHTMLDocument.Get_images: IHTMLElementCollection;
begin
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.images', TjsHTMLElement);
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
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.links', TjsHTMLElement);
end;

function TjsHTMLDocument.Get_location: IHTMLLocation;
begin
  Result:=TjsHTMLLocation.Create(FApplication, _JSVar+'.location');
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
  Result:=TjsHTMLWindow.Create(FApplication, _JSVar+'.parentWindow');
end;

function TjsHTMLDocument.Get_plugins: IHTMLElementCollection;
begin
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.plugins', TjsHTMLElement);
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
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.scripts', TjsHTMLElement);
end;

function TjsHTMLDocument.Get_security: WideString;
begin
  GetPropertyValue('security', Result);
end;

function TjsHTMLDocument.Get_selection: IHTMLSelectionObject;
begin
  Result:=nil;
end;

function TjsHTMLDocument.Get_styleSheets: IHTMLStyleSheetsCollection;
begin
  Result:=TjsHTMLStyleSheetsCollection.Create(FApplication, _JSVar+'.styleSheets', TjsHTMLStyleSheet);
end;

function TjsHTMLDocument.Get_title: WideString;
begin
  GetPropertyValue('title', Result);
end;

function TjsHTMLDocument.Get_url: WideString;
begin
  GetPropertyValue('url', Result);
end;

function TjsHTMLDocument.Get_vlinkColor: OleVariant;
begin
  GetPropertyValue('vlinkColor', Result);
end;

function TjsHTMLDocument.open(const url: WideString; name, features,
  replace: OleVariant): IDispatch;
begin
  Result:=TjsHTMLDocument.Create(FApplication, _JSVar+'.open('+ToJSCode(url)+','+ToJSCode(name)+','+ToJSCode(features)+','+ToJSCode(replace)+')');
end;

function TjsHTMLDocument.queryCommandEnabled(const cmdID: WideString): WordBool;
begin
  Result:=IsPositiveMethodResult(ExecMethod('queryCommandEnabled('+ToJSCode(cmdID)+')',True));
end;

function TjsHTMLDocument.queryCommandIndeterm(const cmdID: WideString): WordBool;
begin
  Result:=IsPositiveMethodResult(ExecMethod('queryCommandIndeterm('+ToJSCode(cmdID)+')',True));
end;

function TjsHTMLDocument.queryCommandState(const cmdID: WideString): WordBool;
begin
  Result:=IsPositiveMethodResult(ExecMethod('queryCommandState('+ToJSCode(cmdID)+')',True));
end;

function TjsHTMLDocument.queryCommandSupported(
  const cmdID: WideString): WordBool;
begin
  Result:=IsPositiveMethodResult(ExecMethod('queryCommandSupported('+ToJSCode(cmdID)+')',True));
end;

function TjsHTMLDocument.queryCommandText(const cmdID: WideString): WideString;
begin
  Result:=ExecMethod('queryCommandText('+ToJSCode(cmdID)+')',True);
end;

function TjsHTMLDocument.queryCommandValue(const cmdID: WideString): OleVariant;
begin
  Result:=ExecMethod('queryCommandValue('+ToJSCode(cmdID)+')',True);
end;

procedure TjsHTMLDocument.Set_alinkColor(p: OleVariant);
begin
  SetPropertyValue('alinkColor', p);
end;

procedure TjsHTMLDocument.Set_bgColor(p: OleVariant);
begin
  SetPropertyValue('bgColor', p);
end;

procedure TjsHTMLDocument.Set_charset(const p: WideString);
begin
  SetPropertyValue('charset', p);
end;

procedure TjsHTMLDocument.Set_cookie(const p: WideString);
begin
  SetPropertyValue('cookie', p);
end;

procedure TjsHTMLDocument.Set_defaultCharset(const p: WideString);
begin
  SetPropertyValue('defaultCharset', p);
end;

procedure TjsHTMLDocument.Set_designMode(const p: WideString);
begin
  SetPropertyValue('designMode', p);
end;

procedure TjsHTMLDocument.Set_domain(const p: WideString);
begin
  SetPropertyValue('designMode', p);
end;

procedure TjsHTMLDocument.Set_expando(p: WordBool);
begin
  SetPropertyValue('expando', p);
end;

procedure TjsHTMLDocument.Set_fgColor(p: OleVariant);
begin
  SetPropertyValue('fgColor', p);
end;

procedure TjsHTMLDocument.Set_linkColor(p: OleVariant);
begin
  SetPropertyValue('linkColor', p);
end;

procedure TjsHTMLDocument.Set_onafterupdate(p: OleVariant);
begin
  SetPropertyValue('onafterupdate', p, True);
end;

procedure TjsHTMLDocument.Set_onbeforeupdate(p: OleVariant);
begin
  SetPropertyValue('onbeforeupdate', p, True);
end;

procedure TjsHTMLDocument.Set_onclick(p: OleVariant);
begin
  SetPropertyValue('onclick', p, True);
end;

procedure TjsHTMLDocument.Set_ondblclick(p: OleVariant);
begin
  SetPropertyValue('ondblclick', p, True);
end;

procedure TjsHTMLDocument.Set_ondragstart(p: OleVariant);
begin
  SetPropertyValue('ondragstart', p, True);
end;

procedure TjsHTMLDocument.Set_onerrorupdate(p: OleVariant);
begin
  SetPropertyValue('onerrorupdate', p, True);
end;

procedure TjsHTMLDocument.Set_onhelp(p: OleVariant);
begin
  SetPropertyValue('onhelp', p, True);
end;

procedure TjsHTMLDocument.Set_onkeydown(p: OleVariant);
begin
  SetPropertyValue('onkeydown', p, True);
end;

procedure TjsHTMLDocument.Set_onkeypress(p: OleVariant);
begin
  SetPropertyValue('onkeypress', p, True);
end;

procedure TjsHTMLDocument.Set_onkeyup(p: OleVariant);
begin
  SetPropertyValue('onkeyup', p, True);
end;

procedure TjsHTMLDocument.Set_onmousedown(p: OleVariant);
begin
  SetPropertyValue('onmousedown', p, True);
end;

procedure TjsHTMLDocument.Set_onmousemove(p: OleVariant);
begin
  SetPropertyValue('onmousemove', p, True);
end;

procedure TjsHTMLDocument.Set_onmouseout(p: OleVariant);
begin
  SetPropertyValue('onmouseout', p, True);
end;

procedure TjsHTMLDocument.Set_onmouseover(p: OleVariant);
begin
  SetPropertyValue('onmouseover', p, True);
end;

procedure TjsHTMLDocument.Set_onmouseup(p: OleVariant);
begin
  SetPropertyValue('onmouseup', p, true);
end;

procedure TjsHTMLDocument.Set_onreadystatechange(p: OleVariant);
begin
  SetPropertyValue('onreadystatechange', p, True);
end;

procedure TjsHTMLDocument.Set_onrowenter(p: OleVariant);
begin
  SetPropertyValue('onrowenter', p, True);
end;

procedure TjsHTMLDocument.Set_onrowexit(p: OleVariant);
begin
  SetPropertyValue('onrowexit', p, True);

end;

procedure TjsHTMLDocument.Set_onselectstart(p: OleVariant);
begin
  SetPropertyValue('onselectstart', p, True);
end;

procedure TjsHTMLDocument.Set_title(const p: WideString);
begin
  SetPropertyValue('title', p);
end;

procedure TjsHTMLDocument.Set_url(const p: WideString);
begin
  SetPropertyValue('url', p);
end;

procedure TjsHTMLDocument.Set_vlinkColor(p: OleVariant);
begin
  SetPropertyValue('vlinkColor', p);
end;

function TjsHTMLDocument.toString: WideString;
begin
  Result:=ExecMethod('toString', True);
end;

procedure TjsHTMLDocument.write(psarray: PSafeArray);
var
 va : TVarData;
 v : variant absolute va;
begin
  va.VType:=varArray or varVariant;
  va.VArray:=PVarArray(psarray);
  ExecMethod('write('+ToJSCode(v)+')');
end;

procedure TjsHTMLDocument.writeln(psarray: PSafeArray);
var
 va : TVarData;
 v : variant absolute va;
begin
  va.VType:=varArray or varVariant;
  va.VArray:=PVarArray(psarray);
  ExecMethod('write('+ToJSCode(v)+')');
end;

{ TjsHTMLAttributeCollection }

function TjsHTMLAttributeCollection.getNamedItem(
  const bstrName: WideString): IHTMLDOMAttribute;
begin
  Result:=TjsHTMLDOMAttribute.Create(FApplication, _JSVar+'.getNamedItem("'+bstrName+'")');
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
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.getElementsByTagName('+ToJSString(tagName)+')', TjsHTMLElement);
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
  Result:=IsPositiveMethodResult(ExecMethod('contains('+GetDOMObjectJsName(pChild)+')', True));
end;

function TjsHTMLElement.getAttribute(const strAttributeName: WideString;
  lFlags: Integer): OleVariant;
begin
  Result:=ExecMethod('getAttribute('+ToJSString(strAttributeName)+','+IntToStr(lFlags)+')',true);
end;

function TjsHTMLElement.Get_all: IDispatch;
begin
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.all', TjsHTMLElement);
end;

function TjsHTMLElement.Get_children: IDispatch;
begin
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.children', TjsHTMLElement);
end;

function TjsHTMLElement.Get_className: WideString;
begin
  GetPropertyValue('className', Result);
end;

function TjsHTMLElement.Get_document: IDispatch;
begin
  Result:=TjsHTMLDocument.Create(FApplication, _JSVar+'.document');
end;

function TjsHTMLElement.Get_filters: IHTMLFiltersCollection;
begin
  Result:=nil;
end;

function TjsHTMLElement.addBehavior(const bstrUrl: WideString; var pvarFactory: OleVariant): Integer; safecall;
begin
  Result:=0;
end;

function TjsHTMLElement.applyElement(const apply: IHTMLElement; const where: WideString): IHTMLElement; safecall;
begin
  Result:=TjsHTMLElement.Create(FApplication, _JSVar+'.applyElement('+ToJSCode(apply)+','+ToJSCode(where)+')');
end;

function TjsHTMLElement.attachEvent(const event: WideString; const pdisp: IDispatch): WordBool; safecall;
begin
  Result:=False;
end;

function TjsHTMLElement.componentFromPoint(x: Integer; y: Integer): WideString; safecall;
begin
  Result:=ExecMethod('componentFromPoint('+ToJSCode(x)+','+ToJSCode(y)+')', True);
end;

function TjsHTMLElement.createControlRange: IDispatch; safecall;
begin
  Result:=nil;
end;

function TjsHTMLElement.dragDrop: WordBool; safecall;
begin
  Result:=IsPositiveMethodResult(ExecMethod('dragDrop()', True));
end;

function TjsHTMLElement.FireEvent(const bstrEventName: WideString; var pvarEventObject: OleVariant): WordBool; safecall;
begin
  Result:=IsPositiveMethodResult(ExecMethod('FireEvent('+ToJSCode(bstrEventName)+','+ToJSCode(pvarEventObject)+')', true));
end;

function TjsHTMLElement.Get_accessKey: WideString; safecall;
begin
  GetPropertyValue('accessKey', Result)
end;

function TjsHTMLElement.Get_behaviorUrns: IDispatch; safecall;
begin
  Result:=nil;
end;

function TjsHTMLElement.Get_canHaveChildren: WordBool; safecall;
begin
  GetPropertyValue('canHaveChildren', Result)
end;

function TjsHTMLElement.Get_canHaveHTML: WordBool; safecall;
begin
  GetPropertyValue('canHaveHTML', Result)
end;

function TjsHTMLElement.Get_clientHeight: Integer; safecall;
begin
  GetPropertyValue('clientHeight', Result)
end;

function TjsHTMLElement.Get_clientLeft: Integer; safecall;
begin
  GetPropertyValue('clientLeft', Result)
end;

function TjsHTMLElement.Get_clientTop: Integer; safecall;
begin
  GetPropertyValue('clientTop', Result)
end;

function TjsHTMLElement.Get_clientWidth: Integer; safecall;
begin
  GetPropertyValue('clientWidth', Result)
end;

function TjsHTMLElement.Get_contentEditable: WideString; safecall;
begin
  GetPropertyValue('contentEditable', Result)
end;

function TjsHTMLElement.Get_currentStyle: IHTMLCurrentStyle; safecall;
begin
  Result:=nil;
end;

function TjsHTMLElement.Get_dir: WideString; safecall;
begin
  GetPropertyValue('dir', Result)
end;

function TjsHTMLElement.Get_disabled: WordBool; safecall;
begin
  GetPropertyValue('disabled', Result)
end;

function TjsHTMLElement.Get_glyphMode: Integer; safecall;
begin
  GetPropertyValue('glyphMode', Result)
end;

function TjsHTMLElement.Get_hideFocus: WordBool; safecall;
begin
  GetPropertyValue('hideFocus', Result)
end;

function TjsHTMLElement.Get_id: WideString; safecall;
begin
  GetPropertyValue('id', Result)
end;

function TjsHTMLElement.Get_inflateBlock: WordBool; safecall;
begin
  GetPropertyValue('inflateBlock', Result)
end;

function TjsHTMLElement.Get_innerHTML: WideString; safecall;
begin
  GetPropertyValue('innerHTML', Result)
end;

function TjsHTMLElement.Get_innerText: WideString; safecall;
begin
  GetPropertyValue('innerText', Result)
end;

function TjsHTMLElement.Get_isContentEditable: WordBool; safecall;
begin
  GetPropertyValue('isContentEditable', Result)
end;

function TjsHTMLElement.Get_isDisabled: WordBool; safecall;
begin
  GetPropertyValue('isDisabled', Result)
end;

function TjsHTMLElement.Get_isMultiLine: WordBool; safecall;
begin
  GetPropertyValue('isMultiLine', Result)
end;

function TjsHTMLElement.Get_isTextEdit: WordBool; safecall;
begin
  GetPropertyValue('isTextEdit', Result)
end;

function TjsHTMLElement.Get_lang: WideString; safecall;
begin
  GetPropertyValue('lang', Result)
end;

function TjsHTMLElement.Get_language: WideString; safecall;
begin
  GetPropertyValue('language', Result)
end;

function TjsHTMLElement.Get_offsetHeight: Integer; safecall;
begin
  GetPropertyValue('offsetHeight', Result)
end;

function TjsHTMLElement.Get_offsetLeft: Integer; safecall;
begin
  GetPropertyValue('offsetLeft', Result)
end;

function TjsHTMLElement.Get_offsetParent: IHTMLElement; safecall;
begin
  Result:=TjsHTMLElement.Create(FApplication, _JSVar+'.offsetParent');
end;

function TjsHTMLElement.Get_offsetTop: Integer; safecall;
begin
  GetPropertyValue('offsetTop', Result)
end;

function TjsHTMLElement.Get_offsetWidth: Integer; safecall;
begin
  GetPropertyValue('offsetWidth', Result)
end;

function TjsHTMLElement.Get_onactivate: OleVariant; safecall;
begin
  GetPropertyValue('onactivate', Result)
end;

function TjsHTMLElement.Get_onafterupdate: OleVariant; safecall;
begin
  GetPropertyValue('onafterupdate', Result)
end;

function TjsHTMLElement.Get_onbeforeactivate: OleVariant; safecall;
begin
  GetPropertyValue('onbeforeactivate', Result)
end;

function TjsHTMLElement.Get_onbeforecopy: OleVariant; safecall;
begin
  GetPropertyValue('onbeforecopy', Result)
end;

function TjsHTMLElement.Get_onbeforecut: OleVariant; safecall;
begin
  GetPropertyValue('onbeforecut', Result)
end;

function TjsHTMLElement.Get_onbeforedeactivate: OleVariant; safecall;
begin
  GetPropertyValue('onbeforedeactivate', Result)
end;

function TjsHTMLElement.Get_onbeforeeditfocus: OleVariant; safecall;
begin
  GetPropertyValue('onbeforeeditfocus', Result)
end;

function TjsHTMLElement.Get_onbeforepaste: OleVariant; safecall;
begin
  GetPropertyValue('onbeforepaste', Result)
end;

function TjsHTMLElement.Get_onbeforeupdate: OleVariant; safecall;
begin
  GetPropertyValue('onbeforeupdate', Result)
end;

function TjsHTMLElement.Get_onblur: OleVariant; safecall;
begin
  GetPropertyValue('onblur', Result)
end;

function TjsHTMLElement.Get_oncellchange: OleVariant; safecall;
begin
  GetPropertyValue('oncellchange', Result)
end;

function TjsHTMLElement.Get_onclick: OleVariant; safecall;
begin
  GetPropertyValue('onclick', Result)
end;

function TjsHTMLElement.Get_oncontextmenu: OleVariant; safecall;
begin
  GetPropertyValue('oncontextmenu', Result)
end;

function TjsHTMLElement.Get_oncontrolselect: OleVariant; safecall;
begin
  GetPropertyValue('oncontrolselect', Result)
end;

function TjsHTMLElement.Get_oncopy: OleVariant; safecall;
begin
  GetPropertyValue('oncopy', Result)
end;

function TjsHTMLElement.Get_oncut: OleVariant; safecall;
begin
  GetPropertyValue('oncut', Result)
end;

function TjsHTMLElement.Get_ondataavailable: OleVariant; safecall;
begin
  GetPropertyValue('ondataavailable', Result)
end;

function TjsHTMLElement.Get_ondatasetchanged: OleVariant; safecall;
begin
  GetPropertyValue('ondatasetchanged', Result)
end;

function TjsHTMLElement.Get_ondatasetcomplete: OleVariant; safecall;
begin
  GetPropertyValue('ondatasetcomplete', Result)
end;

function TjsHTMLElement.Get_ondblclick: OleVariant; safecall;
begin
  GetPropertyValue('ondblclick', Result)
end;

function TjsHTMLElement.Get_ondeactivate: OleVariant; safecall;
begin
  GetPropertyValue('ondeactivate', Result)
end;

function TjsHTMLElement.Get_ondrag: OleVariant; safecall;
begin
  GetPropertyValue('ondrag', Result)
end;

function TjsHTMLElement.Get_ondragend: OleVariant; safecall;
begin
  GetPropertyValue('ondragend', Result)
end;

function TjsHTMLElement.Get_ondragenter: OleVariant; safecall;
begin
  GetPropertyValue('ondragenter', Result)
end;

function TjsHTMLElement.Get_ondragleave: OleVariant; safecall;
begin
  GetPropertyValue('ondragleave', Result)
end;

function TjsHTMLElement.Get_ondragover: OleVariant; safecall;
begin
  GetPropertyValue('ondragover', Result)
end;

function TjsHTMLElement.Get_ondragstart: OleVariant; safecall;
begin
  GetPropertyValue('ondragstart', Result)
end;

function TjsHTMLElement.Get_ondrop: OleVariant; safecall;
begin
  GetPropertyValue('ondrop', Result)
end;

function TjsHTMLElement.Get_onerrorupdate: OleVariant; safecall;
begin
  GetPropertyValue('onerrorupdate', Result)
end;

function TjsHTMLElement.Get_onfilterchange: OleVariant; safecall;
begin
  GetPropertyValue('onfilterchange', Result)
end;

function TjsHTMLElement.Get_onfocus: OleVariant; safecall;
begin
  GetPropertyValue('onfocus', Result)
end;

function TjsHTMLElement.Get_onfocusin: OleVariant; safecall;
begin
  GetPropertyValue('onfocusin', Result)
end;

function TjsHTMLElement.Get_onfocusout: OleVariant; safecall;
begin
  GetPropertyValue('onfocusout', Result)
end;

function TjsHTMLElement.Get_onhelp: OleVariant; safecall;
begin
  GetPropertyValue('onhelp', Result)
end;

function TjsHTMLElement.Get_onkeydown: OleVariant; safecall;
begin
  GetPropertyValue('onkeydown', Result)
end;

function TjsHTMLElement.Get_onkeypress: OleVariant; safecall;
begin
  GetPropertyValue('onkeypress', Result)
end;

function TjsHTMLElement.Get_onkeyup: OleVariant; safecall;
begin
  GetPropertyValue('onkeyup', Result)
end;

function TjsHTMLElement.Get_onlayoutcomplete: OleVariant; safecall;
begin
  GetPropertyValue('onlayoutcomplete', Result)
end;

function TjsHTMLElement.Get_onlosecapture: OleVariant; safecall;
begin
  GetPropertyValue('onlosecapture', Result)
end;

function TjsHTMLElement.Get_onmousedown: OleVariant; safecall;
begin
  GetPropertyValue('onmousedown', Result)
end;

function TjsHTMLElement.Get_onmouseenter: OleVariant; safecall;
begin
  GetPropertyValue('onmouseenter', Result)
end;

function TjsHTMLElement.Get_onmouseleave: OleVariant; safecall;
begin
  GetPropertyValue('onmouseleave', Result)
end;

function TjsHTMLElement.Get_onmousemove: OleVariant; safecall;
begin
  GetPropertyValue('onmousemove', Result)
end;

function TjsHTMLElement.Get_onmouseout: OleVariant; safecall;
begin
  GetPropertyValue('onmouseout', Result)
end;

function TjsHTMLElement.Get_onmouseover: OleVariant; safecall;
begin
  GetPropertyValue('onmouseover', Result)
end;

function TjsHTMLElement.Get_onmouseup: OleVariant; safecall;
begin
  GetPropertyValue('onmouseup', Result)
end;

function TjsHTMLElement.Get_onmousewheel: OleVariant; safecall;
begin
  GetPropertyValue('onmousewheel', Result)
end;

function TjsHTMLElement.Get_onmove: OleVariant; safecall;
begin
  GetPropertyValue('onmove', Result)
end;

function TjsHTMLElement.Get_onmoveend: OleVariant; safecall;
begin
  GetPropertyValue('onmoveend', Result)
end;

function TjsHTMLElement.Get_onmovestart: OleVariant; safecall;
begin
  GetPropertyValue('onmovestart', Result)
end;

function TjsHTMLElement.Get_onpage: OleVariant; safecall;
begin
  GetPropertyValue('onpage', Result)
end;

function TjsHTMLElement.Get_onpaste: OleVariant; safecall;
begin
  GetPropertyValue('onpaste', Result)
end;

function TjsHTMLElement.Get_onpropertychange: OleVariant; safecall;
begin
  GetPropertyValue('onpropertychange', Result)
end;

function TjsHTMLElement.Get_onreadystatechange: OleVariant; safecall;
begin
  GetPropertyValue('onreadystatechange', Result)
end;

function TjsHTMLElement.Get_onresize: OleVariant; safecall;
begin
  GetPropertyValue('onresize', Result)
end;

function TjsHTMLElement.Get_onresizeend: OleVariant; safecall;
begin
  GetPropertyValue('onresizeend', Result)
end;

function TjsHTMLElement.Get_onresizestart: OleVariant; safecall;
begin
  GetPropertyValue('onresizestart', Result)
end;

function TjsHTMLElement.Get_onrowenter: OleVariant; safecall;
begin
  GetPropertyValue('onrowenter', Result)
end;

function TjsHTMLElement.Get_onrowexit: OleVariant; safecall;
begin
  GetPropertyValue('onrowexit', Result)
end;

function TjsHTMLElement.Get_onrowsdelete: OleVariant; safecall;
begin
  GetPropertyValue('onrowsdelete', Result)
end;

function TjsHTMLElement.Get_onrowsinserted: OleVariant; safecall;
begin
  GetPropertyValue('onrowsinserted', Result)
end;

function TjsHTMLElement.Get_onscroll: OleVariant; safecall;
begin
  GetPropertyValue('onscroll', Result)
end;

function TjsHTMLElement.Get_onselectstart: OleVariant; safecall;
begin
  GetPropertyValue('onselectstart', Result)
end;

function TjsHTMLElement.Get_outerHTML: WideString; safecall;
begin
  GetPropertyValue('outerHTML', Result)
end;

function TjsHTMLElement.Get_outerText: WideString; safecall;
begin
  GetPropertyValue('outerText', Result)
end;

function TjsHTMLElement.Get_parentElement: IHTMLElement; safecall;
begin
  Result:=TjsHTMLElement.Create(FApplication, _JSVar+'.parentElement');
end;

function TjsHTMLElement.Get_parentTextEdit: IHTMLElement; safecall;
begin
  Result:=TjsHTMLElement.Create(FApplication, _JSVar+'.parentTextEdit');
end;

function TjsHTMLElement.Get_readyState: OleVariant; safecall;
begin
  GetPropertyValue('readyState', Result)
end;

function TjsHTMLElement.Get_readyStateValue: Integer; safecall;
begin
  GetPropertyValue('readyStateValue', Result)
end;

function TjsHTMLElement.Get_recordNumber: OleVariant; safecall;
begin
  GetPropertyValue('recordNumber', Result)
end;

function TjsHTMLElement.Get_runtimeStyle: IHTMLStyle; safecall;
begin
  Result:=nil;
end;

function TjsHTMLElement.Get_scopeName: WideString; safecall;
begin
  GetPropertyValue('scopeName', Result)
end;

function TjsHTMLElement.Get_scrollHeight: Integer; safecall;
begin
  GetPropertyValue('scrollHeight', Result)
end;

function TjsHTMLElement.Get_scrollLeft: Integer; safecall;
begin
  GetPropertyValue('scrollLeft', Result)
end;

function TjsHTMLElement.Get_scrollTop: Integer; safecall;
begin
  GetPropertyValue('scrollTop', Result)
end;

function TjsHTMLElement.Get_scrollWidth: Integer; safecall;
begin
  GetPropertyValue('scrollWidth', Result)
end;

function TjsHTMLElement.Get_sourceIndex: Integer; safecall;
begin
  GetPropertyValue('sourceIndex', Result)
end;

function TjsHTMLElement.Get_style: IHTMLStyle; safecall;
begin
  Result:=TjsHTMLStyle.Create(FApplication, _JSVar+'.style');
end;

function TjsHTMLElement.Get_tabIndex: Smallint; safecall;
begin
  GetPropertyValue('tabIndex', Result);
end;

function TjsHTMLElement.Get_tagName: WideString; safecall;
begin
  GetPropertyValue('tagName', Result)
end;

function TjsHTMLElement.Get_tagUrn: WideString; safecall;
begin
  GetPropertyValue('tagUrn', Result)
end;

function TjsHTMLElement.Get_title: WideString; safecall;
begin
  GetPropertyValue('title', Result)
end;

function TjsHTMLElement.getAdjacentText(const where: WideString): WideString; safecall;
begin
  Result:=ExecMethod('getAdjacentText('+ToJSCode(where)+')', true);
end;

function TjsHTMLElement.getAttributeNode(const bstrName: WideString): IHTMLDOMAttribute; safecall;
begin
  Result:=TjsHTMLDOMAttribute.Create(FApplication, _JSVar+'.getAttributeNode('+ToJSCode(bstrName)+')');
end;

function TjsHTMLElement.getBoundingClientRect: IHTMLRect; safecall;
begin
  Result:=tjsHTMLRect.Create(FApplication, _JSVar+'.getBoundingClientRect()');
end;

function TjsHTMLElement.getClientRects: IHTMLRectCollection; safecall;
begin
  Result:=tjsHTMLRectCollection.Create(FApplication,_JSVar+'.getClientRects()', TjsHTMLRect);
end;

function TjsHTMLElement.getElementsByTagName(const v: WideString): IHTMLElementCollection; safecall;
begin
  Result:=TjsHTMLElementCollection.Create(FApplication,_JSVar+'.getElementsByTagName('+ToJSCode(v)+')', TjsHTMLElement);
end;

function TjsHTMLElement.getExpression(const propname: WideString): OleVariant; safecall;
begin
  ExecMethod('getExpression('+ToJSCode(propname)+')');
end;

function TjsHTMLElement.insertAdjacentElement(const where: WideString; const insertedElement: IHTMLElement): IHTMLElement; safecall;
begin
  ExecMethod('insertAdjacentElement('+ToJSCode(where)+','+ToJSCode(insertedElement)+')');
end;

function TjsHTMLElement.removeAttribute(const strAttributeName: WideString; lFlags: Integer): WordBool; safecall;
begin
  ExecMethod('removeAttribute('+ToJSCode(strAttributeName)+','+ToJSCode(lFlags)+')');
end;

function TjsHTMLElement.removeAttributeNode(const pattr: IHTMLDOMAttribute): IHTMLDOMAttribute; safecall;
begin
  ExecMethod('removeAttributeNode('+ToJSCode(pattr)+')');
end;

function TjsHTMLElement.removeBehavior(cookie: Integer): WordBool; safecall;
begin
  ExecMethod('removeBehavior('+ToJSCode(cookie)+')');
end;

function TjsHTMLElement.removeExpression(const propname: WideString): WordBool; safecall;
begin
  ExecMethod('removeExpression('+ToJSCode(propname)+')');
end;

function TjsHTMLElement.replaceAdjacentText(const where: WideString; const newText: WideString): WideString; safecall;
begin
  ExecMethod('replaceAdjacentText('+ToJSCode(where)+','+ToJSCode(newText)+')');
end;

function TjsHTMLElement.setAttributeNode(const pattr: IHTMLDOMAttribute): IHTMLDOMAttribute; safecall;
begin
  ExecMethod('setAttributeNode('+ToJSCode(pattr)+')');
end;

function TjsHTMLElement.toString: WideString; safecall;
begin
  ExecMethod('toString('+')');
end;

procedure TjsHTMLElement.addFilter(const pUnk: IUnknown); safecall;
begin
end;

procedure TjsHTMLElement.blur; safecall;
begin
  ExecMethod('blur()');
end;

procedure TjsHTMLElement.clearAttributes; safecall;
begin
  ExecMethod('clearAttributes('+')');
end;

procedure TjsHTMLElement.detachEvent(const event: WideString; const pdisp: IDispatch); safecall;
begin
  ExecMethod('detachEvent('+ToJSCode(event)+','+ToJSCode(pdisp)+')');
end;

procedure TjsHTMLElement.doScroll(component: OleVariant); safecall;
begin
  ExecMethod('doScroll('+ToJSCode(component)+')');
end;

procedure TjsHTMLElement.focus; safecall;
begin
  ExecMethod('focus('+')');
end;

procedure TjsHTMLElement.insertAdjacentHTML(const where: WideString; const html: WideString); safecall;
begin
  ExecMethod('insertAdjacentHTML('+ToJSCode(where)+','+ToJSCode(html)+')');
end;

procedure TjsHTMLElement.insertAdjacentText(const where: WideString; const text: WideString); safecall;
begin
  ExecMethod('insertAdjacentText('+ToJSCode(where)+','+ToJSCode(text)+')');
end;

procedure TjsHTMLElement.mergeAttributes(const mergeThis: IHTMLElement); safecall;
begin
  ExecMethod('mergeAttributes('+ToJSCode(mergeThis)+')');
end;

procedure TjsHTMLElement.mergeAttributes(const mergeThis: IHTMLElement; var pvarFlags: OleVariant); safecall;
begin
  ExecMethod('mergeAttributes('+ToJSCode(mergeThis)+','+ToJSCode(pvarFlags)+')');
end;

procedure TjsHTMLElement.normalize; safecall;
begin
  ExecMethod('normalize('+')');
end;

procedure TjsHTMLElement.releaseCapture; safecall;
begin
  ExecMethod('releaseCapture('+')');
end;

procedure TjsHTMLElement.removeFilter(const pUnk: IUnknown); safecall;
begin
end;

procedure TjsHTMLElement.scrollIntoView(varargStart: OleVariant); safecall;
begin
  ExecMethod('scrollIntoView('+ToJSCode(varargStart)+')');
end;

procedure TjsHTMLElement.Set_accessKey(const p: WideString); safecall;
begin
  SetPropertyValue('accessKey', p);
end;

procedure TjsHTMLElement.Set_className(const p: WideString); safecall;
begin
  SetPropertyValue('className', p);
end;

procedure TjsHTMLElement.Set_contentEditable(const p: WideString); safecall;
begin
  SetPropertyValue('contentEditable', p);
end;

procedure TjsHTMLElement.Set_dir(const p: WideString); safecall;
begin
  SetPropertyValue('dir', p);
end;

procedure TjsHTMLElement.Set_disabled(p: WordBool); safecall;
begin
  SetPropertyValue('disabled', p);
end;

procedure TjsHTMLElement.Set_hideFocus(p: WordBool); safecall;
begin
  SetPropertyValue('hideFocus', p);
end;

procedure TjsHTMLElement.Set_id(const p: WideString); safecall;
begin
  SetPropertyValue('id', p);
end;

procedure TjsHTMLElement.Set_inflateBlock(p: WordBool); safecall;
begin
  SetPropertyValue('inflateBlock', p);
end;

procedure TjsHTMLElement.Set_innerHTML(const p: WideString); safecall;
begin
  SetPropertyValue('innerHTML', p);
end;

procedure TjsHTMLElement.Set_innerText(const p: WideString); safecall;
begin
  SetPropertyValue('innerText', p);
end;

procedure TjsHTMLElement.Set_lang(const p: WideString); safecall;
begin
  SetPropertyValue('lang', p);
end;

procedure TjsHTMLElement.Set_language(const p: WideString); safecall;
begin
  SetPropertyValue('language', p);
end;

procedure TjsHTMLElement.Set_onactivate(p: OleVariant); safecall;
begin
  SetPropertyValue('onactivate', p, True);
end;

procedure TjsHTMLElement.Set_onafterupdate(p: OleVariant); safecall;
begin
  SetPropertyValue('onafterupdate', p, True);
end;

procedure TjsHTMLElement.Set_onbeforeactivate(p: OleVariant); safecall;
begin
  SetPropertyValue('onbeforeactivate', p, True);
end;

procedure TjsHTMLElement.Set_onbeforecopy(p: OleVariant); safecall;
begin
  SetPropertyValue('onbeforecopy', p, True);
end;

procedure TjsHTMLElement.Set_onbeforecut(p: OleVariant); safecall;
begin
  SetPropertyValue('onbeforecut', p, True);
end;

procedure TjsHTMLElement.Set_onbeforedeactivate(p: OleVariant); safecall;
begin
  SetPropertyValue('onbeforedeactivate', p, True);
end;

procedure TjsHTMLElement.Set_onbeforeeditfocus(p: OleVariant); safecall;
begin
  SetPropertyValue('onbeforeeditfocus', p, True);
end;

procedure TjsHTMLElement.Set_onbeforepaste(p: OleVariant); safecall;
begin
  SetPropertyValue('onbeforepaste', p, True);
end;

procedure TjsHTMLElement.Set_onbeforeupdate(p: OleVariant); safecall;
begin
  SetPropertyValue('onbeforeupdate', p, True);
end;

procedure TjsHTMLElement.Set_onblur(p: OleVariant); safecall;
begin
  SetPropertyValue('onblur', p, True);
end;

procedure TjsHTMLElement.Set_oncellchange(p: OleVariant); safecall;
begin
  SetPropertyValue('oncellchange', p, True);
end;

procedure TjsHTMLElement.Set_onclick(p: OleVariant); safecall;
begin
  SetPropertyValue('onclick', p, True);
end;

procedure TjsHTMLElement.Set_oncontextmenu(p: OleVariant); safecall;
begin
  SetPropertyValue('oncontextmenu', p, True);
end;

procedure TjsHTMLElement.Set_oncontrolselect(p: OleVariant); safecall;
begin
  SetPropertyValue('oncontrolselect', p, True);
end;

procedure TjsHTMLElement.Set_oncopy(p: OleVariant); safecall;
begin
  SetPropertyValue('oncopy', p, True);
end;

procedure TjsHTMLElement.Set_oncut(p: OleVariant); safecall;
begin
  SetPropertyValue('oncut', p, True);
end;

procedure TjsHTMLElement.Set_ondataavailable(p: OleVariant); safecall;
begin
  SetPropertyValue('ondataavailable', p, True);
end;

procedure TjsHTMLElement.Set_ondatasetchanged(p: OleVariant); safecall;
begin
  SetPropertyValue('ondatasetchanged', p, True);
end;

procedure TjsHTMLElement.Set_ondatasetcomplete(p: OleVariant); safecall;
begin
  SetPropertyValue('ondatasetcomplete', p, True);
end;

procedure TjsHTMLElement.Set_ondblclick(p: OleVariant); safecall;
begin
  SetPropertyValue('ondblclick', p, True);
end;

procedure TjsHTMLElement.Set_ondeactivate(p: OleVariant); safecall;
begin
  SetPropertyValue('ondeactivate', p, True);
end;

procedure TjsHTMLElement.Set_ondrag(p: OleVariant); safecall;
begin
  SetPropertyValue('ondrag', p, True);
end;

procedure TjsHTMLElement.Set_ondragend(p: OleVariant); safecall;
begin
  SetPropertyValue('ondragend', p, True);
end;

procedure TjsHTMLElement.Set_ondragenter(p: OleVariant); safecall;
begin
  SetPropertyValue('ondragenter', p, True);
end;

procedure TjsHTMLElement.Set_ondragleave(p: OleVariant); safecall;
begin
  SetPropertyValue('ondragleave', p, True);
end;

procedure TjsHTMLElement.Set_ondragover(p: OleVariant); safecall;
begin
  SetPropertyValue('ondragover', p, True);
end;

procedure TjsHTMLElement.Set_ondragstart(p: OleVariant); safecall;
begin
  SetPropertyValue('ondragstart', p, True);
end;

procedure TjsHTMLElement.Set_ondrop(p: OleVariant); safecall;
begin
  SetPropertyValue('ondrop', p, True);
end;

procedure TjsHTMLElement.Set_onerrorupdate(p: OleVariant); safecall;
begin
  SetPropertyValue('onerrorupdate', p, True);
end;

procedure TjsHTMLElement.Set_onfilterchange(p: OleVariant); safecall;
begin
  SetPropertyValue('onfilterchange', p, True);
end;

procedure TjsHTMLElement.Set_onfocus(p: OleVariant); safecall;
begin
  SetPropertyValue('onfocus', p, True);
end;

procedure TjsHTMLElement.Set_onfocusin(p: OleVariant); safecall;
begin
  SetPropertyValue('onfocusin', p, True);
end;

procedure TjsHTMLElement.Set_onfocusout(p: OleVariant); safecall;
begin
  SetPropertyValue('onfocusout', p, True);
end;

procedure TjsHTMLElement.Set_onhelp(p: OleVariant); safecall;
begin
  SetPropertyValue('onhelp', p, True);
end;

procedure TjsHTMLElement.Set_onkeydown(p: OleVariant); safecall;
begin
  SetPropertyValue('onkeydown', p, True);
end;

procedure TjsHTMLElement.Set_onkeypress(p: OleVariant); safecall;
begin
  SetPropertyValue('onkeypress', p, True);
end;

procedure TjsHTMLElement.Set_onkeyup(p: OleVariant); safecall;
begin
  SetPropertyValue('onkeyup', p, True);
end;

procedure TjsHTMLElement.Set_onlayoutcomplete(p: OleVariant); safecall;
begin
  SetPropertyValue('onlayoutcomplete', p, True);
end;

procedure TjsHTMLElement.Set_onlosecapture(p: OleVariant); safecall;
begin
  SetPropertyValue('onlosecapture', p, True);
end;

procedure TjsHTMLElement.Set_onmousedown(p: OleVariant); safecall;
begin
  SetPropertyValue('onmousedown', p, True);
end;

procedure TjsHTMLElement.Set_onmouseenter(p: OleVariant); safecall;
begin
  SetPropertyValue('onmouseenter', p, True);
end;

procedure TjsHTMLElement.Set_onmouseleave(p: OleVariant); safecall;
begin
  SetPropertyValue('onmouseleave', p, True);
end;

procedure TjsHTMLElement.Set_onmousemove(p: OleVariant); safecall;
begin
  SetPropertyValue('onmousemove', p, True);
end;

procedure TjsHTMLElement.Set_onmouseout(p: OleVariant); safecall;
begin
  SetPropertyValue('onmouseout', p, True);
end;

procedure TjsHTMLElement.Set_onmouseover(p: OleVariant); safecall;
begin
  SetPropertyValue('onmouseover', p, True);
end;

procedure TjsHTMLElement.Set_onmouseup(p: OleVariant); safecall;
begin
  SetPropertyValue('onmouseup', p, True);
end;

procedure TjsHTMLElement.Set_onmousewheel(p: OleVariant); safecall;
begin
  SetPropertyValue('onmousewheel', p, True);
end;

procedure TjsHTMLElement.Set_onmove(p: OleVariant); safecall;
begin
  SetPropertyValue('onmove', p, True);
end;

procedure TjsHTMLElement.Set_onmoveend(p: OleVariant); safecall;
begin
  SetPropertyValue('onmoveend', p, True);
end;

procedure TjsHTMLElement.Set_onmovestart(p: OleVariant); safecall;
begin
  SetPropertyValue('onmovestart', p, True);
end;

procedure TjsHTMLElement.Set_onpage(p: OleVariant); safecall;
begin
  SetPropertyValue('onpage', p, True);
end;

procedure TjsHTMLElement.Set_onpaste(p: OleVariant); safecall;
begin
  SetPropertyValue('onpaste', p, True);
end;

procedure TjsHTMLElement.Set_onpropertychange(p: OleVariant); safecall;
begin
  SetPropertyValue('onpropertychange', p, True);
end;

procedure TjsHTMLElement.Set_onreadystatechange(p: OleVariant); safecall;
begin
  SetPropertyValue('onreadystatechange', p, True);
end;

procedure TjsHTMLElement.Set_onresize(p: OleVariant); safecall;
begin
  SetPropertyValue('onresize', p, True);
end;

procedure TjsHTMLElement.Set_onresizeend(p: OleVariant); safecall;
begin
  SetPropertyValue('onresizeend', p, True);
end;

procedure TjsHTMLElement.Set_onresizestart(p: OleVariant); safecall;
begin
  SetPropertyValue('onresizestart', p, True);
end;

procedure TjsHTMLElement.Set_onrowenter(p: OleVariant); safecall;
begin
  SetPropertyValue('onrowenter', p, True);
end;

procedure TjsHTMLElement.Set_onrowexit(p: OleVariant); safecall;
begin
  SetPropertyValue('onrowexit', p, True);
end;

procedure TjsHTMLElement.Set_onrowsdelete(p: OleVariant); safecall;
begin
  SetPropertyValue('onrowsdelete', p, True);
end;

procedure TjsHTMLElement.Set_onrowsinserted(p: OleVariant); safecall;
begin
  SetPropertyValue('onrowsinserted', p, True);
end;

procedure TjsHTMLElement.Set_onscroll(p: OleVariant); safecall;
begin
  SetPropertyValue('onscroll', p, True);
end;

procedure TjsHTMLElement.Set_onselectstart(p: OleVariant); safecall;
begin
  SetPropertyValue('onselectstart', p, True);
end;

procedure TjsHTMLElement.Set_outerHTML(const p: WideString); safecall;
begin
  SetPropertyValue('outerHTML', p);
end;

procedure TjsHTMLElement.Set_outerText(const p: WideString); safecall;
begin
  SetPropertyValue('outerText', p);
end;

procedure TjsHTMLElement.Set_scrollLeft(p: Integer); safecall;
begin
  SetPropertyValue('scrollLeft', p);
end;

procedure TjsHTMLElement.Set_scrollTop(p: Integer); safecall;
begin
  SetPropertyValue('scrollTop', p);
end;

procedure TjsHTMLElement.Set_tabIndex(p: Smallint); safecall;
begin
  SetPropertyValue('tabIndex', p);
end;

procedure TjsHTMLElement.Set_tagUrn(const p: WideString); safecall;
begin
  SetPropertyValue('tagUrn', p);
end;

procedure TjsHTMLElement.Set_title(const p: WideString); safecall;
begin
  SetPropertyValue('title', p);
end;

procedure TjsHTMLElement.setActive; safecall;
begin
  ExecMethod('setActive('+')');
end;

procedure TjsHTMLElement.setAttribute(const strAttributeName: WideString; AttributeValue: OleVariant;  lFlags: Integer); safecall;
begin
  ExecMethod('setAttribute('+ToJSCode(strAttributeName)+','+ToJSCode(AttributeValue)+','+ToJSCode(lFlags)+')');
end;

procedure TjsHTMLElement.setCapture(containerCapture: WordBool); safecall;
begin
  ExecMethod('setCapture('+ToJSCode(containerCapture)+')');
end;

procedure TjsHTMLElement.setExpression(const propname: WideString; const expression: WideString;  const language: WideString); safecall;
begin
  ExecMethod('setExpression('+ToJSCode(propname)+','+ToJSCode(expression)+','+ToJSCode(language)+')');
end;

{ TjsHTMLFramesCollection }


function TjsHTMLFramesCollection.item(const pvarIndex: OleVariant): OleVariant;
begin
  Result:=inherited item(pvarIndex);
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
  ExecMethod('clearInterval('+ToJSCode(timerID)+')');
end;

procedure TjsHTMLWindow.clearTimeout(timerID: Integer);
begin
  ExecMethod('clearTimeout('+ToJSCode(timerID)+')');
end;

procedure TjsHTMLWindow.close;
begin
  ExecMethod('close');
end;

function TjsHTMLWindow.confirm(const message: WideString): WordBool;
begin
  Result:=IsPositiveMethodResult(ExecMethod('confirm('+ToJSString(message)+')',True));
end;

constructor TjsHTMLWindow.Create(AApplication: TjsdApplication;
  ACreateCommand: String);
begin
  inherited Create(AApplication, ACreateCommand, TjsHTMLWindow);
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
  Result:=TjsHTMLFramesCollection.Create(FApplication, _JSVar+'.frames', TjsHTMLWindow);
end;

function TjsHTMLWindow.Get_history: IOmHistory;
begin
  Result:=TjsOMHistory.Create(FApplication, _JSVar+'.history');
end;

function TjsHTMLWindow.Get_Image: IHTMLImageElementFactory;
begin
  Result:=TjsHTMLImageElementFactory.Create(FApplication, _JSVar+'.Image');
end;

function TjsHTMLWindow.Get_location: IHTMLLocation;
begin
  Result:=TjsHTMLLocation.Create(FApplication, _JSVar + '.location');
end;

function TjsHTMLWindow.Get_name: WideString;
begin
  GetPropertyValue('name', Result);
end;

function TjsHTMLWindow.Get_navigator: IOmNavigator;
begin
  Result:=tjsOMNavigator.Create(FApplication, _JSVar+'.navigator');
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
  result:=TjsHTMLOptionElementFactory.Create(FApplication,_JSVar+'.Option');
end;

function TjsHTMLWindow.Get_parent: IHTMLWindow2;
begin
  Result:=TjsHTMLWindow.Create(FApplication, _JSVar+'.parent');
end;

function TjsHTMLWindow.Get_screen: IHTMLScreen;
begin
  Result:=tjsHTMLScreen.Create(FApplication, _JSVar+'.screen');
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
  ExecMethod('moveBy('+ToJSCode(x)+','+ToJSCode(y)+')');
end;

procedure TjsHTMLWindow.moveTo(x, y: Integer);
begin
  ExecMethod('moveTo('+ToJSCode(x)+','+ToJSCode(y)+')');
end;

procedure TjsHTMLWindow.navigate(const url: WideString);
begin
  ExecMethod('navigate('+ToJSCode(url)+')');
end;

function TjsHTMLWindow.open(const url, name, features: WideString;
  replace: WordBool): IHTMLWindow2;
begin
  Result:=TjsHTMLWindow.Create(FApplication,  _JSVar+'.open('+
  ToJSCode(url)+','+ToJSCode(name)+','+ToJSCode(features)+','+ToJSCode(replace)+')');
end;

function TjsHTMLWindow.prompt(const message, defstr: WideString): OleVariant;
begin
  Result:=ExecMethod('prompt('+ToJSCode(message)+','+ToJSCode(defStr)+')', True);
end;

procedure TjsHTMLWindow.resizeBy(x, y: Integer);
begin
  ExecMethod('resizeBy('+ToJSCode(x)+','+ToJSCode(y)+')');
end;

procedure TjsHTMLWindow.resizeTo(x, y: Integer);
begin
  ExecMethod('resizeTo('+ToJSCode(x)+','+ToJSCode(y)+')');
end;

procedure TjsHTMLWindow.scroll(x, y: Integer);
begin
  ExecMethod('scroll('+ToJSCode(x)+','+ToJSCode(y)+')');
end;

procedure TjsHTMLWindow.scrollBy(x, y: Integer);
begin
  ExecMethod('scrollBy('+ToJSCode(x)+','+ToJSCode(y)+')');
end;

procedure TjsHTMLWindow.scrollTo(x, y: Integer);
begin
  ExecMethod('scrollTo('+ToJSCode(x)+','+ToJSCode(y)+')');
end;

function TjsHTMLWindow.setInterval(const expression: WideString; msec: Integer;
  var language: OleVariant): Integer;
begin
  Result:=StrToInt(
  ExecMethod('setInterval('+ToJSCode(expression)+','+ToJSCode(msec)+','+ToJSCode(language)+')' ,True));
end;

function TjsHTMLWindow.setTimeout(const expression: WideString; msec: Integer;
  var language: OleVariant): Integer;
begin
  Result:=StrToInt(
  ExecMethod('setTimeout('+ToJSCode(expression)+','+ToJSCode(msec)+','+ToJSCode(language)+')' ,True));
end;

procedure TjsHTMLWindow.Set_defaultStatus(const p: WideString);
begin
  SetPropertyValue('defaultStatus',p);
end;

procedure TjsHTMLWindow.Set_name(const p: WideString);
begin
  SetPropertyValue('name',p);
end;

procedure TjsHTMLWindow.Set_offscreenBuffering(p: OleVariant);
begin
  SetPropertyValue('offscreenBuffering',p);
end;

procedure TjsHTMLWindow.Set_onbeforeunload(p: OleVariant);
begin
  SetPropertyValue('onbeforeunload', p, true);
end;

procedure TjsHTMLWindow.Set_onblur(p: OleVariant);
begin
  SetPropertyValue('onblur', p, true);
end;

procedure TjsHTMLWindow.Set_onerror(p: OleVariant);
begin
  SetPropertyValue('onerror', p, true);
end;

procedure TjsHTMLWindow.Set_onfocus(p: OleVariant);
begin
  SetPropertyValue('onfocus', p, true);
end;

procedure TjsHTMLWindow.Set_onhelp(p: OleVariant);
begin
  SetPropertyValue('onhelp', p, true);
end;

procedure TjsHTMLWindow.Set_onload(p: OleVariant);
begin
  SetPropertyValue('onload', p, true);
end;

procedure TjsHTMLWindow.Set_onresize(p: OleVariant);
begin
  SetPropertyValue('onresize', p, true);
end;

procedure TjsHTMLWindow.Set_onscroll(p: OleVariant);
begin
  SetPropertyValue('onscroll', p, true);
end;

procedure TjsHTMLWindow.Set_onunload(p: OleVariant);
begin
  SetPropertyValue('onunload', p, true);
end;

procedure TjsHTMLWindow.Set_opener(p: OleVariant);
begin
  SetPropertyValue('opener', p);
end;

procedure TjsHTMLWindow.Set_status(const p: WideString);
begin
  SetPropertyValue('status',p);
end;

procedure TjsHTMLWindow.showHelp(const helpURL: WideString; helpArg: OleVariant;
  const features: WideString);
begin
  ExecMethod('showModalDialog('+ToJSCode(helpURL)+','+ToJSCode(helpArg)+','+ToJSCode(features)+')');
end;

function TjsHTMLWindow.showModalDialog(const dialog: WideString; var varArgIn,
  varOptions: OleVariant): OleVariant;
begin
  Result:=ExecMethod('showModalDialog('+ToJSCode(dialog)+','+ToJSCode(varArgIn)+','+ToJSCode(varOptions)+')',true);
end;

function TjsHTMLWindow.toString: WideString;
begin
  Result:=ExecMethod('toString',true);
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

{ TjsHTMLStyle }

function TjsHTMLStyle.Get_accelerator: WideString; safecall;
begin
  GetPropertyValue('accelerator', Result)
end;

function TjsHTMLStyle.Get_background: WideString; safecall;
begin
  GetPropertyValue('background', Result)
end;

function TjsHTMLStyle.Get_backgroundAttachment: WideString; safecall;
begin
  GetPropertyValue('backgroundAttachment', Result)
end;

function TjsHTMLStyle.Get_backgroundColor: OleVariant; safecall;
begin
  GetPropertyValue('backgroundColor', Result)
end;

function TjsHTMLStyle.Get_backgroundImage: WideString; safecall;
begin
  GetPropertyValue('backgroundImage', Result)
end;

function TjsHTMLStyle.Get_backgroundPosition: WideString; safecall;
begin
  GetPropertyValue('backgroundPosition', Result)
end;

function TjsHTMLStyle.Get_backgroundPositionX: OleVariant; safecall;
begin
  GetPropertyValue('backgroundPositionX', Result)
end;

function TjsHTMLStyle.Get_backgroundPositionY: OleVariant; safecall;
begin
  GetPropertyValue('backgroundPositionY', Result)
end;

function TjsHTMLStyle.Get_backgroundRepeat: WideString; safecall;
begin
  GetPropertyValue('backgroundRepeat', Result)
end;

function TjsHTMLStyle.Get_behavior: WideString; safecall;
begin
  GetPropertyValue('behavior', Result)
end;

function TjsHTMLStyle.Get_border: WideString; safecall;
begin
  GetPropertyValue('border', Result)
end;

function TjsHTMLStyle.Get_borderBottom: WideString; safecall;
begin
  GetPropertyValue('borderBottom', Result)
end;

function TjsHTMLStyle.Get_borderBottomColor: OleVariant; safecall;
begin
  GetPropertyValue('borderBottomColor', Result)
end;

function TjsHTMLStyle.Get_borderBottomStyle: WideString; safecall;
begin
  GetPropertyValue('borderBottomStyle', Result)
end;

function TjsHTMLStyle.Get_borderBottomWidth: OleVariant; safecall;
begin
  GetPropertyValue('borderBottomWidth', Result)
end;

function TjsHTMLStyle.Get_borderCollapse: WideString; safecall;
begin
  GetPropertyValue('borderCollapse', Result)
end;

function TjsHTMLStyle.Get_borderColor: WideString; safecall;
begin
  GetPropertyValue('borderColor', Result)
end;

function TjsHTMLStyle.Get_borderLeft: WideString; safecall;
begin
  GetPropertyValue('borderLeft', Result)
end;

function TjsHTMLStyle.Get_borderLeftColor: OleVariant; safecall;
begin
  GetPropertyValue('borderLeftColor', Result)
end;

function TjsHTMLStyle.Get_borderLeftStyle: WideString; safecall;
begin
  GetPropertyValue('borderLeftStyle', Result)
end;

function TjsHTMLStyle.Get_borderLeftWidth: OleVariant; safecall;
begin
  GetPropertyValue('borderLeftWidth', Result)
end;

function TjsHTMLStyle.Get_borderRight: WideString; safecall;
begin
  GetPropertyValue('borderRight', Result)
end;

function TjsHTMLStyle.Get_borderRightColor: OleVariant; safecall;
begin
  GetPropertyValue('borderRightColor', Result)
end;

function TjsHTMLStyle.Get_borderRightStyle: WideString; safecall;
begin
  GetPropertyValue('borderRightStyle', Result)
end;

function TjsHTMLStyle.Get_borderRightWidth: OleVariant; safecall;
begin
  GetPropertyValue('borderRightWidth', Result)
end;

function TjsHTMLStyle.Get_borderStyle: WideString; safecall;
begin
  GetPropertyValue('borderStyle', Result)
end;

function TjsHTMLStyle.Get_borderTop: WideString; safecall;
begin
  GetPropertyValue('borderTop', Result)
end;

function TjsHTMLStyle.Get_borderTopColor: OleVariant; safecall;
begin
  GetPropertyValue('borderTopColor', Result)
end;

function TjsHTMLStyle.Get_borderTopStyle: WideString; safecall;
begin
  GetPropertyValue('borderTopStyle', Result)
end;

function TjsHTMLStyle.Get_borderTopWidth: OleVariant; safecall;
begin
  GetPropertyValue('borderTopWidth', Result)
end;

function TjsHTMLStyle.Get_borderWidth: WideString; safecall;
begin
  GetPropertyValue('borderWidth', Result)
end;

function TjsHTMLStyle.Get_bottom: OleVariant; safecall;
begin
  GetPropertyValue('bottom', Result)
end;

function TjsHTMLStyle.Get_clear: WideString; safecall;
begin
  GetPropertyValue('clear', Result)
end;

function TjsHTMLStyle.Get_clip: WideString; safecall;
begin
  GetPropertyValue('clip', Result)
end;

function TjsHTMLStyle.Get_color: OleVariant; safecall;
begin
  GetPropertyValue('color', Result)
end;

function TjsHTMLStyle.Get_cssText: WideString; safecall;
begin
  GetPropertyValue('cssText', Result)
end;

function TjsHTMLStyle.Get_cursor: WideString; safecall;
begin
  GetPropertyValue('cursor', Result)
end;

function TjsHTMLStyle.Get_direction: WideString; safecall;
begin
  GetPropertyValue('direction', Result)
end;

function TjsHTMLStyle.Get_display: WideString; safecall;
begin
  GetPropertyValue('display', Result)
end;

function TjsHTMLStyle.Get_filter: WideString; safecall;
begin
  GetPropertyValue('filter', Result)
end;

function TjsHTMLStyle.Get_font: WideString; safecall;
begin
  GetPropertyValue('font', Result)
end;

function TjsHTMLStyle.Get_fontFamily: WideString; safecall;
begin
  GetPropertyValue('fontFamily', Result)
end;

function TjsHTMLStyle.Get_fontSize: OleVariant; safecall;
begin
  GetPropertyValue('fontSize', Result)
end;

function TjsHTMLStyle.Get_fontStyle: WideString; safecall;
begin
  GetPropertyValue('fontStyle', Result)
end;

function TjsHTMLStyle.Get_fontVariant: WideString; safecall;
begin
  GetPropertyValue('fontVariant', Result)
end;

function TjsHTMLStyle.Get_fontWeight: WideString; safecall;
begin
  GetPropertyValue('fontWeight', Result)
end;

function TjsHTMLStyle.Get_height: OleVariant; safecall;
begin
  GetPropertyValue('height', Result)
end;

function TjsHTMLStyle.Get_imeMode: WideString; safecall;
begin
  GetPropertyValue('imeMode', Result)
end;

function TjsHTMLStyle.Get_layoutFlow: WideString; safecall;
begin
  GetPropertyValue('layoutFlow', Result)
end;

function TjsHTMLStyle.Get_layoutGrid: WideString; safecall;
begin
  GetPropertyValue('layoutGrid', Result)
end;

function TjsHTMLStyle.Get_layoutGridChar: OleVariant; safecall;
begin
  GetPropertyValue('layoutGridChar', Result)
end;

function TjsHTMLStyle.Get_layoutGridLine: OleVariant; safecall;
begin
  GetPropertyValue('layoutGridLine', Result)
end;

function TjsHTMLStyle.Get_layoutGridMode: WideString; safecall;
begin
  GetPropertyValue('layoutGridMode', Result)
end;

function TjsHTMLStyle.Get_layoutGridType: WideString; safecall;
begin
  GetPropertyValue('layoutGridType', Result)
end;

function TjsHTMLStyle.Get_left: OleVariant; safecall;
begin
  GetPropertyValue('left', Result)
end;

function TjsHTMLStyle.Get_letterSpacing: OleVariant; safecall;
begin
  GetPropertyValue('letterSpacing', Result)
end;

function TjsHTMLStyle.Get_lineBreak: WideString; safecall;
begin
  GetPropertyValue('lineBreak', Result)
end;

function TjsHTMLStyle.Get_lineHeight: OleVariant; safecall;
begin
  GetPropertyValue('lineHeight', Result)
end;

function TjsHTMLStyle.Get_listStyle: WideString; safecall;
begin
  GetPropertyValue('listStyle', Result)
end;

function TjsHTMLStyle.Get_listStyleImage: WideString; safecall;
begin
  GetPropertyValue('listStyleImage', Result)
end;

function TjsHTMLStyle.Get_listStylePosition: WideString; safecall;
begin
  GetPropertyValue('listStylePosition', Result)
end;

function TjsHTMLStyle.Get_listStyleType: WideString; safecall;
begin
  GetPropertyValue('listStyleType', Result)
end;

function TjsHTMLStyle.Get_margin: WideString; safecall;
begin
  GetPropertyValue('margin', Result)
end;

function TjsHTMLStyle.Get_marginBottom: OleVariant; safecall;
begin
  GetPropertyValue('marginBottom', Result)
end;

function TjsHTMLStyle.Get_marginLeft: OleVariant; safecall;
begin
  GetPropertyValue('marginLeft', Result)
end;

function TjsHTMLStyle.Get_marginRight: OleVariant; safecall;
begin
  GetPropertyValue('marginRight', Result)
end;

function TjsHTMLStyle.Get_marginTop: OleVariant; safecall;
begin
  GetPropertyValue('marginTop', Result)
end;

function TjsHTMLStyle.Get_minHeight: OleVariant; safecall;
begin
  GetPropertyValue('minHeight', Result)
end;

function TjsHTMLStyle.Get_overflow: WideString; safecall;
begin
  GetPropertyValue('overflow', Result)
end;

function TjsHTMLStyle.Get_overflowX: WideString; safecall;
begin
  GetPropertyValue('overflowX', Result)
end;

function TjsHTMLStyle.Get_overflowY: WideString; safecall;
begin
  GetPropertyValue('overflowY', Result)
end;

function TjsHTMLStyle.Get_padding: WideString; safecall;
begin
  GetPropertyValue('padding', Result)
end;

function TjsHTMLStyle.Get_paddingBottom: OleVariant; safecall;
begin
  GetPropertyValue('paddingBottom', Result)
end;

function TjsHTMLStyle.Get_paddingLeft: OleVariant; safecall;
begin
  GetPropertyValue('paddingLeft', Result)
end;

function TjsHTMLStyle.Get_paddingRight: OleVariant; safecall;
begin
  GetPropertyValue('paddingRight', Result)
end;

function TjsHTMLStyle.Get_paddingTop: OleVariant; safecall;
begin
  GetPropertyValue('paddingTop', Result)
end;

function TjsHTMLStyle.Get_pageBreakAfter: WideString; safecall;
begin
  GetPropertyValue('pageBreakAfter', Result)
end;

function TjsHTMLStyle.Get_pageBreakBefore: WideString; safecall;
begin
  GetPropertyValue('pageBreakBefore', Result)
end;

function TjsHTMLStyle.Get_pixelBottom: Integer; safecall;
begin
  GetPropertyValue('pixelBottom', Result)
end;

function TjsHTMLStyle.Get_pixelHeight: Integer; safecall;
begin
  GetPropertyValue('pixelHeight', Result)
end;

function TjsHTMLStyle.Get_pixelLeft: Integer; safecall;
begin
  GetPropertyValue('pixelLeft', Result)
end;

function TjsHTMLStyle.Get_pixelRight: Integer; safecall;
begin
  GetPropertyValue('pixelRight', Result)
end;

function TjsHTMLStyle.Get_pixelTop: Integer; safecall;
begin
  GetPropertyValue('pixelTop', Result)
end;

function TjsHTMLStyle.Get_pixelWidth: Integer; safecall;
begin
  GetPropertyValue('pixelWidth', Result)
end;

function TjsHTMLStyle.Get_posBottom: Single; safecall;
begin
  GetPropertyValue('posBottom', Result)
end;

function TjsHTMLStyle.Get_posHeight: Single; safecall;
begin
  GetPropertyValue('posHeight', Result)
end;

function TjsHTMLStyle.Get_position: WideString; safecall;
begin
  GetPropertyValue('position', Result)
end;

function TjsHTMLStyle.Get_posLeft: Single; safecall;
begin
  GetPropertyValue('posLeft', Result)
end;

function TjsHTMLStyle.Get_posRight: Single; safecall;
begin
  GetPropertyValue('posRight', Result)
end;

function TjsHTMLStyle.Get_posTop: Single; safecall;
begin
  GetPropertyValue('posTop', Result)
end;

function TjsHTMLStyle.Get_posWidth: Single; safecall;
begin
  GetPropertyValue('posWidth', Result)
end;

function TjsHTMLStyle.Get_right: OleVariant; safecall;
begin
  GetPropertyValue('right', Result)
end;

function TjsHTMLStyle.Get_rubyAlign: WideString; safecall;
begin
  GetPropertyValue('rubyAlign', Result)
end;

function TjsHTMLStyle.Get_rubyOverhang: WideString; safecall;
begin
  GetPropertyValue('rubyOverhang', Result)
end;

function TjsHTMLStyle.Get_rubyPosition: WideString; safecall;
begin
  GetPropertyValue('rubyPosition', Result)
end;

function TjsHTMLStyle.Get_scrollbar3dLightColor: OleVariant; safecall;
begin
  GetPropertyValue('scrollbar3dLightColor', Result)
end;

function TjsHTMLStyle.Get_scrollbarArrowColor: OleVariant; safecall;
begin
  GetPropertyValue('scrollbarArrowColor', Result)
end;

function TjsHTMLStyle.Get_scrollbarBaseColor: OleVariant; safecall;
begin
  GetPropertyValue('scrollbarBaseColor', Result)
end;

function TjsHTMLStyle.Get_scrollbarDarkShadowColor: OleVariant; safecall;
begin
  GetPropertyValue('scrollbarDarkShadowColor', Result)
end;

function TjsHTMLStyle.Get_scrollbarFaceColor: OleVariant; safecall;
begin
  GetPropertyValue('scrollbarFaceColor', Result)
end;

function TjsHTMLStyle.Get_scrollbarHighlightColor: OleVariant; safecall;
begin
  GetPropertyValue('scrollbarHighlightColor', Result)
end;

function TjsHTMLStyle.Get_scrollbarShadowColor: OleVariant; safecall;
begin
  GetPropertyValue('scrollbarShadowColor', Result)
end;

function TjsHTMLStyle.Get_scrollbarTrackColor: OleVariant; safecall;
begin
  GetPropertyValue('scrollbarTrackColor', Result)
end;

function TjsHTMLStyle.Get_styleFloat: WideString; safecall;
begin
  GetPropertyValue('styleFloat', Result)
end;

function TjsHTMLStyle.Get_tableLayout: WideString; safecall;
begin
  GetPropertyValue('tableLayout', Result)
end;

function TjsHTMLStyle.Get_textAlign: WideString; safecall;
begin
  GetPropertyValue('textAlign', Result)
end;

function TjsHTMLStyle.Get_textAlignLast: WideString; safecall;
begin
  GetPropertyValue('textAlignLast', Result)
end;

function TjsHTMLStyle.Get_textAutospace: WideString; safecall;
begin
  GetPropertyValue('textAutospace', Result)
end;

function TjsHTMLStyle.Get_textDecoration: WideString; safecall;
begin
  GetPropertyValue('textDecoration', Result)
end;

function TjsHTMLStyle.Get_textDecorationBlink: WordBool; safecall;
begin
  GetPropertyValue('textDecorationBlink', Result)
end;

function TjsHTMLStyle.Get_textDecorationLineThrough: WordBool; safecall;
begin
  GetPropertyValue('textDecorationLineThrough', Result)
end;

function TjsHTMLStyle.Get_textDecorationNone: WordBool; safecall;
begin
  GetPropertyValue('textDecorationNone', Result)
end;

function TjsHTMLStyle.Get_textDecorationOverline: WordBool; safecall;
begin
  GetPropertyValue('textDecorationOverline', Result)
end;

function TjsHTMLStyle.Get_textDecorationUnderline: WordBool; safecall;
begin
  GetPropertyValue('textDecorationUnderline', Result)
end;

function TjsHTMLStyle.Get_textIndent: OleVariant; safecall;
begin
  GetPropertyValue('textIndent', Result)
end;

function TjsHTMLStyle.Get_textJustify: WideString; safecall;
begin
  GetPropertyValue('textJustify', Result)
end;

function TjsHTMLStyle.Get_textJustifyTrim: WideString; safecall;
begin
  GetPropertyValue('textJustifyTrim', Result)
end;

function TjsHTMLStyle.Get_textKashida: OleVariant; safecall;
begin
  GetPropertyValue('textKashida', Result)
end;

function TjsHTMLStyle.Get_textKashidaSpace: OleVariant; safecall;
begin
  GetPropertyValue('textKashidaSpace', Result)
end;

function TjsHTMLStyle.Get_textOverflow: WideString; safecall;
begin
  GetPropertyValue('textOverflow', Result)
end;

function TjsHTMLStyle.Get_textTransform: WideString; safecall;
begin
  GetPropertyValue('textTransform', Result)
end;

function TjsHTMLStyle.Get_textUnderlinePosition: WideString; safecall;
begin
  GetPropertyValue('textUnderlinePosition', Result)
end;

function TjsHTMLStyle.Get_top: OleVariant; safecall;
begin
  GetPropertyValue('top', Result)
end;

function TjsHTMLStyle.Get_unicodeBidi: WideString; safecall;
begin
  GetPropertyValue('unicodeBidi', Result)
end;

function TjsHTMLStyle.Get_verticalAlign: OleVariant; safecall;
begin
  GetPropertyValue('verticalAlign', Result)
end;

function TjsHTMLStyle.Get_visibility: WideString; safecall;
begin
  GetPropertyValue('visibility', Result)
end;

function TjsHTMLStyle.Get_whiteSpace: WideString; safecall;
begin
  GetPropertyValue('whiteSpace', Result)
end;

function TjsHTMLStyle.Get_width: OleVariant; safecall;
begin
  GetPropertyValue('width', Result)
end;

function TjsHTMLStyle.Get_wordBreak: WideString; safecall;
begin
  GetPropertyValue('wordBreak', Result)
end;

function TjsHTMLStyle.Get_wordSpacing: OleVariant; safecall;
begin
  GetPropertyValue('wordSpacing', Result)
end;

function TjsHTMLStyle.Get_wordWrap: WideString; safecall;
begin
  GetPropertyValue('wordWrap', Result)
end;

function TjsHTMLStyle.Get_writingMode: WideString; safecall;
begin
  GetPropertyValue('writingMode', Result)
end;

function TjsHTMLStyle.Get_zIndex: OleVariant; safecall;
begin
  GetPropertyValue('zIndex', Result)
end;

function TjsHTMLStyle.Get_zoom: OleVariant; safecall;
begin
  GetPropertyValue('zoom', Result)
end;

function TjsHTMLStyle.getAttribute(const strAttributeName: WideString; lFlags: Integer): OleVariant; safecall;
begin
  Result:=ExecMethod('getAttribute('+ToJSCode(strAttributeName)+','+ToJSCode(lFlags)+')', True);
end;

function TjsHTMLStyle.getExpression(const propname: WideString): OleVariant; safecall;
begin
  Result:=ExecMethod('getExpression('+ToJSCode(propname)+')', True);
end;

function TjsHTMLStyle.removeAttribute(const strAttributeName: WideString; lFlags: Integer): WordBool; safecall;
begin
  Result:=IsPositiveMethodResult(ExecMethod('removeAttribute('+ToJSCode(strAttributeName)+','+ToJSCode(lFlags)+')',True));
end;

function TjsHTMLStyle.removeExpression(const propname: WideString): WordBool; safecall;
begin
  Result:=IsPositiveMethodResult(ExecMethod('removeExpression('+ToJSCode(propname)+')', true));
end;

function TjsHTMLStyle.toString: WideString; safecall;
begin
  Result:=ExecMethod('toString()', True);
end;

procedure TjsHTMLStyle.Set_accelerator(const p: WideString); safecall;
begin
  SetPropertyValue('accelerator', p);
end;

procedure TjsHTMLStyle.Set_background(const p: WideString); safecall;
begin
  SetPropertyValue('background', p);
end;

procedure TjsHTMLStyle.Set_backgroundAttachment(const p: WideString); safecall;
begin
  SetPropertyValue('backgroundAttachment', p);
end;

procedure TjsHTMLStyle.Set_backgroundColor(p: OleVariant); safecall;
begin
  SetPropertyValue('backgroundColor', p);
end;

procedure TjsHTMLStyle.Set_backgroundImage(const p: WideString); safecall;
begin
  SetPropertyValue('backgroundImage', p);
end;

procedure TjsHTMLStyle.Set_backgroundPosition(const p: WideString); safecall;
begin
  SetPropertyValue('backgroundPosition', p);
end;

procedure TjsHTMLStyle.Set_backgroundPositionX(p: OleVariant); safecall;
begin
  SetPropertyValue('backgroundPositionX', p);
end;

procedure TjsHTMLStyle.Set_backgroundPositionY(p: OleVariant); safecall;
begin
  SetPropertyValue('backgroundPositionY', p);
end;

procedure TjsHTMLStyle.Set_backgroundRepeat(const p: WideString); safecall;
begin
  SetPropertyValue('backgroundRepeat', p);
end;

procedure TjsHTMLStyle.Set_behavior(const p: WideString); safecall;
begin
  SetPropertyValue('behavior', p);
end;

procedure TjsHTMLStyle.Set_border(const p: WideString); safecall;
begin
  SetPropertyValue('border', p);
end;

procedure TjsHTMLStyle.Set_borderBottom(const p: WideString); safecall;
begin
  SetPropertyValue('borderBottom', p);
end;

procedure TjsHTMLStyle.Set_borderBottomColor(p: OleVariant); safecall;
begin
  SetPropertyValue('borderBottomColor', p);
end;

procedure TjsHTMLStyle.Set_borderBottomStyle(const p: WideString); safecall;
begin
  SetPropertyValue('borderBottomStyle', p);
end;

procedure TjsHTMLStyle.Set_borderBottomWidth(p: OleVariant); safecall;
begin
  SetPropertyValue('borderBottomWidth', p);
end;

procedure TjsHTMLStyle.Set_borderCollapse(const p: WideString); safecall;
begin
  SetPropertyValue('borderCollapse', p);
end;

procedure TjsHTMLStyle.Set_borderColor(const p: WideString); safecall;
begin
  SetPropertyValue('borderColor', p);
end;

procedure TjsHTMLStyle.Set_borderLeft(const p: WideString); safecall;
begin
  SetPropertyValue('borderLeft', p);
end;

procedure TjsHTMLStyle.Set_borderLeftColor(p: OleVariant); safecall;
begin
  SetPropertyValue('borderLeftColor', p);
end;

procedure TjsHTMLStyle.Set_borderLeftStyle(const p: WideString); safecall;
begin
  SetPropertyValue('borderLeftStyle', p);
end;

procedure TjsHTMLStyle.Set_borderLeftWidth(p: OleVariant); safecall;
begin
  SetPropertyValue('borderLeftWidth', p);
end;

procedure TjsHTMLStyle.Set_borderRight(const p: WideString); safecall;
begin
  SetPropertyValue('borderRight', p);
end;

procedure TjsHTMLStyle.Set_borderRightColor(p: OleVariant); safecall;
begin
  SetPropertyValue('borderRightColor', p);
end;

procedure TjsHTMLStyle.Set_borderRightStyle(const p: WideString); safecall;
begin
  SetPropertyValue('borderRightStyle', p);
end;

procedure TjsHTMLStyle.Set_borderRightWidth(p: OleVariant); safecall;
begin
  SetPropertyValue('borderRightWidth', p);
end;

procedure TjsHTMLStyle.Set_borderStyle(const p: WideString); safecall;
begin
  SetPropertyValue('borderStyle', p);
end;

procedure TjsHTMLStyle.Set_borderTop(const p: WideString); safecall;
begin
  SetPropertyValue('borderTop', p);
end;

procedure TjsHTMLStyle.Set_borderTopColor(p: OleVariant); safecall;
begin
  SetPropertyValue('borderTopColor', p);
end;

procedure TjsHTMLStyle.Set_borderTopStyle(const p: WideString); safecall;
begin
  SetPropertyValue('borderTopStyle', p);
end;

procedure TjsHTMLStyle.Set_borderTopWidth(p: OleVariant); safecall;
begin
  SetPropertyValue('borderTopWidth', p);
end;

procedure TjsHTMLStyle.Set_borderWidth(const p: WideString); safecall;
begin
  SetPropertyValue('borderWidth', p);
end;

procedure TjsHTMLStyle.Set_bottom(p: OleVariant); safecall;
begin
  SetPropertyValue('bottom', p);
end;

procedure TjsHTMLStyle.Set_clear(const p: WideString); safecall;
begin
  SetPropertyValue('clear', p);
end;

procedure TjsHTMLStyle.Set_clip(const p: WideString); safecall;
begin
  SetPropertyValue('clip', p);
end;

procedure TjsHTMLStyle.Set_color(p: OleVariant); safecall;
begin
  SetPropertyValue('color', p);
end;

procedure TjsHTMLStyle.Set_cssText(const p: WideString); safecall;
begin
  SetPropertyValue('cssText', p);
end;

procedure TjsHTMLStyle.Set_cursor(const p: WideString); safecall;
begin
  SetPropertyValue('cursor', p);
end;

procedure TjsHTMLStyle.Set_direction(const p: WideString); safecall;
begin
  SetPropertyValue('direction', p);
end;

procedure TjsHTMLStyle.Set_display(const p: WideString); safecall;
begin
  SetPropertyValue('display', p);
end;

procedure TjsHTMLStyle.Set_filter(const p: WideString); safecall;
begin
  SetPropertyValue('filter', p);
end;

procedure TjsHTMLStyle.Set_font(const p: WideString); safecall;
begin
  SetPropertyValue('font', p);
end;

procedure TjsHTMLStyle.Set_fontFamily(const p: WideString); safecall;
begin
  SetPropertyValue('fontFamily', p);
end;

procedure TjsHTMLStyle.Set_fontSize(p: OleVariant); safecall;
begin
  SetPropertyValue('fontSize', p);
end;

procedure TjsHTMLStyle.Set_fontStyle(const p: WideString); safecall;
begin
  SetPropertyValue('fontStyle', p);
end;

procedure TjsHTMLStyle.Set_fontVariant(const p: WideString); safecall;
begin
  SetPropertyValue('fontVariant', p);
end;

procedure TjsHTMLStyle.Set_fontWeight(const p: WideString); safecall;
begin
  SetPropertyValue('fontWeight', p);
end;

procedure TjsHTMLStyle.Set_height(p: OleVariant); safecall;
begin
  SetPropertyValue('height', p);
end;

procedure TjsHTMLStyle.Set_imeMode(const p: WideString); safecall;
begin
  SetPropertyValue('imeMode', p);
end;

procedure TjsHTMLStyle.Set_layoutFlow(const p: WideString); safecall;
begin
  SetPropertyValue('layoutFlow', p);
end;

procedure TjsHTMLStyle.Set_layoutGrid(const p: WideString); safecall;
begin
  SetPropertyValue('layoutGrid', p);
end;

procedure TjsHTMLStyle.Set_layoutGridChar(p: OleVariant); safecall;
begin
  SetPropertyValue('layoutGridChar', p);
end;

procedure TjsHTMLStyle.Set_layoutGridLine(p: OleVariant); safecall;
begin
  SetPropertyValue('layoutGridLine', p);
end;

procedure TjsHTMLStyle.Set_layoutGridMode(const p: WideString); safecall;
begin
  SetPropertyValue('layoutGridMode', p);
end;

procedure TjsHTMLStyle.Set_layoutGridType(const p: WideString); safecall;
begin
  SetPropertyValue('layoutGridType', p);
end;

procedure TjsHTMLStyle.Set_left(p: OleVariant); safecall;
begin
  SetPropertyValue('left', p);
end;

procedure TjsHTMLStyle.Set_letterSpacing(p: OleVariant); safecall;
begin
  SetPropertyValue('letterSpacing', p);
end;

procedure TjsHTMLStyle.Set_lineBreak(const p: WideString); safecall;
begin
  SetPropertyValue('lineBreak', p);
end;

procedure TjsHTMLStyle.Set_lineHeight(p: OleVariant); safecall;
begin
  SetPropertyValue('lineHeight', p);
end;

procedure TjsHTMLStyle.Set_listStyle(const p: WideString); safecall;
begin
  SetPropertyValue('listStyle', p);
end;

procedure TjsHTMLStyle.Set_listStyleImage(const p: WideString); safecall;
begin
  SetPropertyValue('listStyleImage', p);
end;

procedure TjsHTMLStyle.Set_listStylePosition(const p: WideString); safecall;
begin
  SetPropertyValue('listStylePosition', p);
end;

procedure TjsHTMLStyle.Set_listStyleType(const p: WideString); safecall;
begin
  SetPropertyValue('listStyleType', p);
end;

procedure TjsHTMLStyle.Set_margin(const p: WideString); safecall;
begin
  SetPropertyValue('margin', p);
end;

procedure TjsHTMLStyle.Set_marginBottom(p: OleVariant); safecall;
begin
  SetPropertyValue('marginBottom', p);
end;

procedure TjsHTMLStyle.Set_marginLeft(p: OleVariant); safecall;
begin
  SetPropertyValue('marginLeft', p);
end;

procedure TjsHTMLStyle.Set_marginRight(p: OleVariant); safecall;
begin
  SetPropertyValue('marginRight', p);
end;

procedure TjsHTMLStyle.Set_marginTop(p: OleVariant); safecall;
begin
  SetPropertyValue('marginTop', p);
end;

procedure TjsHTMLStyle.Set_minHeight(p: OleVariant); safecall;
begin
  SetPropertyValue('minHeight', p);
end;

procedure TjsHTMLStyle.Set_overflow(const p: WideString); safecall;
begin
  SetPropertyValue('overflow', p);
end;

procedure TjsHTMLStyle.Set_overflowX(const p: WideString); safecall;
begin
  SetPropertyValue('overflowX', p);
end;

procedure TjsHTMLStyle.Set_overflowY(const p: WideString); safecall;
begin
  SetPropertyValue('overflowY', p);
end;

procedure TjsHTMLStyle.Set_padding(const p: WideString); safecall;
begin
  SetPropertyValue('padding', p);
end;

procedure TjsHTMLStyle.Set_paddingBottom(p: OleVariant); safecall;
begin
  SetPropertyValue('paddingBottom', p);
end;

procedure TjsHTMLStyle.Set_paddingLeft(p: OleVariant); safecall;
begin
  SetPropertyValue('paddingLeft', p);
end;

procedure TjsHTMLStyle.Set_paddingRight(p: OleVariant); safecall;
begin
  SetPropertyValue('paddingRight', p);
end;

procedure TjsHTMLStyle.Set_paddingTop(p: OleVariant); safecall;
begin
  SetPropertyValue('paddingTop', p);
end;

procedure TjsHTMLStyle.Set_pageBreakAfter(const p: WideString); safecall;
begin
  SetPropertyValue('pageBreakAfter', p);
end;

procedure TjsHTMLStyle.Set_pageBreakBefore(const p: WideString); safecall;
begin
  SetPropertyValue('pageBreakBefore', p);
end;

procedure TjsHTMLStyle.Set_pixelBottom(p: Integer); safecall;
begin
  SetPropertyValue('pixelBottom', p);
end;

procedure TjsHTMLStyle.Set_pixelHeight(p: Integer); safecall;
begin
  SetPropertyValue('pixelHeight', p);
end;

procedure TjsHTMLStyle.Set_pixelLeft(p: Integer); safecall;
begin
  SetPropertyValue('pixelLeft', p);
end;

procedure TjsHTMLStyle.Set_pixelRight(p: Integer); safecall;
begin
  SetPropertyValue('pixelRight', p);
end;

procedure TjsHTMLStyle.Set_pixelTop(p: Integer); safecall;
begin
  SetPropertyValue('pixelTop', p);
end;

procedure TjsHTMLStyle.Set_pixelWidth(p: Integer); safecall;
begin
  SetPropertyValue('pixelWidth', p);
end;

procedure TjsHTMLStyle.Set_posBottom(p: Single); safecall;
begin
  SetPropertyValue('posBottom', p);
end;

procedure TjsHTMLStyle.Set_posHeight(p: Single); safecall;
begin
  SetPropertyValue('posHeight', p);
end;

procedure TjsHTMLStyle.Set_position(const p: WideString); safecall;
begin
  SetPropertyValue('position', p);
end;

procedure TjsHTMLStyle.Set_posLeft(p: Single); safecall;
begin
  SetPropertyValue('posLeft', p);
end;

procedure TjsHTMLStyle.Set_posRight(p: Single); safecall;
begin
  SetPropertyValue('posRight', p);
end;

procedure TjsHTMLStyle.Set_posTop(p: Single); safecall;
begin
  SetPropertyValue('posTop', p);
end;

procedure TjsHTMLStyle.Set_posWidth(p: Single); safecall;
begin
  SetPropertyValue('posWidth', p);
end;

procedure TjsHTMLStyle.Set_right(p: OleVariant); safecall;
begin
  SetPropertyValue('right', p);
end;

procedure TjsHTMLStyle.Set_rubyAlign(const p: WideString); safecall;
begin
  SetPropertyValue('rubyAlign', p);
end;

procedure TjsHTMLStyle.Set_rubyOverhang(const p: WideString); safecall;
begin
  SetPropertyValue('rubyOverhang', p);
end;

procedure TjsHTMLStyle.Set_rubyPosition(const p: WideString); safecall;
begin
  SetPropertyValue('rubyPosition', p);
end;

procedure TjsHTMLStyle.Set_scrollbar3dLightColor(p: OleVariant); safecall;
begin
  SetPropertyValue('scrollbar3dLightColor', p);
end;

procedure TjsHTMLStyle.Set_scrollbarArrowColor(p: OleVariant); safecall;
begin
  SetPropertyValue('scrollbarArrowColor', p);
end;

procedure TjsHTMLStyle.Set_scrollbarBaseColor(p: OleVariant); safecall;
begin
  SetPropertyValue('scrollbarBaseColor', p);
end;

procedure TjsHTMLStyle.Set_scrollbarDarkShadowColor(p: OleVariant); safecall;
begin
  SetPropertyValue('scrollbarDarkShadowColor', p);
end;

procedure TjsHTMLStyle.Set_scrollbarFaceColor(p: OleVariant); safecall;
begin
  SetPropertyValue('scrollbarFaceColor', p);
end;

procedure TjsHTMLStyle.Set_scrollbarHighlightColor(p: OleVariant); safecall;
begin
  SetPropertyValue('scrollbarHighlightColor', p);
end;

procedure TjsHTMLStyle.Set_scrollbarShadowColor(p: OleVariant); safecall;
begin
  SetPropertyValue('scrollbarShadowColor', p);
end;

procedure TjsHTMLStyle.Set_scrollbarTrackColor(p: OleVariant); safecall;
begin
  SetPropertyValue('scrollbarTrackColor', p);
end;

procedure TjsHTMLStyle.Set_styleFloat(const p: WideString); safecall;
begin
  SetPropertyValue('styleFloat', p);
end;

procedure TjsHTMLStyle.Set_tableLayout(const p: WideString); safecall;
begin
  SetPropertyValue('tableLayout', p);
end;

procedure TjsHTMLStyle.Set_textAlign(const p: WideString); safecall;
begin
  SetPropertyValue('textAlign', p);
end;

procedure TjsHTMLStyle.Set_textAlignLast(const p: WideString); safecall;
begin
  SetPropertyValue('textAlignLast', p);
end;

procedure TjsHTMLStyle.Set_textAutospace(const p: WideString); safecall;
begin
  SetPropertyValue('textAutospace', p);
end;

procedure TjsHTMLStyle.Set_textDecoration(const p: WideString); safecall;
begin
  SetPropertyValue('textDecoration', p);
end;

procedure TjsHTMLStyle.Set_textDecorationBlink(p: WordBool); safecall;
begin
  SetPropertyValue('textDecorationBlink', p);
end;

procedure TjsHTMLStyle.Set_textDecorationLineThrough(p: WordBool); safecall;
begin
  SetPropertyValue('textDecorationLineThrough', p);
end;

procedure TjsHTMLStyle.Set_textDecorationNone(p: WordBool); safecall;
begin
  SetPropertyValue('textDecorationNone', p);
end;

procedure TjsHTMLStyle.Set_textDecorationOverline(p: WordBool); safecall;
begin
  SetPropertyValue('textDecorationOverline', p);
end;

procedure TjsHTMLStyle.Set_textDecorationUnderline(p: WordBool); safecall;
begin
  SetPropertyValue('textDecorationUnderline', p);
end;

procedure TjsHTMLStyle.Set_textIndent(p: OleVariant); safecall;
begin
  SetPropertyValue('textIndent', p);
end;

procedure TjsHTMLStyle.Set_textJustify(const p: WideString); safecall;
begin
  SetPropertyValue('textJustify', p);
end;

procedure TjsHTMLStyle.Set_textJustifyTrim(const p: WideString); safecall;
begin
  SetPropertyValue('textJustifyTrim', p);
end;

procedure TjsHTMLStyle.Set_textKashida(p: OleVariant); safecall;
begin
  SetPropertyValue('textKashida', p);
end;

procedure TjsHTMLStyle.Set_textKashidaSpace(p: OleVariant); safecall;
begin
  SetPropertyValue('textKashidaSpace', p);
end;

procedure TjsHTMLStyle.Set_textOverflow(const p: WideString); safecall;
begin
  SetPropertyValue('textOverflow', p);
end;

procedure TjsHTMLStyle.Set_textTransform(const p: WideString); safecall;
begin
  SetPropertyValue('textTransform', p);
end;

procedure TjsHTMLStyle.Set_textUnderlinePosition(const p: WideString); safecall;
begin
  SetPropertyValue('textUnderlinePosition', p);
end;

procedure TjsHTMLStyle.Set_top(p: OleVariant); safecall;
begin
  SetPropertyValue('top', p);
end;

procedure TjsHTMLStyle.Set_unicodeBidi(const p: WideString); safecall;
begin
  SetPropertyValue('unicodeBidi', p);
end;

procedure TjsHTMLStyle.Set_verticalAlign(p: OleVariant); safecall;
begin
  SetPropertyValue('verticalAlign', p);
end;

procedure TjsHTMLStyle.Set_visibility(const p: WideString); safecall;
begin
  SetPropertyValue('visibility', p);
end;

procedure TjsHTMLStyle.Set_whiteSpace(const p: WideString); safecall;
begin
  SetPropertyValue('whiteSpace', p);
end;

procedure TjsHTMLStyle.Set_width(p: OleVariant); safecall;
begin
  SetPropertyValue('width', p);
end;

procedure TjsHTMLStyle.Set_wordBreak(const p: WideString); safecall;
begin
  SetPropertyValue('wordBreak', p);
end;

procedure TjsHTMLStyle.Set_wordSpacing(p: OleVariant); safecall;
begin
  SetPropertyValue('wordSpacing', p);
end;

procedure TjsHTMLStyle.Set_wordWrap(const p: WideString); safecall;
begin
  SetPropertyValue('wordWrap', p);
end;

procedure TjsHTMLStyle.Set_writingMode(const p: WideString); safecall;
begin
  SetPropertyValue('writingMode', p);
end;

procedure TjsHTMLStyle.Set_zIndex(p: OleVariant); safecall;
begin
  SetPropertyValue('zIndex', p);
end;

procedure TjsHTMLStyle.Set_zoom(p: OleVariant); safecall;
begin
  SetPropertyValue('zoom', p);
end;

procedure TjsHTMLStyle.setAttribute(const strAttributeName: WideString; AttributeValue: OleVariant;  lFlags: Integer); safecall;
begin
  ExecMethod('setAttribute('+ToJSCode(strAttributeName)+','+ToJSCode(AttributeValue)+','+ToJSCode(lFlags)+')');
end;

procedure TjsHTMLStyle.setExpression(const propname: WideString; const expression: WideString;  const language: WideString); safecall;
begin
  ExecMethod('setExpression('+ToJSCode(propname)+','+ToJSCode(expression)+','+ToJSCode(language)+')');
end;

{ TjsHTMLRect }

function TjsHTMLRect.Get_bottom: Integer; safecall;
begin
  GetPropertyValue('bottom', Result)
end;

function TjsHTMLRect.Get_left: Integer; safecall;
begin
  GetPropertyValue('left', Result)
end;

function TjsHTMLRect.Get_right: Integer; safecall;
begin
  GetPropertyValue('right', Result)
end;

function TjsHTMLRect.Get_top: Integer; safecall;
begin
  GetPropertyValue('top', Result)
end;

procedure TjsHTMLRect.Set_bottom(p: Integer); safecall;
begin
  SetPropertyValue('bottom', p);
end;

procedure TjsHTMLRect.Set_left(p: Integer); safecall;
begin
  SetPropertyValue('left', p);
end;

procedure TjsHTMLRect.Set_right(p: Integer); safecall;
begin
  SetPropertyValue('right', p);
end;

procedure TjsHTMLRect.Set_top(p: Integer); safecall;
begin
  SetPropertyValue('top', p);
end;


{ TjsDOMCollection }

constructor TjsDOMCollection.Create(AApplication: TjsdApplication;
  ACreateCommand: String; AElementClass: TjsDOMObjectClass);
begin
  inherited Create(AApplication, ACreateCommand);
  FElementClass:=AElementClass;
end;

function TjsDOMCollection.Get_length: Integer;
begin
  GetPropertyValue('length');
end;

function TjsDOMCollection.Get__newEnum: IUnknown;
begin
  Result:=nil;
end;

function TjsDOMCollection.item(const name: OleVariant): IDispatch;
begin
  Result:=FElementClass.Create(FApplication, _JSVar+'.item('+ToJSCode(name)+')');
end;

function TjsDOMCollection.item(name, index: OleVariant): IDispatch;
begin
  Result:=FElementClass.Create(FApplication, _JSVar+'.item('+ToJSCode(name)+', '+ToJSString(index)+')');
end;

{ TjsHTMLRectCollection }

function TjsHTMLRectCollection.item(const pvarIndex: OleVariant): OleVariant;
begin
  Result:=Inherited item(pvarIndex);
end;

{ TjsHTMLStyleSheet }

function TjsHTMLStyleSheet.addImport(const bstrUrl: WideString; lIndex: Integer): Integer; safecall;
begin
  Result:=StrToInt(ExecMethod('addImport('+ToJSCode(bstrUrl)+','+ToJSCode(lIndex)+')', true));
end;

function TjsHTMLStyleSheet.addPageRule(const bstrSelector: WideString; const bstrStyle: WideString;  lIndex: Integer): Integer; safecall;
begin
  Result:=StrToInt(ExecMethod('addPageRule('+ToJSCode(bstrSelector)+','+ToJSCode(bstrStyle)+','+ToJSCode(lIndex)+')', true));
end;

function TjsHTMLStyleSheet.addRule(const bstrSelector: WideString; const bstrStyle: WideString; lIndex: Integer): Integer; safecall;
begin
  Result:=StrToInt(ExecMethod('addRule('+ToJSCode(bstrSelector)+','+ToJSCode(bstrStyle)+','+ToJSCode(lIndex)+')', true));
end;

function TjsHTMLStyleSheet.Get_cssText: WideString; safecall;
begin
  GetPropertyValue('cssText', Result)
end;

function TjsHTMLStyleSheet.Get_disabled: WordBool; safecall;
begin
  GetPropertyValue('disabled', Result)
end;

function TjsHTMLStyleSheet.Get_href: WideString; safecall;
begin
  GetPropertyValue('href', Result)
end;

function TjsHTMLStyleSheet.Get_id: WideString; safecall;
begin
  GetPropertyValue('id', Result)
end;

function TjsHTMLStyleSheet.Get_imports: IHTMLStyleSheetsCollection; safecall;
begin
  Result:=TjsHTMLStyleSheetsCollection.Create(FApplication, _JSVar+'.imports', TjsHTMLStyleSheet);
end;

function TjsHTMLStyleSheet.Get_media: WideString; safecall;
begin
  GetPropertyValue('media', Result)
end;

function TjsHTMLStyleSheet.Get_owningElement: IHTMLElement; safecall;
begin
  Result:=TjsHTMLElement.Create(FApplication, _JSVar+'.owningElement');
end;

function TjsHTMLStyleSheet.Get_pages: IHTMLStyleSheetPagesCollection; safecall;
begin
  Result:=TjsHTMLStyleSheetPagesCollection.Create(FApplication, _JSVar+'.pages', TjsHTMLStyleSheetPage);
end;

function TjsHTMLStyleSheet.Get_parentStyleSheet: IHTMLStyleSheet; safecall;
begin
  Result:=TjsHTMLStyleSheet.Create(FApplication, _JSVar+'.parentStyleSheet');
end;

function TjsHTMLStyleSheet.Get_readOnly: WordBool; safecall;
begin
  GetPropertyValue('readOnly', Result)
end;

function TjsHTMLStyleSheet.Get_rules: IHTMLStyleSheetRulesCollection; safecall;
begin
  Result:=TjsHTMLStyleSheetRulesCollection.Create(FApplication, _JSVar+'.rules', TjsHTMLStyleSheetRule);
end;

function TjsHTMLStyleSheet.Get_title: WideString; safecall;
begin
  GetPropertyValue('title', Result)
end;

function TjsHTMLStyleSheet.Get_type_: WideString; safecall;
begin
  GetPropertyValue('type', Result)
end;

procedure TjsHTMLStyleSheet.removeImport(lIndex: Integer); safecall;
begin
  ExecMethod('removeImport('+ToJSCode(lIndex)+')');
end;

procedure TjsHTMLStyleSheet.removeRule(lIndex: Integer); safecall;
begin
  ExecMethod('removeRule('+ToJSCode(lIndex)+')');
end;

procedure TjsHTMLStyleSheet.Set_cssText(const p: WideString); safecall;
begin
  SetPropertyValue('cssText', p);
end;

procedure TjsHTMLStyleSheet.Set_disabled(p: WordBool); safecall;
begin
  SetPropertyValue('disabled', p);
end;

procedure TjsHTMLStyleSheet.Set_href(const p: WideString); safecall;
begin
  SetPropertyValue('href', p);
end;

procedure TjsHTMLStyleSheet.Set_media(const p: WideString); safecall;
begin
  SetPropertyValue('media', p);
end;

procedure TjsHTMLStyleSheet.Set_title(const p: WideString); safecall;
begin
  SetPropertyValue('title', p);
end;

{ TjsHTMLStyleSheetsCollection }

function TjsHTMLStyleSheetsCollection.item(
  const pvarIndex: OleVariant): OleVariant;
begin
  Result:=Inherited item(pvarIndex);
end;

{ TjsHTMLStyleSheetPage }

function TjsHTMLStyleSheetPage.Get_pseudoClass: WideString; safecall;
begin
  GetPropertyValue('pseudoClass', Result)
end;

function TjsHTMLStyleSheetPage.Get_selector: WideString; safecall;
begin
  GetPropertyValue('selector', Result)
end;

{ TjsHTMLStyleSheetPagesCollection }

function TjsHTMLStyleSheetPagesCollection.item(
  index: Integer): IHTMLStyleSheetPage;
begin
  if not supports(inherited item(index), IHTMLStyleSheetPage, Result) then
    Result:=nil;
end;

{ TjsHTMLRuleStyle }

function TjsHTMLRuleStyle.Get_accelerator: WideString; safecall;
begin
  GetPropertyValue('accelerator', Result)
end;

function TjsHTMLRuleStyle.Get_background: WideString; safecall;
begin
  GetPropertyValue('background', Result)
end;

function TjsHTMLRuleStyle.Get_backgroundAttachment: WideString; safecall;
begin
  GetPropertyValue('backgroundAttachment', Result)
end;

function TjsHTMLRuleStyle.Get_backgroundColor: OleVariant; safecall;
begin
  GetPropertyValue('backgroundColor', Result)
end;

function TjsHTMLRuleStyle.Get_backgroundImage: WideString; safecall;
begin
  GetPropertyValue('backgroundImage', Result)
end;

function TjsHTMLRuleStyle.Get_backgroundPosition: WideString; safecall;
begin
  GetPropertyValue('backgroundPosition', Result)
end;

function TjsHTMLRuleStyle.Get_backgroundPositionX: OleVariant; safecall;
begin
  GetPropertyValue('backgroundPositionX', Result)
end;

function TjsHTMLRuleStyle.Get_backgroundPositionY: OleVariant; safecall;
begin
  GetPropertyValue('backgroundPositionY', Result)
end;

function TjsHTMLRuleStyle.Get_backgroundRepeat: WideString; safecall;
begin
  GetPropertyValue('backgroundRepeat', Result)
end;

function TjsHTMLRuleStyle.Get_behavior: WideString; safecall;
begin
  GetPropertyValue('behavior', Result)
end;

function TjsHTMLRuleStyle.Get_border: WideString; safecall;
begin
  GetPropertyValue('border', Result)
end;

function TjsHTMLRuleStyle.Get_borderBottom: WideString; safecall;
begin
  GetPropertyValue('borderBottom', Result)
end;

function TjsHTMLRuleStyle.Get_borderBottomColor: OleVariant; safecall;
begin
  GetPropertyValue('borderBottomColor', Result)
end;

function TjsHTMLRuleStyle.Get_borderBottomStyle: WideString; safecall;
begin
  GetPropertyValue('borderBottomStyle', Result)
end;

function TjsHTMLRuleStyle.Get_borderBottomWidth: OleVariant; safecall;
begin
  GetPropertyValue('borderBottomWidth', Result)
end;

function TjsHTMLRuleStyle.Get_borderCollapse: WideString; safecall;
begin
  GetPropertyValue('borderCollapse', Result)
end;

function TjsHTMLRuleStyle.Get_borderColor: WideString; safecall;
begin
  GetPropertyValue('borderColor', Result)
end;

function TjsHTMLRuleStyle.Get_borderLeft: WideString; safecall;
begin
  GetPropertyValue('borderLeft', Result)
end;

function TjsHTMLRuleStyle.Get_borderLeftColor: OleVariant; safecall;
begin
  GetPropertyValue('borderLeftColor', Result)
end;

function TjsHTMLRuleStyle.Get_borderLeftStyle: WideString; safecall;
begin
  GetPropertyValue('borderLeftStyle', Result)
end;

function TjsHTMLRuleStyle.Get_borderLeftWidth: OleVariant; safecall;
begin
  GetPropertyValue('borderLeftWidth', Result)
end;

function TjsHTMLRuleStyle.Get_borderRight: WideString; safecall;
begin
  GetPropertyValue('borderRight', Result)
end;

function TjsHTMLRuleStyle.Get_borderRightColor: OleVariant; safecall;
begin
  GetPropertyValue('borderRightColor', Result)
end;

function TjsHTMLRuleStyle.Get_borderRightStyle: WideString; safecall;
begin
  GetPropertyValue('borderRightStyle', Result)
end;

function TjsHTMLRuleStyle.Get_borderRightWidth: OleVariant; safecall;
begin
  GetPropertyValue('borderRightWidth', Result)
end;

function TjsHTMLRuleStyle.Get_borderStyle: WideString; safecall;
begin
  GetPropertyValue('borderStyle', Result)
end;

function TjsHTMLRuleStyle.Get_borderTop: WideString; safecall;
begin
  GetPropertyValue('borderTop', Result)
end;

function TjsHTMLRuleStyle.Get_borderTopColor: OleVariant; safecall;
begin
  GetPropertyValue('borderTopColor', Result)
end;

function TjsHTMLRuleStyle.Get_borderTopStyle: WideString; safecall;
begin
  GetPropertyValue('borderTopStyle', Result)
end;

function TjsHTMLRuleStyle.Get_borderTopWidth: OleVariant; safecall;
begin
  GetPropertyValue('borderTopWidth', Result)
end;

function TjsHTMLRuleStyle.Get_borderWidth: WideString; safecall;
begin
  GetPropertyValue('borderWidth', Result)
end;

function TjsHTMLRuleStyle.Get_bottom: OleVariant; safecall;
begin
  GetPropertyValue('bottom', Result)
end;

function TjsHTMLRuleStyle.Get_clear: WideString; safecall;
begin
  GetPropertyValue('clear', Result)
end;

function TjsHTMLRuleStyle.Get_clip: WideString; safecall;
begin
  GetPropertyValue('clip', Result)
end;

function TjsHTMLRuleStyle.Get_color: OleVariant; safecall;
begin
  GetPropertyValue('color', Result)
end;

function TjsHTMLRuleStyle.Get_cssText: WideString; safecall;
begin
  GetPropertyValue('cssText', Result)
end;

function TjsHTMLRuleStyle.Get_cursor: WideString; safecall;
begin
  GetPropertyValue('cursor', Result)
end;

function TjsHTMLRuleStyle.Get_direction: WideString; safecall;
begin
  GetPropertyValue('direction', Result)
end;

function TjsHTMLRuleStyle.Get_display: WideString; safecall;
begin
  GetPropertyValue('display', Result)
end;

function TjsHTMLRuleStyle.Get_filter: WideString; safecall;
begin
  GetPropertyValue('filter', Result)
end;

function TjsHTMLRuleStyle.Get_font: WideString; safecall;
begin
  GetPropertyValue('font', Result)
end;

function TjsHTMLRuleStyle.Get_fontFamily: WideString; safecall;
begin
  GetPropertyValue('fontFamily', Result)
end;

function TjsHTMLRuleStyle.Get_fontSize: OleVariant; safecall;
begin
  GetPropertyValue('fontSize', Result)
end;

function TjsHTMLRuleStyle.Get_fontStyle: WideString; safecall;
begin
  GetPropertyValue('fontStyle', Result)
end;

function TjsHTMLRuleStyle.Get_fontVariant: WideString; safecall;
begin
  GetPropertyValue('fontVariant', Result)
end;

function TjsHTMLRuleStyle.Get_fontWeight: WideString; safecall;
begin
  GetPropertyValue('fontWeight', Result)
end;

function TjsHTMLRuleStyle.Get_height: OleVariant; safecall;
begin
  GetPropertyValue('height', Result)
end;

function TjsHTMLRuleStyle.Get_imeMode: WideString; safecall;
begin
  GetPropertyValue('imeMode', Result)
end;

function TjsHTMLRuleStyle.Get_layoutFlow: WideString; safecall;
begin
  GetPropertyValue('layoutFlow', Result)
end;

function TjsHTMLRuleStyle.Get_layoutGrid: WideString; safecall;
begin
  GetPropertyValue('layoutGrid', Result)
end;

function TjsHTMLRuleStyle.Get_layoutGridChar: OleVariant; safecall;
begin
  GetPropertyValue('layoutGridChar', Result)
end;

function TjsHTMLRuleStyle.Get_layoutGridLine: OleVariant; safecall;
begin
  GetPropertyValue('layoutGridLine', Result)
end;

function TjsHTMLRuleStyle.Get_layoutGridMode: WideString; safecall;
begin
  GetPropertyValue('layoutGridMode', Result)
end;

function TjsHTMLRuleStyle.Get_layoutGridType: WideString; safecall;
begin
  GetPropertyValue('layoutGridType', Result)
end;

function TjsHTMLRuleStyle.Get_left: OleVariant; safecall;
begin
  GetPropertyValue('left', Result)
end;

function TjsHTMLRuleStyle.Get_letterSpacing: OleVariant; safecall;
begin
  GetPropertyValue('letterSpacing', Result)
end;

function TjsHTMLRuleStyle.Get_lineBreak: WideString; safecall;
begin
  GetPropertyValue('lineBreak', Result)
end;

function TjsHTMLRuleStyle.Get_lineHeight: OleVariant; safecall;
begin
  GetPropertyValue('lineHeight', Result)
end;

function TjsHTMLRuleStyle.Get_listStyle: WideString; safecall;
begin
  GetPropertyValue('listStyle', Result)
end;

function TjsHTMLRuleStyle.Get_listStyleImage: WideString; safecall;
begin
  GetPropertyValue('listStyleImage', Result)
end;

function TjsHTMLRuleStyle.Get_listStylePosition: WideString; safecall;
begin
  GetPropertyValue('listStylePosition', Result)
end;

function TjsHTMLRuleStyle.Get_listStyleType: WideString; safecall;
begin
  GetPropertyValue('listStyleType', Result)
end;

function TjsHTMLRuleStyle.Get_margin: WideString; safecall;
begin
  GetPropertyValue('margin', Result)
end;

function TjsHTMLRuleStyle.Get_marginBottom: OleVariant; safecall;
begin
  GetPropertyValue('marginBottom', Result)
end;

function TjsHTMLRuleStyle.Get_marginLeft: OleVariant; safecall;
begin
  GetPropertyValue('marginLeft', Result)
end;

function TjsHTMLRuleStyle.Get_marginRight: OleVariant; safecall;
begin
  GetPropertyValue('marginRight', Result)
end;

function TjsHTMLRuleStyle.Get_marginTop: OleVariant; safecall;
begin
  GetPropertyValue('marginTop', Result)
end;

function TjsHTMLRuleStyle.Get_minHeight: OleVariant; safecall;
begin
  GetPropertyValue('minHeight', Result)
end;

function TjsHTMLRuleStyle.Get_overflow: WideString; safecall;
begin
  GetPropertyValue('overflow', Result)
end;

function TjsHTMLRuleStyle.Get_overflowX: WideString; safecall;
begin
  GetPropertyValue('overflowX', Result)
end;

function TjsHTMLRuleStyle.Get_overflowY: WideString; safecall;
begin
  GetPropertyValue('overflowY', Result)
end;

function TjsHTMLRuleStyle.Get_padding: WideString; safecall;
begin
  GetPropertyValue('padding', Result)
end;

function TjsHTMLRuleStyle.Get_paddingBottom: OleVariant; safecall;
begin
  GetPropertyValue('paddingBottom', Result)
end;

function TjsHTMLRuleStyle.Get_paddingLeft: OleVariant; safecall;
begin
  GetPropertyValue('paddingLeft', Result)
end;

function TjsHTMLRuleStyle.Get_paddingRight: OleVariant; safecall;
begin
  GetPropertyValue('paddingRight', Result)
end;

function TjsHTMLRuleStyle.Get_paddingTop: OleVariant; safecall;
begin
  GetPropertyValue('paddingTop', Result)
end;

function TjsHTMLRuleStyle.Get_pageBreakAfter: WideString; safecall;
begin
  GetPropertyValue('pageBreakAfter', Result)
end;

function TjsHTMLRuleStyle.Get_pageBreakBefore: WideString; safecall;
begin
  GetPropertyValue('pageBreakBefore', Result)
end;

function TjsHTMLRuleStyle.Get_pixelBottom: Integer; safecall;
begin
  GetPropertyValue('pixelBottom', Result)
end;

function TjsHTMLRuleStyle.Get_pixelRight: Integer; safecall;
begin
  GetPropertyValue('pixelRight', Result)
end;

function TjsHTMLRuleStyle.Get_posBottom: Single; safecall;
begin
  GetPropertyValue('posBottom', Result)
end;

function TjsHTMLRuleStyle.Get_position: WideString; safecall;
begin
  GetPropertyValue('position', Result)
end;

function TjsHTMLRuleStyle.Get_posRight: Single; safecall;
begin
  GetPropertyValue('posRight', Result)
end;

function TjsHTMLRuleStyle.Get_right: OleVariant; safecall;
begin
  GetPropertyValue('right', Result)
end;

function TjsHTMLRuleStyle.Get_rubyAlign: WideString; safecall;
begin
  GetPropertyValue('rubyAlign', Result)
end;

function TjsHTMLRuleStyle.Get_rubyOverhang: WideString; safecall;
begin
  GetPropertyValue('rubyOverhang', Result)
end;

function TjsHTMLRuleStyle.Get_rubyPosition: WideString; safecall;
begin
  GetPropertyValue('rubyPosition', Result)
end;

function TjsHTMLRuleStyle.Get_scrollbar3dLightColor: OleVariant; safecall;
begin
  GetPropertyValue('scrollbar3dLightColor', Result)
end;

function TjsHTMLRuleStyle.Get_scrollbarArrowColor: OleVariant; safecall;
begin
  GetPropertyValue('scrollbarArrowColor', Result)
end;

function TjsHTMLRuleStyle.Get_scrollbarBaseColor: OleVariant; safecall;
begin
  GetPropertyValue('scrollbarBaseColor', Result)
end;

function TjsHTMLRuleStyle.Get_scrollbarDarkShadowColor: OleVariant; safecall;
begin
  GetPropertyValue('scrollbarDarkShadowColor', Result)
end;

function TjsHTMLRuleStyle.Get_scrollbarFaceColor: OleVariant; safecall;
begin
  GetPropertyValue('scrollbarFaceColor', Result)
end;

function TjsHTMLRuleStyle.Get_scrollbarHighlightColor: OleVariant; safecall;
begin
  GetPropertyValue('scrollbarHighlightColor', Result)
end;

function TjsHTMLRuleStyle.Get_scrollbarShadowColor: OleVariant; safecall;
begin
  GetPropertyValue('scrollbarShadowColor', Result)
end;

function TjsHTMLRuleStyle.Get_scrollbarTrackColor: OleVariant; safecall;
begin
  GetPropertyValue('scrollbarTrackColor', Result)
end;

function TjsHTMLRuleStyle.Get_styleFloat: WideString; safecall;
begin
  GetPropertyValue('styleFloat', Result)
end;

function TjsHTMLRuleStyle.Get_tableLayout: WideString; safecall;
begin
  GetPropertyValue('tableLayout', Result)
end;

function TjsHTMLRuleStyle.Get_textAlign: WideString; safecall;
begin
  GetPropertyValue('textAlign', Result)
end;

function TjsHTMLRuleStyle.Get_textAlignLast: WideString; safecall;
begin
  GetPropertyValue('textAlignLast', Result)
end;

function TjsHTMLRuleStyle.Get_textAutospace: WideString; safecall;
begin
  GetPropertyValue('textAutospace', Result)
end;

function TjsHTMLRuleStyle.Get_textDecoration: WideString; safecall;
begin
  GetPropertyValue('textDecoration', Result)
end;

function TjsHTMLRuleStyle.Get_textDecorationBlink: WordBool; safecall;
begin
  GetPropertyValue('textDecorationBlink', Result)
end;

function TjsHTMLRuleStyle.Get_textDecorationLineThrough: WordBool; safecall;
begin
  GetPropertyValue('textDecorationLineThrough', Result)
end;

function TjsHTMLRuleStyle.Get_textDecorationNone: WordBool; safecall;
begin
  GetPropertyValue('textDecorationNone', Result)
end;

function TjsHTMLRuleStyle.Get_textDecorationOverline: WordBool; safecall;
begin
  GetPropertyValue('textDecorationOverline', Result)
end;

function TjsHTMLRuleStyle.Get_textDecorationUnderline: WordBool; safecall;
begin
  GetPropertyValue('textDecorationUnderline', Result)
end;

function TjsHTMLRuleStyle.Get_textIndent: OleVariant; safecall;
begin
  GetPropertyValue('textIndent', Result)
end;

function TjsHTMLRuleStyle.Get_textJustify: WideString; safecall;
begin
  GetPropertyValue('textJustify', Result)
end;

function TjsHTMLRuleStyle.Get_textJustifyTrim: WideString; safecall;
begin
  GetPropertyValue('textJustifyTrim', Result)
end;

function TjsHTMLRuleStyle.Get_textKashida: OleVariant; safecall;
begin
  GetPropertyValue('textKashida', Result)
end;

function TjsHTMLRuleStyle.Get_textKashidaSpace: OleVariant; safecall;
begin
  GetPropertyValue('textKashidaSpace', Result)
end;

function TjsHTMLRuleStyle.Get_textOverflow: WideString; safecall;
begin
  GetPropertyValue('textOverflow', Result)
end;

function TjsHTMLRuleStyle.Get_textTransform: WideString; safecall;
begin
  GetPropertyValue('textTransform', Result)
end;

function TjsHTMLRuleStyle.Get_textUnderlinePosition: WideString; safecall;
begin
  GetPropertyValue('textUnderlinePosition', Result)
end;

function TjsHTMLRuleStyle.Get_top: OleVariant; safecall;
begin
  GetPropertyValue('top', Result)
end;

function TjsHTMLRuleStyle.Get_unicodeBidi: WideString; safecall;
begin
  GetPropertyValue('unicodeBidi', Result)
end;

function TjsHTMLRuleStyle.Get_verticalAlign: OleVariant; safecall;
begin
  GetPropertyValue('verticalAlign', Result)
end;

function TjsHTMLRuleStyle.Get_visibility: WideString; safecall;
begin
  GetPropertyValue('visibility', Result)
end;

function TjsHTMLRuleStyle.Get_whiteSpace: WideString; safecall;
begin
  GetPropertyValue('whiteSpace', Result)
end;

function TjsHTMLRuleStyle.Get_width: OleVariant; safecall;
begin
  GetPropertyValue('width', Result)
end;

function TjsHTMLRuleStyle.Get_wordBreak: WideString; safecall;
begin
  GetPropertyValue('wordBreak', Result)
end;

function TjsHTMLRuleStyle.Get_wordSpacing: OleVariant; safecall;
begin
  GetPropertyValue('wordSpacing', Result)
end;

function TjsHTMLRuleStyle.Get_wordWrap: WideString; safecall;
begin
  GetPropertyValue('wordWrap', Result)
end;

function TjsHTMLRuleStyle.Get_writingMode: WideString; safecall;
begin
  GetPropertyValue('writingMode', Result)
end;

function TjsHTMLRuleStyle.Get_zIndex: OleVariant; safecall;
begin
  GetPropertyValue('zIndex', Result)
end;

function TjsHTMLRuleStyle.Get_zoom: OleVariant; safecall;
begin
  GetPropertyValue('zoom', Result)
end;

function TjsHTMLRuleStyle.getAttribute(const strAttributeName: WideString; lFlags: Integer): OleVariant; safecall;
begin
  Result:=ExecMethod('getAttribute('+ToJSCode(strAttributeName)+','+ToJSCode(lFlags)+')', true);
end;

function TjsHTMLRuleStyle.removeAttribute(const strAttributeName: WideString; lFlags: Integer): WordBool; safecall;
begin
  Result:=IsPositiveMethodResult(ExecMethod('removeAttribute('+ToJSCode(strAttributeName)+','+ToJSCode(lFlags)+')', True));
end;

procedure TjsHTMLRuleStyle.Set_accelerator(const p: WideString); safecall;
begin
  SetPropertyValue('accelerator', p);
end;

procedure TjsHTMLRuleStyle.Set_background(const p: WideString); safecall;
begin
  SetPropertyValue('background', p);
end;

procedure TjsHTMLRuleStyle.Set_backgroundAttachment(const p: WideString); safecall;
begin
  SetPropertyValue('backgroundAttachment', p);
end;

procedure TjsHTMLRuleStyle.Set_backgroundColor(p: OleVariant); safecall;
begin
  SetPropertyValue('backgroundColor', p);
end;

procedure TjsHTMLRuleStyle.Set_backgroundImage(const p: WideString); safecall;
begin
  SetPropertyValue('backgroundImage', p);
end;

procedure TjsHTMLRuleStyle.Set_backgroundPosition(const p: WideString); safecall;
begin
  SetPropertyValue('backgroundPosition', p);
end;

procedure TjsHTMLRuleStyle.Set_backgroundPositionX(p: OleVariant); safecall;
begin
  SetPropertyValue('backgroundPositionX', p);
end;

procedure TjsHTMLRuleStyle.Set_backgroundPositionY(p: OleVariant); safecall;
begin
  SetPropertyValue('backgroundPositionY', p);
end;

procedure TjsHTMLRuleStyle.Set_backgroundRepeat(const p: WideString); safecall;
begin
  SetPropertyValue('backgroundRepeat', p);
end;

procedure TjsHTMLRuleStyle.Set_behavior(const p: WideString); safecall;
begin
  SetPropertyValue('behavior', p);
end;

procedure TjsHTMLRuleStyle.Set_border(const p: WideString); safecall;
begin
  SetPropertyValue('border', p);
end;

procedure TjsHTMLRuleStyle.Set_borderBottom(const p: WideString); safecall;
begin
  SetPropertyValue('borderBottom', p);
end;

procedure TjsHTMLRuleStyle.Set_borderBottomColor(p: OleVariant); safecall;
begin
  SetPropertyValue('borderBottomColor', p);
end;

procedure TjsHTMLRuleStyle.Set_borderBottomStyle(const p: WideString); safecall;
begin
  SetPropertyValue('borderBottomStyle', p);
end;

procedure TjsHTMLRuleStyle.Set_borderBottomWidth(p: OleVariant); safecall;
begin
  SetPropertyValue('borderBottomWidth', p);
end;

procedure TjsHTMLRuleStyle.Set_borderCollapse(const p: WideString); safecall;
begin
  SetPropertyValue('borderCollapse', p);
end;

procedure TjsHTMLRuleStyle.Set_borderColor(const p: WideString); safecall;
begin
  SetPropertyValue('borderColor', p);
end;

procedure TjsHTMLRuleStyle.Set_borderLeft(const p: WideString); safecall;
begin
  SetPropertyValue('borderLeft', p);
end;

procedure TjsHTMLRuleStyle.Set_borderLeftColor(p: OleVariant); safecall;
begin
  SetPropertyValue('borderLeftColor', p);
end;

procedure TjsHTMLRuleStyle.Set_borderLeftStyle(const p: WideString); safecall;
begin
  SetPropertyValue('borderLeftStyle', p);
end;

procedure TjsHTMLRuleStyle.Set_borderLeftWidth(p: OleVariant); safecall;
begin
  SetPropertyValue('borderLeftWidth', p);
end;

procedure TjsHTMLRuleStyle.Set_borderRight(const p: WideString); safecall;
begin
  SetPropertyValue('borderRight', p);
end;

procedure TjsHTMLRuleStyle.Set_borderRightColor(p: OleVariant); safecall;
begin
  SetPropertyValue('borderRightColor', p);
end;

procedure TjsHTMLRuleStyle.Set_borderRightStyle(const p: WideString); safecall;
begin
  SetPropertyValue('borderRightStyle', p);
end;

procedure TjsHTMLRuleStyle.Set_borderRightWidth(p: OleVariant); safecall;
begin
  SetPropertyValue('borderRightWidth', p);
end;

procedure TjsHTMLRuleStyle.Set_borderStyle(const p: WideString); safecall;
begin
  SetPropertyValue('borderStyle', p);
end;

procedure TjsHTMLRuleStyle.Set_borderTop(const p: WideString); safecall;
begin
  SetPropertyValue('borderTop', p);
end;

procedure TjsHTMLRuleStyle.Set_borderTopColor(p: OleVariant); safecall;
begin
  SetPropertyValue('borderTopColor', p);
end;

procedure TjsHTMLRuleStyle.Set_borderTopStyle(const p: WideString); safecall;
begin
  SetPropertyValue('borderTopStyle', p);
end;

procedure TjsHTMLRuleStyle.Set_borderTopWidth(p: OleVariant); safecall;
begin
  SetPropertyValue('borderTopWidth', p);
end;

procedure TjsHTMLRuleStyle.Set_borderWidth(const p: WideString); safecall;
begin
  SetPropertyValue('borderWidth', p);
end;

procedure TjsHTMLRuleStyle.Set_bottom(p: OleVariant); safecall;
begin
  SetPropertyValue('bottom', p);
end;

procedure TjsHTMLRuleStyle.Set_clear(const p: WideString); safecall;
begin
  SetPropertyValue('clear', p);
end;

procedure TjsHTMLRuleStyle.Set_clip(const p: WideString); safecall;
begin
  SetPropertyValue('clip', p);
end;

procedure TjsHTMLRuleStyle.Set_color(p: OleVariant); safecall;
begin
  SetPropertyValue('color', p);
end;

procedure TjsHTMLRuleStyle.Set_cssText(const p: WideString); safecall;
begin
  SetPropertyValue('cssText', p);
end;

procedure TjsHTMLRuleStyle.Set_cursor(const p: WideString); safecall;
begin
  SetPropertyValue('cursor', p);
end;

procedure TjsHTMLRuleStyle.Set_direction(const p: WideString); safecall;
begin
  SetPropertyValue('direction', p);
end;

procedure TjsHTMLRuleStyle.Set_display(const p: WideString); safecall;
begin
  SetPropertyValue('display', p);
end;

procedure TjsHTMLRuleStyle.Set_filter(const p: WideString); safecall;
begin
  SetPropertyValue('filter', p);
end;

procedure TjsHTMLRuleStyle.Set_font(const p: WideString); safecall;
begin
  SetPropertyValue('font', p);
end;

procedure TjsHTMLRuleStyle.Set_fontFamily(const p: WideString); safecall;
begin
  SetPropertyValue('fontFamily', p);
end;

procedure TjsHTMLRuleStyle.Set_fontSize(p: OleVariant); safecall;
begin
  SetPropertyValue('fontSize', p);
end;

procedure TjsHTMLRuleStyle.Set_fontStyle(const p: WideString); safecall;
begin
  SetPropertyValue('fontStyle', p);
end;

procedure TjsHTMLRuleStyle.Set_fontVariant(const p: WideString); safecall;
begin
  SetPropertyValue('fontVariant', p);
end;

procedure TjsHTMLRuleStyle.Set_fontWeight(const p: WideString); safecall;
begin
  SetPropertyValue('fontWeight', p);
end;

procedure TjsHTMLRuleStyle.Set_height(p: OleVariant); safecall;
begin
  SetPropertyValue('height', p);
end;

procedure TjsHTMLRuleStyle.Set_imeMode(const p: WideString); safecall;
begin
  SetPropertyValue('imeMode', p);
end;

procedure TjsHTMLRuleStyle.Set_layoutFlow(const p: WideString); safecall;
begin
  SetPropertyValue('layoutFlow', p);
end;

procedure TjsHTMLRuleStyle.Set_layoutGrid(const p: WideString); safecall;
begin
  SetPropertyValue('layoutGrid', p);
end;

procedure TjsHTMLRuleStyle.Set_layoutGridChar(p: OleVariant); safecall;
begin
  SetPropertyValue('layoutGridChar', p);
end;

procedure TjsHTMLRuleStyle.Set_layoutGridLine(p: OleVariant); safecall;
begin
  SetPropertyValue('layoutGridLine', p);
end;

procedure TjsHTMLRuleStyle.Set_layoutGridMode(const p: WideString); safecall;
begin
  SetPropertyValue('layoutGridMode', p);
end;

procedure TjsHTMLRuleStyle.Set_layoutGridType(const p: WideString); safecall;
begin
  SetPropertyValue('layoutGridType', p);
end;

procedure TjsHTMLRuleStyle.Set_left(p: OleVariant); safecall;
begin
  SetPropertyValue('left', p);
end;

procedure TjsHTMLRuleStyle.Set_letterSpacing(p: OleVariant); safecall;
begin
  SetPropertyValue('letterSpacing', p);
end;

procedure TjsHTMLRuleStyle.Set_lineBreak(const p: WideString); safecall;
begin
  SetPropertyValue('lineBreak', p);
end;

procedure TjsHTMLRuleStyle.Set_lineHeight(p: OleVariant); safecall;
begin
  SetPropertyValue('lineHeight', p);
end;

procedure TjsHTMLRuleStyle.Set_listStyle(const p: WideString); safecall;
begin
  SetPropertyValue('listStyle', p);
end;

procedure TjsHTMLRuleStyle.Set_listStyleImage(const p: WideString); safecall;
begin
  SetPropertyValue('listStyleImage', p);
end;

procedure TjsHTMLRuleStyle.Set_listStylePosition(const p: WideString); safecall;
begin
  SetPropertyValue('listStylePosition', p);
end;

procedure TjsHTMLRuleStyle.Set_listStyleType(const p: WideString); safecall;
begin
  SetPropertyValue('listStyleType', p);
end;

procedure TjsHTMLRuleStyle.Set_margin(const p: WideString); safecall;
begin
  SetPropertyValue('margin', p);
end;

procedure TjsHTMLRuleStyle.Set_marginBottom(p: OleVariant); safecall;
begin
  SetPropertyValue('marginBottom', p);
end;

procedure TjsHTMLRuleStyle.Set_marginLeft(p: OleVariant); safecall;
begin
  SetPropertyValue('marginLeft', p);
end;

procedure TjsHTMLRuleStyle.Set_marginRight(p: OleVariant); safecall;
begin
  SetPropertyValue('marginRight', p);
end;

procedure TjsHTMLRuleStyle.Set_marginTop(p: OleVariant); safecall;
begin
  SetPropertyValue('marginTop', p);
end;

procedure TjsHTMLRuleStyle.Set_minHeight(p: OleVariant); safecall;
begin
  SetPropertyValue('minHeight', p);
end;

procedure TjsHTMLRuleStyle.Set_overflow(const p: WideString); safecall;
begin
  SetPropertyValue('overflow', p);
end;

procedure TjsHTMLRuleStyle.Set_overflowX(const p: WideString); safecall;
begin
  SetPropertyValue('overflowX', p);
end;

procedure TjsHTMLRuleStyle.Set_overflowY(const p: WideString); safecall;
begin
  SetPropertyValue('overflowY', p);
end;

procedure TjsHTMLRuleStyle.Set_padding(const p: WideString); safecall;
begin
  SetPropertyValue('padding', p);
end;

procedure TjsHTMLRuleStyle.Set_paddingBottom(p: OleVariant); safecall;
begin
  SetPropertyValue('paddingBottom', p);
end;

procedure TjsHTMLRuleStyle.Set_paddingLeft(p: OleVariant); safecall;
begin
  SetPropertyValue('paddingLeft', p);
end;

procedure TjsHTMLRuleStyle.Set_paddingRight(p: OleVariant); safecall;
begin
  SetPropertyValue('paddingRight', p);
end;

procedure TjsHTMLRuleStyle.Set_paddingTop(p: OleVariant); safecall;
begin
  SetPropertyValue('paddingTop', p);
end;

procedure TjsHTMLRuleStyle.Set_pageBreakAfter(const p: WideString); safecall;
begin
  SetPropertyValue('pageBreakAfter', p);
end;

procedure TjsHTMLRuleStyle.Set_pageBreakBefore(const p: WideString); safecall;
begin
  SetPropertyValue('pageBreakBefore', p);
end;

procedure TjsHTMLRuleStyle.Set_pixelBottom(p: Integer); safecall;
begin
  SetPropertyValue('pixelBottom', p);
end;

procedure TjsHTMLRuleStyle.Set_pixelRight(p: Integer); safecall;
begin
  SetPropertyValue('pixelRight', p);
end;

procedure TjsHTMLRuleStyle.Set_posBottom(p: Single); safecall;
begin
  SetPropertyValue('posBottom', p);
end;

procedure TjsHTMLRuleStyle.Set_position(const p: WideString); safecall;
begin
  SetPropertyValue('position', p);
end;

procedure TjsHTMLRuleStyle.Set_posRight(p: Single); safecall;
begin
  SetPropertyValue('posRight', p);
end;

procedure TjsHTMLRuleStyle.Set_right(p: OleVariant); safecall;
begin
  SetPropertyValue('right', p);
end;

procedure TjsHTMLRuleStyle.Set_rubyAlign(const p: WideString); safecall;
begin
  SetPropertyValue('rubyAlign', p);
end;

procedure TjsHTMLRuleStyle.Set_rubyOverhang(const p: WideString); safecall;
begin
  SetPropertyValue('rubyOverhang', p);
end;

procedure TjsHTMLRuleStyle.Set_rubyPosition(const p: WideString); safecall;
begin
  SetPropertyValue('rubyPosition', p);
end;

procedure TjsHTMLRuleStyle.Set_scrollbar3dLightColor(p: OleVariant); safecall;
begin
  SetPropertyValue('scrollbar3dLightColor', p);
end;

procedure TjsHTMLRuleStyle.Set_scrollbarArrowColor(p: OleVariant); safecall;
begin
  SetPropertyValue('scrollbarArrowColor', p);
end;

procedure TjsHTMLRuleStyle.Set_scrollbarBaseColor(p: OleVariant); safecall;
begin
  SetPropertyValue('scrollbarBaseColor', p);
end;

procedure TjsHTMLRuleStyle.Set_scrollbarDarkShadowColor(p: OleVariant); safecall;
begin
  SetPropertyValue('scrollbarDarkShadowColor', p);
end;

procedure TjsHTMLRuleStyle.Set_scrollbarFaceColor(p: OleVariant); safecall;
begin
  SetPropertyValue('scrollbarFaceColor', p);
end;

procedure TjsHTMLRuleStyle.Set_scrollbarHighlightColor(p: OleVariant); safecall;
begin
  SetPropertyValue('scrollbarHighlightColor', p);
end;

procedure TjsHTMLRuleStyle.Set_scrollbarShadowColor(p: OleVariant); safecall;
begin
  SetPropertyValue('scrollbarShadowColor', p);
end;

procedure TjsHTMLRuleStyle.Set_scrollbarTrackColor(p: OleVariant); safecall;
begin
  SetPropertyValue('scrollbarTrackColor', p);
end;

procedure TjsHTMLRuleStyle.Set_styleFloat(const p: WideString); safecall;
begin
  SetPropertyValue('styleFloat', p);
end;

procedure TjsHTMLRuleStyle.Set_tableLayout(const p: WideString); safecall;
begin
  SetPropertyValue('tableLayout', p);
end;

procedure TjsHTMLRuleStyle.Set_textAlign(const p: WideString); safecall;
begin
  SetPropertyValue('textAlign', p);
end;

procedure TjsHTMLRuleStyle.Set_textAlignLast(const p: WideString); safecall;
begin
  SetPropertyValue('textAlignLast', p);
end;

procedure TjsHTMLRuleStyle.Set_textAutospace(const p: WideString); safecall;
begin
  SetPropertyValue('textAutospace', p);
end;

procedure TjsHTMLRuleStyle.Set_textDecoration(const p: WideString); safecall;
begin
  SetPropertyValue('textDecoration', p);
end;

procedure TjsHTMLRuleStyle.Set_textDecorationBlink(p: WordBool); safecall;
begin
  SetPropertyValue('textDecorationBlink', p);
end;

procedure TjsHTMLRuleStyle.Set_textDecorationLineThrough(p: WordBool); safecall;
begin
  SetPropertyValue('textDecorationLineThrough', p);
end;

procedure TjsHTMLRuleStyle.Set_textDecorationNone(p: WordBool); safecall;
begin
  SetPropertyValue('textDecorationNone', p);
end;

procedure TjsHTMLRuleStyle.Set_textDecorationOverline(p: WordBool); safecall;
begin
  SetPropertyValue('textDecorationOverline', p);
end;

procedure TjsHTMLRuleStyle.Set_textDecorationUnderline(p: WordBool); safecall;
begin
  SetPropertyValue('textDecorationUnderline', p);
end;

procedure TjsHTMLRuleStyle.Set_textIndent(p: OleVariant); safecall;
begin
  SetPropertyValue('textIndent', p);
end;

procedure TjsHTMLRuleStyle.Set_textJustify(const p: WideString); safecall;
begin
  SetPropertyValue('textJustify', p);
end;

procedure TjsHTMLRuleStyle.Set_textJustifyTrim(const p: WideString); safecall;
begin
  SetPropertyValue('textJustifyTrim', p);
end;

procedure TjsHTMLRuleStyle.Set_textKashida(p: OleVariant); safecall;
begin
  SetPropertyValue('textKashida', p);
end;

procedure TjsHTMLRuleStyle.Set_textKashidaSpace(p: OleVariant); safecall;
begin
  SetPropertyValue('textKashidaSpace', p);
end;

procedure TjsHTMLRuleStyle.Set_textOverflow(const p: WideString); safecall;
begin
  SetPropertyValue('textOverflow', p);
end;

procedure TjsHTMLRuleStyle.Set_textTransform(const p: WideString); safecall;
begin
  SetPropertyValue('textTransform', p);
end;

procedure TjsHTMLRuleStyle.Set_textUnderlinePosition(const p: WideString); safecall;
begin
  SetPropertyValue('textUnderlinePosition', p);
end;

procedure TjsHTMLRuleStyle.Set_top(p: OleVariant); safecall;
begin
  SetPropertyValue('top', p);
end;

procedure TjsHTMLRuleStyle.Set_unicodeBidi(const p: WideString); safecall;
begin
  SetPropertyValue('unicodeBidi', p);
end;

procedure TjsHTMLRuleStyle.Set_verticalAlign(p: OleVariant); safecall;
begin
  SetPropertyValue('verticalAlign', p);
end;

procedure TjsHTMLRuleStyle.Set_visibility(const p: WideString); safecall;
begin
  SetPropertyValue('visibility', p);
end;

procedure TjsHTMLRuleStyle.Set_whiteSpace(const p: WideString); safecall;
begin
  SetPropertyValue('whiteSpace', p);
end;

procedure TjsHTMLRuleStyle.Set_width(p: OleVariant); safecall;
begin
  SetPropertyValue('width', p);
end;

procedure TjsHTMLRuleStyle.Set_wordBreak(const p: WideString); safecall;
begin
  SetPropertyValue('wordBreak', p);
end;

procedure TjsHTMLRuleStyle.Set_wordSpacing(p: OleVariant); safecall;
begin
  SetPropertyValue('wordSpacing', p);
end;

procedure TjsHTMLRuleStyle.Set_wordWrap(const p: WideString); safecall;
begin
  SetPropertyValue('wordWrap', p);
end;

procedure TjsHTMLRuleStyle.Set_writingMode(const p: WideString); safecall;
begin
  SetPropertyValue('writingMode', p);
end;

procedure TjsHTMLRuleStyle.Set_zIndex(p: OleVariant); safecall;
begin
  SetPropertyValue('zIndex', p);
end;

procedure TjsHTMLRuleStyle.Set_zoom(p: OleVariant); safecall;
begin
  SetPropertyValue('zoom', p);
end;

procedure TjsHTMLRuleStyle.setAttribute(const strAttributeName: WideString; AttributeValue: OleVariant;  lFlags: Integer); safecall;
begin
  ExecMethod('setAttribute('+ToJSCode(strAttributeName)+','+ToJSCode(AttributeValue)+','+ToJSCode(lFlags)+')');
end;

{ TjsHTMLStyleSheetRule }

function TjsHTMLStyleSheetRule.Get_readOnly: WordBool; safecall;
begin
  GetPropertyValue('readOnly', Result)
end;

function TjsHTMLStyleSheetRule.Get_selectorText: WideString; safecall;
begin
  GetPropertyValue('selectorText', Result)
end;

function TjsHTMLStyleSheetRule.Get_style: IHTMLRuleStyle; safecall;
begin
  Result:=TjsHTMLRuleStyle.Create(FApplication, _JSVar+'.style');
end;

procedure TjsHTMLStyleSheetRule.Set_selectorText(const p: WideString); safecall;
begin
  SetPropertyValue('selectorText', p);
end;

{ TjsHTMLStyleSheetRulesCollection }

function TjsHTMLStyleSheetRulesCollection.item(
  index: Integer): IHTMLStyleSheetRule;
begin
  if not Supports(inherited item(index), IHTMLStyleSheetRule, Result) then
    Result:=nil;
end;

{ TjsOmHistory }

function TjsOmHistory.Get_length: Smallint; safecall;
begin
  GetPropertyValue('length', Result)
end;

procedure TjsOmHistory.back(var pvargdistance: OleVariant); safecall;
begin
  ExecMethod('back('+ToJSCode(pvargdistance)+')');
end;

procedure TjsOmHistory.forward(var pvargdistance: OleVariant); safecall;
begin
  ExecMethod('forward('+ToJSCode(pvargdistance)+')');
end;

procedure TjsOmHistory.go(var pvargdistance: OleVariant); safecall;
begin
  ExecMethod('go('+ToJSCode(pvargdistance)+')');
end;

{ TjsHTMLImageElementFactory }

function TjsHTMLImageElementFactory.create(width,
  height: OleVariant): IHTMLImgElement;
begin
  Result:=TjsHTMLImgElement.Create(FApplication, 'new '+_JSVar+'('+ToJSCode(width)+','+ToJSCode(height)+')');
end;

{ TjsHTMLImgElement }

function TjsHTMLImgElement.Get_align: WideString; safecall;
begin
  GetPropertyValue('align', Result)
end;

function TjsHTMLImgElement.Get_alt: WideString; safecall;
begin
  GetPropertyValue('alt', Result)
end;

function TjsHTMLImgElement.Get_border: OleVariant; safecall;
begin
  GetPropertyValue('border', Result)
end;

function TjsHTMLImgElement.Get_complete: WordBool; safecall;
begin
  GetPropertyValue('complete', Result)
end;

function TjsHTMLImgElement.Get_dynsrc: WideString; safecall;
begin
  GetPropertyValue('dynsrc', Result)
end;

function TjsHTMLImgElement.Get_fileCreatedDate: WideString; safecall;
begin
  GetPropertyValue('fileCreatedDate', Result)
end;

function TjsHTMLImgElement.Get_fileModifiedDate: WideString; safecall;
begin
  GetPropertyValue('fileModifiedDate', Result)
end;

function TjsHTMLImgElement.Get_fileSize: WideString; safecall;
begin
  GetPropertyValue('fileSize', Result)
end;

function TjsHTMLImgElement.Get_fileUpdatedDate: WideString; safecall;
begin
  GetPropertyValue('fileUpdatedDate', Result)
end;

function TjsHTMLImgElement.Get_height: Integer; safecall;
begin
  GetPropertyValue('height', Result)
end;

function TjsHTMLImgElement.Get_href: WideString; safecall;
begin
  GetPropertyValue('href', Result)
end;

function TjsHTMLImgElement.Get_hspace: Integer; safecall;
begin
  GetPropertyValue('hspace', Result)
end;

function TjsHTMLImgElement.Get_isMap: WordBool; safecall;
begin
  GetPropertyValue('isMap', Result)
end;

function TjsHTMLImgElement.Get_longDesc: WideString; safecall;
begin
  GetPropertyValue('longDesc', Result)
end;

function TjsHTMLImgElement.Get_loop: OleVariant; safecall;
begin
  GetPropertyValue('loop', Result)
end;

function TjsHTMLImgElement.Get_lowsrc: WideString; safecall;
begin
  GetPropertyValue('lowsrc', Result)
end;

function TjsHTMLImgElement.Get_mimeType: WideString; safecall;
begin
  GetPropertyValue('mimeType', Result)
end;

function TjsHTMLImgElement.Get_name: WideString; safecall;
begin
  GetPropertyValue('name', Result)
end;

function TjsHTMLImgElement.Get_nameProp: WideString; safecall;
begin
  GetPropertyValue('nameProp', Result)
end;

function TjsHTMLImgElement.Get_onabort: OleVariant; safecall;
begin
  GetPropertyValue('onabort', Result)
end;

function TjsHTMLImgElement.Get_onerror: OleVariant; safecall;
begin
  GetPropertyValue('onerror', Result)
end;

function TjsHTMLImgElement.Get_onload: OleVariant; safecall;
begin
  GetPropertyValue('onload', Result)
end;

function TjsHTMLImgElement.Get_protocol: WideString; safecall;
begin
  GetPropertyValue('protocol', Result)
end;

function TjsHTMLImgElement.Get_readyState: WideString; safecall;
begin
  GetPropertyValue('readyState', Result)
end;

function TjsHTMLImgElement.Get_src: WideString; safecall;
begin
  GetPropertyValue('src', Result)
end;

function TjsHTMLImgElement.Get_Start: WideString; safecall;
begin
  GetPropertyValue('Start', Result)
end;

function TjsHTMLImgElement.Get_useMap: WideString; safecall;
begin
  GetPropertyValue('useMap', Result)
end;

function TjsHTMLImgElement.Get_vrml: WideString; safecall;
begin
  GetPropertyValue('vrml', Result)
end;

function TjsHTMLImgElement.Get_vspace: Integer; safecall;
begin
  GetPropertyValue('vspace', Result)
end;

function TjsHTMLImgElement.Get_width: Integer; safecall;
begin
  GetPropertyValue('width', Result)
end;

procedure TjsHTMLImgElement.Set_align(const p: WideString); safecall;
begin
  SetPropertyValue('align', p);
end;

procedure TjsHTMLImgElement.Set_alt(const p: WideString); safecall;
begin
  SetPropertyValue('alt', p);
end;

procedure TjsHTMLImgElement.Set_border(p: OleVariant); safecall;
begin
  SetPropertyValue('border', p);
end;

procedure TjsHTMLImgElement.Set_dynsrc(const p: WideString); safecall;
begin
  SetPropertyValue('dynsrc', p);
end;

procedure TjsHTMLImgElement.Set_height(p: Integer); safecall;
begin
  SetPropertyValue('height', p);
end;

procedure TjsHTMLImgElement.Set_hspace(p: Integer); safecall;
begin
  SetPropertyValue('hspace', p);
end;

procedure TjsHTMLImgElement.Set_isMap(p: WordBool); safecall;
begin
  SetPropertyValue('isMap', p);
end;

procedure TjsHTMLImgElement.Set_longDesc(const p: WideString); safecall;
begin
  SetPropertyValue('longDesc', p);
end;

procedure TjsHTMLImgElement.Set_loop(p: OleVariant); safecall;
begin
  SetPropertyValue('loop', p);
end;

procedure TjsHTMLImgElement.Set_lowsrc(const p: WideString); safecall;
begin
  SetPropertyValue('lowsrc', p);
end;

procedure TjsHTMLImgElement.Set_name(const p: WideString); safecall;
begin
  SetPropertyValue('name', p);
end;

procedure TjsHTMLImgElement.Set_onabort(p: OleVariant); safecall;
begin
  SetPropertyValue('onabort', p);
end;

procedure TjsHTMLImgElement.Set_onerror(p: OleVariant); safecall;
begin
  SetPropertyValue('onerror', p);
end;

procedure TjsHTMLImgElement.Set_onload(p: OleVariant); safecall;
begin
  SetPropertyValue('onload', p);
end;

procedure TjsHTMLImgElement.Set_src(const p: WideString); safecall;
begin
  SetPropertyValue('src', p);
end;

procedure TjsHTMLImgElement.Set_Start(const p: WideString); safecall;
begin
  SetPropertyValue('Start', p);
end;

procedure TjsHTMLImgElement.Set_useMap(const p: WideString); safecall;
begin
  SetPropertyValue('useMap', p);
end;

procedure TjsHTMLImgElement.Set_vrml(const p: WideString); safecall;
begin
  SetPropertyValue('vrml', p);
end;

procedure TjsHTMLImgElement.Set_vspace(p: Integer); safecall;
begin
  SetPropertyValue('vspace', p);
end;

procedure TjsHTMLImgElement.Set_width(p: Integer); safecall;
begin
  SetPropertyValue('width', p);
end;

{ TjsHTMLLocation }

function TjsHTMLLocation.Get_hash: WideString; safecall;
begin
  GetPropertyValue('hash', Result)
end;

function TjsHTMLLocation.Get_host: WideString; safecall;
begin
  GetPropertyValue('host', Result)
end;

function TjsHTMLLocation.Get_hostname: WideString; safecall;
begin
  GetPropertyValue('hostname', Result)
end;

function TjsHTMLLocation.Get_href: WideString; safecall;
begin
  GetPropertyValue('href', Result)
end;

function TjsHTMLLocation.Get_pathname: WideString; safecall;
begin
  GetPropertyValue('pathname', Result)
end;

function TjsHTMLLocation.Get_port: WideString; safecall;
begin
  GetPropertyValue('port', Result)
end;

function TjsHTMLLocation.Get_protocol: WideString; safecall;
begin
  GetPropertyValue('protocol', Result)
end;

function TjsHTMLLocation.Get_search: WideString; safecall;
begin
  GetPropertyValue('search', Result)
end;

function TjsHTMLLocation.toString: WideString; safecall;
begin
  Result:=ExecMethod('toString('+')', True);
end;

procedure TjsHTMLLocation.assign(const bstr: WideString); safecall;
begin
  ExecMethod('assign('+ToJSCode(bstr)+')');
end;

procedure TjsHTMLLocation.reload(flag: WordBool); safecall;
begin
  ExecMethod('reload('+ToJSCode(flag)+')');
end;

procedure TjsHTMLLocation.replace(const bstr: WideString); safecall;
begin
  ExecMethod('replace('+ToJSCode(bstr)+')');
end;

procedure TjsHTMLLocation.Set_hash(const p: WideString); safecall;
begin
  SetPropertyValue('hash', p);
end;

procedure TjsHTMLLocation.Set_host(const p: WideString); safecall;
begin
  SetPropertyValue('host', p);
end;

procedure TjsHTMLLocation.Set_hostname(const p: WideString); safecall;
begin
  SetPropertyValue('hostname', p);
end;

procedure TjsHTMLLocation.Set_href(const p: WideString); safecall;
begin
  SetPropertyValue('href', p);
end;

procedure TjsHTMLLocation.Set_pathname(const p: WideString); safecall;
begin
  SetPropertyValue('pathname', p);
end;

procedure TjsHTMLLocation.Set_port(const p: WideString); safecall;
begin
  SetPropertyValue('port', p);
end;

procedure TjsHTMLLocation.Set_protocol(const p: WideString); safecall;
begin
  SetPropertyValue('protocol', p);
end;

procedure TjsHTMLLocation.Set_search(const p: WideString); safecall;
begin
  SetPropertyValue('search', p);
end;

{ TjsHTMLOptionElementFactory }

function TjsHTMLOptionElementFactory.create(text, value, defaultSelected,
  selected: OleVariant): IHTMLOptionElement;
begin
  Result:=TjsHTMLOptionElement.Create(FApplication, 'new '+_JSVar+'('+ToJSCode(text)+','+ToJSCode(value)+','+ToJSCode(defaultSelected)+','+ToJSCode(selected)+')');
end;

{ TjsHTMLOptionElement }

function TjsHTMLOptionElement.Get_defaultSelected: WordBool; safecall;
begin
  GetPropertyValue('defaultSelected', Result)
end;

function TjsHTMLOptionElement.Get_form: IHTMLFormElement; safecall;
begin
  result:=TjsHTMLFormElement.Create(FApplication, _JSVar+'.form');
end;

function TjsHTMLOptionElement.Get_index: Integer; safecall;
begin
  GetPropertyValue('index', Result)
end;

function TjsHTMLOptionElement.Get_label_: WideString; safecall;
begin
  GetPropertyValue('Prop', Result)
end;

function TjsHTMLOptionElement.Get_selected: WordBool; safecall;
begin
  GetPropertyValue('selected', Result)
end;

function TjsHTMLOptionElement.Get_text: WideString; safecall;
begin
  GetPropertyValue('text', Result)
end;

function TjsHTMLOptionElement.Get_value: WideString; safecall;
begin
  GetPropertyValue('value', Result)
end;

procedure TjsHTMLOptionElement.Set_defaultSelected(p: WordBool); safecall;
begin
  SetPropertyValue('defaultSelected', p);
end;

procedure TjsHTMLOptionElement.Set_index(p: Integer); safecall;
begin
  SetPropertyValue('index', p);
end;

procedure TjsHTMLOptionElement.Set_label_(const p: WideString); safecall;
begin
  SetPropertyValue('Prop', p);
end;

procedure TjsHTMLOptionElement.Set_selected(p: WordBool); safecall;
begin
  SetPropertyValue('selected', p);
end;

procedure TjsHTMLOptionElement.Set_text(const p: WideString); safecall;
begin
  SetPropertyValue('text', p);
end;

procedure TjsHTMLOptionElement.Set_value(const p: WideString); safecall;
begin
  SetPropertyValue('value', p);
end;

{ TjsHTMLFormElement }

function TjsHTMLFormElement.Get__newEnum: IUnknown; safecall;
begin
  Result:=nil;
end;

function TjsHTMLFormElement.Get_acceptCharset: WideString; safecall;
begin
  GetPropertyValue('acceptCharset', Result)
end;

function TjsHTMLFormElement.Get_action: WideString; safecall;
begin
  GetPropertyValue('action', Result)
end;

function TjsHTMLFormElement.Get_dir: WideString; safecall;
begin
  GetPropertyValue('dir', Result)
end;

function TjsHTMLFormElement.Get_elements: IDispatch; safecall;
begin
  Result:=TjsHTMLElementCollection.Create(FApplication, _JSVar+'.elements', TjsHTMLElement);
end;

function TjsHTMLFormElement.Get_encoding: WideString; safecall;
begin
  GetPropertyValue('encoding', Result)
end;

function TjsHTMLFormElement.Get_length: Integer; safecall;
begin
  GetPropertyValue('length', Result)
end;

function TjsHTMLFormElement.Get_method: WideString; safecall;
begin
  GetPropertyValue('method', Result)
end;

function TjsHTMLFormElement.Get_name: WideString; safecall;
begin
  GetPropertyValue('name', Result)
end;

function TjsHTMLFormElement.Get_onreset: OleVariant; safecall;
begin
  GetPropertyValue('onreset', Result)
end;

function TjsHTMLFormElement.Get_onsubmit: OleVariant; safecall;
begin
  GetPropertyValue('onsubmit', Result)
end;

function TjsHTMLFormElement.Get_target: WideString; safecall;
begin
  GetPropertyValue('target', Result)
end;

function TjsHTMLFormElement.item(name: OleVariant; index: OleVariant): IDispatch; safecall;
begin
  Result:=TjsHTMLElement.Create(FApplication, _JSVar+'.item('+ToJSCode(name)+','+ToJSCode(index)+')');
end;

function TjsHTMLFormElement.namedItem(const name: WideString): IDispatch; safecall;
begin
  Result:=TjsHTMLElement.Create(FApplication, _JSVar+'.namedItem('+ToJSCode(name)+')');
end;

function TjsHTMLFormElement.tags(tagName: OleVariant): IDispatch; safecall;
begin
  result:=nil;
end;

function TjsHTMLFormElement.urns(urn: OleVariant): IDispatch; safecall;
begin
  result:=nil;
end;

procedure TjsHTMLFormElement.reset; safecall;
begin
  ExecMethod('reset('+')');
end;

procedure TjsHTMLFormElement.Set_acceptCharset(const p: WideString); safecall;
begin
  SetPropertyValue('acceptCharset', p);
end;

procedure TjsHTMLFormElement.Set_action(const p: WideString); safecall;
begin
  SetPropertyValue('action', p);
end;

procedure TjsHTMLFormElement.Set_dir(const p: WideString); safecall;
begin
  SetPropertyValue('dir', p);
end;

procedure TjsHTMLFormElement.Set_encoding(const p: WideString); safecall;
begin
  SetPropertyValue('encoding', p);
end;

procedure TjsHTMLFormElement.Set_length(p: Integer); safecall;
begin
  SetPropertyValue('length', p);
end;

procedure TjsHTMLFormElement.Set_method(const p: WideString); safecall;
begin
  SetPropertyValue('method', p);
end;

procedure TjsHTMLFormElement.Set_name(const p: WideString); safecall;
begin
  SetPropertyValue('name', p);
end;

procedure TjsHTMLFormElement.Set_onreset(p: OleVariant); safecall;
begin
  SetPropertyValue('onreset', p, True);
end;

procedure TjsHTMLFormElement.Set_onsubmit(p: OleVariant); safecall;
begin
  SetPropertyValue('onsubmit', p, True);
end;

procedure TjsHTMLFormElement.Set_target(const p: WideString); safecall;
begin
  SetPropertyValue('target', p);
end;

procedure TjsHTMLFormElement.submit; safecall;
begin
  ExecMethod('submit('+')');
end;

{ TjsHTMLScreen }

function TjsHTMLScreen.Get_availHeight: Integer; safecall;
begin
  GetPropertyValue('availHeight', Result)
end;

function TjsHTMLScreen.Get_availWidth: Integer; safecall;
begin
  GetPropertyValue('availWidth', Result)
end;

function TjsHTMLScreen.Get_bufferDepth: Integer; safecall;
begin
  GetPropertyValue('bufferDepth', Result)
end;

function TjsHTMLScreen.Get_colorDepth: Integer; safecall;
begin
  GetPropertyValue('colorDepth', Result)
end;

function TjsHTMLScreen.Get_deviceXDPI: Integer; safecall;
begin
  GetPropertyValue('deviceXDPI', Result)
end;

function TjsHTMLScreen.Get_deviceYDPI: Integer; safecall;
begin
  GetPropertyValue('deviceYDPI', Result)
end;

function TjsHTMLScreen.Get_fontSmoothingEnabled: WordBool; safecall;
begin
  GetPropertyValue('fontSmoothingEnabled', Result)
end;

function TjsHTMLScreen.Get_height: Integer; safecall;
begin
  GetPropertyValue('height', Result)
end;

function TjsHTMLScreen.Get_logicalXDPI: Integer; safecall;
begin
  GetPropertyValue('logicalXDPI', Result)
end;

function TjsHTMLScreen.Get_logicalYDPI: Integer; safecall;
begin
  GetPropertyValue('logicalYDPI', Result)
end;

function TjsHTMLScreen.Get_updateInterval: Integer; safecall;
begin
  GetPropertyValue('updateInterval', Result)
end;

function TjsHTMLScreen.Get_width: Integer; safecall;
begin
  GetPropertyValue('width', Result)
end;

procedure TjsHTMLScreen.Set_bufferDepth(p: Integer); safecall;
begin
  SetPropertyValue('bufferDepth', p);
end;

procedure TjsHTMLScreen.Set_updateInterval(p: Integer); safecall;
begin
  SetPropertyValue('updateInterval', p);
end;

end.
