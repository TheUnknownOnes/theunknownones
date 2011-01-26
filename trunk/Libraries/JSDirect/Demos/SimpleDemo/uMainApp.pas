{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N-,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE CONSOLE}
{$WARN SYMBOL_DEPRECATED ON}
{$WARN SYMBOL_LIBRARY ON}
{$WARN SYMBOL_PLATFORM ON}
{$WARN SYMBOL_EXPERIMENTAL ON}
{$WARN UNIT_LIBRARY ON}
{$WARN UNIT_PLATFORM ON}
{$WARN UNIT_DEPRECATED ON}
{$WARN UNIT_EXPERIMENTAL ON}
{$WARN HRESULT_COMPAT ON}
{$WARN HIDING_MEMBER ON}
{$WARN HIDDEN_VIRTUAL ON}
{$WARN GARBAGE ON}
{$WARN BOUNDS_ERROR ON}
{$WARN ZERO_NIL_COMPAT ON}
{$WARN STRING_CONST_TRUNCED ON}
{$WARN FOR_LOOP_VAR_VARPAR ON}
{$WARN TYPED_CONST_VARPAR ON}
{$WARN ASG_TO_TYPED_CONST ON}
{$WARN CASE_LABEL_RANGE ON}
{$WARN FOR_VARIABLE ON}
{$WARN CONSTRUCTING_ABSTRACT ON}
{$WARN COMPARISON_FALSE ON}
{$WARN COMPARISON_TRUE ON}
{$WARN COMPARING_SIGNED_UNSIGNED ON}
{$WARN COMBINING_SIGNED_UNSIGNED ON}
{$WARN UNSUPPORTED_CONSTRUCT ON}
{$WARN FILE_OPEN ON}
{$WARN FILE_OPEN_UNITSRC ON}
{$WARN BAD_GLOBAL_SYMBOL ON}
{$WARN DUPLICATE_CTOR_DTOR ON}
{$WARN INVALID_DIRECTIVE ON}
{$WARN PACKAGE_NO_LINK ON}
{$WARN PACKAGED_THREADVAR ON}
{$WARN IMPLICIT_IMPORT ON}
{$WARN HPPEMIT_IGNORED ON}
{$WARN NO_RETVAL ON}
{$WARN USE_BEFORE_DEF ON}
{$WARN FOR_LOOP_VAR_UNDEF ON}
{$WARN UNIT_NAME_MISMATCH ON}
{$WARN NO_CFG_FILE_FOUND ON}
{$WARN IMPLICIT_VARIANTS ON}
{$WARN UNICODE_TO_LOCALE ON}
{$WARN LOCALE_TO_UNICODE ON}
{$WARN IMAGEBASE_MULTIPLE ON}
{$WARN SUSPICIOUS_TYPECAST ON}
{$WARN PRIVATE_PROPACCESSOR ON}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN OPTION_TRUNCATED ON}
{$WARN WIDECHAR_REDUCED ON}
{$WARN DUPLICATES_IGNORED ON}
{$WARN UNIT_INIT_SEQ ON}
{$WARN LOCAL_PINVOKE ON}
{$WARN MESSAGE_DIRECTIVE ON}
{$WARN TYPEINFO_IMPLICITLY_ADDED ON}
{$WARN RLINK_WARNING ON}
{$WARN IMPLICIT_STRING_CAST ON}
{$WARN IMPLICIT_STRING_CAST_LOSS ON}
{$WARN EXPLICIT_STRING_CAST OFF}
{$WARN EXPLICIT_STRING_CAST_LOSS OFF}
{$WARN CVT_WCHAR_TO_ACHAR ON}
{$WARN CVT_NARROWING_STRING_LOST ON}
{$WARN CVT_ACHAR_TO_WCHAR OFF}
{$WARN CVT_WIDENING_STRING_LOST OFF}
{$WARN XML_WHITESPACE_NOT_ALLOWED ON}
{$WARN XML_UNKNOWN_ENTITY ON}
{$WARN XML_INVALID_NAME_START ON}
{$WARN XML_INVALID_NAME ON}
{$WARN XML_EXPECTED_CHARACTER ON}
{$WARN XML_CREF_NO_RESOLVE ON}
{$WARN XML_NO_PARM ON}
{$WARN XML_NO_MATCHING_PARM ON}
unit uMainApp;

interface

uses
  uJSDirect,
  uJSApplication,
  uJSHelper,
  mshtml,
  uJSDOM;

type
  TmyApp = class(TjsApplication)
  private
    FmyFunction : TjsdFunction;

    FDocument : IjsHTMLDocument;
    FUserInterface : IjsElement;
  public
    procedure MyFunction(AParams : TjsdFunctionHandlerParams);
    procedure DoCreated(); override;
  end;

  TmyUI = class(TjsHTMLElement)
  private
    FButton1 : IHTMLElement;
    FButton2 : IHTMLElement;
    FButton3 : IHTMLElement;
    FButton4 : IHTMLElement;
    FButtonMouseDownFunction : TjsdFunction;

    FCheckBox: IHTMLElement;
    FCheckBoxClickFunction : TjsdFunction;
    FDefaultInterface : IHTMLElement;

    procedure OnButtonClick(AParams : TjsdFunctionHandlerParams);

    function CreateButton(AText: String): IHTMLElement;
    function CreateCheckBox(AText: String): IHTMLElement;
    procedure AddBreak;
    procedure AddText(AText: String);
  public
    constructor Create(AApplication: TjsApplication; const AParent: IHTMLElement);

  end;

implementation

uses
  ujsXMLDOM, xmldom;

{ TmyApp }

procedure TmyApp.DoCreated;
var
  doc : IDOMDocument;
  xt : IDOMText;
begin
  inherited;
  //FDocument:=Self.Window.document as IjsHTMLDocument;
  //FUserInterface:=TmyUI.Create(self, FDocument.body);

  doc:= ujsXMLDOM.TjsDOMDocument.Create(Self, 'document');
  xt:=doc.createTextNode('Das ist ein lustiger Text');
  doc.getElementsByTagName('body').item[0].appendChild(xt);
end;

procedure TmyApp.MyFunction(AParams : TjsdFunctionHandlerParams);
var
  s : String;
  v : Variant;
  vp : PVarArray;
  el : IHTMLElement;
begin
  s:=Window.prompt('what shall be written about you?' , 'I am ...'+AParams.Values['p']);
  Self.Exec('Ext.Msg.alert('+ToJSCode(s)+')', True);
end;

{ TmyUI }

procedure TmyUI.AddBreak;
var
  instance: IHTMLElement;
begin
  instance:=(Self.Get_document as IHTMLDocument2).createElement('br');
  appendChild(instance as IHTMLDOMNode);
end;

procedure TmyUI.AddText(AText: String);
var
  instance: IHTMLDOMNode;
begin
  instance:=(Self.Get_document as IHTMLDocument3).createTextNode(AText);
  appendChild(instance);
end;

constructor TmyUI.Create(AApplication: TjsApplication;
  const AParent: IHTMLElement);
var
  instance : IHTMLElement;
  document : IHTMLDocument2;
begin
  document:=(AParent.document as IHTMLDocument2);
  instance:=document.createElement('div');
  inherited Create(AApplication, (instance as IjsElement).get_JSVar);

  setAttribute('width', 100, 0);
  setAttribute('height', 100, 0);
  setAttribute('style', 'text-align:center; padding:20px; border:thin solid red; margin:25px', 0);

  FButtonMouseDownFunction:=TjsdFunction.Create(FApplication, 'Param0=this.textContent;Param1=arguments[0].which', OnButtonClick);

  FButton1:=CreateButton('Button1');
  FButton2:=CreateButton('Button2');
  FButton3:=CreateButton('Button3');
  FButton4:=CreateButton('Button4');
  FButton1.onmousedown:=FButtonMouseDownFunction._JSVar;
  FButton2.onmousedown:=FButtonMouseDownFunction._JSVar;
  FButton3.onmousedown:=FButtonMouseDownFunction._JSVar;
  FButton4.onmousedown:=FButtonMouseDownFunction._JSVar;

  AddBreak;

  FCheckBox:=CreateCheckBox('Show debug info in Console');
  FApplication.Exec((FCheckBox as IjsElement).get_JSVar+'.checked=TUOWebSocketDebug.debug');
  FCheckBoxClickFunction:=TjsdFunction.Create(FApplication, '', nil, 'TUOWebSocketDebug.debug='+(FCheckBox as IjsElement).get_JSVar+'.checked;');
  FCheckBox.onclick:=FCheckBoxClickFunction._JSVar;

  (AParent as IHTMLDOMNode).appendChild(instance as IhtmlDomNode);
  self.FDefaultInterface:=Self;
end;

function TmyUI.CreateButton(AText: String): IHTMLElement;
begin
  result:=(Self.Get_document as IHTMLDocument2).createElement('button');
  result.innerText:=AText;
  appendChild(result as IHTMLDOMNode);
end;

function TmyUI.CreateCheckBox(AText: String): IHTMLElement;
begin
  result:=(Self.Get_document as IHTMLDocument2).createElement('input');
  result.setAttribute('type','checkbox', 0);
  (result as IjsElement).SetPropertyValue('_jsvar', (result as IjsElement).get_JSVar);
  appendChild(result as IHTMLDOMNode);
  AddText(AText);
end;

procedure TmyUI.OnButtonClick(AParams: TjsdFunctionHandlerParams);
begin
  (FApplication as TjsApplication).Window.alert(
  'you pressed mouse button No.'+AParams.Values['Param1']+#13#10+
  'on button '+AParams.Values['Param0']);
end;

end.
