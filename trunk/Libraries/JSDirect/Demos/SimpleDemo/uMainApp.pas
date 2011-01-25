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


{ TmyApp }

procedure TmyApp.DoCreated;
begin
  inherited;
  FDocument:=Self.Window.document as IjsHTMLDocument;
  FUserInterface:=TmyUI.Create(self, FDocument.body);
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
