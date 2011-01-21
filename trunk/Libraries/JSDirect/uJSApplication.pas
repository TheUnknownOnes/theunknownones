unit uJSApplication;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  uJSHelper,
  uJSDirect,
  IdUri,
  mshtml;

type
  TjsWindow = class;

  TjsWindowInterval = class(TjsdObject)
  public
    constructor Create(AApplication : TjsdApplication;
                       AWindow : TjsWindow;
                       AFunction : TjsdFunction;
                       APause : Integer); reintroduce;
  end;

  TjsWindowTimeout = class(TjsdObject)
  public
    constructor Create(AApplication : TjsdApplication;
                       AWindow : TjsWindow;
                       AFunction : TjsdFunction;
                       APause : Integer); reintroduce;
  end;

  TjsWindowOnErrorProc = procedure(AMessage : String;
                                   AURL : TIdURI;
                                   ALineNumber : Integer) of object;

  TjsWindowOnBlurProc = procedure of object;
  TjsWindowOnFocusProc = procedure of object;
  TjsWindowOnLoadProc = procedure of object;
  TjsWindowOnResizeProc = procedure of object;
  TjsWindowOnScrollProc = procedure of object;
  TjsWindowOnBeforeUnloadProc = procedure of object;
  TjsWindowOnUnloadProc = procedure of object;

  TjsFrames = class(TjsdObjectEx)
  private
    function GetLength: Integer;
  public
    function GetFrame(AIndex : Integer) : TjsWindow;
    property Length : Integer read GetLength;
  end;

  TjsWindow = class(TjsdObjectEx)
  private
    FOnErrorFunc,
    FOnBlurFunc,
    FOnFocusFunc,
    FOnLoadFunc,
    FOnResizeFunc,
    FOnScrollFunc,
    FOnBeforeUnloadFunc,
    FOnUnloadFunc : TjsdFunction;

    FOnError: TjsWindowOnErrorProc;
    FOnBlur : TjsWindowOnBlurProc;
    FOnFocus : TjsWindowOnFocusProc;
    FOnLoad : TjsWindowOnLoadProc;
    FOnResize : TjsWindowOnResizeProc;
    FOnScroll : TjsWindowOnScrollProc;
    FOnBeforeUnload : TjsWindowOnBeforeUnloadProc;
    FOnUnload : TjsWindowOnUnloadProc;
    FFrames: TjsFrames;

    function GetClosed: Boolean;
    function GetInnerHeight: Integer;
    procedure SetInnerHeight(const Value: Integer);
    function GetInnerWidth: Integer;
    procedure SetInnerWidth(const Value: Integer);
    function GetLocationBar: Boolean;
    procedure SetLocationBar(const Value: Boolean);
    function GetName: String;
    procedure SetName(const Value: String);
    function GetOuterHeight: Integer;
    procedure SetOuterHeight(const Value: Integer);
    function GetOuterWidth: Integer;
    procedure SetOuterwidth(const Value: Integer);
    function GetPageXOffset: Integer;
    function GetPageYOffset: Integer;
    procedure SetPageXOffset(const Value: Integer);
    procedure SetPageYOffset(const Value: Integer);
    procedure OnErrorFuncHandler(AParams : TjsdFunctionHandlerParams);
    procedure OnBeforeUnloadFuncHandler(AParams : TjsdFunctionHandlerParams);
    procedure OnBlurFuncHandler(AParams : TjsdFunctionHandlerParams);
    procedure OnFocusFuncHandler(AParams : TjsdFunctionHandlerParams);
    procedure OnLoadFuncHandler(AParams : TjsdFunctionHandlerParams);
    procedure OnResizeFuncHandler(AParams : TjsdFunctionHandlerParams);
    procedure OnScrollFuncHandler(AParams : TjsdFunctionHandlerParams);
    procedure OnUnloadFuncHandler(AParams : TjsdFunctionHandlerParams);
    function GetDocument: IHTMLDocument2;
    procedure SetOnError(const Value: TjsWindowOnErrorProc);
    procedure SetOnBeforeUnload(const Value: TjsWindowOnBeforeUnloadProc);
    procedure SetOnBlur(const Value: TjsWindowOnBlurProc);
    procedure SetOnFocus(const Value: TjsWindowOnFocusProc);
    procedure SetOnLoad(const Value: TjsWindowOnLoadProc);
    procedure SetOnResize(const Value: TjsWindowOnResizeProc);
    procedure SetOnScroll(const Value: TjsWindowOnScrollProc);
    procedure SetOnUnload(const Value: TjsWindowOnUnloadProc);
    function GetFrames: TjsFrames;
  public
    constructor Create(AApplication : TjsdApplication;
                       ACreateCommand : String;
                       ARefsExisting : Boolean); override;

    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;

    function Open(uri : String;
                  width : Integer = -1;
                  height : Integer = -1;
                  top : Integer = -1;
                  left : Integer = -1;
                  dependent : Boolean = false;
                  hotkeys : Boolean = true;
                  innerHeight : Integer = -1;
                  innerWidth : Integer = -1;
                  location : Boolean = false;
                  menubar : Boolean = false;
                  resizable : Boolean = false;
                  screenX : Integer = -1;
                  screenY : Integer = -1;
                  scrollbars : Boolean = false;
                  status : Boolean = false;
                  toolbar : Boolean = false
                  ) : TjsWindow;

    procedure Alert(AMessage : String);
    function AToB(ABase64 : String) : String;
    procedure Back();
    procedure Blur();
    function BtoA(AData : String) : String; //returns Base64
    procedure Close();
    function Confirm(AMessage : String): Boolean;
    procedure Focus();
    procedure Forward();
    procedure Home();
    procedure MoveBy(AX, AY : Integer);
    procedure MoveTo(AX, AY : Integer);
    procedure Print();
    function Prompt(APrompt : String; ADefault : String = '') : String;
    procedure ResizeBy(AX, AY : Integer);
    procedure ResizeTo(AX, AY : Integer);
    procedure ScrollBy(AX, AY : Integer);
    procedure ScrollTo(AX, AY : Integer);
    function SetInterval(AFunction : TjsdFunction; APause : Integer) : TjsWindowInterval;
    function SetTimeout(AFunction : TjsdFunction; APause : Integer) : TjsWindowTimeout;
    procedure Stop();

    property Closed : Boolean read GetClosed;
    property Document : IHTMLDocument2 read GetDocument;
    property Frames : TjsFrames read GetFrames;
    property InnerHeight : Integer read GetInnerHeight write SetInnerHeight;
    property InnerWidth : Integer read GetInnerWidth write SetInnerWidth;
    property LocationBar : Boolean read GetLocationBar write SetLocationBar;
    property Name : String read GetName write SetName;
    property OuterHeight : Integer read GetOuterHeight write SetOuterHeight;
    property OuterWidth : Integer read GetOuterWidth write SetOuterwidth;
    property PageXOffset : Integer read GetPageXOffset write SetPageXOffset;
    property PageYOffset : Integer read GetPageYOffset write SetPageYOffset;

    property OnError : TjsWindowOnErrorProc read FOnError write SetOnError;
    property OnBlur : TjsWindowOnBlurProc read FOnBlur write SetOnBlur;
    property OnFocus : TjsWindowOnFocusProc read FOnFocus write SetOnFocus;
    property OnLoad : TjsWindowOnLoadProc read FOnLoad write SetOnLoad;
    property OnResize : TjsWindowOnResizeProc read FOnResize write SetOnResize;
    property OnScroll : TjsWindowOnScrollProc read FOnScroll write SetOnScroll;
    property OnBeforeUnload : TjsWindowOnBeforeUnloadProc read FOnBeforeUnload write SetOnBeforeUnload;
    property OnUnload : TjsWindowOnUnloadProc read FOnUnload write SetOnUnload;
  end;

  TjsApplication = class(TjsdApplication)
  protected
    Window : TjsWindow;

    procedure DoCreated(); override;
    procedure DoTerminated(); override;
  end;

implementation

uses uJSDOM;

{ TjsWindow }

procedure TjsWindow.AfterConstruction;
begin
  inherited;

  if not FRefsExisting then
    Name := _JSVar;
end;

procedure TjsWindow.Alert(AMessage: String);
begin
  ExecMethod('alert('+ToJSString(AMessage)+')', true);
end;

function TjsWindow.AToB(ABase64: String): String;
begin
  Result := ExecMethod('atob(' + ToJSString(ABase64) +')', true);
end;

procedure TjsWindow.Back;
begin
  ExecMethod('back', true);
end;

procedure TjsWindow.BeforeDestruction;
begin
  OnError := nil;
  OnBlur := nil;
  OnFocus := nil;
  OnLoad := nil;
  OnResize := nil;
  OnScroll := nil;
  OnBeforeUnload := nil;
  OnUnload := nil;

  if Assigned(FFrames) then
    FFrames.Free;

  if not FRefsExisting then
    Close;

  inherited;
end;

procedure TjsWindow.Blur;
begin
  ExecMethod('blur');
end;

function TjsWindow.BtoA(AData: String): String;
begin
  Result := ExecMethod('btoa(' + ToJSString(AData) + ')', true);
end;

procedure TjsWindow.Close;
begin
  ExecMethod('close');
end;

function TjsWindow.Confirm(AMessage: String): Boolean;
begin
  result:=StrToBool(ExecMethod('confirm(' + ToJSString(AMessage) + ')', true));
end;

constructor TjsWindow.Create(AApplication: TjsdApplication;
  ACreateCommand: String; ARefsExisting: Boolean);
begin
  inherited;

  FInitialJSCommand := '';

  if FApplication.Exec(ACreateCommand, true) = 'undefined' then
    raise Exception.Create('Could not create window');
end;

procedure TjsWindow.Focus;
begin
  ExecMethod('focus');
end;

procedure TjsWindow.Forward;
begin
  ExecMethod('forward');
end;

function TjsWindow.GetClosed: Boolean;
begin
  GetPropertyValue('closed', Result);
end;

function TjsWindow.GetDocument: IHTMLDocument2;
begin
  Result:=TjsDOMDocument.Create(FApplication, _JSVar + '.document');
end;

function TjsWindow.GetFrames: TjsFrames;
begin
  if not Assigned(FFrames) then
    FFrames := TjsFrames.Create(FApplication, _JSVar + '.frames', true);

  Result := FFrames;
end;

function TjsWindow.GetInnerHeight: Integer;
begin
  GetPropertyValue('innerHeight', Result);
end;

function TjsWindow.GetInnerWidth: Integer;
begin
  GetPropertyValue('innerWidth', Result);
end;

function TjsWindow.GetLocationBar: Boolean;
begin
  GetPropertyValue('locationbar', Result);
end;

function TjsWindow.GetName: String;
begin
  GetPropertyValue('name', Result);
end;

function TjsWindow.GetOuterHeight: Integer;
begin
  GetPropertyValue('outerHeight', Result);
end;

function TjsWindow.GetOuterWidth: Integer;
begin
  GetPropertyValue('outerWidth', Result);
end;

function TjsWindow.GetPageXOffset: Integer;
begin
  GetPropertyValue('pageXOffset', Result);
end;

function TjsWindow.GetPageYOffset: Integer;
begin
  GetPropertyValue('pageYOffset', Result);
end;

procedure TjsWindow.Home;
begin
  ExecMethod('home');
end;

procedure TjsWindow.MoveBy(AX, AY: Integer);
begin
  ExecMethod(Format('moveBy(%d, %d)', [AX, AY]));
end;

procedure TjsWindow.MoveTo(AX, AY: Integer);
begin
  ExecMethod(Format('moveBy(%d, %d)', [AX, AY]));
end;

procedure TjsWindow.OnBeforeUnloadFuncHandler(
  AParams: TjsdFunctionHandlerParams);
begin
  if Assigned(FOnBeforeUnload) then
    FOnBeforeUnload();
end;

procedure TjsWindow.OnBlurFuncHandler(AParams: TjsdFunctionHandlerParams);
begin
  if Assigned(FOnBlur) then
    FOnBlur();
end;

procedure TjsWindow.OnErrorFuncHandler(AParams: TjsdFunctionHandlerParams);
var
  uri : TIdURI;
begin
  if Assigned(FOnError) then
  begin
    uri := TIdURI.Create(AParams.Values['url']);
    try
      FOnError(AParams.Values['msg'], uri, StrToIntDef(AParams.Values['line'], 0));
    finally
      uri.Free;
    end;
  end;
end;

procedure TjsWindow.OnFocusFuncHandler(AParams: TjsdFunctionHandlerParams);
begin
  if Assigned(FOnFocus) then
    FOnFocus();
end;

procedure TjsWindow.OnLoadFuncHandler(AParams: TjsdFunctionHandlerParams);
begin
  if Assigned(FOnLoad) then
    FOnLoad();
end;

procedure TjsWindow.OnResizeFuncHandler(AParams: TjsdFunctionHandlerParams);
begin
  if Assigned(FOnResize) then
    FOnResize();
end;

procedure TjsWindow.OnScrollFuncHandler(AParams: TjsdFunctionHandlerParams);
begin
  if Assigned(FOnScroll) then
    FOnScroll();
end;

procedure TjsWindow.OnUnloadFuncHandler(AParams: TjsdFunctionHandlerParams);
begin
  if Assigned(FOnUnload) then
    FOnUnload();
end;

function TjsWindow.Open(uri: String; width, height, top, left: Integer;
  dependent, hotkeys: Boolean; innerHeight, innerWidth: Integer; location,
  menubar, resizable: Boolean; screenX, screenY: Integer; scrollbars, status,
  toolbar: Boolean): TjsWindow;
var
  opts : TStringList;
begin
  opts := TStringList.Create;
  try
    if width > -1 then opts.Values['width'] := IntToStr(width);
    if height > -1 then opts.Values['height'] := IntToStr(height);
    if top > -1 then opts.Values['top'] := IntToStr(top);
    if left > -1 then opts.Values['left'] := IntToStr(left);
    if dependent then opts.Values['dependent'] := 'yes';
    if innerHeight > -1 then opts.Values['innerHeight'] := IntToStr(innerHeight);
    if innerWidth > -1 then opts.Values['innerWidth'] := IntToStr(innerWidth);
    if location then opts.Values['location'] := 'yes';
    if menubar then opts.Values['menubar'] := 'yes';
    if resizable then opts.Values['resizable'] := 'yes';
    if screenX > -1 then opts.Values['screenX'] := IntToStr(screenX);
    if screenY > -1 then opts.Values['screenY'] := IntToStr(screenY);
    if scrollbars then opts.Values['scrollbars'] := 'yes';
    if status then opts.Values['status'] := 'yes';
    if toolbar then opts.Values['toolbar'] := 'yes';

    Result := TjsWindow.Create(FApplication, _JSVar + '.open(' + ToJSString(uri) + ', "",  ' +
                                                               ToJSString(opts.CommaText) + ')',
                                                               true);
  finally
    opts.Free;
  end;
end;

procedure TjsWindow.Print;
begin
  ExecMethod('print', true);
end;

function TjsWindow.Prompt(APrompt, ADefault: String): String;
begin
  Result := ExecMethod(Format('prompt(%s, %s)', [ToJSString(APrompt), ToJSString(ADefault)]), true);
end;

procedure TjsWindow.ResizeBy(AX, AY: Integer);
begin
  ExecMethod(Format('resizeBy(%d, %d)', [AX, AY]));
end;

procedure TjsWindow.ResizeTo(AX, AY: Integer);
begin
  ExecMethod(Format('resizeTo(%d, %d)', [AX, AY]));
end;

procedure TjsWindow.ScrollBy(AX, AY: Integer);
begin
  ExecMethod(Format('scrollBy(%d, %d)', [AX, AY]));
end;

procedure TjsWindow.ScrollTo(AX, AY: Integer);
begin
  ExecMethod(Format('scrollTo(%d, %d)', [AX, AY]));
end;

procedure TjsWindow.SetInnerHeight(const Value: Integer);
begin
  SetPropertyValue('innerHeight', Value);
end;

procedure TjsWindow.SetInnerWidth(const Value: Integer);
begin
  SetPropertyValue('innerWidth', Value);
end;

function TjsWindow.SetInterval(AFunction: TjsdFunction;
  APause: Integer): TjsWindowInterval;
begin
  Result := TjsWindowInterval.Create(FApplication, Self, AFunction, APause);
end;

procedure TjsWindow.SetLocationBar(const Value: Boolean);
begin
  SetPropertyValue('locationbar', Value);
end;

procedure TjsWindow.SetName(const Value: String);
begin
  SetPropertyValue('name', Value);
end;

procedure TjsWindow.SetOnBeforeUnload(const Value: TjsWindowOnBeforeUnloadProc);
begin
  if Assigned(Value) and (not Assigned(FOnBeforeUnload)) then
  begin
    FOnBeforeUnloadFunc := TjsdFunction.Create(FApplication, '', '', OnBeforeUnloadFuncHandler);
    FApplication.Exec(Format('%s.onbeforeunload=%s', [_JSVar, FOnBeforeUnloadFunc._JSVar]));
  end
  else
  if (not Assigned(Value)) and (Assigned(FOnBeforeUnload)) then
  begin
    FApplication.Exec(Format('%s.onbeforeunload=null', [_JSVar]));
    FOnBeforeUnloadFunc.Free;
    FOnBeforeUnloadFunc := nil;
  end;

  FOnBeforeUnload := Value;
end;

procedure TjsWindow.SetOnBlur(const Value: TjsWindowOnBlurProc);
begin
  if Assigned(Value) and (not Assigned(FOnBlur)) then
  begin
    FOnBlurFunc := TjsdFunction.Create(FApplication, '', '', OnBlurFuncHandler);
    FApplication.Exec(Format('%s.onblur=%s', [_JSVar, FOnBlurFunc._JSVar]));
  end
  else
  if (not Assigned(Value)) and (Assigned(FOnBlur)) then
  begin
    FApplication.Exec(Format('%s.onblur=null', [_JSVar]));
    FOnBlurFunc.Free;
    FOnBlurFunc := nil;
  end;

  FOnBlur := Value;
end;

procedure TjsWindow.SetOnError(const Value: TjsWindowOnErrorProc);
begin
  if Assigned(Value) and (not Assigned(FOnErrorFunc)) then
  begin
    FOnErrorFunc := TjsdFunction.Create(FApplication, 'msg, url, line', 'msg=msg;url=url;line=line', OnErrorFuncHandler);
    FApplication.Exec(Format('%s.onerror=%s', [_JSVar, FOnErrorFunc._JSVar]));
  end
  else
  if (not Assigned(Value)) and (Assigned(FOnErrorFunc)) then
  begin
    FApplication.Exec(Format('%s.onerror=null', [_JSVar]));
    FOnErrorFunc.Free;
    FOnErrorFunc := nil;
  end;

  FOnError := Value;
end;

procedure TjsWindow.SetOnFocus(const Value: TjsWindowOnFocusProc);
begin
  if Assigned(Value) and (not Assigned(FOnFocus)) then
  begin
    FOnFocusFunc := TjsdFunction.Create(FApplication, '', '', OnFocusFuncHandler);
    FApplication.Exec(Format('%s.onfocus=%s', [_JSVar, FOnFocusFunc._JSVar]));
  end
  else
  if (not Assigned(Value)) and (Assigned(FOnFocus)) then
  begin
    FApplication.Exec(Format('%s.onfocus=null', [_JSVar]));
    FOnFocusFunc.Free;
    FOnFocusFunc := nil;
  end;

  FOnFocus := Value;
end;

procedure TjsWindow.SetOnLoad(const Value: TjsWindowOnLoadProc);
begin
  if Assigned(Value) and (not Assigned(FOnLoad)) then
  begin
    FOnLoadFunc := TjsdFunction.Create(FApplication, '', '', OnLoadFuncHandler);
    FApplication.Exec(Format('%s.onload=%s', [_JSVar, FOnLoadFunc._JSVar]));
  end
  else
  if (not Assigned(Value)) and (Assigned(FOnLoad)) then
  begin
    FApplication.Exec(Format('%s.onload=null', [_JSVar]));
    FOnLoadFunc.Free;
    FOnLoadFunc := nil;
  end;

  FOnLoad := Value;
end;

procedure TjsWindow.SetOnResize(const Value: TjsWindowOnResizeProc);
begin
  if Assigned(Value) and (not Assigned(FOnResize)) then
  begin
    FOnResizeFunc := TjsdFunction.Create(FApplication, '', '', OnResizeFuncHandler);
    FApplication.Exec(Format('%s.onresize=%s', [_JSVar, FOnResizeFunc._JSVar]));
  end
  else
  if (not Assigned(Value)) and (Assigned(FOnResize)) then
  begin
    FApplication.Exec(Format('%s.onresize=null', [_JSVar]));
    FOnResizeFunc.Free;
    FOnResizeFunc := nil;
  end;

  FOnResize := Value;
end;

procedure TjsWindow.SetOnScroll(const Value: TjsWindowOnScrollProc);
begin
  if Assigned(Value) and (not Assigned(FOnScroll)) then
  begin
    FOnScrollFunc := TjsdFunction.Create(FApplication, '', '', OnScrollFuncHandler);
    FApplication.Exec(Format('%s.onScroll=%s', [_JSVar, FOnScrollFunc._JSVar]));
  end
  else
  if (not Assigned(Value)) and (Assigned(FOnScroll)) then
  begin
    FApplication.Exec(Format('%s.onScroll=null', [_JSVar]));
    FOnScrollFunc.Free;
    FOnScrollFunc := nil;
  end;

  FOnScroll := Value;
end;

procedure TjsWindow.SetOnUnload(const Value: TjsWindowOnUnloadProc);
begin
  if Assigned(Value) and (not Assigned(FOnUnload)) then
  begin
    FOnUnloadFunc := TjsdFunction.Create(FApplication, '', '', OnUnloadFuncHandler);
    FApplication.Exec(Format('%s.onUnload=%s', [_JSVar, FOnUnloadFunc._JSVar]));
  end
  else
  if (not Assigned(Value)) and (Assigned(FOnUnload)) then
  begin
    FApplication.Exec(Format('%s.onUnload=null', [_JSVar]));
    FOnUnloadFunc.Free;
    FOnUnloadFunc := nil;
  end;

  FOnUnload := Value;
end;

procedure TjsWindow.SetOuterHeight(const Value: Integer);
begin
  SetPropertyValue('outerHeight', Value);
end;

procedure TjsWindow.SetOuterwidth(const Value: Integer);
begin
  SetPropertyValue('outerWidth', Value);
end;

procedure TjsWindow.SetPageXOffset(const Value: Integer);
begin
  SetPropertyValue('pageXOffset', Value);
end;

procedure TjsWindow.SetPageYOffset(const Value: Integer);
begin
  SetPropertyValue('pageYOffset', Value);
end;

function TjsWindow.SetTimeout(AFunction: TjsdFunction;
  APause: Integer): TjsWindowTimeout;
begin
  Result := TjsWindowTimeout.Create(FApplication, Self, AFunction, APause);
end;

procedure TjsWindow.Stop;
begin
  ExecMethod('stop');
end;

{ TjsWindowInterval }

constructor TjsWindowInterval.Create(AApplication : TjsdApplication;
                               AWindow : TjsWindow;
                               AFunction : TjsdFunction;
                               APause : Integer);
begin
  inherited Create(AApplication);

  FInitialJSCommand := Format('%s = %s.setInterval("%s()", %d)', [_JSVar, AWindow._JSVar, AFunction._JSVar, APause]);
  FFinalJSCommand := Format('%s.clearInterval(%s); %1:s = null', [AWindow._JSVar, _JSVar]);
end;

{ TjsWindowTimeout }

constructor TjsWindowTimeout.Create(AApplication: TjsdApplication; AWindow: TjsWindow;
  AFunction: TjsdFunction; APause: Integer);
begin
  inherited Create(AApplication);

  FInitialJSCommand := Format('%s = %s.setTimeout("%s", %d)', [_JSVar, AWindow._JSVar, AFunction._JSVar, APause]);
  FFinalJSCommand := Format('%s.clearTimeout(%s); %1:s = null', [AWindow._JSVar, _JSVar]);
end;

{ TjsApplication }

procedure TjsApplication.DoCreated;
begin
  inherited;

  Window := TjsWindow.Create(Self, 'window', true);
end;

procedure TjsApplication.DoTerminated;
begin
  Window.Free;
  inherited;
end;

{ TjsFrames }

function TjsFrames.GetFrame(AIndex: Integer): TjsWindow;
begin
  try
    Result := TjsWindow.Create(FApplication, Format('%s[%d]', [_JSVar, AIndex]), true);
  except
    Result := nil;
  end;
end;

function TjsFrames.GetLength: Integer;
begin
  GetPropertyValue('length', Result, 0);
end;

end.
