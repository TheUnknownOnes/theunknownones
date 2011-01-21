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

  TjsWindow = class(TjsdObjectEx)
  private
    FOnErrorFunc : TjsdFunction;
    FOnError: TjsWindowOnErrorProc;
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
    function GetDocument: IHTMLDocument;
    procedure SetOnError(const Value: TjsWindowOnErrorProc);
  public
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
    property Document : IHTMLDocument read GetDocument;
    property InnerHeight : Integer read GetInnerHeight write SetInnerHeight;
    property InnerWidth : Integer read GetInnerWidth write SetInnerWidth;
    property LocationBar : Boolean read GetLocationBar write SetLocationBar;
    property Name : String read GetName write SetName;
    property OuterHeight : Integer read GetOuterHeight write SetOuterHeight;
    property OuterWidth : Integer read GetOuterWidth write SetOuterwidth;
    property PageXOffset : Integer read GetPageXOffset write SetPageXOffset;
    property PageYOffset : Integer read GetPageYOffset write SetPageYOffset;

    property OnError : TjsWindowOnErrorProc read FOnError write SetOnError;
  end;

  TjsApplication = class(TjsdApplication)
  protected
    Window : TjsWindow;
  public
    constructor Create(AContext : TjsdContext); override;
    destructor Destroy(); override;
  end;

implementation

uses uJSDOM;

{ TjsApplication }

constructor TjsApplication.Create(AContext: TjsdContext);
begin
  inherited;

  Window := TjsWindow.Create(Self, 'window');
end;


destructor TjsApplication.Destroy;
begin
  Window.Free;
  inherited;
end;

{ TjsWindow }

procedure TjsWindow.AfterConstruction;
begin
  inherited;
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

function TjsWindow.GetDocument: IHTMLDocument;
begin
  Result:=TjsDOMDocument.Create(FApplication, _JSVar + '.document');
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

    Result := TjsWindow.Create(FApplication, _JSVar + '.open(' + ToJSString(uri) + ', ' +
                                                               ToJSString(name) + ', ' +
                                                               ToJSString(opts.CommaText) + ')');
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

procedure TjsWindow.SetOnError(const Value: TjsWindowOnErrorProc);
begin
  FOnError := Value;

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

end.
