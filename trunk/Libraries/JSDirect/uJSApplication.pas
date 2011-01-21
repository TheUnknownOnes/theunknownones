unit uJSApplication;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  uJSHelper,
  uJSDirect,
  mshtml;

type
  TjsWindow = class(TjsdObjectEx)
  private
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
    function GetDocument: IHTMLDocument;
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
    procedure Back();
    procedure Blur();
    procedure Close();
    function Confirm(AMessage : String): Boolean;

    property Closed : Boolean read GetClosed;
    property InnerHeight : Integer read GetInnerHeight write SetInnerHeight;
    property InnerWidth : Integer read GetInnerWidth write SetInnerWidth;
    property LocationBar : Boolean read GetLocationBar write SetLocationBar;
    property Name : String read GetName write SetName;
    property OuterHeight : Integer read GetOuterHeight write SetOuterHeight;
    property OuterWidth : Integer read GetOuterWidth write SetOuterwidth;
    property PageXOffset : Integer read GetPageXOffset write SetPageXOffset;
    property PageYOffset : Integer read GetPageYOffset write SetPageYOffset;

    property Document : IHTMLDocument read GetDocument;
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

procedure TjsWindow.Back;
begin
  ExecMethod('back', true);
end;

procedure TjsWindow.BeforeDestruction;
begin
  Close;
  inherited;
end;

procedure TjsWindow.Blur;
begin
  ExecMethod('blur');
end;

procedure TjsWindow.Close;
begin
  ExecMethod('close');
end;

function TjsWindow.Confirm(AMessage: String): Boolean;
begin
  result:=StrToBool(ExecMethod('confirm(' + ToJSString(AMessage) + ')', true));
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

procedure TjsWindow.SetInnerHeight(const Value: Integer);
begin
  SetPropertyValue('innerHeight', Value);
end;

procedure TjsWindow.SetInnerWidth(const Value: Integer);
begin
  SetPropertyValue('innerWidth', Value);
end;

procedure TjsWindow.SetLocationBar(const Value: Boolean);
begin
  SetPropertyValue('locationbar', Value);
end;

procedure TjsWindow.SetName(const Value: String);
begin
  SetPropertyValue('name', Value);
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

end.
