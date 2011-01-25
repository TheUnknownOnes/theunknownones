unit uJSApplication;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  uJSHelper,
  uJSDirect,
  IdUri,
  mshtml,
  uJSDom;

type
  TjsApplication = class(TjsdApplication)
  protected
    procedure DoCreated(); override;
    procedure DoTerminated(); override;
  public
    Window : IjsHTMLWindow;
  end;

implementation

{ TjsApplication }

procedure TjsApplication.DoCreated;
begin
  inherited;

  Window := TjsHTMLWindow.Create(Self, 'window');
end;

procedure TjsApplication.DoTerminated;
begin
  inherited;
end;

end.
