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
  TjsApplication = class(TjsdApplication)
  protected
    Window : IHTMLWindow2;

    procedure DoCreated(); override;
    procedure DoTerminated(); override;
  end;

implementation

uses uJSDOM;

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
