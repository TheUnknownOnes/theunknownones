unit uDRPApplication;

interface

uses
  uDelphiRemoteIDEClientPlugin;

type
  TDRPApplication = class(TDelphiRemoteIDEClientPlugin)
  public
    procedure Close;
    procedure Minimize;
    procedure Restore;
    procedure BringToFront;
    function MessageBox(AText : String; ACaption : String; AFlags : Integer) : Integer;
  end;

implementation

uses uDelphiRemoteIDEClient, Forms;

{ TDRPApplication }

procedure TDRPApplication.BringToFront;
begin
  Application.BringToFront;
end;

procedure TDRPApplication.Close;
begin
  Application.MainForm.Close;
end;

procedure TDRPApplication.Restore;
begin
  Application.Restore;
end;

function TDRPApplication.MessageBox(AText, ACaption: String;
  AFlags: Integer): Integer;
begin
  Result := Application.MessageBox(Pchar(AText), PChar(ACaption), AFlags);
end;

procedure TDRPApplication.Minimize;
begin
  Application.Minimize;
end;

initialization
  GlobalDelphiRemoteIDEClient.RegisterChild('Application', TDRPApplication.Create);

end.
