unit uDRPApplication;

interface

uses
  uDelphiRemoteIDEClientPlugin, ComLib, Classes;

type
  TDRPApplication = class(TDelphiRemoteIDEClientPlugin)
  private
    procedure GetActionList(AParent: TComponent; AList : TStringList);
  public
    procedure Close;
    procedure Minimize;
    procedure Restore;
    procedure BringToFront;
    function MessageBox(AText : String; ACaption : String; AFlags : Integer) : Integer;

    //Action Support
    function ListActions: IEnumVariant;
    function ExecuteAction(AActionName : String): Boolean;
  end;

implementation

uses uDelphiRemoteIDEClient, Forms, SysUtils, Dialogs;

{ TDRPApplication }

procedure TDRPApplication.BringToFront;
begin
  Application.BringToFront;
end;

procedure TDRPApplication.Close;
begin
  Application.MainForm.Close;
end;

function TDRPApplication.ExecuteAction(AActionName: String): Boolean;
var
  sl : TStringList;
  idx : Integer;
begin
  Result:=False;
  sl:=TStringList.Create;
  sl.Duplicates:=dupIgnore;
  sl.Sorted:=True;
  try
    GetActionList(Application, sl);

    idx := sl.IndexOf(AActionName);

    if idx<0 then
      raise Exception.Create('Application does not know an action named "'+AActionName+'"');

    Result:=TBasicAction(sl.Objects[idx]).Execute;
  finally
    sl.Free;
  end;
end;

procedure TDRPApplication.GetActionList(AParent: TComponent;
  AList: TStringList);
var
  idx : integer;
begin
  if AParent is TBasicAction then
    AList.AddObject(AParent.Name, AParent);

  for idx := 0 to AParent.ComponentCount - 1 do
    GetActionList(AParent.Components[idx], AList);
end;

function TDRPApplication.ListActions: IEnumVariant;
var
  Actions : TVariantCollection;
  sl : TStringList;
  s  : String;
begin
  sl:=TStringList.Create;
  sl.Duplicates:=dupIgnore;
  sl.Sorted:=True;
  Actions:=TVariantCollection.Create(nil);
  try
    GetActionList(Application, sl);

    for s in sl do
      Actions.Add(s);

    Result:=Actions.GetEnum;
  finally
    sl.Free;
  end;
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
