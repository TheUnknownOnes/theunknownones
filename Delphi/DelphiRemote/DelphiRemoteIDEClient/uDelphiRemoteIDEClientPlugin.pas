unit uDelphiRemoteIDEClientPlugin;

interface

{$TYPEINFO ON}

uses
  Classes;

type  
  {$METHODINFO ON}
  TDelphiRemoteIDEClientPlugin = class(TObject)
  private
    FChildren: TStringList;
    FParent: TDelphiRemoteIDEClientPlugin;
    FInterface : IDispatch;
  protected
    function GetName : string; virtual;
    function GetHelpText : string; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function GetChild(AString: String): IDispatch;

    {$METHODINFO OFF}
    function GetDispatchInterface  : IDispatch;
    procedure RegisterPlugin(AName: String; APlugin : TDelphiRemoteIDEClientPlugin);
    procedure UnregisterPlugin(APlugin : TDelphiRemoteIDEClientPlugin);
    function GetPluginByName(AName: String): TDelphiRemoteIDEClientPlugin;
    function GetNameByPlugin(APlugin: TDelphiRemoteIDEClientPlugin): String;
    {$METHODINFO ON}

    function GetHelp: String;
  published
    property Name : String read GetName;
    property Parent : TDelphiRemoteIDEClientPlugin read FParent write FParent;
  end;
  {$METHODINFO OFF}

implementation

uses uObjectDispatchEx;

{ TDelphiRemoteIDEClientPlugin }

function TDelphiRemoteIDEClientPlugin.GetChild(AString: String): IDispatch;
var
  ChildObj : TDelphiRemoteIDEClientPlugin;
begin
  ChildObj:=GetPluginByName(AString);

  if Assigned(ChildObj) then
    Result:=ChildObj.GetDispatchInterface
  else
    Result:=nil;
end;

constructor TDelphiRemoteIDEClientPlugin.Create;
begin
  FChildren:=TStringList.Create;
  FChildren.Sorted:=True;
  FChildren.Duplicates:=dupError;

  FInterface:=TObjectDispatchEx.Create(self, False);
end;

destructor TDelphiRemoteIDEClientPlugin.Destroy;
begin
  FChildren.Free;

  FInterface:=nil;
  inherited;
end;

function TDelphiRemoteIDEClientPlugin.GetDispatchInterface: IDispatch;
begin
  Result:=FInterface;
end;

function TDelphiRemoteIDEClientPlugin.GetHelp: String;
begin
  Result:=GetHelpText;
end;

function TDelphiRemoteIDEClientPlugin.GetHelpText: string;
begin
  Result:='';
end;

function TDelphiRemoteIDEClientPlugin.GetName: string;
begin
  Result:='Root';
  if Assigned(FParent) then
    Result:=FParent.GetNameByPlugin(Self);
end;

function TDelphiRemoteIDEClientPlugin.GetNameByPlugin(
  APlugin: TDelphiRemoteIDEClientPlugin): String;
var
  idx: Integer;
begin
  Result:='';

  idx:=FChildren.IndexOfObject(APlugin);
  if idx>=0 then
    Result:=FChildren[idx];
end;

function TDelphiRemoteIDEClientPlugin.GetPluginByName(
  AName: String): TDelphiRemoteIDEClientPlugin;
var
  idx: Integer;
begin
  Result:=nil;

  idx:=FChildren.IndexOf(AName);
  if idx>=0 then
    Result:=TDelphiRemoteIDEClientPlugin(FChildren.Objects[idx]);
end;

procedure TDelphiRemoteIDEClientPlugin.RegisterPlugin(AName: String;
  APlugin: TDelphiRemoteIDEClientPlugin);
begin
  FChildren.AddObject(AName,APlugin);
end;

procedure TDelphiRemoteIDEClientPlugin.UnregisterPlugin(
  APlugin: TDelphiRemoteIDEClientPlugin);
var
  idx : Integer;
begin
  idx:=FChildren.IndexOfObject(APlugin);
  if idx>=0 then
    FChildren.Delete(idx);
end;

end.
