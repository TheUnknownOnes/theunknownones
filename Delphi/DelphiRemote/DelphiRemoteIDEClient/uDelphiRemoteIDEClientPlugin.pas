unit uDelphiRemoteIDEClientPlugin;

interface

{$TYPEINFO ON}

uses
  Classes, ComLib;

type  
  {$METHODINFO ON}
  TDelphiRemoteIDEClientPlugin = class(TObject)
  private
    FChildren: TStringList;
    FParent: TDelphiRemoteIDEClientPlugin;
    FInterface : IDispatch;
    function GetClassname: String;
  protected
    function GetName : string; virtual;
    function GetHelpText : string; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function GetChild(AString: String): IDispatch;
    function GetChildren: IEnumVariant;

    {$METHODINFO OFF}
    function GetDispatchInterface  : IDispatch;
    procedure RegisterChild(AName: String; AChild : TObject);
    procedure UnregisterChild(AChild : TObject);
    function GetChildByName(AName: String): TObject;
    function GetNameByChild(AChild: TObject): String;
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
  ChildObj : TObject;
begin
  ChildObj:=GetChildByName(AString);

  if Assigned(ChildObj) then
  begin
    if ChildObj is TDelphiRemoteIDEClientPlugin then
      Result:=TDelphiRemoteIDEClientPlugin(ChildObj).GetDispatchInterface
    else
      Result:=TObjectDispatchEx.Create(ChildObj, False);
  end
  else
    Result:=nil;
end;

function TDelphiRemoteIDEClientPlugin.GetChildren: IEnumVariant;
var
  List : TVariantCollection;
  idx: Integer;
begin
  List:=TVariantCollection.Create(nil);

  for idx := 0 to FChildren.Count - 1 do
    List.Add(TDelphiRemoteIDEClientPlugin(FChildren.Objects[idx]).GetDispatchInterface);

  Result:=List.GetEnum;
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
    Result:=FParent.GetNameByChild(Self);
end;

function TDelphiRemoteIDEClientPlugin.GetNameByChild(
  AChild: TObject): String;
var
  idx: Integer;
begin
  Result:='';

  idx:=FChildren.IndexOfObject(AChild);
  if idx>=0 then
    Result:=FChildren[idx];
end;

function TDelphiRemoteIDEClientPlugin.GetChildByName(
  AName: String): TObject;
var
  idx: Integer;
begin
  Result:=nil;

  idx:=FChildren.IndexOf(AName);
  if idx>=0 then
    Result:=FChildren.Objects[idx];
end;

procedure TDelphiRemoteIDEClientPlugin.RegisterChild(AName: String;
  AChild: TObject);
begin
  FChildren.AddObject(AName, AChild);
end;

procedure TDelphiRemoteIDEClientPlugin.UnregisterChild(
  AChild: TObject);
var
  idx : Integer;
begin
  idx:=FChildren.IndexOfObject(AChild);
  if idx>=0 then
    FChildren.Delete(idx);
end;

end.
