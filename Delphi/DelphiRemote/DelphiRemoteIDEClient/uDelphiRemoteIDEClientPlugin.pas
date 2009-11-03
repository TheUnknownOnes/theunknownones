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

uses uObjectDispatchEx, uRTTIHelper, HVVMT, TypInfo, HVMethodSignature, HVMethodInfoClasses;

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
  while FChildren.Count>0 do
  begin
    UnregisterChild(FChildren.Objects[0]);
  end;

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
var
  sl : TStringList;
  idx : Integer;
  prop : PPropInfo;
  s : String;
  ci : TClassInfo;
begin
  sl := TStringList.Create;
  try
    rttihGetPropertiesList(Self, sl, false, [], [tkUnknown, tkMethod, tkClass, tkDynArray, tkRecord]);

    for idx := 0 to sl.Count - 1 do
    begin
      Prop := rttihGetPropertyByName(Self, sl[idx]);

      case Prop^.PropType^.Kind of
        tkInteger : s := 'Integer';
        tkChar : s := 'Char';
        tkEnumeration : s := 'Enum';
        tkFloat : s := 'Float';
        tkString : s := 'String';
        tkSet : s := 'Set';
        tkWChar : s := 'Widechar';
        tkLString : s := 'AnsiString';
        tkWString : s := 'WideString';
        tkVariant : s := 'Variant';
        tkArray : s := 'Array';
        tkInterface : s := 'Interface';
        tkInt64 : s := 'Int64';
      end;

      sl[idx] := sl[idx] + ' Type: ' + s + #13#10;
    end;

    if sl.Count > 0 then
      Result := '[Properties]' + #13#10 + sl.Text + #13#10;

    sl.Clear;

    GetClassInfo(Self.ClassInfo, ci);

    for idx := 0 to ci.MethodCount - 1 do
      sl.Add(MethodSignatureToString(ci.Methods[idx].Name, ci.Methods[idx]));

    if sl.Count > 0 then
      Result := '[Methods]' + #13#10 + sl.Text + #13#10;

    sl.Clear;
    sl.AddStrings(FChildren);
    if sl.Count > 0 then
      Result := '[Children]' + #13#10 + sl.Text + #13#10;
  finally
    sl.Free;
  end;
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

  if AChild is TDelphiRemoteIDEClientPlugin then
    TDelphiRemoteIDEClientPlugin(AChild).Parent:=Self;
end;

procedure TDelphiRemoteIDEClientPlugin.UnregisterChild(
  AChild: TObject);
var
  idx : Integer;
begin
  idx:=FChildren.IndexOfObject(AChild);
  if idx>=0 then
  begin
    FChildren.Delete(idx);
  end;
  AChild.Free;
end;

end.
