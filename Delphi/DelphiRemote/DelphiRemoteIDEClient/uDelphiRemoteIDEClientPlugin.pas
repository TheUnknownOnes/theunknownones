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

    function GetDispatchInterface  : IDispatch;
    procedure RegisterChild(AName: String; AChild : TDelphiRemoteIDEClientPlugin);
    procedure UnregisterChild(AChild : TDelphiRemoteIDEClientPlugin);
    function GetChildByName(AName: String): TDelphiRemoteIDEClientPlugin;
    function GetNameByChild(AChild: TDelphiRemoteIDEClientPlugin): String;

    function GetHelp: String;
  published
    property Name : String read GetName;
    property Parent : TDelphiRemoteIDEClientPlugin read FParent write FParent;
  end;

implementation

uses uObjectDispatchEx, uRTTIHelper, HVVMT, TypInfo, HVMethodSignature, HVMethodInfoClasses, 
  SysUtils;

{ TDelphiRemoteIDEClientPlugin }

function TDelphiRemoteIDEClientPlugin.GetChild(AString: String): IDispatch;
var
  ChildObj : TObject;
begin
  ChildObj:=GetChildByName(AString);

  if Assigned(ChildObj) then
  begin
    Result:=TDelphiRemoteIDEClientPlugin(ChildObj).GetDispatchInterface
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
    UnregisterChild(TDelphiRemoteIDEClientPlugin(FChildren.Objects[0]));
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
  Result := '';
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

      sl[idx] := sl[idx] + ' : ' + s + #13#10;
    end;

    if sl.Count > 0 then
      Result := Result + '[Properties]' + #13#10 + trim(sl.Text) + #13#10;

    sl.Clear;

    GetClassInfo(Self.ClassInfo, ci);

    for idx := 0 to ci.MethodCount - 1 do
      sl.Add(MethodSignatureToString(ci.Methods[idx].Name, ci.Methods[idx]));

    if sl.Count > 0 then
      Result := Result + '[Methods]' + #13#10 + trim(sl.Text) + #13#10;

    sl.Clear;
    sl.AddStrings(FChildren);
    if sl.Count > 0 then
      Result := Result + '[Children]' + #13#10 + trim(sl.Text) + #13#10;
  finally
    sl.Free;
  end;

  Result := Trim(Result);
end;

function TDelphiRemoteIDEClientPlugin.GetName: string;
begin
  Result:='';
  if Assigned(FParent) then
    Result:=FParent.GetNameByChild(Self);
end;

function TDelphiRemoteIDEClientPlugin.GetNameByChild(
  AChild: TDelphiRemoteIDEClientPlugin): String;
var
  idx: Integer;
begin
  Result:='';

  idx:=FChildren.IndexOfObject(AChild);
  if idx>=0 then
    Result:=FChildren[idx];
end;

function TDelphiRemoteIDEClientPlugin.GetChildByName(
  AName: String): TDelphiRemoteIDEClientPlugin;
var
  idx: Integer;
begin
  Result:=nil;

  idx:=FChildren.IndexOf(AName);
  if idx>=0 then
    Result:=TDelphiRemoteIDEClientPlugin(FChildren.Objects[idx]);
end;

procedure TDelphiRemoteIDEClientPlugin.RegisterChild(AName: String;
  AChild: TDelphiRemoteIDEClientPlugin);
begin
  FChildren.AddObject(AName, AChild);
  AChild.Parent:=Self;
end;

procedure TDelphiRemoteIDEClientPlugin.UnregisterChild(
  AChild: TDelphiRemoteIDEClientPlugin);
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
