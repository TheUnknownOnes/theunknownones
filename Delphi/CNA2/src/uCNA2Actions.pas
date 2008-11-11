//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uCNA2Actions;

interface

uses
  Classes,
  SysUtils,
  uRTTIHelper,
  WideStrings,
  WideStrUtils,
  uCNA2Settings,
  uSettingsBase,
  StdCtrls,
  Dialogs,
  TypInfo,
  ToolsAPI;

type
  TCNA2ActionProvider = class;

  TCNA2Action = class
  protected
    FProvider : TCNA2ActionProvider;
  public
    constructor Create(const AProvider : TCNA2ActionProvider); virtual;
    procedure Execute(AComponent : IOTAComponent; AProperty : WideString); virtual; abstract;

    procedure SaveToSettings(ASettingsPath : TSettingName); virtual; abstract;
    procedure LoadFromSettings(ASettingsPath : TSettingName); virtual; abstract;

    function CanBeConfigured : Boolean; virtual;
    procedure Configure(ATypeInfo : PTypeInfo); virtual; abstract;

    function AsString : String; virtual;

    property Provider : TCNA2ActionProvider read FProvider;
  end;

  TCNA2ActionProvider = class
  protected
    function GetID : TGUID; virtual; abstract;
    function GetName : WideString; virtual; abstract;
  public
    constructor Create(); reintroduce; virtual; 
    destructor Destroy(); override;

    function CreateAction : TCNA2Action; virtual; abstract;
    function Handles(ATypeInfo : PTypeInfo) : Boolean; virtual;

    property ID : TGUID read GetID;
    property Name : WideString read GetName;
  end;

  TCNA2ActionManager = class(TList)
  private
    function Get(Index: Integer): TCNA2ActionProvider;
    procedure Put(Index: Integer; const Value: TCNA2ActionProvider);
  public
    function Add(Item: TCNA2ActionProvider): Integer;
    function Extract(Item: TCNA2ActionProvider): TCNA2ActionProvider;
    function First: TCNA2ActionProvider;
    function IndexOf(Item: TCNA2ActionProvider): Integer;
    procedure Insert(Index: Integer; Item: TCNA2ActionProvider);
    function Last: TCNA2ActionProvider;
    function Remove(Item: TCNA2ActionProvider): Integer;

    function GetItemByID(AID : TGUID; out AItem : TCNA2ActionProvider) : Boolean;

    property Items[Index: Integer]: TCNA2ActionProvider read Get write Put; default;

  end;

  

var
  cna2ActionManager : TCNA2ActionManager;

procedure InitActions;
procedure FreeActions;

implementation

uses
  uCNA2ActionSetValue;


procedure InitActions;
begin
  cna2ActionManager := TCNA2ActionManager.Create;
  TCNA2ActionProviderSetValue.Create;
end;

procedure FreeActions;
begin
  if Assigned(cna2ActionManager) then
  begin
    while cna2ActionManager.Count > 0 do
      cna2ActionManager.First.Free;

    cna2ActionManager.Free;
    cna2ActionManager := nil;
  end;
end;

{ TCNA2ActionManager }

function TCNA2ActionManager.Add(Item: TCNA2ActionProvider): Integer;
begin
  Result := inherited Add(Item);
end;

function TCNA2ActionManager.Extract(
  Item: TCNA2ActionProvider): TCNA2ActionProvider;
begin
  Result := inherited Extract(Item);
end;

function TCNA2ActionManager.First: TCNA2ActionProvider;
begin
  Result := inherited First;
end;

function TCNA2ActionManager.Get(Index: Integer): TCNA2ActionProvider;
begin
  Result := inherited Get(Index);
end;

function TCNA2ActionManager.GetItemByID(AID: TGUID;
  out AItem: TCNA2ActionProvider): Boolean;
var
  idx : Integer;
  GivenID : String;
begin
  Result := false;

  GivenID := GUIDToString(AID);

  for idx := 0 to Count - 1 do
  begin
    if SameText(GUIDToString(Items[idx].GetID), GivenID) then
    begin
      Result := true;
      AItem := Items[idx];
      break;
    end;    
  end;
end;

function TCNA2ActionManager.IndexOf(Item: TCNA2ActionProvider): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TCNA2ActionManager.Insert(Index: Integer; Item: TCNA2ActionProvider);
begin
  inherited Insert(Index, Item);
end;

function TCNA2ActionManager.Last: TCNA2ActionProvider;
begin
  Result := inherited Last;
end;

procedure TCNA2ActionManager.Put(Index: Integer;
  const Value: TCNA2ActionProvider);
begin
  inherited Put(Index, Value);
end;

function TCNA2ActionManager.Remove(Item: TCNA2ActionProvider): Integer;
begin
  Result := inherited Remove(Item);
end;

{ TCNA2ActionProvider }

constructor TCNA2ActionProvider.Create;
begin
  if Assigned(cna2ActionManager) then
    cna2ActionManager.Add(Self);
end;

destructor TCNA2ActionProvider.Destroy;
begin
  if Assigned(cna2ActionManager) then
    cna2ActionManager.Extract(Self);

  inherited;
end;

function TCNA2ActionProvider.Handles(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := false;
end;

{ TCNA2Action }

function TCNA2Action.AsString: String;
begin
  Result := EmptyStr;
end;

function TCNA2Action.CanBeConfigured: Boolean;
begin
  Result := false;
end;

constructor TCNA2Action.Create(const AProvider: TCNA2ActionProvider);
begin
  FProvider := AProvider;
end;

initialization
  cna2ActionManager := nil;

end.
