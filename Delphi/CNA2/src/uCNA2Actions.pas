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
  Tcna2Action = class
  protected
    FTypeInfo : PTypeInfo;
  public
    constructor Create(ATypeInfo : PTypeInfo); reintroduce; virtual;

    procedure LoadFromSettings(APath : TSettingName); virtual; abstract;
    procedure SaveToSettings(APath : TSettingName); virtual; abstract;

    function AsString() : WideString; virtual; abstract;
    procedure Configure(); virtual; abstract;

    procedure Execute(AEditor : IOTAFormEditor;
                      AComponent : IOTAComponent;
                      AProperty : WideString); virtual; abstract;

    class function GetDisplayName : WideString; virtual; abstract;
    class function CanHandle(ATypeInfo : PTypeInfo) : Boolean; virtual; abstract;
    class function HasConfigDialog : Boolean; virtual; abstract;
  end;
  
  Tcna2ActionClass = class of Tcna2Action;
                                                             
  Tcna2Actions = class(TList)
  private
    function Get(Index: Integer): Tcna2ActionClass;
    procedure Put(Index: Integer; const Value: Tcna2ActionClass);
  public
    function Add(Item: Tcna2ActionClass): Integer;
    function Extract(Item: Tcna2ActionClass): Tcna2ActionClass;
    function First: Tcna2ActionClass;
    function IndexOf(Item: Tcna2ActionClass): Integer;
    procedure Insert(Index: Integer; Item: Tcna2ActionClass);
    function Last: Tcna2ActionClass;
    function Remove(Item: Tcna2ActionClass): Integer;
    property Items[Index: Integer]: Tcna2ActionClass read Get write Put; default;

    function FindByClassName(AClassName : WideString; out AActionClass : Tcna2ActionClass) : Boolean;
  end;

var
  cna2Actions : Tcna2Actions;

procedure InitActions;
procedure FreeActions;

implementation

uses uCNA2ActSetValue, uCNA2ActCreateObject;

procedure InitActions;
begin
  cna2Actions := Tcna2Actions.Create;

  //Register available actions
  cna2Actions.Add(Tcna2ActSetValue);
  cna2Actions.Add(Tcna2ActCreateObject);
end;

procedure FreeActions;
begin
  if Assigned(cna2Actions) then
  begin
    cna2Actions.Free;
    cna2Actions := nil;
  end;
end;

{ Tcna2Actions }

function Tcna2Actions.Add(Item: Tcna2ActionClass): Integer;
begin
  Result := inherited Add(Item);
end;

function Tcna2Actions.Extract(Item: Tcna2ActionClass): Tcna2ActionClass;
begin
  Result := inherited Extract(Item);
end;

function Tcna2Actions.FindByClassName(AClassName: WideString;
  out AActionClass: Tcna2ActionClass): Boolean;
var
  idx : Integer;
begin
  Result := false;

  for idx := 0 to Count - 1 do
  begin
    if WideSameText(Items[idx].ClassName, AClassName) then
    begin
      Result := true;
      AActionClass := Items[idx];
      break;
    end;
  end;
end;

function Tcna2Actions.First: Tcna2ActionClass;
begin
  Result := inherited First;
end;

function Tcna2Actions.Get(Index: Integer): Tcna2ActionClass;
begin
  Result := inherited Get(Index);
end;

function Tcna2Actions.IndexOf(Item: Tcna2ActionClass): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure Tcna2Actions.Insert(Index: Integer; Item: Tcna2ActionClass);
begin
  inherited Insert(Index, Item);
end;

function Tcna2Actions.Last: Tcna2ActionClass;
begin
  Result := inherited Last;
end;

procedure Tcna2Actions.Put(Index: Integer; const Value: Tcna2ActionClass);
begin
  inherited Put(Index, Value);
end;

function Tcna2Actions.Remove(Item: Tcna2ActionClass): Integer;
begin
  Result := inherited Remove(Item);
end;

{ Tcna2Action }

constructor Tcna2Action.Create(ATypeInfo: PTypeInfo);
begin
  FTypeInfo := ATypeInfo;
end;

initialization
  cna2Actions := nil

finalization
  FreeActions;

end.
