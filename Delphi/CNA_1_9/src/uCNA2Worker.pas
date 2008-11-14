//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uCNA2Worker;

interface

uses
  Classes,
  TypInfo,
  ExtCtrls,
  ToolsAPI,
  SysUtils,
  Dialogs;

type
  Tcna2DroppedComponentData = record
    ComponentHandle : TOTAHandle;
    Editor : IOTAFormEditor;
  end;
  Pcna2DroppedComponentData = ^Tcna2DroppedComponentData;

  Tcna2ComponentCache = class(TList)
  private
    function Get(Index: Integer): Pcna2DroppedComponentData;
    procedure Put(Index: Integer; const Value: Pcna2DroppedComponentData);
  public
    destructor Destroy(); override; 

    function Add(Item: Pcna2DroppedComponentData): Integer; overload;
    function Add(AComponentHandle : TOTAHandle; AEditor : IOTAFormEditor): Integer; overload;
    procedure Delete(Index: Integer);
    function Extract(Item: Pcna2DroppedComponentData): Pcna2DroppedComponentData;
    function First: Pcna2DroppedComponentData;
    function IndexOf(Item: Pcna2DroppedComponentData): Integer;
    procedure Insert(Index: Integer; Item: Pcna2DroppedComponentData);
    function Last: Pcna2DroppedComponentData;
    function Remove(Item: Pcna2DroppedComponentData): Integer;
    property Items[Index: Integer]: Pcna2DroppedComponentData read Get write Put; default;
  end;

  Tcna2Worker = class
  private
    FCache : Tcna2ComponentCache;
    FTimer : TTimer;
    FWorking : Boolean;

    procedure OnTimer(Sender : TObject);
    procedure DoProcessComponent(AData : Tcna2DroppedComponentData);
  public
    constructor Create();
    destructor Destroy; override;

    procedure ProcessComponent(AEditor : IOTAFormEditor; AComponent : TOTAHandle);
  end;

var
  cna2Worker : Tcna2Worker;

implementation

uses uCNA2Profiles, WideStrings, uCNA2Actions;

{ Tcna2Worker }

constructor Tcna2Worker.Create;
begin
  FWorking := false;

  FCache := Tcna2ComponentCache.Create;

  FTimer := TTimer.Create(nil);
  FTimer.Enabled := false;
  FTimer.Interval := 500;
  FTimer.OnTimer := OnTimer;
end;

destructor Tcna2Worker.Destroy;
begin
  FCache.Free;
  FTimer.Free;

  inherited;
end;

procedure Tcna2Worker.DoProcessComponent(AData: Tcna2DroppedComponentData);
var
  Component : IOTAComponent;
  cnaCompo : Tcna2Component;
  Obj : TObject;
  Profile : Tcna2Profile;
  idxAction,
  idxGroup : Integer;
  Group : Tcna2Group;
  CompoClass : TClass;
  Action : Tcna2Action;
begin
  Obj := TObject(AData.ComponentHandle);
  CompoClass := Obj.ClassType;
  Component := AData.Editor.GetComponentFromHandle(Adata.ComponentHandle);
  Profile := cna2Profiles.CurrentProfile;

  if Assigned(Component) then
  begin
    for idxGroup := 0 to Profile.Groups.Count - 1 do
    begin
      Group := Profile.Groups[idxGroup];

      if Group.FindComponent(CompoClass, cnaCompo) then
      begin
        for idxAction := 0 to Group.Actions.Count - 1 do
        begin
          Action := Tcna2Action(Group.Actions.Objects[idxAction]);
          try
            Action.Execute(AData.Editor, Component, Group.Actions[idxAction]);
          except
            on e : Exception do
              MessageDlg(e.Message, mtError, [mbOK], 0);
          end;
        end;
      end;
    end;
  end;
end;

procedure Tcna2Worker.OnTimer(Sender: TObject);
begin
  FTimer.Enabled := false;

  FWorking := true;
  try
    if cna2Profiles.CurrentProfile <> nil then
    begin
      while FCache.Count > 0 do
      begin
        DoProcessComponent(FCache[0]^);

        FCache.Delete(0);
      end;
    end
    else
    begin
      MessageDlg('CNA2 has no active profile!'+#13+#10+'Please select one in the config dialog.', mtWarning, [mbOK], 0);
      FCache.Clear;
    end;
  finally
    FWorking := false;
  end;
end;

procedure Tcna2Worker.ProcessComponent(AEditor: IOTAFormEditor;
  AComponent: TOTAHandle);
begin
  FTimer.Enabled := false;

  FCache.Add(AComponent, AEditor);

  if not FWorking then
    FTimer.Enabled := true;
end;

{ Tcna2ComponentCache }

function Tcna2ComponentCache.Add(Item: Pcna2DroppedComponentData): Integer;
begin
  Result := inherited Add(Item);
end;

function Tcna2ComponentCache.Add(AComponentHandle: TOTAHandle;
  AEditor: IOTAFormEditor): Integer;
var
  Data : Pcna2DroppedComponentData;
begin
  New(Data);
  Data.ComponentHandle := AComponentHandle;
  Data.Editor := AEditor;
  Result := Add(Data);
end;

procedure Tcna2ComponentCache.Delete(Index: Integer);
begin
  if (Index > -1) and (Index < Count) then
    Dispose(Pcna2DroppedComponentData(Items[Index]));

  inherited;
end;

destructor Tcna2ComponentCache.Destroy;
begin
  while Count > 0 do
    Delete(0);
    
  inherited;
end;

function Tcna2ComponentCache.Extract(
  Item: Pcna2DroppedComponentData): Pcna2DroppedComponentData;
begin
  Result := inherited Extract(Item);
end;

function Tcna2ComponentCache.First: Pcna2DroppedComponentData;
begin
  Result := inherited First;
end;

function Tcna2ComponentCache.Get(Index: Integer): Pcna2DroppedComponentData;
begin
  Result := inherited Get(Index);
end;

function Tcna2ComponentCache.IndexOf(Item: Pcna2DroppedComponentData): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure Tcna2ComponentCache.Insert(Index: Integer;
  Item: Pcna2DroppedComponentData);
begin
  inherited Insert(Index, Item);
end;

function Tcna2ComponentCache.Last: Pcna2DroppedComponentData;
begin
  Result := inherited Last;
end;

procedure Tcna2ComponentCache.Put(Index: Integer;
  const Value: Pcna2DroppedComponentData);
begin
  inherited Put(Index, Value);
end;

function Tcna2ComponentCache.Remove(Item: Pcna2DroppedComponentData): Integer;
begin
  Result := inherited Remove(Item);
end;

initialization
  cna2Worker := nil;

finalization

end.
