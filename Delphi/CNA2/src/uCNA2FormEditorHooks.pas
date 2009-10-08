//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uCNA2FormEditorHooks;

interface

uses
  ToolsAPI,
  Classes,
  SysUtils,
  Dialogs;

type
  Tcna2FormEditorHook = class(TInterfacedObject, IOTANotifier, IOTAFormNotifier)
  protected
    FEditor : IOTAFormEditor;
    FRegisterIndex : Integer;

    //IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;

    //IOTAFormNotifier
    procedure FormActivated;
    procedure FormSaving;
    procedure ComponentRenamed(ComponentHandle: TOTAHandle;
      const OldName, NewName: string);
  public
    constructor Create(const AEditor : IOTAFormEditor);
    destructor Destroy(); override;

    procedure Unregister;

    property Editor : IOTAFormEditor read FEditor;
  end;

  Tcna2FormEditorHooks = class(TList)
  private
    function Get(Index: Integer): Tcna2FormEditorHook;
    procedure Put(Index: Integer; const Value: Tcna2FormEditorHook);
  public
    destructor Destroy; override;

    function Add(Item: Tcna2FormEditorHook): Integer;
    function Extract(Item: Tcna2FormEditorHook): Tcna2FormEditorHook;
    function First: Tcna2FormEditorHook;
    function IndexOf(Item: Tcna2FormEditorHook): Integer;
    procedure Insert(Index: Integer; Item: Tcna2FormEditorHook);
    function Last: Tcna2FormEditorHook;
    function Remove(Item: Tcna2FormEditorHook): Integer;
    property Items[Index: Integer]: Tcna2FormEditorHook read Get write Put; default;

    function FindByEditor(const AEditor : IOTAFormEditor; out AHook : Tcna2FormEditorHook) : Boolean;
  end;

var
  cna2FormEditorHooks : Tcna2FormEditorHooks;

implementation

uses uCNA2Worker, uCNA2Settings;

{ Tcna2FormEditorHook }

procedure Tcna2FormEditorHook.AfterSave;
begin

end;

procedure Tcna2FormEditorHook.BeforeSave;
begin

end;

procedure Tcna2FormEditorHook.ComponentRenamed(ComponentHandle: TOTAHandle;
  const OldName, NewName: string);
begin
  if (OldName = EmptyStr) and
     (NewName <> EmptyStr) and
     (cna2Settings.ExpertActive) then
     
    cna2Worker.ProcessComponent(FEditor, ComponentHandle);
end;

constructor Tcna2FormEditorHook.Create(const AEditor: IOTAFormEditor);
begin
  FEditor := AEditor;
  FRegisterIndex := AEditor.AddNotifier(Self);
  cna2FormEditorHooks.Add(Self);
end;

destructor Tcna2FormEditorHook.Destroy;
begin
  cna2FormEditorHooks.Remove(Self);
  
  inherited;
end;

procedure Tcna2FormEditorHook.Destroyed;
begin

end;

procedure Tcna2FormEditorHook.FormActivated;
begin

end;

procedure Tcna2FormEditorHook.FormSaving;
begin

end;

procedure Tcna2FormEditorHook.Modified;
begin

end;

procedure Tcna2FormEditorHook.Unregister;
begin
  if FRegisterIndex > -1 then
  begin
    FEditor.RemoveNotifier(FRegisterIndex);
    FRegisterIndex := -1;
  end;
end;

{ Tcna2FormEditorHooks }

function Tcna2FormEditorHooks.Add(Item: Tcna2FormEditorHook): Integer;
begin
  Result := inherited Add(Item);
end;

destructor Tcna2FormEditorHooks.Destroy;
begin
  while Count > 0 do
    First.Unregister;
    
  inherited;
end;

function Tcna2FormEditorHooks.Extract(
  Item: Tcna2FormEditorHook): Tcna2FormEditorHook;
begin
  Result := inherited Extract(Item);
end;

function Tcna2FormEditorHooks.FindByEditor(const AEditor: IOTAFormEditor;
  out AHook: Tcna2FormEditorHook): Boolean;
var
  idx : Integer;
begin
  Result := false;

  for idx := 0 to Count - 1 do
  begin
    if AEditor = Items[idx].Editor then
    begin
      Result := true;
      AHook := Items[idx];
      break;
    end;
  end;
end;

function Tcna2FormEditorHooks.First: Tcna2FormEditorHook;
begin
  Result := inherited First;
end;

function Tcna2FormEditorHooks.Get(Index: Integer): Tcna2FormEditorHook;
begin
  Result := inherited Get(Index);
end;

function Tcna2FormEditorHooks.IndexOf(Item: Tcna2FormEditorHook): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure Tcna2FormEditorHooks.Insert(Index: Integer;
  Item: Tcna2FormEditorHook);
begin
  inherited Insert(Index, Item);
end;

function Tcna2FormEditorHooks.Last: Tcna2FormEditorHook;
begin
  Result := inherited Last;
end;

procedure Tcna2FormEditorHooks.Put(Index: Integer;
  const Value: Tcna2FormEditorHook);
begin
  inherited Put(Index, Value);
end;

function Tcna2FormEditorHooks.Remove(Item: Tcna2FormEditorHook): Integer;
begin
  Result := inherited Remove(Item);                                
end;

initialization
  cna2FormEditorHooks := nil;

finalization

end.
