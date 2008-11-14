//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uCNA2ValueEditors;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs,
  TypInfo;

type
  Tcna2ValueEditor = class(TFrame)
  protected
    FValue : Variant;
    FTypeInfo : PTypeInfo;
    FExpressionEvaluation : Boolean;

    function GetValue : Variant; virtual; abstract;
  public
    procedure Init(ATypeInfo : PTypeInfo;
                   AValue : Variant;
                   AExpressionEvaluation : Boolean); virtual;

    function GetDesiredHeight : Integer; virtual; abstract;

    property Value : Variant read GetValue;

    class function CanHandle(ATypeInfo : PTypeInfo) : Boolean; virtual;
  end;

  Tcna2ValueEditorClass = class of Tcna2ValueEditor;

  Tcna2ValueEditors = class(TList)
  private
    function Get(Index: Integer): Tcna2ValueEditorClass;
    procedure Put(Index: Integer; const Value: Tcna2ValueEditorClass);
  public
    function Add(Item: Tcna2ValueEditorClass): Integer;
    function Extract(Item: Tcna2ValueEditorClass): Tcna2ValueEditorClass;
    function First: Tcna2ValueEditorClass;
    function IndexOf(Item: Tcna2ValueEditorClass): Integer;
    procedure Insert(Index: Integer; Item: Tcna2ValueEditorClass);
    function Last: Tcna2ValueEditorClass;
    function Remove(Item: Tcna2ValueEditorClass): Integer;
    property Items[Index: Integer]: Tcna2ValueEditorClass read Get write Put; default;

    function FindByTypeInfo(ATypeInfo : PTypeInfo; out AValueEditorClass : Tcna2ValueEditorClass) : Boolean;
  end;

var
  cna2ValueEditors : Tcna2ValueEditors;

procedure InitValueEditors;
procedure FreeValueEditors;

implementation

uses uCNA2ValueEditorString, uCNA2ValueEditorChar, uCNA2ValueEditorInteger,
  uCNA2ValueEditorInt64, uCNA2ValueEditorFloat, uCNA2ValueEditorEnum,
  uCNA2ValueEditorSet, uCNA2ValueEditorColor;

procedure InitValueEditors;
begin
  cna2ValueEditors := Tcna2ValueEditors.Create;

  //register ValueEditors here
  cna2ValueEditors.Add(Tcna2ValueEditorString);
  cna2ValueEditors.Add(Tcna2ValueEditorChar);
  //Color is a special integer ... register it before the generic integer editor
  cna2ValueEditors.Add(Tcna2ValueEditorColor);
  cna2ValueEditors.Add(Tcna2ValueEditorInteger);
  cna2ValueEditors.Add(Tcna2ValueEditorInt64);
  cna2ValueEditors.Add(Tcna2ValueEditorFloat);
  cna2ValueEditors.Add(Tcna2ValueEditorEnum);
  cna2ValueEditors.Add(Tcna2ValueEditorSet);
end;

procedure FreeValueEditors;
begin
  if Assigned(cna2ValueEditors) then
  begin
    cna2ValueEditors.Free;
    cna2ValueEditors := nil;
  end;
end;

{$R *.dfm}

{ Tframe_ValueEditor }

class function Tcna2ValueEditor.CanHandle(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := false;
end;

procedure Tcna2ValueEditor.Init(ATypeInfo: PTypeInfo; AValue: Variant; AExpressionEvaluation : Boolean);
begin
  FExpressionEvaluation := AExpressionEvaluation;
  FTypeInfo := ATypeInfo;
  FValue := AValue;
end;

{ Tcna2ValueEditors }

function Tcna2ValueEditors.Add(Item: Tcna2ValueEditorClass): Integer;
begin
  Result := inherited Add(Item);
end;

function Tcna2ValueEditors.Extract(
  Item: Tcna2ValueEditorClass): Tcna2ValueEditorClass;
begin
  Result := inherited Extract(Item);
end;

function Tcna2ValueEditors.FindByTypeInfo(ATypeInfo: PTypeInfo;
  out AValueEditorClass: Tcna2ValueEditorClass): Boolean;
var
  idx : Integer;
begin
  Result := false;

  for idx := 0 to Count - 1 do
  begin
    if Items[idx].CanHandle(ATypeInfo) then
    begin
      Result := true;
      AValueEditorClass := Items[idx];
      break;
    end;
  end;
end;

function Tcna2ValueEditors.First: Tcna2ValueEditorClass;
begin
  Result := inherited First;
end;

function Tcna2ValueEditors.Get(Index: Integer): Tcna2ValueEditorClass;
begin
  Result := inherited Get(Index);
end;

function Tcna2ValueEditors.IndexOf(Item: Tcna2ValueEditorClass): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure Tcna2ValueEditors.Insert(Index: Integer; Item: Tcna2ValueEditorClass);
begin
  inherited Insert(Index, Item);
end;

function Tcna2ValueEditors.Last: Tcna2ValueEditorClass;
begin
  Result := inherited Last;
end;

procedure Tcna2ValueEditors.Put(Index: Integer;
  const Value: Tcna2ValueEditorClass);
begin
  inherited Put(Index, Value);
end;

function Tcna2ValueEditors.Remove(Item: Tcna2ValueEditorClass): Integer;
begin
  Result := inherited Remove(Item);
end;

initialization
  cna2ValueEditors := nil;

finalization
  FreeValueEditors;
  
end.
