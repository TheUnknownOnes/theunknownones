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
  end;

var
  cna2Actions : Tcna2Actions;

implementation


{ Tcna2Actions }

function Tcna2Actions.Add(Item: Tcna2ActionClass): Integer;
begin
  Result := inherited Add(Item);
end;

function Tcna2Actions.Extract(Item: Tcna2ActionClass): Tcna2ActionClass;
begin
  Result := inherited Extract(Item);
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

initialization
  cna2Actions := nil

finalization

end.
