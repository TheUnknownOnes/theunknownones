//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uControlTools;

interface

uses
  ComCtrls, SysUtils, Controls, Menus, Windows;

{$REGION 'Controls im Allgemeinen'}
procedure EnableControlAndSubControls(const AControl : TControl; AEnabled : Boolean);
{$ENDREGION}

{$REGION 'TCustomListView'}
function FindItemByCaption(const AListView : TListView; ACaption : String) : TListItem;

{$ENDREGION}

{$REGION 'Menu'}
function FindMenuItemByHandle(AStartAt : TMenuItem; AHandle : HMENU) : TMenuItem;
{$ENDREGION}

implementation

uses Classes;

{$REGION 'Controls im Allgemeinen'}
procedure EnableControlAndSubControls(const AControl : TControl; AEnabled : Boolean);
var
  idx : Integer;
begin
  AControl.Enabled:=AEnabled;

  for idx:=0 to AControl.ComponentCount-1 do
    if AControl is TControl then
    try
      EnableControlAndSubControls(TControl(AControl.Components[idx]), AEnabled);
    except end;
end;
{$ENDREGION}

{$REGION 'TCustomListView'}
function FindItemByCaption(const AListView : TListView; ACaption : String) : TListItem;
var
  Item : TListItem;
begin
  Result:=nil;

  for Item in AListView.Items do
  begin
    if SameStr(Item.Caption, ACaption) then
    begin
      Result:=Item;
      break;
    end;
  end;
end;
{$ENDREGION}

{$REGION 'Menu'}
function FindMenuItemByHandle(AStartAt : TMenuItem; AHandle : HMENU) : TMenuItem;
var
  idx : Integer;
begin
  Result:=nil;

  if AStartAt.Handle=AHandle then
  begin
    Result:=AStartAt;
    exit;
  end;

  for idx:=0 to AStartAt.Count-1 do
  begin
    Result:=FindMenuItemByHandle(AStartAt[idx], AHandle);
    if Assigned(Result) then
      exit;
  end;
end;
{$ENDREGION}

end.
