unit uForm_EditSettingsComponentPropertyList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls,
  uSettingsBase,
  WideStrUtils, Menus, StdCtrls, ExtCtrls;

type
  Tform_EditComponentPropertyList = class(TForm)
    lv_Properties: TListView;
    pan_Bottom: TPanel;
    btn_OK: TButton;
    btn_Cancel: TButton;
    pum_ListView: TPopupMenu;
    mi_CheckAll: TMenuItem;
    mi_UncheckAll: TMenuItem;
    mi_InvertChecks: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure lv_PropertiesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mi_CheckAllClick(Sender: TObject);
    procedure mi_UncheckAllClick(Sender: TObject);
    procedure mi_InvertChecksClick(Sender: TObject);
  private
    FOrigList,
    FWorkList : TSettingsComponentPropertyList;


    procedure ApplyCheckState(const AItem : TListItem);

  public
  
  end;


function ShowComponentPropertyListEditor(const AList : TSettingsComponentPropertyList) : Boolean;

implementation

{$R *.dfm}

function ShowComponentPropertyListEditor(const AList : TSettingsComponentPropertyList) : Boolean;
var
  Form : Tform_EditComponentPropertyList;
begin
  Form := Tform_EditComponentPropertyList.Create(nil);
  try
    Form.FOrigList := AList;
    Form.FWorkList := TSettingsComponentPropertyList.Create;
    try
      Form.FWorkList.CopyFrom(AList);
      Result := IsPositiveResult(form.ShowModal);

      if Result then
      begin
        AList.CopyFrom(Form.FWorkList);
      end;
    finally
      Form.FWorkList.Free;
    end;

  finally
    Form.Release;
  end;
end;

{ Tform_EditComponentPropertyList }

procedure Tform_EditComponentPropertyList.ApplyCheckState(
  const AItem: TListItem);
begin
  if Assigned(AItem) then
  begin
    PSettingsComponentProperty(AItem.Data).Save := AItem.Checked;
  end;
end;

procedure Tform_EditComponentPropertyList.FormShow(Sender: TObject);
var
  idx : Integer;
  Li : TListItem;
  Name : WideString;
begin
  for idx := 0 to FWorkList.Count - 1 do
  begin
    Name := WideStringReplace(FWorkList[idx]^.Name, SettingsPathDelimiter, '.', [rfReplaceAll]);
    //remove leading dot
    Name := Copy(Name, 2, Length(Name));

    LI := lv_Properties.Items.Add;
    Li.Caption := Name;
    Li.Checked := FWorkList[idx]^.Save;

    Li.Data := FWorkList[idx];
  end;
end;

procedure Tform_EditComponentPropertyList.lv_PropertiesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ApplyCheckState(lv_Properties.GetItemAt(X, Y));
end;

procedure Tform_EditComponentPropertyList.mi_CheckAllClick(Sender: TObject);
var
  idx : Integer;
begin
  for idx := 0 to lv_Properties.Items.Count - 1 do
  begin
    lv_Properties.Items[idx].Checked := true;
    ApplyCheckState(lv_Properties.Items[idx]);
  end;
end;

procedure Tform_EditComponentPropertyList.mi_InvertChecksClick(Sender: TObject);
var
  idx : Integer;
begin
  for idx := 0 to lv_Properties.Items.Count - 1 do
  begin
    lv_Properties.Items[idx].Checked := lv_Properties.Items[idx].Checked xor true;
    ApplyCheckState(lv_Properties.Items[idx]);
  end;
end;

procedure Tform_EditComponentPropertyList.mi_UncheckAllClick(Sender: TObject);
var
  idx : Integer;
begin
  for idx := 0 to lv_Properties.Items.Count - 1 do
  begin
    lv_Properties.Items[idx].Checked := false;
    ApplyCheckState(lv_Properties.Items[idx]);
  end;
end;

end.
