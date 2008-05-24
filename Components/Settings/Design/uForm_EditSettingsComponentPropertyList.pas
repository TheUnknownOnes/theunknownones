unit uForm_EditSettingsComponentPropertyList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls,
  uSettingsBase, uSettingsRTTI,
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
    procedure mi_CheckAllClick(Sender: TObject);
    procedure mi_UncheckAllClick(Sender: TObject);
    procedure mi_InvertChecksClick(Sender: TObject);
    procedure btn_OKClick(Sender: TObject);
  private
    FOrigList,
    FAllProperties,
    FWorkList : TsrPropertyList;
    function GetToSave(APropertyPath: TSettingName): Boolean;
    procedure SetToSave(APropertyPath: TSettingName; const Value: Boolean);

    property ToSave[APropertyPath : TSettingName] : Boolean read GetToSave write SetToSave;
  public
  
  end;


function ShowComponentPropertyListEditor(const AList : TsrPropertyList;
                                         const AComponent : TObject) : Boolean;

implementation

{$R *.dfm}

function ShowComponentPropertyListEditor(const AList : TsrPropertyList;
                                         const AComponent : TObject) : Boolean;
var
  Form : Tform_EditComponentPropertyList;
  idx : Integer;
begin
  Form := Tform_EditComponentPropertyList.Create(nil);
  try
    Form.FOrigList := AList;
    Form.FWorkList := TsrPropertyList.Create(nil);
    Form.FAllProperties := TsrPropertyList.Create(nil);
    Form.FAllProperties.ReadPropertiesFromObject(AComponent);
    try
      Form.FWorkList.Assign(AList);

      Result := IsPositiveResult(form.ShowModal);

      if Result then
        AList.Assign(Form.FWorkList);

    finally
      Form.FAllProperties.Free;
      Form.FWorkList.Free;
    end;

  finally
    Form.Release;
  end;
end;

{ Tform_EditComponentPropertyList }

procedure Tform_EditComponentPropertyList.btn_OKClick(Sender: TObject);
var
  idx : Integer;
begin
  for idx := 0 to lv_Properties.Items.Count - 1 do
    ToSave[lv_Properties.Items[idx].SubItems[0]] := lv_Properties.Items[idx].Checked;
end;

procedure Tform_EditComponentPropertyList.FormShow(Sender: TObject);
var
  idx : Integer;
  Li : TListItem;
  PropList : TsrPropertyList;
begin
  for idx := 0 to FAllProperties.Count - 1 do
  begin
    LI := lv_Properties.Items.Add;
    Li.SubItems.Add(FAllProperties[idx]);
    Li.Caption := WideStringReplace(FAllProperties[idx], SettingsPathDelimiter, '.', [rfReplaceAll]);
    Li.Checked := ToSave[FAllProperties[idx]];
  end;
end;

function Tform_EditComponentPropertyList.GetToSave(
  APropertyPath: TSettingName): Boolean;
begin
  Result := FWorkList.IndexOf(APropertyPath) > -1;
end;

procedure Tform_EditComponentPropertyList.mi_CheckAllClick(Sender: TObject);
var
  idx : Integer;
begin
  for idx := 0 to lv_Properties.Items.Count - 1 do
  begin
    lv_Properties.Items[idx].Checked := true;
  end;
end;

procedure Tform_EditComponentPropertyList.mi_InvertChecksClick(Sender: TObject);
var
  idx : Integer;
begin
  for idx := 0 to lv_Properties.Items.Count - 1 do
  begin
    lv_Properties.Items[idx].Checked := lv_Properties.Items[idx].Checked xor true;
  end;
end;

procedure Tform_EditComponentPropertyList.mi_UncheckAllClick(Sender: TObject);
var
  idx : Integer;
begin
  for idx := 0 to lv_Properties.Items.Count - 1 do
  begin
    lv_Properties.Items[idx].Checked := false;
  end;
end;

procedure Tform_EditComponentPropertyList.SetToSave(APropertyPath: TSettingName;
  const Value: Boolean);
var
  idx : Integer;
begin
  idx := FWorkList.IndexOf(APropertyPath);

  if Value then
  begin
    if idx = -1 then
      FWorkList.Add(APropertyPath);
  end
  else
  begin
    if idx > -1 then
      FWorkList.Delete(idx);                                   
  end;

end;

end.
