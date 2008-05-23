{-----------------------------------------------------------------------------
 Project: Settings_Designers_D100D
 Purpose: Holds the declarations of the designers for Settings
 Created: 23.05.2008 07:06:26
 
 (c) by TheUnknownOnes
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uSettings_Designers;

interface

uses
  Classes,
  SysUtils,
  Windows,
  DesignIntf,
  DesignEditors,
  PropertyCategories,
  uSettingsBase,
  TypInfo, Dialogs;


procedure Register;

implementation

uses uForm_EditSettingsComponentPropertyList;

type
  TSettingsSavePropertiesEditor = class(TPropertyEditor)
  protected
    function GetList(out AList : TSettingsComponentPropertyList) : Boolean;
  public
    procedure SetValue(const AValue : String); override;
    function GetValue : String; override;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TCustomSettingsLinkComponentDefaultEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
  end;

procedure Register;
begin
  RegisterComponentEditor(TCustomSettingsComponentLink, TCustomSettingsLinkComponentDefaultEditor);

  RegisterPropertyEditor(TypeInfo(TSettingsComponentPropertyList),
                         nil,
                         EmptyStr,
                         TSettingsSavePropertiesEditor);
end;

{ TSettingsSavePropertiesEditor }

procedure TSettingsSavePropertiesEditor.Edit;
var
  List : TSettingsComponentPropertyList;
begin
  if GetList(List) then
  begin
    if ShowComponentPropertyListEditor(List) then
      Modified;
  end;

end;

function TSettingsSavePropertiesEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog, paReadOnly];
end;

function TSettingsSavePropertiesEditor.GetList(
  out AList: TSettingsComponentPropertyList): Boolean;
begin
  Result := (GetPropType^.Kind = tkClass) and
            (GetOrdValue <> 0) and
            (TObject(GetOrdValue) is TSettingsComponentPropertyList);

  if Result then
    AList := TSettingsComponentPropertyList(GetOrdValue);
end;

function TSettingsSavePropertiesEditor.GetValue: String;
var
  List : TSettingsComponentPropertyList;
begin
  if GetList(List) then
    Result := Format('%d Properties to save', [List.GetPropertiesToSaveCount])
  else
    Result := 'Invalid value';
end;

procedure TSettingsSavePropertiesEditor.SetValue(const AValue: String);
begin
  //ignore ... is readonly
end;

{ TCustomSettingsLinkComponentDefaultEditor }

procedure TCustomSettingsLinkComponentDefaultEditor.EditProperty(
  const Prop: IProperty; var Continue: Boolean);
begin
  if SameText(Prop.GetName, 'SaveProperties') then
  begin
    Prop.Edit;
    Continue := false;
  end;
end;

end.
