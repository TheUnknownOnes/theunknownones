{-----------------------------------------------------------------------------
 Project: Settings
 Purpose: Contains links for components shipped with Delphi 
 Created: 21.05.2008 14:40:01
 
 (c) by TheUnknownOnes
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}
unit uSettingsLinksDefault;

interface

uses
  Classes,
  Controls,
  SysUtils,
  Variants,
  uSettingsBase,
  Forms,
  ComCtrls,
  TypInfo;

type

  TSettingsLinkComponent = class(TCustomSettingsComponentLink)
  published
    property Active;
    property DefaultRootSetting;
    property SaveProperties;
    property Settings;
    property OnNeedRootSetting;
    property OnBeforeSaveSettings;
    property OnAfterSaveSettings;
    property OnBeforeApplySettings;
    property OnAfterApplySettings;

    property Component;
  end;


//==============================================================================


  TCustomSettingsComponentLinkForm = class(TCustomSettingsComponentLink)
  public
    constructor Create(AOwner: TComponent); override;
  end;


//==============================================================================


  TSettingsLinkForm = class(TCustomSettingsComponentLinkForm)
  published
    property Active;
    property DefaultRootSetting;
    property SaveProperties;
    property Settings;
    property OnNeedRootSetting;
    property OnBeforeSaveSettings;
    property OnAfterSaveSettings;
    property OnBeforeApplySettings;
    property OnAfterApplySettings;
  end;

//==============================================================================


  TCustomSettingsComponentLinkListView = class(TCustomSettingsComponentLink)
  protected
    FSaveColumnWidth : Boolean;

    function GetListView: TListView;
    procedure SetListView(const Value: TListView);

    procedure DoApplySettings(const ARootSetting : TSettingName); override;
    procedure DoSaveSettings(const ARootSetting : TSettingName); override;
  public
    constructor Create(AOwner: TComponent); override;

    property ListView : TListView read GetListView write SetListView;

    property SaveColumnWidth : Boolean read FSaveColumnWidth write FSaveColumnWidth default true;
  end;


//==============================================================================


  TSettingsLinkListView = class(TCustomSettingsComponentLinkListView)
  published
    property Active;
    property DefaultRootSetting;
    property SaveProperties;
    property Settings;
    property OnNeedRootSetting;
    property OnBeforeSaveSettings;
    property OnAfterSaveSettings;
    property OnBeforeApplySettings;
    property OnAfterApplySettings;

    property ListView;
    property SaveColumnWidth;
  end;

implementation



{ TCustomSettingsComponentLinkForm }

constructor TCustomSettingsComponentLinkForm.Create(AOwner: TComponent);
begin
  inherited;

  if not (AOwner is TCustomForm) then
    raise Exception.Create('This link may only be owned by a Form');

  Component := TCustomForm(AOwner);
  FDefaultRootSetting := SettingsPathDelimiter + 'GUI' + SettingsPathDelimiter + Component.Name;
end;


//==============================================================================


{ TCustomSettingsComponentLinkListView }

constructor TCustomSettingsComponentLinkListView.Create(AOwner: TComponent);
begin
  inherited;

  FSaveColumnWidth := true;
end;

procedure TCustomSettingsComponentLinkListView.DoApplySettings(const ARootSetting : TSettingName);
var
  ListView : TListView;
  Value : Variant;
  Col : TListColumn;
  idx : Integer;
const
  ColSettingNamePattern = 'Column%.4d';
begin
  inherited;

  if Assigned(Component) and Assigned(Settings) then
  begin
    ListView := TListView(Component);

    for idx := 0 to ListView.Columns.Count - 1 do
    begin
      Col := ListView.Columns[idx];

      if FSaveColumnWidth then
      begin
        Value := Settings.GetValue(ARootSetting + SettingsPathDelimiter +
                                    Format(ColSettingNamePattern, [idx]) +
                                    SettingsPathDelimiter + 'Width',
                                   Col.Width);

        if not VarIsEmpty(Value) then
          Col.Width := Value;
      end;
    end;
  end;
end;

procedure TCustomSettingsComponentLinkListView.DoSaveSettings(const ARootSetting : TSettingName);
var
  ListView : TListView;
  Col : TListColumn;
  idx : Integer;
const
  ColSettingNamePattern = 'Column%.4d';
begin
  inherited;

  if Assigned(Component) and Assigned(Settings) then
  begin
    ListView := TListView(Component);

    for idx := 0 to ListView.Columns.Count - 1 do
    begin
      Col := ListView.Columns[idx];

      if FSaveColumnWidth then
      begin
        Settings.SetValue(ARootSetting + SettingsPathDelimiter +
                           Format(ColSettingNamePattern, [idx]) +
                           SettingsPathDelimiter + 'Width', Col.Width);
      end;
    end;
  end;
end;


function TCustomSettingsComponentLinkListView.GetListView: TListView;
begin
  Result := TListView(Component);
end;

procedure TCustomSettingsComponentLinkListView.SetListView(
  const Value: TListView);
begin
  Component := Value;
end;

end.
