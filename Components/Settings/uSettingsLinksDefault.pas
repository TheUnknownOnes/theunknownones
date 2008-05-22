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
  ComCtrls;

type
  TCustomSettingsLinkControl = class(TCustomSettingsComponentLink)
  protected
    FSaveControlOptions : array[0..3] of Boolean;

    function GetControl: TControl;
    procedure SetControl(const Value: TControl);

    function GetSaveControlOption(const Index: Integer): Boolean;
    procedure SetSaveControlOption(const Index: Integer; const Value: Boolean);

    procedure DoApplySettings(const ARootSetting : TSettingName); override;
    procedure DoSaveSettings(const ARootSetting : TSettingName); override;
  public
    constructor Create(AOwner: TComponent); override;

    property Control : TControl read GetControl write SetControl;

    property SaveLeft : Boolean index 0 read GetSaveControlOption write SetSaveControlOption default false;
    property SaveTop : Boolean index 1 read GetSaveControlOption write SetSaveControlOption default false;
    property SaveWidth : Boolean index 2 read GetSaveControlOption write SetSaveControlOption default false;
    property SaveHeight : Boolean index 3 read GetSaveControlOption write SetSaveControlOption default false;
  end;


//==============================================================================


  TSettingsLinkControl = class(TCustomSettingsLinkControl)
  published
    property Settings;
    property DefaultRootSetting;
    property OnNeedRootSetting;

    property Control;
    property SaveLeft;
    property SaveTop;
    property SaveWidth;
    property SaveHeight;
  end;


//==============================================================================

  TCustomSettingsComponentLinkTabControl = class(TCustomSettingsLinkControl)
  protected
    FSaveTabIndex: Boolean;

    function GetTabControl: TTabControl;
    procedure SetTabControl(const Value: TTabControl);

    procedure DoApplySettings(const ARootSetting : TSettingName); override;
    procedure DoSaveSettings(const ARootSetting : TSettingName); override;
  public
    constructor Create(AOwner: TComponent); override;

    property TabControl : TTabControl read GetTabControl write SetTabControl;

    property SaveTabIndex : Boolean read FSaveTabIndex write FSaveTabIndex default true;
  end;


//==============================================================================


  TSettingsLinkTabControl = class(TCustomSettingsComponentLinkTabControl)
  published
    property Settings;
    property DefaultRootSetting;
    property OnNeedRootSetting;

    property SaveLeft;
    property SaveTop;
    property SaveWidth;
    property SaveHeight;

    property TabControl;
    property SaveTabIndex;
  end;


//==============================================================================


  TCustomSettingsComponentLinkForm = class(TCustomSettingsLinkControl)
  protected
    FSaveWindowState: Boolean;

    procedure DoApplySettings(const ARootSetting : TSettingName); override;
    procedure DoSaveSettings(const ARootSetting : TSettingName); override;
  public
    constructor Create(AOwner: TComponent); override;

    property SaveWindowState : Boolean read FSaveWindowState write FSaveWindowState default true;
  end;


//==============================================================================


  TSettingsLinkForm = class(TCustomSettingsComponentLinkForm)
  published
    property Settings;
    property DefaultRootSetting;
    property OnNeedRootSetting;

    property SaveLeft;
    property SaveTop;
    property SaveWidth;
    property SaveHeight;

    property SaveWindowState;
  end;


//==============================================================================


  TCustomSettingsComponentLinkPageControl = class(TCustomSettingsLinkControl)
  protected
    FSaveTabIndex: Boolean;

    function GetPageControl: TPageControl;
    procedure SetPageControl(const Value: TPageControl);

    procedure DoApplySettings(const ARootSetting : TSettingName); override;
    procedure DoSaveSettings(const ARootSetting : TSettingName); override;
  public
    constructor Create(AOwner: TComponent); override;

    property PageControl : TPageControl read GetPageControl write SetPageControl;

    property SaveTabIndex : Boolean read FSaveTabIndex write FSaveTabIndex default true;
  end;


//==============================================================================


  TSettingsLinkPageControl = class(TCustomSettingsComponentLinkPageControl)
  published
    property Settings;
    property DefaultRootSetting;
    property OnNeedRootSetting;

    property SaveLeft;
    property SaveTop;
    property SaveWidth;
    property SaveHeight;

    property PageControl;
    property SaveTabIndex;
  end;


//==============================================================================


  TCustomSettingsComponentLinkListView = class(TCustomSettingsLinkControl)
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
    property Settings;
    property DefaultRootSetting;
    property OnNeedRootSetting;

    property SaveLeft;
    property SaveTop;
    property SaveWidth;
    property SaveHeight;

    property ListView;
    property SaveColumnWidth;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents(SettingsComponentGroup, [ TSettingsLinkControl,
                                               TSettingsLinkTabControl,
                                               TSettingsLinkForm,
                                               TSettingsLinkPageControl,
                                               TSettingsLinkListView]);
end;

{ TCustomSettingsLinkControl }


constructor TCustomSettingsLinkControl.Create(AOwner: TComponent);
begin
  inherited;

  SaveLeft := false;
  SaveTop := false;
  SaveWidth := false;
  SaveHeight := false;
end;

procedure TCustomSettingsLinkControl.DoApplySettings(const ARootSetting : TSettingName);
var
  Control : TControl;
  Value : Variant;
begin
  if Assigned(Component) and Assigned(Settings) then
  begin
    Control := TControl(Component);

    if SaveLeft then
    begin
      Value := Settings.GetValue(ARootSetting + SettingsPathDelimiter + 'Left');
      if not VarIsEmpty(Value) then
        Control.Left := Value;
    end;

    if SaveTop then
    begin
      Value := Settings.GetValue(ARootSetting + SettingsPathDelimiter + 'Top');
      if not VarIsEmpty(Value) then
        Control.Top := Value;
    end;

    if SaveWidth then
    begin
      Value := Settings.GetValue(ARootSetting + SettingsPathDelimiter + 'Width');
      if not VarIsEmpty(Value) then
        Control.Width := Value;
    end;

    if SaveHeight then
    begin
      Value := Settings.GetValue(ARootSetting + SettingsPathDelimiter + 'Height');
      if not VarIsEmpty(Value) then
        Control.Height := Value;
    end;
  end;
end;

procedure TCustomSettingsLinkControl.DoSaveSettings(const ARootSetting : TSettingName);
var
  Control : TControl;
begin
  if Assigned(Component) and Assigned(Settings) then
  begin
    Control := TControl(Component);

    if SaveLeft then
      Settings.SetValue(ARootSetting + SettingsPathDelimiter + 'Left', Control.Left);

    if SaveTop then
      Settings.SetValue(ARootSetting + SettingsPathDelimiter + 'Top', Control.Top);

    if SaveWidth then
      Settings.SetValue(ARootSetting + SettingsPathDelimiter + 'Width', Control.Width);

    if SaveHeight then
      Settings.SetValue(ARootSetting + SettingsPathDelimiter + 'Height', Control.Height);
  end;
end;

function TCustomSettingsLinkControl.GetControl: TControl;
begin
  Result := TControl(Component);
end;

function TCustomSettingsLinkControl.GetSaveControlOption(
  const Index: Integer): Boolean;
begin
  Result := FSaveControlOptions[Index];
end;

procedure TCustomSettingsLinkControl.SetControl(const Value: TControl);
begin
  Component := Value;
end;

procedure TCustomSettingsLinkControl.SetSaveControlOption(
  const Index: Integer; const Value: Boolean);
begin
  FSaveControlOptions[Index] := Value;
end;


//==============================================================================


{ TCustomSettingsComponentLinkTabControl }

constructor TCustomSettingsComponentLinkTabControl.Create(AOwner: TComponent);
begin
  inherited;

  FSaveTabIndex := true;
end;

procedure TCustomSettingsComponentLinkTabControl.DoApplySettings(const ARootSetting : TSettingName);
var
  Value : Variant;
  TabControl : TTabControl;
begin
  inherited;

  if Assigned(Component) and Assigned(Settings) then
  begin
    TabControl := TTabControl(Component);

    if FSaveTabIndex then
    begin
      Value := Settings.GetValue(ARootSetting + SettingsPathDelimiter + 'TabIndex');
      if not VarIsEmpty(Value) then
        TabControl.TabIndex := Value;
    end;
  end;
end;

procedure TCustomSettingsComponentLinkTabControl.DoSaveSettings(const ARootSetting : TSettingName);
var
  TabControl : TTabControl;
begin
  inherited;

  if Assigned(Component) and Assigned(Settings) then
  begin
    TabControl := TTabControl(Component);

    if FSaveTabIndex then
      Settings.SetValue(ARootSetting + SettingsPathDelimiter + 'TabIndex', TabControl.TabIndex);
  end;
end;


function TCustomSettingsComponentLinkTabControl.GetTabControl: TTabControl;
begin
  Result := TTabControl(Component);
end;

procedure TCustomSettingsComponentLinkTabControl.SetTabControl(
  const Value: TTabControl);
begin
  Component := Value;
end;

//==============================================================================


{ TCustomSettingsComponentLinkForm }

constructor TCustomSettingsComponentLinkForm.Create(AOwner: TComponent);
begin
  inherited;

  if not (AOwner is TCustomForm) then
    raise Exception.Create('This link may only be owned by a Form');

  Component := TCustomForm(AOwner);
  FDefaultRootSetting := SettingsPathDelimiter + Component.Name;

  SaveWindowState := true;
end;

procedure TCustomSettingsComponentLinkForm.DoApplySettings(const ARootSetting : TSettingName);
var
  Form : TCustomForm;
  Value : Variant;
begin
  inherited;

  if Assigned(Component) and Assigned(Settings) then
  begin
    Form := TCustomForm(Component);

    if SaveWindowState then
    begin
      Value := Settings.GetValue(ARootSetting + SettingsPathDelimiter + 'WindowState');
      if not VarIsEmpty(Value) then
        Form.WindowState := TWindowState(Value);
    end;
  end;
end;

procedure TCustomSettingsComponentLinkForm.DoSaveSettings(const ARootSetting : TSettingName);
var
  Form : TCustomForm;
begin
  inherited;

  if Assigned(Component) and Assigned(Settings) then
  begin
    Form := TCustomForm(Component);

    if SaveWindowState then
      Settings.SetValue(ARootSetting + SettingsPathDelimiter + 'WindowState', Integer(Form.WindowState));
  end;
end;


//==============================================================================


{ TCustomSettingsComponentLinkPageControl }

constructor TCustomSettingsComponentLinkPageControl.Create(AOwner: TComponent);
begin
  inherited;

  FSaveTabIndex := true;
end;

procedure TCustomSettingsComponentLinkPageControl.DoApplySettings(const ARootSetting : TSettingName);
var
  Value : Variant;
  PageControl : TPageControl;
begin
  inherited;

  if Assigned(Component) and Assigned(Settings) then
  begin
    PageControl := TPageControl(Component);

    if FSaveTabIndex then
    begin
      Value := Settings.GetValue(ARootSetting + SettingsPathDelimiter + 'TabIndex');
      if not VarIsEmpty(Value) then
        PageControl.TabIndex := Value;
    end;
  end;
end;

procedure TCustomSettingsComponentLinkPageControl.DoSaveSettings(const ARootSetting : TSettingName);
var
  PageControl : TPageControl;
begin
  inherited;

  if Assigned(Component) and Assigned(Settings) then
  begin
    PageControl := TPageControl(Component);

    if FSaveTabIndex then
      Settings.SetValue(ARootSetting + SettingsPathDelimiter + 'TabIndex', PageControl.TabIndex);
  end;
end;


function TCustomSettingsComponentLinkPageControl.GetPageControl: TPageControl;
begin
  Result := TPageControl(Component);
end;

procedure TCustomSettingsComponentLinkPageControl.SetPageControl(
  const Value: TPageControl);
begin
  Component := Value;
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
                                    SettingsPathDelimiter + 'Width');

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
