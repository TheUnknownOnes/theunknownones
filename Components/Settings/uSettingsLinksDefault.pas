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

    function GetSaveControlOption(const Index: Integer): Boolean;
    procedure SetSaveControlOption(const Index: Integer; const Value: Boolean);

    function ValidComponent(const AComponent : TComponent) : Boolean; override;

    procedure DoApplySettings; override;
    procedure DoSaveSettings; override;
  public
    constructor Create(AOwner: TComponent); override;

    property SaveLeft : Boolean index 0 read GetSaveControlOption write SetSaveControlOption default false;
    property SaveTop : Boolean index 1 read GetSaveControlOption write SetSaveControlOption default false;
    property SaveWidth : Boolean index 2 read GetSaveControlOption write SetSaveControlOption default false;
    property SaveHeight : Boolean index 3 read GetSaveControlOption write SetSaveControlOption default false;
  end;


//==============================================================================


  TSettingsLinkControl = class(TCustomSettingsLinkControl)
  published
    property Component;
    property Settings;
    property RootSetting;

    property SaveLeft;
    property SaveTop;
    property SaveWidth;
    property SaveHeight;
  end;


//==============================================================================

  TCustomSettingsComponentLinkTabControl = class(TCustomSettingsLinkControl)
  protected
    FSaveTabIndex: Boolean;

    procedure DoApplySettings; override;
    procedure DoSaveSettings; override;

    function ValidComponent(const AComponent : TComponent) : Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;

    property SaveTabIndex : Boolean read FSaveTabIndex write FSaveTabIndex default true;
  end;


//==============================================================================


  TSettingsLinkTabControl = class(TCustomSettingsComponentLinkTabControl)
  published
    property Component;
    property Settings;
    property RootSetting;

    property SaveLeft;
    property SaveTop;
    property SaveWidth;
    property SaveHeight;

    property SaveTabIndex;
  end;


//==============================================================================


  TCustomSettingsComponentLinkForm = class(TCustomSettingsLinkControl)
  protected
    FSaveWindowState: Boolean;

    procedure DoApplySettings; override;
    procedure DoSaveSettings; override;

    function ValidComponent(const AComponent : TComponent) : Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;

    property SaveWindowState : Boolean read FSaveWindowState write FSaveWindowState default true;
  end;


//==============================================================================


  TSettingsLinkForm = class(TCustomSettingsComponentLinkForm)
  published
    property Settings;
    property RootSetting;

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

    procedure DoApplySettings; override;
    procedure DoSaveSettings; override;

    function ValidComponent(const AComponent : TComponent) : Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;

    property SaveTabIndex : Boolean read FSaveTabIndex write FSaveTabIndex default true;
  end;


//==============================================================================


  TSettingsLinkPageControl = class(TCustomSettingsComponentLinkPageControl)
  published
    property Component;
    property Settings;
    property RootSetting;

    property SaveLeft;
    property SaveTop;
    property SaveWidth;
    property SaveHeight;

    property SaveTabIndex;
  end;


//==============================================================================


  TCustomSettingsComponentLinkListView = class(TCustomSettingsLinkControl)
  protected
    FSaveColumnWidth : Boolean;

    procedure DoApplySettings; override;
    procedure DoSaveSettings; override;

    function ValidComponent(const AComponent : TComponent) : Boolean; override;

  public
    constructor Create(AOwner: TComponent); override;

    property SaveColumnWidth : Boolean read FSaveColumnWidth write FSaveColumnWidth default true;
  end;


//==============================================================================


  TSettingsLinkListView = class(TCustomSettingsComponentLinkListView)
  published
    property Component;
    property Settings;
    property RootSetting;

    property SaveLeft;
    property SaveTop;
    property SaveWidth;
    property SaveHeight;

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

procedure TCustomSettingsLinkControl.DoApplySettings;
var
  Control : TControl;
  Value : Variant;
begin
  if Assigned(FComponent) and Assigned(FSettings) then
  begin
    Control := TControl(FComponent);

    if SaveLeft then
    begin
      Value := FSettings.GetValue(FRootSetting + SettingsPathDelimiter + 'Left');
      if not VarIsEmpty(Value) then
        Control.Left := Value;
    end;

    if SaveTop then
    begin
      Value := FSettings.GetValue(FRootSetting + SettingsPathDelimiter + 'Top');
      if not VarIsEmpty(Value) then
        Control.Top := Value;
    end;

    if SaveWidth then
    begin
      Value := FSettings.GetValue(FRootSetting + SettingsPathDelimiter + 'Width');
      if not VarIsEmpty(Value) then
        Control.Width := Value;
    end;

    if SaveHeight then
    begin
      Value := FSettings.GetValue(FRootSetting + SettingsPathDelimiter + 'Height');
      if not VarIsEmpty(Value) then
        Control.Height := Value;
    end;
  end;
end;

procedure TCustomSettingsLinkControl.DoSaveSettings;
var
  Control : TControl;
begin
  if Assigned(FComponent) and Assigned(FSettings) then
  begin
    Control := TControl(FComponent);

    if SaveLeft then
      FSettings.SetValue(FRootSetting + SettingsPathDelimiter + 'Left', Control.Left);

    if SaveTop then
      FSettings.SetValue(FRootSetting + SettingsPathDelimiter + 'Top', Control.Top);

    if SaveWidth then
      FSettings.SetValue(FRootSetting + SettingsPathDelimiter + 'Width', Control.Width);

    if SaveHeight then
      FSettings.SetValue(FRootSetting + SettingsPathDelimiter + 'Height', Control.Height);
  end;
end;

function TCustomSettingsLinkControl.GetSaveControlOption(
  const Index: Integer): Boolean;
begin
  Result := FSaveControlOptions[Index];
end;

procedure TCustomSettingsLinkControl.SetSaveControlOption(
  const Index: Integer; const Value: Boolean);
begin
  FSaveControlOptions[Index] := Value;
end;

function TCustomSettingsLinkControl.ValidComponent(
  const AComponent: TComponent): Boolean;
begin
  Result := AComponent is TControl;
end;

{ TCustomSettingsComponentLinkTabControl }

constructor TCustomSettingsComponentLinkTabControl.Create(AOwner: TComponent);
begin
  inherited;

  FSaveTabIndex := true;
end;

procedure TCustomSettingsComponentLinkTabControl.DoApplySettings;
var
  Value : Variant;
  TabControl : TTabControl;
begin
  inherited;

  if Assigned(FComponent) and Assigned(FSettings) then
  begin
    TabControl := TTabControl(FComponent);

    if FSaveTabIndex then
    begin
      Value := FSettings.GetValue(FRootSetting + SettingsPathDelimiter + 'TabIndex');
      if not VarIsEmpty(Value) then
        TabControl.TabIndex := Value;
    end;
  end;
end;

procedure TCustomSettingsComponentLinkTabControl.DoSaveSettings;
var
  TabControl : TTabControl;
begin
  inherited;

  if Assigned(FComponent) and Assigned(FSettings) then
  begin
    TabControl := TTabControl(FComponent);

    if FSaveTabIndex then
      FSettings.SetValue(FRootSetting + SettingsPathDelimiter + 'TabIndex', TabControl.TabIndex);
  end;
end;

function TCustomSettingsComponentLinkTabControl.ValidComponent(
  const AComponent: TComponent): Boolean;
begin
  Result := AComponent is TTabControl;
end;

{ TCustomSettingsComponentLinkForm }

constructor TCustomSettingsComponentLinkForm.Create(AOwner: TComponent);
begin
  inherited;

  if not (AOwner is TCustomForm) then
    raise Exception.Create('This link may only be owned by a Form');

  FComponent := TCustomForm(AOwner);
  FRootSetting := SettingsPathDelimiter + FComponent.Name;

  SaveWindowState := true;
end;

procedure TCustomSettingsComponentLinkForm.DoApplySettings;
var
  Form : TCustomForm;
  Value : Variant;
begin
  inherited;

  if Assigned(FComponent) and Assigned(FSettings) then
  begin
    Form := TCustomForm(FComponent);

    if SaveWindowState then
    begin
      Value := FSettings.GetValue(FRootSetting + SettingsPathDelimiter + 'WindowState');
      if not VarIsEmpty(Value) then
        Form.WindowState := TWindowState(Value);
    end;
  end;
end;

procedure TCustomSettingsComponentLinkForm.DoSaveSettings;
var
  Form : TCustomForm;
begin
  inherited;

  if Assigned(FComponent) and Assigned(FSettings) then
  begin
    Form := TCustomForm(FComponent);

    if SaveWindowState then
      FSettings.SetValue(FRootSetting + SettingsPathDelimiter + 'WindowState', Integer(Form.WindowState));
  end;
end;

function TCustomSettingsComponentLinkForm.ValidComponent(
  const AComponent: TComponent): Boolean;
begin
  Result := AComponent is TCustomForm;
end;

{ TCustomSettingsComponentLinkPageControl }

constructor TCustomSettingsComponentLinkPageControl.Create(AOwner: TComponent);
begin
  inherited;

  FSaveTabIndex := true;
end;

procedure TCustomSettingsComponentLinkPageControl.DoApplySettings;
var
  Value : Variant;
  PageControl : TPageControl;
begin
  inherited;

  if Assigned(FComponent) and Assigned(FSettings) then
  begin
    PageControl := TPageControl(FComponent);

    if FSaveTabIndex then
    begin
      Value := FSettings.GetValue(FRootSetting + SettingsPathDelimiter + 'TabIndex');
      if not VarIsEmpty(Value) then
        PageControl.TabIndex := Value;
    end;
  end;
end;

procedure TCustomSettingsComponentLinkPageControl.DoSaveSettings;
var
  PageControl : TPageControl;
begin
  inherited;

  if Assigned(FComponent) and Assigned(FSettings) then
  begin
    PageControl := TPageControl(FComponent);

    if FSaveTabIndex then
      FSettings.SetValue(FRootSetting + SettingsPathDelimiter + 'TabIndex', PageControl.TabIndex);
  end;
end;

function TCustomSettingsComponentLinkPageControl.ValidComponent(
  const AComponent: TComponent): Boolean;
begin
  Result := AComponent is TPageControl;
end;

{ TCustomSettingsComponentLinkListView }

constructor TCustomSettingsComponentLinkListView.Create(AOwner: TComponent);
begin
  inherited;

  FSaveColumnWidth := true;
end;

procedure TCustomSettingsComponentLinkListView.DoApplySettings;
var
  ListView : TListView;
  Value : Variant;
  Col : TListColumn;
  idx : Integer;
const
  ColSettingNamePattern = 'Column%.4d';
begin
  inherited;

  if Assigned(FComponent) and Assigned(FSettings) then
  begin
    ListView := TListView(FComponent);

    for idx := 0 to ListView.Columns.Count - 1 do
    begin
      Col := ListView.Columns[idx];

      if FSaveColumnWidth then
      begin
        Value := FSettings.GetValue(FRootSetting + SettingsPathDelimiter +
                                    Format(ColSettingNamePattern, [idx]) +
                                    SettingsPathDelimiter + 'Width');

        if not VarIsEmpty(Value) then
          Col.Width := Value;
      end;
    end;
  end;
end;

procedure TCustomSettingsComponentLinkListView.DoSaveSettings;
var
  ListView : TListView;
  Col : TListColumn;
  idx : Integer;
const
  ColSettingNamePattern = 'Column%.4d';
begin
  inherited;

  if Assigned(FComponent) and Assigned(FSettings) then
  begin
    ListView := TListView(FComponent);

    for idx := 0 to ListView.Columns.Count - 1 do
    begin
      Col := ListView.Columns[idx];

      if FSaveColumnWidth then
      begin
        FSettings.SetValue(FRootSetting + SettingsPathDelimiter +
                           Format(ColSettingNamePattern, [idx]) +
                           SettingsPathDelimiter + 'Width', Col.Width);
      end;
    end;
  end;
end;

function TCustomSettingsComponentLinkListView.ValidComponent(
  const AComponent: TComponent): Boolean;
begin
  Result := AComponent is TListView;
end;

end.
