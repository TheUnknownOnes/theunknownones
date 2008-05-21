unit uSettingsCompLinksDefault;

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
  TCustomSettingsCompLinkControl = class(TCustomSettingsCompLink)
  protected
    FSaveOptions : array of Boolean;

    function GetSaveOption(const Index: Integer): Boolean;
    procedure SetSaveOption(const Index: Integer; const Value: Boolean);

    function ValidComponent(const AComponent : TComponent) : Boolean; override;

    procedure DoLoadSettings; override;
    procedure DoSaveSettings; override;
  public
    constructor Create(AOwner: TComponent); override;

    property SaveLeft : Boolean index 0 read GetSaveOption write SetSaveOption default false;
    property SaveTop : Boolean index 1 read GetSaveOption write SetSaveOption default false;
    property SaveWidth : Boolean index 2 read GetSaveOption write SetSaveOption default false;
    property SaveHeight : Boolean index 3 read GetSaveOption write SetSaveOption default false;
  end;


//==============================================================================


  TSettingsCompLinkControl = class(TCustomSettingsCompLinkControl)
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

  TCustomSettingsCompLinkTabControl = class(TCustomSettingsCompLinkControl)
  protected
    FSaveTabIndex: Boolean;

    procedure DoLoadSettings; override;
    procedure DoSaveSettings; override;

    function ValidComponent(const AComponent : TComponent) : Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;

    property SaveTabIndex : Boolean read FSaveTabIndex write FSaveTabIndex default true;
  end;


//==============================================================================


  TSettingsCompLinkTabControl = class(TCustomSettingsCompLinkTabControl)
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


  TCustomSettingsCompLinkForm = class(TCustomSettingsCompLinkControl)
  protected
    FSaveWindowState: Boolean;

    procedure DoLoadSettings; override;
    procedure DoSaveSettings; override;

    function ValidComponent(const AComponent : TComponent) : Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;

    property SaveWindowState : Boolean read FSaveWindowState write FSaveWindowState default true;
  end;


//==============================================================================


  TSettingsCompLinkForm = class(TCustomSettingsCompLinkForm)
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


  TCustomSettingsCompLinkPageControl = class(TCustomSettingsCompLinkControl)
  protected
    FSaveTabIndex: Boolean;

    procedure DoLoadSettings; override;
    procedure DoSaveSettings; override;

    function ValidComponent(const AComponent : TComponent) : Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;

    property SaveTabIndex : Boolean read FSaveTabIndex write FSaveTabIndex default true;
  end;


//==============================================================================


  TSettingsCompLinkPageControl = class(TCustomSettingsCompLinkPageControl)
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


  TCustomSettingsCompLinkListView = class(TCustomSettingsCompLinkControl)
  protected
    FSaveColumnWidth : Boolean;

    procedure DoLoadSettings; override;
    procedure DoSaveSettings; override;

    function ValidComponent(const AComponent : TComponent) : Boolean; override;

  public
    constructor Create(AOwner: TComponent); override;

    property SaveColumnWidth : Boolean read FSaveColumnWidth write FSaveColumnWidth default true;
  end;


//==============================================================================


  TSettingsCompLinkListView = class(TCustomSettingsCompLinkListView)
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
  RegisterComponents(SettingsComponentGroup, [ TSettingsCompLinkControl,
                                               TSettingsCompLinkTabControl,
                                               TSettingsCompLinkForm,
                                               TSettingsCompLinkPageControl,
                                               TSettingsCompLinkListView]);
end;

{ TCustomSettingsCompLinkControl }


constructor TCustomSettingsCompLinkControl.Create(AOwner: TComponent);
begin
  inherited;

  SetLength(FSaveOptions, 4);

  SaveLeft := false;
  SaveTop := false;
  SaveWidth := false;
  SaveHeight := false;
end;

procedure TCustomSettingsCompLinkControl.DoLoadSettings;
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

procedure TCustomSettingsCompLinkControl.DoSaveSettings;
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

function TCustomSettingsCompLinkControl.GetSaveOption(
  const Index: Integer): Boolean;
begin
  Result := FSaveOptions[Index];
end;

procedure TCustomSettingsCompLinkControl.SetSaveOption(
  const Index: Integer; const Value: Boolean);
begin
  FSaveOptions[Index] := Value;
end;

function TCustomSettingsCompLinkControl.ValidComponent(
  const AComponent: TComponent): Boolean;
begin
  Result := AComponent is TControl;
end;

{ TCustomSettingsCompLinkTabControl }

constructor TCustomSettingsCompLinkTabControl.Create(AOwner: TComponent);
begin
  inherited;

  FSaveTabIndex := true;
end;

procedure TCustomSettingsCompLinkTabControl.DoLoadSettings;
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

procedure TCustomSettingsCompLinkTabControl.DoSaveSettings;
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

function TCustomSettingsCompLinkTabControl.ValidComponent(
  const AComponent: TComponent): Boolean;
begin
  Result := AComponent is TTabControl;
end;

{ TCustomSettingsCompLinkForm }

constructor TCustomSettingsCompLinkForm.Create(AOwner: TComponent);
begin
  inherited;

  if not (AOwner is TCustomForm) then
    raise Exception.Create('This link may only be owned by a Form');

  FComponent := TCustomForm(AOwner);
  FRootSetting := SettingsPathDelimiter + FComponent.Name;

  SaveWindowState := true;
end;

procedure TCustomSettingsCompLinkForm.DoLoadSettings;
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

procedure TCustomSettingsCompLinkForm.DoSaveSettings;
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

function TCustomSettingsCompLinkForm.ValidComponent(
  const AComponent: TComponent): Boolean;
begin
  Result := AComponent is TCustomForm;
end;

{ TCustomSettingsCompLinkPageControl }

constructor TCustomSettingsCompLinkPageControl.Create(AOwner: TComponent);
begin
  inherited;

  FSaveTabIndex := true;
end;

procedure TCustomSettingsCompLinkPageControl.DoLoadSettings;
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

procedure TCustomSettingsCompLinkPageControl.DoSaveSettings;
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

function TCustomSettingsCompLinkPageControl.ValidComponent(
  const AComponent: TComponent): Boolean;
begin
  Result := AComponent is TPageControl;
end;

{ TCustomSettingsCompLinkListView }

constructor TCustomSettingsCompLinkListView.Create(AOwner: TComponent);
begin
  inherited;

  FSaveColumnWidth := true;
end;

procedure TCustomSettingsCompLinkListView.DoLoadSettings;
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

procedure TCustomSettingsCompLinkListView.DoSaveSettings;
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

function TCustomSettingsCompLinkListView.ValidComponent(
  const AComponent: TComponent): Boolean;
begin
  Result := AComponent is TListView;
end;

end.
