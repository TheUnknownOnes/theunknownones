unit uSettingsCompLinkVirtualTrees;

interface

uses
  Classes,
  SysUtils,
  Variants,
  uSettingsBase,
  uSettingsCompLinksDefault,
  VirtualTrees;

type
  TCustomSettingsCompLinkVST = class(TCustomSettingsCompLinkControl)
  protected
    FSaveHeaderPos: Boolean;
    FSaveHeaderWidth: Boolean;

    procedure DoLoadSettings; override;
    procedure DoSaveSettings; override;

    function ValidComponent(const AComponent : TComponent) : Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;

    property SaveHeaderWidth : Boolean read FSaveHeaderWidth write FSaveHeaderWidth default true;
    property SaveHeaderPos : Boolean read FSaveHeaderPos write FSaveHeaderPos default true;
  end;


//==============================================================================


  TSettingsCompLinkVST = class(TCustomSettingsCompLinkVST)
  published
    property Component;
    property Settings;
    property RootSetting;

    property SaveLeft;
    property SaveTop;
    property SaveWidth;
    property SaveHeight;

    property SaveHeaderWidth;
    property SaveHeaderPos;
  end;


//==============================================================================


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents(SettingsComponentGroup, [TSettingsCompLinkVST]);
end;

const
  ColSettingNamePattern = 'Column%.4d';


//==============================================================================


{ TCustomSettingsCompLinkVST }

constructor TCustomSettingsCompLinkVST.Create(AOwner: TComponent);
begin
  inherited;

  FSaveHeaderWidth := true;
  FSaveHeaderPos := true;
end;

procedure TCustomSettingsCompLinkVST.DoLoadSettings;
var
  Value : Variant;
  VST : TVirtualStringTree;
  idx : Integer;
  Col : TVirtualTreeColumn;
begin
  inherited;

  if Assigned(FComponent) and Assigned(FSettings) then
  begin
    VST := TVirtualStringTree(FComponent);

    for idx := 0 to VST.Header.Columns.Count - 1 do
    begin
      Col := VST.Header.Columns[idx];

      if FSaveHeaderPos then
      begin
        Value := FSettings.GetValue(FRootSetting + SettingsPathDelimiter +
                                    Format(ColSettingNamePattern, [Col.Index]) +
                                    SettingsPathDelimiter + 'Position');
        if not VarIsEmpty(Value) then
          Col.Position := Value;
      end;

      if FSaveHeaderWidth then
      begin
        Value := FSettings.GetValue(FRootSetting + SettingsPathDelimiter +
                                    Format(ColSettingNamePattern, [Col.Index]) +
                                    SettingsPathDelimiter + 'Width');
        if not VarIsEmpty(Value) then
          Col.Width := Value;
      end;
    end;
    
  end;
end;

procedure TCustomSettingsCompLinkVST.DoSaveSettings;
var
  VST : TVirtualStringTree;
  idx : Integer;
  Col : TVirtualTreeColumn;
begin
  inherited;

  if Assigned(FComponent) and Assigned(FSettings) then
  begin
    VST := TVirtualStringTree(FComponent);

    for idx := 0 to VST.Header.Columns.Count - 1 do
    begin
      Col := VST.Header.Columns[idx];

      if FSaveHeaderPos then
      begin
        FSettings.SetValue(FRootSetting + SettingsPathDelimiter +
                           Format(ColSettingNamePattern, [Col.Index]) +
                           SettingsPathDelimiter + 'Position', col.Position);
      end;

      if FSaveHeaderWidth then
      begin
        FSettings.SetValue(FRootSetting + SettingsPathDelimiter +
                           Format(ColSettingNamePattern, [Col.Index]) +
                           SettingsPathDelimiter + 'Width', Col.Width);
      end;
    end;
    
  end;
end;

function TCustomSettingsCompLinkVST.ValidComponent(
  const AComponent: TComponent): Boolean;
begin
  Result := AComponent is TVirtualStringTree;
end;

end.
