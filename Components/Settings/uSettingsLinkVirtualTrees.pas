{-----------------------------------------------------------------------------
 Project: Settings_VirtualTrees
 Purpose: Contains Settings support for the VirtualTree components 
 Created: 21.05.2008 14:47:02
 
 (c) by TheUnknownOnes
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uSettingsLinkVirtualTrees;

interface
                                         
uses
  Classes,
  SysUtils,
  Variants,
  uSettingsBase,
  uSettingsLinksDefault,
  VirtualTrees;

type
  TCustomSettingsLinkVST = class(TCustomSettingsLinkControl)
  protected
    FSaveVSTOptions : array[0..10] of Boolean;

    function GetSaveVSTOption(const Index: Integer): Boolean;
    procedure SetSaveVSTOption(const Index: Integer; const Value: Boolean);

    procedure DoApplySettings; override;
    procedure DoSaveSettings; override;

    function ValidComponent(const AComponent : TComponent) : Boolean; override;
  published
  public
    constructor Create(AOwner: TComponent); override;

    property SaveHeaderWidth : Boolean index 0 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveHeaderPos : Boolean index 1 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveHeaderAllowClick : Boolean index 2 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveHeaderDraggable : Boolean index 3 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveHeaderEnabled : Boolean index 4 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveHeaderParentColor : Boolean index 5 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveHeaderResizable : Boolean index 6 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveHeaderShowDropMark : Boolean index 7 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveHeaderVisible : Boolean index 8 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveHeaderAutoSpring : Boolean index 9 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveHeaderFixed : Boolean index 10 read GetSaveVSTOption write SetSaveVSTOption default true;
  end;


//==============================================================================


  TSettingsLinkVST = class(TCustomSettingsLinkVST)
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
    property SaveHeaderAllowClick;
    property SaveHeaderDraggable;
    property SaveHeaderEnabled;
    property SaveHeaderParentColor;
    property SaveHeaderResizable;
    property SaveHeaderShowDropMark;
    property SaveHeaderVisible;
    property SaveHeaderAutoSpring;
    property SaveHeaderFixed;
  end;


//==============================================================================


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents(SettingsComponentGroup, [TSettingsLinkVST]);
end;

const
  ColSettingNamePattern = 'Column%.4d';


//==============================================================================


{ TCustomSettingsLinkVST }

constructor TCustomSettingsLinkVST.Create(AOwner: TComponent);
var
  idx : Integer;
begin
  inherited;

  for idx := Low(FSaveVSTOptions) to High(FSaveVSTOptions) do
    FSaveVSTOptions[idx] := true;
end;

procedure TCustomSettingsLinkVST.DoApplySettings;
var
  Value : Variant;
  VST : TVirtualStringTree;
  idx : Integer;
  Col : TVirtualTreeColumn;
  ColumSettingsPath : TSettingName;

  procedure SetColumnOption(AOption : TVTColumnOption; ASettingName : TSettingName);
  begin
    Value := FSettings.GetValue(ColumSettingsPath + ASettingName);
    if not VarIsEmpty(Value) then
    begin
      if Boolean(Value) then
        Col.Options := Col.Options + [AOption]
      else
        Col.Options := Col.Options - [AOption];
    end;
  end;
begin
  inherited;

  if Assigned(FComponent) and Assigned(FSettings) then
  begin
    VST := TVirtualStringTree(FComponent);
    VST.BeginUpdate;
    
    try
      for idx := 0 to VST.Header.Columns.Count - 1 do
      begin
        Col := VST.Header.Columns[idx];

        ColumSettingsPath := FRootSetting + SettingsPathDelimiter +
                             Format(ColSettingNamePattern, [Col.Index]) + SettingsPathDelimiter;

        if SaveHeaderPos then
        begin
          Value := FSettings.GetValue(ColumSettingsPath + 'Position');
          if not VarIsEmpty(Value) then
            Col.Position := Value;
        end;

        if SaveHeaderWidth then
        begin
          Value := FSettings.GetValue(ColumSettingsPath + 'Width');
          if not VarIsEmpty(Value) then
            Col.Width := Value;
        end;

        if SaveHeaderAllowClick then
          SetColumnOption(coAllowClick, 'AllowClick');

        if SaveHeaderAllowClick then
          SetColumnOption(coDraggable, 'Draggable');

        if SaveHeaderAllowClick then
          SetColumnOption(coEnabled, 'Enabled');

        if SaveHeaderAllowClick then
          SetColumnOption(coParentColor, 'ParentColor');

        if SaveHeaderAllowClick then
          SetColumnOption(coResizable, 'Resizable');

        if SaveHeaderAllowClick then
          SetColumnOption(coShowDropMark, 'ShowDropMark');

        if SaveHeaderAllowClick then
          SetColumnOption(coVisible, 'Visible');

        if SaveHeaderAllowClick then
          SetColumnOption(coAutoSpring, 'AutoSpring');

        if SaveHeaderAllowClick then
          SetColumnOption(coFixed, 'Fixed');
      end;

    finally
      VST.EndUpdate;
    end;
  end;
end;

procedure TCustomSettingsLinkVST.DoSaveSettings;
var
  VST : TVirtualStringTree;
  idx : Integer;
  Col : TVirtualTreeColumn;
  ColumSettingsPath : TSettingName;

  procedure DoWriteColumnOption(AOption : TVTColumnOption; ASettingName : TSettingName);
  begin
    FSettings.SetValue(ColumSettingsPath + ASettingName, AOption in Col.Options)
  end;
begin
  inherited;

  if Assigned(FComponent) and Assigned(FSettings) then
  begin
    VST := TVirtualStringTree(FComponent);

    for idx := 0 to VST.Header.Columns.Count - 1 do
    begin
      Col := VST.Header.Columns[idx];

      ColumSettingsPath := FRootSetting + SettingsPathDelimiter +
                             Format(ColSettingNamePattern, [Col.Index]) + SettingsPathDelimiter;

      if SaveHeaderPos then
        FSettings.SetValue(ColumSettingsPath + 'Position', Col.Position);

      if SaveHeaderWidth then
        FSettings.SetValue(ColumSettingsPath + 'Width', Col.Width);

      if SaveHeaderAllowClick then
          DoWriteColumnOption(coAllowClick, 'AllowClick');

      if SaveHeaderAllowClick then
        DoWriteColumnOption(coDraggable, 'Draggable');

      if SaveHeaderAllowClick then
        DoWriteColumnOption(coEnabled, 'Enabled');

      if SaveHeaderAllowClick then
        DoWriteColumnOption(coParentColor, 'ParentColor');

      if SaveHeaderAllowClick then
        DoWriteColumnOption(coResizable, 'Resizable');

      if SaveHeaderAllowClick then
        DoWriteColumnOption(coShowDropMark, 'ShowDropMark');

      if SaveHeaderAllowClick then
        DoWriteColumnOption(coVisible, 'Visible');

      if SaveHeaderAllowClick then
        DoWriteColumnOption(coAutoSpring, 'AutoSpring');

      if SaveHeaderAllowClick then
        DoWriteColumnOption(coFixed, 'Fixed');
    end;
    
  end;
end;

function TCustomSettingsLinkVST.GetSaveVSTOption(
  const Index: Integer): Boolean;
begin
  Result := FSaveVSTOptions[Index];
end;

procedure TCustomSettingsLinkVST.SetSaveVSTOption(const Index: Integer;
  const Value: Boolean);
begin
  FSaveVSTOptions[Index] := Value;                                    
end;

function TCustomSettingsLinkVST.ValidComponent(
  const AComponent: TComponent): Boolean;
begin
  Result := AComponent is TVirtualStringTree;
end;

end.
