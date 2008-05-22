{-----------------------------------------------------------------------------
 Project: Settings_VirtualTrees
 Purpose: Contains Settings support for the VirtualTree components 
 Created: 21.05.2008 14:47:02
 
 (c) by TheUnknownOnes
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uSettingsLinkVirtualTrees;

interface

{$R Images.res}
                                         
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
    FSaveVSTOptions : array[0..11] of Boolean;

    function GetTree: TVirtualStringTree;
    procedure SetTree(const Value: TVirtualStringTree);

    function GetSaveVSTOption(const Index: Integer): Boolean;
    procedure SetSaveVSTOption(const Index: Integer; const Value: Boolean);

    procedure DoApplySettings; override;
    procedure DoSaveSettings; override;
  published
  public
    constructor Create(AOwner: TComponent); override;

    property Tree : TVirtualStringTree read GetTree write SetTree; 

    property SaveColumnWidth : Boolean index 0 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveColumnPos : Boolean index 1 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveColumnAllowClick : Boolean index 2 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveColumnDraggable : Boolean index 3 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveColumnEnabled : Boolean index 4 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveColumnParentColor : Boolean index 5 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveColumnResizable : Boolean index 6 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveColumnShowDropMark : Boolean index 7 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveColumnVisible : Boolean index 8 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveColumnAutoSpring : Boolean index 9 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveColumnFixed : Boolean index 10 read GetSaveVSTOption write SetSaveVSTOption default true;

    property SaveHeaderStyle : Boolean index 11 read GetSaveVSTOption write SetSaveVSTOption default true;
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

    property SaveColumnWidth;
    property SaveColumnPos;
    property SaveColumnAllowClick;
    property SaveColumnDraggable;
    property SaveColumnEnabled;
    property SaveColumnParentColor;
    property SaveColumnResizable;
    property SaveColumnShowDropMark;
    property SaveColumnVisible;
    property SaveColumnAutoSpring;
    property SaveColumnFixed;
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
  SettingsPath : TSettingName;

  procedure SetColumnOption(AOption : TVTColumnOption; ASettingName : TSettingName);
  begin
    Value := Settings.GetValue(SettingsPath + ASettingName);
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

  if Assigned(Component) and Assigned(Settings) then
  begin
    VST := TVirtualStringTree(Component);

    VST.BeginUpdate;
    try

      SettingsPath := FRootSetting + SettingsPathDelimiter +
                      'Header' + SettingsPathDelimiter;

      if SaveHeaderStyle then
      begin
        Value := Settings.GetValue(SettingsPath + 'Style');
        if not VarIsEmpty(Value) then
          VST.Header.Style := TVTHeaderStyle(Value);
      end;

      for idx := 0 to VST.Header.Columns.Count - 1 do
      begin
        Col := VST.Header.Columns[idx];

        SettingsPath := FRootSetting + SettingsPathDelimiter +
                             Format(ColSettingNamePattern, [Col.Index]) + SettingsPathDelimiter;

        if SaveColumnPos then
        begin
          Value := Settings.GetValue(SettingsPath + 'Position');
          if not VarIsEmpty(Value) then
            Col.Position := Value;
        end;

        if SaveColumnWidth then
        begin
          Value := Settings.GetValue(SettingsPath + 'Width');
          if not VarIsEmpty(Value) then
            Col.Width := Value;
        end;

        if SaveColumnAllowClick then
          SetColumnOption(coAllowClick, 'AllowClick');

        if SaveColumnAllowClick then
          SetColumnOption(coDraggable, 'Draggable');

        if SaveColumnAllowClick then
          SetColumnOption(coEnabled, 'Enabled');

        if SaveColumnAllowClick then
          SetColumnOption(coParentColor, 'ParentColor');

        if SaveColumnAllowClick then
          SetColumnOption(coResizable, 'Resizable');

        if SaveColumnAllowClick then
          SetColumnOption(coShowDropMark, 'ShowDropMark');

        if SaveColumnAllowClick then
          SetColumnOption(coVisible, 'Visible');

        if SaveColumnAllowClick then
          SetColumnOption(coAutoSpring, 'AutoSpring');

        if SaveColumnAllowClick then
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
  SettingsPath : TSettingName;

  procedure DoWriteColumnOption(AOption : TVTColumnOption; ASettingName : TSettingName);
  begin
    Settings.SetValue(SettingsPath + ASettingName, AOption in Col.Options)
  end;
begin
  inherited;

  if Assigned(Component) and Assigned(Settings) then
  begin
    VST := TVirtualStringTree(Component);

    SettingsPath := FRootSetting + SettingsPathDelimiter +
                    'Header' + SettingsPathDelimiter;

    if SaveHeaderStyle then
      Settings.SetValue(SettingsPath + 'Style', Integer(VST.Header.Style));

    for idx := 0 to VST.Header.Columns.Count - 1 do
    begin
      Col := VST.Header.Columns[idx];

      SettingsPath := FRootSetting + SettingsPathDelimiter +
                             Format(ColSettingNamePattern, [Col.Index]) + SettingsPathDelimiter;

      if SaveColumnPos then
        Settings.SetValue(SettingsPath + 'Position', Col.Position);

      if SaveColumnWidth then
        Settings.SetValue(SettingsPath + 'Width', Col.Width);

      if SaveColumnAllowClick then
          DoWriteColumnOption(coAllowClick, 'AllowClick');

      if SaveColumnAllowClick then
        DoWriteColumnOption(coDraggable, 'Draggable');

      if SaveColumnAllowClick then
        DoWriteColumnOption(coEnabled, 'Enabled');

      if SaveColumnAllowClick then
        DoWriteColumnOption(coParentColor, 'ParentColor');

      if SaveColumnAllowClick then
        DoWriteColumnOption(coResizable, 'Resizable');

      if SaveColumnAllowClick then
        DoWriteColumnOption(coShowDropMark, 'ShowDropMark');

      if SaveColumnAllowClick then
        DoWriteColumnOption(coVisible, 'Visible');

      if SaveColumnAllowClick then
        DoWriteColumnOption(coAutoSpring, 'AutoSpring');

      if SaveColumnAllowClick then
        DoWriteColumnOption(coFixed, 'Fixed');
    end;
    
  end;
end;

function TCustomSettingsLinkVST.GetSaveVSTOption(
  const Index: Integer): Boolean;
begin
  Result := FSaveVSTOptions[Index];
end;

function TCustomSettingsLinkVST.GetTree: TVirtualStringTree;
begin
  Result := TVirtualStringTree(Component);
end;

procedure TCustomSettingsLinkVST.SetSaveVSTOption(const Index: Integer;
  const Value: Boolean);
begin
  FSaveVSTOptions[Index] := Value;                                    
end;

procedure TCustomSettingsLinkVST.SetTree(const Value: TVirtualStringTree);
begin
  Component := Tree;
end;

end.
