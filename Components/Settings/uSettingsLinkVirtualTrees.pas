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
    FSaveVSTOptions : array[0..23] of Boolean;

    function GetTree: TVirtualStringTree;
    procedure SetTree(const Value: TVirtualStringTree);

    function GetSaveVSTOption(const Index: Integer): Boolean;
    procedure SetSaveVSTOption(const Index: Integer; const Value: Boolean);

    procedure DoApplySettings(const ARootSetting : TSettingName); override;
    procedure DoSaveSettings(const ARootSetting : TSettingName); override;
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

    property SaveHeaderAutoResize : Boolean index 12 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveHeaderColumnResize : Boolean index 13 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveHeaderDblClickResize : Boolean index 14 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveHeaderDrag : Boolean index 15 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveHeaderHotTrack : Boolean index 16 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveHeaderOwnerDraw : Boolean index 17 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveHeaderRestrictDrag : Boolean index 18 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveHeaderShowHint : Boolean index 19 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveHeaderShowImages : Boolean index 20 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveHeaderShowSortGlyphs : Boolean index 21 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveHeaderVisible : Boolean index 22 read GetSaveVSTOption write SetSaveVSTOption default true;
    property SaveHeaderAutoSpring : Boolean index 23 read GetSaveVSTOption write SetSaveVSTOption default true;
  end;


//==============================================================================


  TSettingsLinkVST = class(TCustomSettingsLinkVST)
  published
    property Settings;
    property DefaultRootSetting;

    property SaveLeft;
    property SaveTop;
    property SaveWidth;
    property SaveHeight;

    property Tree;

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

    property SaveHeaderStyle;

    property SaveHeaderAutoResize;
    property SaveHeaderColumnResize;
    property SaveHeaderDblClickResize;
    property SaveHeaderDrag;
    property SaveHeaderHotTrack;
    property SaveHeaderOwnerDraw;
    property SaveHeaderRestrictDrag;
    property SaveHeaderShowHint;
    property SaveHeaderShowImages;
    property SaveHeaderShowSortGlyphs;
    property SaveHeaderVisible;
    property SaveHeaderAutoSpring;
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

procedure TCustomSettingsLinkVST.DoApplySettings(const ARootSetting : TSettingName);
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

  procedure SetHeaderOption(AOption : TVTHeaderOption; ASettingName : TSettingName);
  begin
    Value := Settings.GetValue(SettingsPath + ASettingName);
    if not VarIsEmpty(Value) then
    begin
      if Boolean(Value) then
        VST.Header.Options := VST.Header.Options + [AOption]
      else
        VST.Header.Options := VST.Header.Options - [AOption];
    end;
  end;
begin
  inherited;

  if Assigned(Component) and Assigned(Settings) then
  begin
    VST := TVirtualStringTree(Component);

    VST.BeginUpdate;
    try

      SettingsPath := ARootSetting + SettingsPathDelimiter +
                      'Header' + SettingsPathDelimiter;

      if SaveHeaderStyle then
      begin
        Value := Settings.GetValue(SettingsPath + 'Style');
        if not VarIsEmpty(Value) then
          VST.Header.Style := TVTHeaderStyle(Value);
      end;

      if SaveHeaderAutoResize then
        SetHeaderOption(hoAutoResize, 'AutoResize');

      if SaveHeaderColumnResize then
        SetHeaderOption(hoColumnResize, 'ColumnResize');

      if SaveHeaderDblClickResize then
        SetHeaderOption(hoDblClickResize, 'DblClickResize');

      if SaveHeaderDrag then
        SetHeaderOption(hoDrag, 'Drag');
        
      if SaveHeaderHotTrack then
        SetHeaderOption(hoHotTrack, 'HotTrack');

      if SaveHeaderOwnerDraw then
        SetHeaderOption(hoOwnerDraw, 'OwnerDraw');

      if SaveHeaderRestrictDrag then
        SetHeaderOption(hoRestrictDrag, 'RestrictDrag');

      if SaveHeaderShowHint then
        SetHeaderOption(hoShowHint, 'ShowHint');

      if SaveHeaderShowImages then
        SetHeaderOption(hoShowImages, 'ShowImages');

      if SaveHeaderShowSortGlyphs then
        SetHeaderOption(hoShowSortGlyphs, 'ShowSortGlyphs');

      if SaveHeaderVisible then
        SetHeaderOption(hoVisible, 'Visible');

      if SaveHeaderAutoSpring then
        SetHeaderOption(hoAutoSpring, 'AutoSpring');

      for idx := 0 to VST.Header.Columns.Count - 1 do
      begin
        Col := VST.Header.Columns[idx];

        SettingsPath := ARootSetting + SettingsPathDelimiter +
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

procedure TCustomSettingsLinkVST.DoSaveSettings(const ARootSetting : TSettingName);
var
  VST : TVirtualStringTree;
  idx : Integer;
  Col : TVirtualTreeColumn;
  SettingsPath : TSettingName;

  procedure DoWriteColumnOption(AOption : TVTColumnOption; ASettingName : TSettingName);
  begin
    Settings.SetValue(SettingsPath + ASettingName, AOption in Col.Options)
  end;

  procedure DoWriteHeaderOption(AOption : TVTHeaderOption; ASettingName : TSettingName);
  begin
    Settings.SetValue(SettingsPath + ASettingName, AOption in VST.Header.Options)
  end;
begin
  inherited;

  if Assigned(Component) and Assigned(Settings) then
  begin
    VST := TVirtualStringTree(Component);

    SettingsPath := ARootSetting + SettingsPathDelimiter +
                    'Header' + SettingsPathDelimiter;

    if SaveHeaderStyle then
      Settings.SetValue(SettingsPath + 'Style', Integer(VST.Header.Style));

    if SaveHeaderAutoResize then
      DoWriteHeaderOption(hoAutoResize, 'AutoResize');

    if SaveHeaderColumnResize then
      DoWriteHeaderOption(hoColumnResize, 'ColumnResize');

    if SaveHeaderDblClickResize then
      DoWriteHeaderOption(hoDblClickResize, 'DblClickResize');

    if SaveHeaderDrag then
      DoWriteHeaderOption(hoDrag, 'Drag');

    if SaveHeaderHotTrack then
      DoWriteHeaderOption(hoHotTrack, 'HotTrack');

    if SaveHeaderOwnerDraw then
      DoWriteHeaderOption(hoOwnerDraw, 'OwnerDraw');

    if SaveHeaderRestrictDrag then
      DoWriteHeaderOption(hoRestrictDrag, 'RestrictDrag');

    if SaveHeaderShowHint then
      DoWriteHeaderOption(hoShowHint, 'ShowHint');

    if SaveHeaderShowImages then
      DoWriteHeaderOption(hoShowImages, 'ShowImages');

    if SaveHeaderShowSortGlyphs then
      DoWriteHeaderOption(hoShowSortGlyphs, 'ShowSortGlyphs');

    if SaveHeaderVisible then
      DoWriteHeaderOption(hoVisible, 'Visible');

    if SaveHeaderAutoSpring then
      DoWriteHeaderOption(hoAutoSpring, 'AutoSpring');

    for idx := 0 to VST.Header.Columns.Count - 1 do
    begin
      Col := VST.Header.Columns[idx];

      SettingsPath := ARootSetting + SettingsPathDelimiter +
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
  Component := Value;
end;

end.
