{-----------------------------------------------------------------------------
 Project: Settings_VirtualTrees
 Purpose: Contains Settings support for the VirtualTree components 
 Created: 21.05.2008 14:47:02
 
 (c) by TheUnknownOnes under dwywbdbu license - see http://theunknownones.googlecode.com/svn/ for the license
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
  TCustomSettingsLinkVST = class(TCustomSettingsComponentLink)
  protected
    FSaveVSTOptions : array[0..12] of Boolean;

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

    property SaveColumnTitle : Boolean index 11 read GetSaveVSTOption write SetSaveVSTOption default false;
    property SaveColumnColor : Boolean index 12 read GetSaveVSTOption write SetSaveVSTOption default false;
  end;


//==============================================================================


  TSettingsLinkVST = class(TCustomSettingsLinkVST)
  published
    property Settings;
    property DefaultRootSetting;
    property OnNeedRootSetting;
    property SaveProperties;
    property Active;

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
    property SaveColumnTitle;
    property SaveColumnColor;
  end;


//==============================================================================


implementation

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

  FSaveVSTOptions[11]:=false; // no auto title saving
  FSaveVSTOptions[12]:=false; // no auto color saving
end;

procedure TCustomSettingsLinkVST.DoApplySettings(const ARootSetting : TSettingName);
var
  Value : Variant;
  VST : TVirtualStringTree;
  idx : Integer;
  Col : TVirtualTreeColumn;
  SettingsPath : TSettingName;
  Positions : array of Integer;

  procedure SetColumnOption(AOption : TVTColumnOption; ASettingName : TSettingName);
  begin
    Value := Settings.GetValue(SettingsPath + ASettingName, AOption in Col.Options);
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

      if SaveColumnPos then
      begin
        {$REGION 'Set positions'}
        SetLength(Positions, VST.Header.Columns.Count);
        for idx := Low(Positions) to High(Positions) do
          Positions[idx] := MaxInt;

        //Save the colum indices to the Positions-array
        for idx := 0 to VST.Header.Columns.Count - 1 do
        begin
          Col := VST.Header.Columns[idx];

          SettingsPath := ARootSetting + SettingsPathDelimiter +
                               Format(ColSettingNamePattern, [Col.Index]) + SettingsPathDelimiter;


          Value := Settings.GetValue(SettingsPath + 'Position', Col.Position);

          if not VarIsEmpty(Value) then
          begin
            if (Integer(Value) >= Low(Positions)) and
               (Integer(Value) <= High(Positions)) then
            Positions[Integer(Value)] := idx;
          end;
        end;

        //apply the positons
        for idx := Low(Positions) to High(Positions) do
        begin
          if Positions[idx] <> MaxInt then
            Vst.Header.Columns[Positions[idx]].Position := idx;
        end;
        {$ENDREGION}
      end;

      for idx := 0 to VST.Header.Columns.Count - 1 do
      begin
        Col := VST.Header.Columns[idx];

        SettingsPath := ARootSetting + SettingsPathDelimiter +
                             Format(ColSettingNamePattern, [Col.Index]) + SettingsPathDelimiter;

        if SaveColumnWidth then
        begin
          Value := Settings.GetValue(SettingsPath + 'Width', Col.Width);
          if not VarIsEmpty(Value) then
            Col.Width := Value;
        end;

        if SaveColumnAllowClick then
          SetColumnOption(coAllowClick, 'AllowClick');

        if SaveColumnDraggable then
          SetColumnOption(coDraggable, 'Draggable');

        if SaveColumnEnabled then
          SetColumnOption(coEnabled, 'Enabled');

        if SaveColumnParentColor then
          SetColumnOption(coParentColor, 'ParentColor');

        if SaveColumnResizable then
          SetColumnOption(coResizable, 'Resizable');

        if SaveColumnShowDropMark then
          SetColumnOption(coShowDropMark, 'ShowDropMark');

        if SaveColumnVisible then
          SetColumnOption(coVisible, 'Visible');

        if SaveColumnAutoSpring then
          SetColumnOption(coAutoSpring, 'AutoSpring');

        if SaveColumnFixed then
          SetColumnOption(coFixed, 'Fixed');

        if SaveColumnTitle then
          Col.Text:=Settings.GetValue(SettingsPath + 'Text', Col.Text);

        if SaveColumnColor then
          Col.Color:=Settings.GetValue(SettingsPath + 'Color', Col.Color);
      end;


    finally
      VST.EndUpdate;
      SetLength(Positions, 0);
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
begin
  inherited;

  if Assigned(Component) and Assigned(Settings) then
  begin
    VST := TVirtualStringTree(Component);



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

      if SaveColumnDraggable then
        DoWriteColumnOption(coDraggable, 'Draggable');

      if SaveColumnEnabled then
        DoWriteColumnOption(coEnabled, 'Enabled');

      if SaveColumnParentColor then
        DoWriteColumnOption(coParentColor, 'ParentColor');

      if SaveColumnResizable then
        DoWriteColumnOption(coResizable, 'Resizable');

      if SaveColumnShowDropMark then
        DoWriteColumnOption(coShowDropMark, 'ShowDropMark');

      if SaveColumnVisible then
        DoWriteColumnOption(coVisible, 'Visible');

      if SaveColumnAutoSpring then
        DoWriteColumnOption(coAutoSpring, 'AutoSpring');

      if SaveColumnFixed then
        DoWriteColumnOption(coFixed, 'Fixed');

      if SaveColumnTitle then
          Settings.SetValue(SettingsPath + 'Text', Col.Text);

      if SaveColumnColor then
          Settings.SetValue(SettingsPath + 'Color', Col.Color);
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
