unit uFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uSettingsBase, uSettingsLinksDefault, VirtualTrees, ToolWin,
  ComCtrls, uEffectPNGToolbar, uSettingsLinkVirtualTrees, ExtCtrls,
  DateUtils, JwaBits;

type

  TNodeType = (ntCast);

  TNodeData = record
    NodeType : TNodeType;
    Content : TObject; 
  end;
  PNodeData = ^TNodeData;

  TPodCast = class
  private
    FGUID : TGUID;
    FSettingsPath : TSettingName;

    procedure SetName(const Value: String);
    function GetName: String;
    function GetURL: String;
    procedure SetURL(const Value: String);
    function GetSaveFolder: String;
    procedure SetSaveFolder(const Value: String);
    function GetCheckInterval: Integer;
    procedure SetCheckInterval(const Value: Integer);
  public
    constructor Create(AGUID : TGUID);

    property Name : String read GetName write SetName;
    property URL : String read GetURL write SetURL;
    property SaveFolder : String read GetSaveFolder write SetSaveFolder;
    property CheckInterval : Integer read GetCheckInterval write SetCheckInterval;
  end;

  Tform_Main = class(TForm)
    SettingsLinkForm: TSettingsLinkForm;
    Toolbar: TEffectPNGToolBar;
    TV: TVirtualStringTree;
    SettingsLinkVST: TSettingsLinkVST;
    btn_AddPodCast: TEffectPNGToolButton;
    tm_Check: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure TVGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure btn_AddPodCastClick(Sender: TObject);
    procedure TVGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure TVAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    procedure tm_CheckTimer(Sender: TObject);
  private
    function GetNodeData(ANode : PVirtualNode; var ANodeData : PNodeData) : Boolean;

    function AddNode(AParent : PVirtualNode) : PVirtualNode;
    function AddCast(AGUID : TGUID) : PVirtualNode;

    procedure CheckCast(AGUID : TGUID);
  public
    { Public-Deklarationen }
  end;


var
  form_Main: Tform_Main;

implementation

uses uData, uFormEditCast;

{$R *.dfm}

function Tform_Main.AddCast(AGUID : TGUID): PVirtualNode;
var
  NData : PNodeData;
  Path : String;
begin
  Result := AddNode(nil);

  Path := '/Casts/' + GUIDToString(AGUID);

  if GetNodeData(Result, NData) then
  begin
    NData.NodeType := ntCast;
    Ndata.Text := Data.Settings.GetValue(Path + '/Name', '??');
    NData.State := 'unknown';
    Ndata.ProgressPercent := 0;
    NData.GUID := AGUID;
  end;
end;

function Tform_Main.AddNode(AParent: PVirtualNode): PVirtualNode;
begin
  Result := Tv.AddChild(AParent);
end;

procedure Tform_Main.btn_AddPodCastClick(Sender: TObject);
var
  CastID : TGUID;
begin
  CastID := CreateNewCastGUID;

  if not EditCast(CastID) then exit;
  
  tv.BeginUpdate;
  try
    AddCast(CastID);
  finally
    tv.EndUpdate;
  end;

end;

procedure Tform_Main.CheckCast(AGUID: TGUID);
var
  CastSettings : TSettingName;
  CastFile : String;
  Job : IBackgroundCopyJob;
  JobID : TGUID;
const
  NoJobID = '{7085974F-5A47-4AC5-AD92-B26E3460B3D3}';
begin
  CastSettings := '/Casts/' + GUIDToString(AGUID);
  CastFile := IncludeTrailingPathDelimiter(Data.Settings.GetValue(CastSettings + '/SaveFolder', null)) +
              'ZPodcast_' + GUIDToString(AGUID);

  JobID := StringToGUID(Data.Settings.GetValue(CastSettings + '/JobID', NoJobID));
 
end;

procedure Tform_Main.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SettingsLinkForm.SaveSettings;
end;

procedure Tform_Main.FormShow(Sender: TObject);
var
  Casts : TSettingNames;
  idx : Integer;
begin
  SettingsLinkForm.ApplySettings;

  Casts := Data.Settings.GetSubNames('/Casts');
  for idx := Low(Casts) to High(Casts) do
    AddCast(StringToGUID(Casts[idx]));    
end;

function Tform_Main.GetNodeData(ANode: PVirtualNode;
  var ANodeData: PNodeData): Boolean;
var
  Data : Pointer;
begin
  Result := Assigned(ANode);
  if Result then
  begin
    Data := tv.GetNodeData(ANode);

    Result := Assigned(Data);
    if Result then
    begin
      ANodeData := PNodeData(Data);
    end;
  end;
end;

procedure Tform_Main.tm_CheckTimer(Sender: TObject);
var
  Casts : TSettingNames;
  CastID : TGUID;
  idx : Integer;
  LastCheck : TDateTime;
  CheckInterval : Integer;
  Path :TSettingName;
begin
  Casts := Data.Settings.GetSubNames('/Casts');

  for idx := Low(Casts) to High(Casts) do
  begin
    CastID := StringToGUID(Casts[idx]);

    CheckInterval := Data.Settings.GetValue('/Casts/' + Casts[idx] + '/CheckInterval', 60);

    Path := '/Casts/' + Casts[idx] + '/LastCheck';
    if VarIsNull(Data.Settings.GetValue(Path, null)) then
    begin
      Data.Settings.SetValue(Path, now);
      LastCheck := now;
    end
    else
      LastCheck := Data.Settings.GetValue(Path, now);

    if MinutesBetween(LastCheck, Now) > CheckInterval then
      CheckCast(CastID); 
  end;
end;

procedure Tform_Main.TVAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
var
  NData : PNodeData;
  ProgressRect : TRect;
const
  Indent = 2;
begin
  if (Column = 3) and
     GetNodeData(Node, NData) then
  begin
    ProgressRect := Rect(CellRect.Left + Indent,
                         CellRect.Top + Indent,
                         Round((CellRect.Right - Indent - CellRect.Left + Indent) / (NData.ProgressPercent / 100)),
                         CellRect.Bottom - Indent);

    TargetCanvas.Brush.Color := clGreen;
    TargetCanvas.Brush.Style := bsSolid;

    TargetCanvas.FillRect(ProgressRect);
  end;
end;

procedure Tform_Main.TVGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNodeData);
end;

procedure Tform_Main.TVGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  NData : PNodeData;
begin
  if GetNodeData(Node, NData) then
  begin
    case Column of
      0 : CellText := Ndata.Text;
      1 : CellText := Ndata.State;
      2 : CellText := EmptyWideStr;  
    end;
  end;
end;

{ TPodCast }

constructor TPodCast.Create(AGUID: TGUID);
begin
  FGUID := AGUID;

  FSettingsPath := '/Casts/' + GUIDToString(AGUID);
end;

function TPodCast.GetCheckInterval: Integer;
begin
  Result := Data.Settings.GetValue(FSettingsPath + '/CheckInterval', 60);
end;

function TPodCast.GetName: String;
begin
  Result := Data.Settings.GetValue(FSettingsPath + '/Name', '??');
end;

function TPodCast.GetSaveFolder: String;
begin
  Result := data.Settings.GetValue(FSettingsPath + '/SaveFolder', '??');
end;

function TPodCast.GetURL: String;
begin
  Result := Data.Settings.GetValue(FSettingsPath + '/URL', '??');
end;

procedure TPodCast.SetCheckInterval(const Value: Integer);
begin
  Data.Settings.SetValue(FSettingsPath + '/CheckInterval', Value);
end;

procedure TPodCast.SetName(const Value: String);
begin
  Data.Settings.SetValue(FSettingsPath + '/Name', Value);
end;

procedure TPodCast.SetSaveFolder(const Value: String);
begin
  Data.Settings.SetValue(FSettingsPath + '/SaveFolder', Value);
end;

procedure TPodCast.SetURL(const Value: String);
begin
  Data.Settings.SetValue(FSettingsPath + '/URL', Value);
end;

end.
