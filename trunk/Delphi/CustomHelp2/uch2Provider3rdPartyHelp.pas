unit uch2Provider3rdPartyHelp;

interface

uses
  Windows, Messages, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  uch2Main, Registry, HelpIntfs, StdCtrls, ExtCtrls, CheckLst, Mask, Spin,
  uch2FrameHelpItemDecoration;

type
  Tch2Provider3rdPartyHelp = class(TInterfacedObject, Ich2Provider)
  private
    FPriority : Integer;
    procedure SetPriority(APriority: Integer);
  protected
    function GetGUIDForHelpViewer(AName: String): TGUID;

    function GetDecoration(AName: String): Tch2HelpItemDecoration;
    procedure SetDecoration(AName: String; ADeco: Tch2HelpItemDecoration);
    function GetVisibilityForHelpViewer(AName: String): Boolean;
    procedure SetVisibilityForHelpViewer(AName: String; AVisible: Boolean);
  public
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;

    {$REGION 'Ich2Provider'}
    function GetGUID : TGUID;
    function GetDescription : String;
    function GetName : String;

    procedure ProvideHelp(AKeyword : String; AGUI : Ich2GUI);

    procedure Configure;

    function GetPriority : Integer;
    {$ENDREGION}
  end;

  Tch2FormProvider3rdPartyHelp = class(TForm)
    clbProviders: TCheckListBox;
    GroupBox2: TGroupBox;
    Panel1: TPanel;
    Label1: TLabel;
    EditPrio: TSpinEdit;
    GroupBox1: TGroupBox;
    FrameHelpItemDeco: Tch2FrameHelpItemDecoration;
    Panel2: TPanel;
    btn_OK: TButton;
    procedure clbProvidersClick(Sender: TObject);
    procedure clbProvidersClickCheck(Sender: TObject);
    procedure EditPrioChange(Sender: TObject);
  private
    FProvider : Tch2Provider3rdPartyHelp;

    procedure Init;
    procedure OnDecoChange(ASender: TObject);
  public
    class procedure Execute(AProvider: Tch2Provider3rdPartyHelp);
  end;

implementation

uses
  SysUtils;

{$R *.dfm}

function GetImplementingObject(const I: IInterface): TObject;
const
  AddByte = $04244483;
  AddLong = $04244481;
type
  PAdjustSelfThunk = ^TAdjustSelfThunk;
  TAdjustSelfThunk = packed record
    case AddInstruction: longint of
      AddByte : (AdjustmentByte: shortint);
      AddLong : (AdjustmentLong: longint);
    end;
  PInterfaceMT = ^TInterfaceMT;
  TInterfaceMT = packed record
    QueryInterfaceThunk: PAdjustSelfThunk;
  end;
  TInterfaceRef = ^PInterfaceMT;
var
  QueryInterfaceThunk: PAdjustSelfThunk;
begin
  Result := Pointer(I);
  if Assigned(Result) then
    try
      QueryInterfaceThunk := TInterfaceRef(I)^.QueryInterfaceThunk;
      case QueryInterfaceThunk.AddInstruction of
        AddByte: Inc(PAnsiChar(Result), QueryInterfaceThunk.AdjustmentByte);
        AddLong: Inc(PAnsiChar(Result), QueryInterfaceThunk.AdjustmentLong);
      else
        Result := nil;
      end;
    except
      Result := nil;
    end;
end;

type
  THelpManagerHack = class(TInterfacedObject)
  private
    FHelpSelector: IHelpSelector;
    FViewerList: TList;
  end;

  THelpViewerNodeHack = class(TObject)
  private
    FViewer: ICustomHelpViewer;
  end;

  Tch2Provider3rdPartyHelpItemCategory = class(TInterfacedObject, Ich2HelpItem)
  private
    FHelpViewer : ICustomHelpViewer;
    FProvider : Tch2Provider3rdPartyHelp;
  public
    {$REGION 'Ich2HelpItem'}
    function GetGUID : TGUID; //used to store stats (expanded, position, ...)
    function GetCaption : String;
    function GetDescription : String;
    function GetDecoration : Tch2HelpItemDecoration;
    function GetFlags : Tch2HelpItemFlags;
    procedure ShowHelp;
    {$ENDREGION}

    constructor Create(AProvider: Tch2Provider3rdPartyHelp;
                       AHelpViewer: ICustomHelpViewer);
  end;

  Tch2Provider3rdPartyHelpItemItem = class(TInterfacedObject, Ich2HelpItem)
  private
    FHelpViewer : ICustomHelpViewer;
    FProvider : Tch2Provider3rdPartyHelp;
    FHelpString : String;
  public
    {$REGION 'Ich2HelpItem'}
    function GetGUID : TGUID; //used to store stats (expanded, position, ...)
    function GetCaption : String;
    function GetDescription : String;
    function GetDecoration : Tch2HelpItemDecoration;
    function GetFlags : Tch2HelpItemFlags;
    procedure ShowHelp;
    {$ENDREGION}

    constructor Create(AProvider: Tch2Provider3rdPartyHelp;
                       AHelpViewer: ICustomHelpViewer;
                       AHelpString: String);
  end;

function GetHelpManagerHackObject: THelpManagerHack;
begin
  Result:=THelpManagerHack(GetImplementingObject(ch2Main.HelpManager));
end;

{ Tch2Provider3rdPartyHelp }

const
  REG_VALUE_PRIORITY = 'Priority';
  REG_KEY_VIEWERS = 'Viewers';
  REG_VALUE_GUID = 'GUID';
  REG_VALUE_VISIBLE = 'Visible';

procedure Tch2Provider3rdPartyHelp.AfterConstruction;
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID], true) then
    begin
      if Reg.ValueExists(REG_VALUE_PRIORITY) then
        FPriority := reg.ReadInteger(REG_VALUE_PRIORITY)
      else
        FPriority := 0;

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure Tch2Provider3rdPartyHelp.SetPriority(APriority: Integer);
var
  Reg : TRegistry;
begin
  FPriority:=APriority;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID], true) then
    begin
      Reg.WriteInteger(REG_VALUE_PRIORITY, FPriority);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure Tch2Provider3rdPartyHelp.BeforeDestruction;
begin
  SetPriority(FPriority);

  inherited;
end;

procedure Tch2Provider3rdPartyHelp.Configure;
begin
  Tch2FormProvider3rdPartyHelp.Execute(Self);
end;


function Tch2Provider3rdPartyHelp.GetDescription: String;
begin
  Result := 'Query other Help providers installed';
end;

function Tch2Provider3rdPartyHelp.GetGUID: TGUID;
const
  g : TGUID = '{A40591A4-1CAC-4F31-A222-EC71AAC0622C}';
begin
  Result := g;
end;

function Tch2Provider3rdPartyHelp.GetGUIDForHelpViewer(AName: String): TGUID;
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_VIEWERS+'\'+AName, true) then
    begin
      if not Reg.ValueExists(REG_VALUE_GUID) then
      begin
        CreateGUID(Result);
        Reg.WriteString(REG_VALUE_GUID, GUIDToString(Result));
      end
      else
        Result := StringToGUID(reg.ReadString(REG_VALUE_GUID));

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function Tch2Provider3rdPartyHelp.GetName: String;
begin
  Result:='3rd party help';
end;

function Tch2Provider3rdPartyHelp.GetPriority: Integer;
begin
  Result:=FPriority;
end;

function Tch2Provider3rdPartyHelp.GetDecoration(AName: String): Tch2HelpItemDecoration;
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_VIEWERS+'\'+AName, true) then
    begin
      Result.LoadFromRegistry(Reg);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure Tch2Provider3rdPartyHelp.SetDecoration(AName: String; ADeco: Tch2HelpItemDecoration);
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_VIEWERS+'\'+AName, true) then
    begin
      ADeco.SaveToRegistry(Reg);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function Tch2Provider3rdPartyHelp.GetVisibilityForHelpViewer(
  AName: String): Boolean;
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_VIEWERS+'\'+AName, true) then
    begin
      if not Reg.ValueExists(REG_VALUE_VISIBLE) then
      begin
        Reg.WriteBool(REG_VALUE_VISIBLE, True);
      end;

      Result:=reg.ReadBool(REG_VALUE_VISIBLE);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;


procedure Tch2Provider3rdPartyHelp.ProvideHelp(AKeyword: String; AGUI: Ich2GUI);
var
  ViewerList : TList;
  P : Pointer;
  Node : THelpViewerNodeHack absolute P;
  HelpStrings: TStrings;
  Parent : Pointer;
  idx: Integer;
begin
  ViewerList:=GetHelpManagerHackObject.FViewerList;

  for P in ViewerList do
  begin
    if (Node.FViewer.GetViewerName<>CH2HelpViewerName) and
       (GetVisibilityForHelpViewer(Node.FViewer.GetViewerName)) then
    begin
      if Node.FViewer.UnderstandsKeyword(AKeyword)>0 then
      begin
        Parent:=AGUI.AddHelpItem(Tch2Provider3rdPartyHelpItemCategory.
                                                    Create(Self,
                                                           Node.FViewer));
        HelpStrings:=Node.FViewer.GetHelpStrings(AKeyword);
        try
          for idx := 0 to HelpStrings.Count - 1 do
            AGUI.AddHelpItem(Tch2Provider3rdPartyHelpItemItem.
                                                    Create(Self,
                                                           Node.FViewer,
                                                           HelpStrings[idx]),
                             Parent);
        finally
          HelpStrings.Free;
        end;
      end;
    end;
  end;

end;

procedure Tch2Provider3rdPartyHelp.SetVisibilityForHelpViewer(AName: String;
  AVisible: Boolean);
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_VIEWERS+'\'+AName, true) then
    begin
      Reg.WriteBool(REG_VALUE_VISIBLE, AVisible);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

{ Tch2FormProvider3rdPartyHelp }

procedure Tch2FormProvider3rdPartyHelp.clbProvidersClickCheck(Sender: TObject);
var
  idx: Integer;
begin
  for idx := 0 to clbProviders.Count - 1 do
  begin
    FProvider.SetVisibilityForHelpViewer(clbProviders.Items[idx], clbProviders.Checked[idx]);
  end;
end;

procedure Tch2FormProvider3rdPartyHelp.EditPrioChange(Sender: TObject);
begin
  FProvider.SetPriority(EditPrio.Value);
end;

class procedure Tch2FormProvider3rdPartyHelp.Execute(AProvider: Tch2Provider3rdPartyHelp);
var
  Form: Tch2FormProvider3rdPartyHelp;
begin
  Form:=Tch2FormProvider3rdPartyHelp.Create(nil);
  try
    Form.FProvider:=AProvider;
    Form.Init;
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

procedure Tch2FormProvider3rdPartyHelp.Init;
var
  ViewerList : TList;
  P : Pointer;
  Node : THelpViewerNodeHack absolute P;
  HelpStrings: TStrings;
  Parent : Pointer;
  idx: Integer;
begin
  FrameHelpItemDeco.OnChange:=onDecoChange;
  ViewerList:=GetHelpManagerHackObject.FViewerList;
  EditPrio.Value:=FProvider.GetPriority;
  clbProviders.Items.BeginUpdate;
  try

    for P in ViewerList do
    begin
      if Node.FViewer.GetViewerName<>CH2HelpViewerName then
      begin
        clbProviders.AddItem(Node.FViewer.GetViewerName, Node);
      end;
    end;

    for idx := 0 to clbProviders.Count - 1 do
    begin
      clbProviders.Checked[idx]:=FProvider.GetVisibilityForHelpViewer(clbProviders.Items[idx]);
    end;

  finally
    clbProviders.Items.EndUpdate
  end;

  clbProviders.OnClick(nil);
end;


procedure Tch2FormProvider3rdPartyHelp.OnDecoChange(ASender: TObject);
var
  ViewerName : String;
begin
  if clbProviders.ItemIndex>=0 then
  begin
    ViewerName:=THelpViewerNodeHack(clbProviders.Items.Objects[clbProviders.ItemIndex]).FViewer.GetViewerName;

    FProvider.SetDecoration(ViewerName, FrameHelpItemDeco.Decoration);
  end;
end;

procedure Tch2FormProvider3rdPartyHelp.clbProvidersClick(Sender: TObject);
var
  idx: Integer;
  ViewerName : String;
  fs : TFontStyles;
begin
  for idx := 0 to FrameHelpItemDeco.ControlCount - 1 do
    FrameHelpItemDeco.Controls[idx].Enabled:=clbProviders.ItemIndex>=0;

  if clbProviders.ItemIndex>=0 then
  begin
    ViewerName:=THelpViewerNodeHack(clbProviders.Items.Objects[clbProviders.ItemIndex]).FViewer.GetViewerName;

    FrameHelpItemDeco.Decoration:=FProvider.GetDecoration(ViewerName);
  end;
end;

{ Tch2Provider3rdPartyHelpItemCategory }

constructor Tch2Provider3rdPartyHelpItemCategory.Create(
  AProvider: Tch2Provider3rdPartyHelp;
  AHelpViewer: ICustomHelpViewer);
begin
  inherited Create();
  FHelpViewer:=AHelpViewer;
  FProvider:=AProvider;
end;

function Tch2Provider3rdPartyHelpItemCategory.GetCaption: String;
begin
  Result:=FHelpViewer.GetViewerName;
end;

function Tch2Provider3rdPartyHelpItemCategory.GetDecoration: Tch2HelpItemDecoration;
begin
  Result:=FProvider.GetDecoration(FHelpViewer.GetViewerName);
end;

function Tch2Provider3rdPartyHelpItemCategory.GetDescription: String;
begin
  Result:='';
end;

function Tch2Provider3rdPartyHelpItemCategory.GetFlags: Tch2HelpItemFlags;
begin
  Result:=[ifSaveStats]
end;

function Tch2Provider3rdPartyHelpItemCategory.GetGUID: TGUID;
begin
  Result:=FProvider.GetGUIDForHelpViewer(FHelpViewer.GetViewerName);
end;

procedure Tch2Provider3rdPartyHelpItemCategory.ShowHelp;
begin
end;

{ Tch2Provider3rdPartyHelpItemItem }

constructor Tch2Provider3rdPartyHelpItemItem.Create(
  AProvider: Tch2Provider3rdPartyHelp; AHelpViewer: ICustomHelpViewer;
  AHelpString: String);
begin
  inherited Create();
  FProvider:=AProvider;
  FHelpViewer:=AHelpViewer;
  FHelpString:=AHelpString;
end;


function Tch2Provider3rdPartyHelpItemItem.GetCaption: String;
begin
  Result:=FHelpString;
end;

function Tch2Provider3rdPartyHelpItemItem.GetDecoration: Tch2HelpItemDecoration;
begin
  Result:=FProvider.GetDecoration(FHelpViewer.GetViewerName);
end;

function Tch2Provider3rdPartyHelpItemItem.GetDescription: String;
begin
  Result:='Help provided by '+FHelpViewer.GetViewerName;
end;

function Tch2Provider3rdPartyHelpItemItem.GetFlags: Tch2HelpItemFlags;
begin
  Result:=[ifProvidesHelp];
end;

function Tch2Provider3rdPartyHelpItemItem.GetGUID: TGUID;
begin
end;

procedure Tch2Provider3rdPartyHelpItemItem.ShowHelp;
begin
  FHelpViewer.ShowHelp(FHelpString);
end;


initialization
  ch2Main.RegisterProvider(Tch2Provider3rdPartyHelp.Create as Ich2Provider);

end.
