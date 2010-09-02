unit uch2Provider3rdPartyHelp;

interface

uses
  Windows, Messages, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  uch2Main, Registry, HelpIntfs, StdCtrls, ExtCtrls, CheckLst, Mask, Spin;

type
  THelpViewerColorType = (hvctForeColor, hvctBackColor);
  THelpViewerNodeType = (hvntCategory, hvntItem);

  Tch2Provider3rdPartyHelp = class(TInterfacedObject, Ich2Provider)
  private
    FPriority : Integer;
    procedure SetPriority(APriority: Integer);
  protected
    function GetGUIDForHelpViewer(AName: String): TGUID;
    function GetColorForHelpViewer(AName: String; ANodeType: THelpViewerNodeType; AColorType: THelpViewerColorType): TColor;
    function GetFontStyleForHelpViewer(AName: String; ANodeType: THelpViewerNodeType): TFontStyles;
    procedure SetColorForHelpViewer(AName: String; ANodeType: THelpViewerNodeType; AColorType: THelpViewerColorType; AColor: TColor);
    procedure SetFontStyleForHelpViewer(AName: String; ANodeType: THelpViewerNodeType; AStyle: TFontStyles);
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
    gbOptions: TGroupBox;
    GroupBox2: TGroupBox;
    cbHeadFC: TColorBox;
    LabelHeader: TLabel;
    cbHeadBC: TColorBox;
    cbHeadFb: TCheckBox;
    cbHeadFi: TCheckBox;
    cbHeadFs: TCheckBox;
    cbHeadFu: TCheckBox;
    LabelItem: TLabel;
    cbItemFC: TColorBox;
    cbItemBC: TColorBox;
    cbItemFb: TCheckBox;
    cbItemFi: TCheckBox;
    cbItemFu: TCheckBox;
    cbItemFs: TCheckBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    Label1: TLabel;
    EditPrio: TSpinEdit;
    procedure clbProvidersClick(Sender: TObject);
    procedure Headerchanged(Sender: TObject);
    procedure ItemChanged(Sender: TObject);
    procedure clbProvidersClickCheck(Sender: TObject);
    procedure EditPrioChange(Sender: TObject);
  private
    FProvider : Tch2Provider3rdPartyHelp;

    procedure Init;
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
    function GetForeColor : TColor;
    function GetBackColor : TColor;
    function GetFlags : Tch2HelpItemFlags;
    function GetFontStyles : TFontStyles;
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
    function GetForeColor : TColor;
    function GetBackColor : TColor;
    function GetFlags : Tch2HelpItemFlags;
    function GetFontStyles : TFontStyles;
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
  REG_VALUE_COLOR_CAT_F = 'Category Foreground Color';
  REG_VALUE_COLOR_CAT_B = 'Category Background Color';
  REG_VALUE_COLOR_ITM_F = 'Item Foreground Color';
  REG_VALUE_COLOR_ITM_B = 'Item Background Color';
  REG_VALUE_FONTSTYLE_CAT = 'Category Font Style';
  REG_VALUE_FONTSTYLE_ITM = 'Item Font Style';
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

function Tch2Provider3rdPartyHelp.GetColorForHelpViewer(AName: String;
  ANodeType: THelpViewerNodeType;
  AColorType: THelpViewerColorType): TColor;
var
  Reg : TRegistry;
  Reg_Value_Color : String;
begin
  inherited;

  case ANodeType of
    hvntCategory: case AColorType of
                    hvctForeColor: begin
                                     Reg_Value_Color:=REG_VALUE_COLOR_CAT_F;
                                     Result:=clWindowText;
                                   end;
                    hvctBackColor: begin
                                     Reg_Value_Color:=REG_VALUE_COLOR_CAT_B;
                                     Result:=clWindow;
                                   end
                  end;
    hvntItem:     case AColorType of
                    hvctForeColor: begin
                                     Reg_Value_Color:=REG_VALUE_COLOR_ITM_F;
                                     Result:=clWindowText;
                                   end;
                    hvctBackColor: begin
                                     Reg_Value_Color:=REG_VALUE_COLOR_ITM_B;
                                     Result:=clWindow;
                                   end
                  end;
  end;
  

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_VIEWERS+'\'+AName, true) then
    begin
      if not Reg.ValueExists(Reg_Value_Color) then
      begin
        Reg.WriteString(Reg_Value_Color, ColorToString(Result));
      end
      else
        Result := StringToColor(reg.ReadString(Reg_Value_Color));

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function Tch2Provider3rdPartyHelp.GetDescription: String;
begin
  Result := 'Query other Help providers installed';
end;

function Tch2Provider3rdPartyHelp.GetFontStyleForHelpViewer(AName: String;
  ANodeType: THelpViewerNodeType): TFontStyles;
var
  Reg : TRegistry;
  Reg_Value_FontStyle : String;
begin
  inherited;

  case ANodeType of
    hvntCategory: begin
                    Result:=[fsBold];
                    Reg_Value_FontStyle:=REG_VALUE_FONTSTYLE_CAT;
                  end;
    hvntItem:     begin
                    Result:=[];
                    Reg_Value_FontStyle:=REG_VALUE_FONTSTYLE_ITM;
                  end;
  end;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_VIEWERS+'\'+AName, true) then
    begin
      if not Reg.ValueExists(Reg_Value_FontStyle) then
      begin
        Reg.WriteBinaryData(Reg_Value_FontStyle, Result, SizeOf(Result));
      end
      else
        reg.ReadBinaryData(Reg_Value_FontStyle, Result, SizeOf(Result));

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
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

procedure Tch2Provider3rdPartyHelp.SetColorForHelpViewer(AName: String;
  ANodeType: THelpViewerNodeType; AColorType: THelpViewerColorType;
  AColor: TColor);
var
  Reg : TRegistry;
  Reg_Value_Color : String;
begin
  inherited;

  case ANodeType of
    hvntCategory: case AColorType of
                    hvctForeColor: begin
                                     Reg_Value_Color:=REG_VALUE_COLOR_CAT_F;
                                   end;
                    hvctBackColor: begin
                                     Reg_Value_Color:=REG_VALUE_COLOR_CAT_B;
                                   end
                  end;
    hvntItem:     case AColorType of
                    hvctForeColor: begin
                                     Reg_Value_Color:=REG_VALUE_COLOR_ITM_F;
                                   end;
                    hvctBackColor: begin
                                     Reg_Value_Color:=REG_VALUE_COLOR_ITM_B;
                                   end
                  end;
  end;


  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_VIEWERS+'\'+AName, true) then
    begin
      Reg.WriteString(Reg_Value_Color, ColorToString(AColor));

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure Tch2Provider3rdPartyHelp.SetFontStyleForHelpViewer(AName: String;
  ANodeType: THelpViewerNodeType; AStyle: TFontStyles);
var
  Reg : TRegistry;
  Reg_Value_FontStyle : String;
begin
  inherited;

  case ANodeType of
    hvntCategory: begin
                    Reg_Value_FontStyle:=REG_VALUE_FONTSTYLE_CAT;
                  end;
    hvntItem:     begin
                    Reg_Value_FontStyle:=REG_VALUE_FONTSTYLE_ITM;
                  end;
  end;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_VIEWERS+'\'+AName, true) then
    begin
      Reg.WriteBinaryData(Reg_Value_FontStyle, AStyle , SizeOf(AStyle));


      Reg.CloseKey;
    end;
  finally
    Reg.Free;
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

procedure Tch2FormProvider3rdPartyHelp.Headerchanged(Sender: TObject);
var
  fs : TFontStyles;
  ViewerName : String;
begin
  LabelHeader.Font.Color:=cbHeadFC.Selected;
  LabelHeader.Color:=cbHeadBC.Selected; 
  fs:=[];
  if cbHeadFb.Checked then
    Include(fs, fsBold);  
  if cbHeadFi.Checked then
    Include(fs, fsItalic);
  if cbHeadFu.Checked then
    Include(fs, fsUnderline);
  if cbHeadFs.Checked then
    Include(fs, fsStrikeOut);
  LabelHeader.Font.Style:=fs;

  if clbProviders.ItemIndex>=0 then
  begin
    ViewerName:=THelpViewerNodeHack(clbProviders.Items.Objects[clbProviders.ItemIndex]).FViewer.GetViewerName;
    FProvider.SetColorForHelpViewer(ViewerName,hvntCategory, hvctForeColor, cbHeadFC.Selected);
    FProvider.SetColorForHelpViewer(ViewerName,hvntCategory, hvctBackColor, cbHeadBC.Selected);
    FProvider.SetFontStyleForHelpViewer(ViewerName, hvntCategory, fs);
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


procedure Tch2FormProvider3rdPartyHelp.ItemChanged(Sender: TObject);
var
  fs : TFontStyles;
  ViewerName : String;
begin
  LabelItem.Font.Color:=cbItemFC.Selected;
  LabelItem.Color:=cbItemBC.Selected; 
  fs:=[];
  if cbItemFb.Checked then
    Include(fs, fsBold);  
  if cbItemFi.Checked then
    Include(fs, fsItalic);
  if cbItemFu.Checked then
    Include(fs, fsUnderline);
  if cbItemFs.Checked then
    Include(fs, fsStrikeOut);
  LabelItem.Font.Style:=fs;

  if clbProviders.ItemIndex>=0 then
  begin
    ViewerName:=THelpViewerNodeHack(clbProviders.Items.Objects[clbProviders.ItemIndex]).FViewer.GetViewerName;
    FProvider.SetColorForHelpViewer(ViewerName,hvntItem, hvctForeColor, cbItemFC.Selected);
    FProvider.SetColorForHelpViewer(ViewerName,hvntItem, hvctBackColor, cbItemBC.Selected);
    FProvider.SetFontStyleForHelpViewer(ViewerName, hvntItem, fs);
  end;
end;

procedure Tch2FormProvider3rdPartyHelp.clbProvidersClick(Sender: TObject);
var
  idx: Integer;
  ViewerName : String;
  fs : TFontStyles;
begin
  for idx := 0 to gbOptions.ControlCount - 1 do
    gbOptions.Controls[idx].Enabled:=clbProviders.ItemIndex>=0;

  if clbProviders.ItemIndex>=0 then
  begin
    ViewerName:=THelpViewerNodeHack(clbProviders.Items.Objects[clbProviders.ItemIndex]).FViewer.GetViewerName;

    cbHeadFC.Selected:=FProvider.GetColorForHelpViewer(ViewerName, hvntCategory, hvctForeColor);
    cbHeadBC.Selected:=FProvider.GetColorForHelpViewer(ViewerName, hvntCategory, hvctBackColor);
    cbItemFC.Selected:=FProvider.GetColorForHelpViewer(ViewerName, hvntItem, hvctForeColor);
    cbItemBC.Selected:=FProvider.GetColorForHelpViewer(ViewerName, hvntItem, hvctBackColor);

    fs:=FProvider.GetFontStyleForHelpViewer(ViewerName, hvntCategory);
    cbHeadFb.Checked:= fsBold in fs;
    cbHeadFu.Checked:= fsUnderline in fs;
    cbHeadFi.Checked:= fsItalic in fs;
    cbHeadFs.Checked:= fsStrikeOut in fs;

    fs:=FProvider.GetFontStyleForHelpViewer(ViewerName, hvntItem);
    cbItemFb.Checked:= fsBold in fs;
    cbItemFu.Checked:= fsUnderline in fs;
    cbItemFi.Checked:= fsItalic in fs;
    cbItemFs.Checked:= fsStrikeOut in fs;

    Headerchanged(nil);
    ItemChanged(nil);
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

function Tch2Provider3rdPartyHelpItemCategory.GetBackColor: TColor;
begin
  Result:=FProvider.GetColorForHelpViewer(FHelpViewer.GetViewerName, hvntCategory, hvctBackColor);
end;

function Tch2Provider3rdPartyHelpItemCategory.GetCaption: String;
begin
  Result:=FHelpViewer.GetViewerName;
end;

function Tch2Provider3rdPartyHelpItemCategory.GetDescription: String;
begin
  Result:='';
end;

function Tch2Provider3rdPartyHelpItemCategory.GetFlags: Tch2HelpItemFlags;
begin
  Result:=[ifSaveStats, ifHasForeColor, ifHasBackColor, ifHasFontStyles]
end;

function Tch2Provider3rdPartyHelpItemCategory.GetFontStyles: TFontStyles;
begin

end;

function Tch2Provider3rdPartyHelpItemCategory.GetForeColor: TColor;
begin
  Result:=FProvider.GetColorForHelpViewer(FHelpViewer.GetViewerName, hvntCategory, hvctForeColor);
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

function Tch2Provider3rdPartyHelpItemItem.GetBackColor: TColor;
begin
  Result:=FProvider.GetColorForHelpViewer(FHelpViewer.GetViewerName,
                                          hvntItem,
                                          hvctBackColor);
end;

function Tch2Provider3rdPartyHelpItemItem.GetCaption: String;
begin
  Result:=FHelpString;
end;

function Tch2Provider3rdPartyHelpItemItem.GetDescription: String;
begin
  Result:='Help provided by '+FHelpViewer.GetViewerName;
end;

function Tch2Provider3rdPartyHelpItemItem.GetFlags: Tch2HelpItemFlags;
begin
  Result:=[ifHasForeColor, ifHasBackColor, ifHasFontStyles, ifProvidesHelp];
end;

function Tch2Provider3rdPartyHelpItemItem.GetFontStyles: TFontStyles;
begin
  Result:=FProvider.GetFontStyleForHelpViewer(FHelpViewer.GetViewerName, hvntItem);
end;

function Tch2Provider3rdPartyHelpItemItem.GetForeColor: TColor;
begin
  Result:=FProvider.GetColorForHelpViewer(FHelpViewer.GetViewerName,
                                          hvntItem,
                                          hvctForeColor);
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
