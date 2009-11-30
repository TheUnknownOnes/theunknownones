{-----------------------------------------------------------------------------
 Purpose: The config dialog of the custom help expert 
 
 (c) by TheUnknownOnes under dwywbdbu license - see http://theunknownones.googlecode.com/svn/ for the license
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uFormConfigCustomHelp;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Registry,
  CheckLst,
  uMSHelpServices,
  uCustomHelpMain,
  uCustomHelpConsts,
  uFrameConfigProviders,
  uFrameConfigColor;

type
  Tform_Config = class(TForm)
    Panel1:         TPanel;
    Button1:        TButton;
    Button2:        TButton;
    lvNamespaces:   TListView;
    Panel4:         TPanel;
    cbFullTextSearch: TCheckBox;
    cbTrimNamespacesHX: TComboBox;
    Label4:         TLabel;
    cbCheckGID:     TCheckBox;
    Tabs:           TPageControl;
    TabSheet1:      TTabSheet;
    TabSheet2:      TTabSheet;
    TabSheet3:      TTabSheet;
    rgDisplayLocation: TRadioGroup;
    TabSheet4:      TTabSheet;
    FrameConfigFileBasedProviders: TFrameConfigProviders;
    FrameConfigWebBasedProviders: TFrameConfigProviders;
    GroupBox1:      TGroupBox;
    GroupBox2:      TGroupBox;
    lbOrder:        TListBox;
    fccMSHelp:      TFrameConfigColor;
    fccWebProvider: TFrameConfigColor;
    fccFileProvider: TFrameConfigColor;
    TabSheet5:      TTabSheet;
    FrameConfigRSSProviders: TFrameConfigProviders;
    fccRSSProvider: TFrameConfigColor;
    Label1:         TLabel;
    TabSheet6:      TTabSheet;
    FlowPanel1:     TFlowPanel;
    ScrollBox1:     TScrollBox;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListView1InfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: string);
    procedure lbOrderDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbOrderDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Label1Click(Sender: TObject);
  private
    FLastDropRect: TRect;
    procedure Save;
    procedure BuildNamespaceList;
    procedure BuildResultOrderList;
    procedure Init3rdParty;
  public
    class function Execute: Boolean;
  end;

implementation

uses
  uUtils,
  ShellAPI,
  uFrameConfig3rdParty;

{$R *.dfm}

{ Tform_Config }

procedure Tform_Config.Button1Click(Sender: TObject);
begin
  Save;
  ModalResult := mrOk;
end;

class function Tform_Config.Execute: Boolean;
var
  form: Tform_Config;
begin
  form := Tform_Config.Create(nil);
  try
    Result := IsPositiveResult(form.ShowModal);
  finally
    form.Free;
  end;
end;

procedure Tform_Config.BuildNamespaceList;
var
  nsl:     IHxRegNamespaceList;
  idx:     Integer;
  Enabled: TStringList;
begin
  Enabled := TStringList.Create;
  try
    GlobalCustomHelp.ReadEnabledNamespacesFromRegistry(Enabled);

    nsl := GlobalCustomHelp.Namespaces;
    for idx := 1 to nsl.Count do
    begin
      with lvNamespaces.Items.Add do
      begin
        Caption := nsl.Item(idx).Name;
        SubItems.Add(nsl.Item(idx).GetProperty(HxRegNamespaceDescription));
        if Enabled.IndexOf(Caption) >= 0 then
          Checked := True
        else
          Checked := False;
      end;
    end;
  finally
    Enabled.Free;
  end;
end;

procedure Tform_Config.FormCreate(Sender: TObject);
var
  i:   Integer;
  idx: TNamespaceTrimOption;
  jdx: TDisplayLocationOption;
begin
  for idx := Low(OPTIONS_NAMESPACETRIM) to High(OPTIONS_NAMESPACETRIM) do
  begin
    cbTrimNamespacesHX.Items.Add(OPTIONS_NAMESPACETRIM[idx]);
  end;
  for jdx := Low(OPTIONS_DISPLAY_LOCATIONS) to High(OPTIONS_DISPLAY_LOCATIONS) do
  begin
    i := rgDisplayLocation.Items.Add(OPTIONS_DISPLAY_LOCATIONS[jdx]);
    if GlobalCustomHelp.DisplayLocation = jdx then
      rgDisplayLocation.ItemIndex := i;
  end;

  BuildResultOrderList;
end;

procedure Tform_Config.BuildResultOrderList;
var
  idx:      Integer;
  s:        string;
  newItems: TStringList;
  i2:       Integer;
begin
  newItems := TStringList.Create;
  try
    newItems.Add(GROUP_LABEL_WEB_BASED);
    newItems.Add(GROUP_LABEL_FILE_BASED);
    newItems.Add(GROUP_LABEL_STANDARD);
    newItems.Add(GROUP_LABEL_DUMMY_MSHELP2);
    newItems.Add(GROUP_PREFIX_RSS);
    newItems.AddStrings(GlobalCustomHelp._3rdPartyViewers);

    for idx := 0 to newItems.Count - 1 do
    begin
      s := GlobalCustomHelp.ResultOrderFromIndex[idx];
      if s <> EmptyStr then
      begin
        i2 := newItems.IndexOf(s);
        lbOrder.Items.Add(s);
        if (i2 >= 0) and (i2 < newItems.Count) then
          newItems.Delete(i2);
      end;
    end;

    if newItems.Count > 0 then
      lbOrder.Items.AddStrings(newItems);
  finally
    newItems.Free;
  end;
end;

procedure Tform_Config.FormShow(Sender: TObject);
var
  Reg: TRegistry;
  sl:  TStringList;
begin
  BuildNamespaceList;

  Reg := TRegistry.Create;
  sl  := TStringList.Create;
  try
    TCustomHelp.ReadSettingsFromRegistry(sl);
    cbFullTextSearch.Checked     := GlobalCustomHelp.PerformFullTextSearch;
    cbTrimNamespacesHX.ItemIndex := StrToIntDef(sl.Values[SETTINGS_TRIMNAMESPACES], 0);
    cbCheckGID.Checked           := GlobalCustomHelp.CheckWinHelpGid;

    fccMSHelp.SelectedColor       := GlobalCustomHelp.Color[GROUP_LABEL_DUMMY_MSHELP2];
    fccWebProvider.SelectedColor  := GlobalCustomHelp.Color[GROUP_LABEL_WEB_BASED];
    fccFileProvider.SelectedColor := GlobalCustomHelp.Color[GROUP_LABEL_FILE_BASED];

    fccRSSProvider.SelectedColor  := GlobalCustomHelp.Color[GROUP_PREFIX_RSS];

    FrameConfigFileBasedProviders.InitContent(cpfFileBased, ptStandard);
    FrameConfigWebBasedProviders.InitContent(cpfWebBased, ptStandard);
    FrameConfigRSSProviders.InitContent(cpfWebBased, ptRSS);

    Init3rdParty;
  finally
    sl.Free;
    Reg.Free;
  end;

  Tabs.ActivePageIndex := 0;
end;

procedure Tform_Config.Init3rdParty;

  procedure CreateItem(ACaption: string);
  var
    frm: TFrameConfig3rdParty;
  begin
    frm        := TFrameConfig3rdParty.Create(self);
    frm.Width  := FlowPanel1.ClientWidth;
    frm.Parent := FlowPanel1;
    frm.SelectedColor := GlobalCustomHelp.Color[ACaption];
    frm.ProviderName := ACaption;
    frm.Name   := '';
  end;

var
  idx: Integer;
begin
  CreateItem(GROUP_LABEL_STANDARD);
  for idx := 0 to GlobalCustomHelp._3rdPartyViewers.Count - 1 do
  begin
    CreateItem(GlobalCustomHelp._3rdPartyViewers[idx]);
  end;
end;

procedure Tform_Config.Label1Click(Sender: TObject);
begin
  ShellExecute(Self.Handle, 'open',
    'http://www.delphipraxis.net/topic165769_ideexperte+customhelp.html&highlight=',
    '', '', SW_show);
end;

procedure Tform_Config.lbOrderDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  OldValue: string;
begin
  OldValue := lbOrder.Items[lbOrder.ItemIndex];
  lbOrder.Items.Delete(lbOrder.ItemIndex);
  lbOrder.Items.Insert(lbOrder.ItemAtPos(Point(X, Y), False), OldValue);
  FLastDropRect := Rect(0, 0, 0, 0);
end;

procedure Tform_Config.lbOrderDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  R: TRect;
begin
  Accept := Sender = Source;
  if Accept then
  begin
    R := lbOrder.ItemRect(lbOrder.ItemAtPos(Point(X, Y), False));
    lbOrder.Canvas.DrawFocusRect(FLastDropRect);
    lbOrder.Canvas.DrawFocusRect(R);
    FLastDropRect := R;
  end;
end;

procedure Tform_Config.ListView1InfoTip(Sender: TObject; Item: TListItem;
  var InfoTip: string);
begin
  InfoTip := 'Name: Name of your search provider' + #13#10 +
    'Desription: some descriptive hint for your searchprovider' +
    #13#10 +
    'URL: URL or filename of your search provider. URLs must start with http://. ';
end;

procedure Tform_Config.Save;
var
  Reg: TRegistry;
  idx: Integer;
begin
  Reg := TRegistry.Create;
  try
    Reg.DeleteKey(REG_ROOT_KEY);
  finally
    Reg.Free;
  end;

  idx := FrameConfigFileBasedProviders.Save(0, ptStandard);
  FrameConfigWebBasedProviders.Save(idx, ptStandard);
  FrameConfigRSSProviders.Save(0, ptRSS);

  GlobalCustomHelp.PerformFullTextSearch   := cbFullTextSearch.Checked;
  GlobalCustomHelp.TrimNamespacesUntilResultFound :=
    TNamespaceTrimOption(cbTrimNamespacesHX.ItemIndex);
  GlobalCustomHelp.CheckWinHelpGid         := cbCheckGID.Checked;
  GlobalCustomHelp.DisplayLocation         := TDisplayLocationOption(rgDisplayLocation.ItemIndex);
  GlobalCustomHelp.Color[GROUP_LABEL_DUMMY_MSHELP2] := fccMSHelp.SelectedColor;
  GlobalCustomHelp.Color[GROUP_LABEL_WEB_BASED] := fccWebProvider.SelectedColor;
  GlobalCustomHelp.Color[GROUP_PREFIX_RSS] := fccRSSProvider.SelectedColor;
  GlobalCustomHelp.Color[GROUP_LABEL_FILE_BASED] := fccFileProvider.SelectedColor;

  GlobalCustomHelp.WriteSettingToRegistry(SETTINGS_3RD_PARTY_VIEWERS,
            GlobalCustomHelp._3rdPartyViewers.CommaText);

  for idx := 0 to FlowPanel1.ControlCount - 1 do
  begin
    if FlowPanel1.Controls[idx] is TFrameConfig3rdParty then
      TFrameConfig3rdParty(FlowPanel1.Controls[idx]).Save;
  end;

  with lvNamespaces do
    for idx := 0 to Items.Count - 1 do
      GlobalCustomHelp.WriteNamespacesToRegistry(Items[idx].Caption, Items[idx].Checked);



  GlobalCustomHelp.WriteResultOrderToRegistry(lbOrder.Items);
end;

end.
