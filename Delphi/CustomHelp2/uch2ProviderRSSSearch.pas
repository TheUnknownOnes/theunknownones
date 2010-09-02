unit uch2ProviderRSSSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uch2Main, Registry, StdCtrls, ExtCtrls, Spin, ImgList, ComCtrls,
  ToolWin, Contnrs, StrUtils, msxml, URLMon;

type
  Tch2RSSURL = class
    Name : String;
    URL : String;
    ForeColor,
    BackColor : TColor;
    OpenLocation : Tch2URLOpenLocation;
    GUID : TGUID;

    constructor Create();

    procedure Load(ARegistry : TRegistry);
    procedure Save(ARegistry : TRegistry);
  end;

  Tch2ProviderRSSSearch = class(TInterfacedObject, Ich2Provider)
  private
    FForeColor,
    FBackColor : TColor;
    FPriority : Integer;
    FURLs: TObjectList;
    function GetURL(AIndex: Integer): Tch2RSSURL;

    procedure LoadSettings;
    procedure SaveSettings;

    procedure DoRSSSearch(AURL : Tch2RSSURL; AGUI : Ich2GUI; AKeyword : String; AParent : Pointer);
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

    property URLs : TObjectList read FURLs;
    property URL[AIndex : Integer] : Tch2RSSURL read GetURL;
  end;

  Tch2FormConfigRSSSearch = class(TForm)
    Panel1: TPanel;
    btn_OK: TButton;
    GroupBox1: TGroupBox;
    ed_Prio: TSpinEdit;
    GroupBox2: TGroupBox;
    LV: TListView;
    Panel2: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ed_Name: TEdit;
    ed_URL: TEdit;
    cob_URLForeColor: TColorBox;
    cob_URLBackColor: TColorBox;
    com_Location: TComboBox;
    ToolBar1: TToolBar;
    btn_Add: TToolButton;
    btn_Del: TToolButton;
    iml_TB: TImageList;
    procedure FormShow(Sender: TObject);
    procedure LVSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure LVAdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure btn_AddClick(Sender: TObject);
    procedure btn_DelClick(Sender: TObject);
    procedure ed_NameChange(Sender: TObject);
    procedure ed_URLChange(Sender: TObject);
    procedure ed_PrioChange(Sender: TObject);
    procedure cob_URLForeColorChange(Sender: TObject);
    procedure cob_URLBackColorChange(Sender: TObject);
    procedure com_LocationChange(Sender: TObject);
  private
    FProvider : Tch2ProviderRSSSearch;
  public
    class function Execute(AProvider : Tch2ProviderRSSSearch) : Boolean;
  end;

implementation

{$R *.dfm}

const
  Settings_Value_Priority = 'Priority';
  Settings_Value_ForeColor = 'ForeColor';
  Settings_Value_BackColor = 'BackColor';
  Settings_Value_Name = 'Name';
  Settings_Value_URL = 'URL';
  Settings_Key_URLs = '\URLs';
  Settings_Value_OpenLocation = 'OpenIn';
  Settings_Value_GUID = 'GUID';

type
  Tch2HIURL = class(TInterfacedObject, Ich2HelpItem)
  private
    FURL : Tch2RSSURL;
  public
    constructor Create(AURL : Tch2RSSURL; AKeyword : AnsiString);

    {$REGION 'Ich2HelpItem'}
    function GetGUID : TGUID;
    function GetCaption : String;
    function GetDescription : String;
    function GetForeColor : TColor;
    function GetBackColor : TColor;
    function GetFontStyles : TFontStyles;
    function GetFlags : Tch2HelpItemFlags;
    procedure ShowHelp;
    {$ENDREGION}
  end;

  Tch2HIRSSEntry = class(TInterfacedObject, Ich2HelpItem)
  private
    FURL : String;
    FCaption : String;
    FDescription : String;
    FOpenLocation : Tch2URLOpenLocation;
    FRSSUrl : Tch2RSSURL;
  public
    constructor Create(ARSSUrl : Tch2RSSURL; AURL, ACaption, ADescription : String; AOpenLocation : Tch2URLOpenLocation);

    {$REGION 'Ich2HelpItem'}
    function GetGUID : TGUID;
    function GetCaption : String;
    function GetDescription : String;
    function GetForeColor : TColor;
    function GetBackColor : TColor;
    function GetFontStyles : TFontStyles;
    function GetFlags : Tch2HelpItemFlags;
    procedure ShowHelp;
    {$ENDREGION}
  end;

{ Tch2ProviderRSSSearch }

procedure Tch2ProviderRSSSearch.AfterConstruction;
begin
  inherited;

  FURLs := TObjectList.Create(true);

  LoadSettings;
end;

procedure Tch2ProviderRSSSearch.BeforeDestruction;
begin
  SaveSettings;

  FURLs.Free;

  inherited;
end;

procedure Tch2ProviderRSSSearch.Configure;
begin
  Tch2FormConfigRSSSearch.Execute(Self);
  SaveSettings;
end;

procedure Tch2ProviderRSSSearch.DoRSSSearch(AURL: Tch2RSSURL; AGUI: Ich2GUI;
  AKeyword: String; AParent: Pointer);
var
  idx, channelidx: Integer;
  Caption, Description, Url, Group:  string;
  provEnabled: Boolean;
  oldc:        string;
  xmldocument: IXMLDomDocument;
  node:        IXMLDOMNode;
  channels, nodes: IXMLDOMNodeList;
  FileName:    string;
  Timeout,
  MaxResults: Integer;
begin
  url := ReplaceText(AURL.URL, '$(HelpString)', AKeyword);

  SetLength(FileName, MAX_PATH + 1);

  try

    if URLDownloadToCacheFile(nil, PChar(Url), PChar(FileName), MAX_PATH,
      0, nil) = s_OK then
    begin
      xmldocument       := CoDOMDocument.Create;
      xmldocument.async := False;
      xmldocument.validateOnParse := False;
      xmldocument.load(FileName);

      if xmldocument.parseError.errorCode <> 0 then
        raise Exception.Create('Could not parse RSS data for ' + Caption +
          #13#10 + 'Reason: ' + xmldocument.parseError.reason +
          'File saved to: ' + FileName);

      channels := xmldocument.selectNodes('/rss/channel');

      for channelidx := 0 to channels.length - 1 do
      begin
        nodes := channels[channelidx].selectNodes('item');

        for idx := 0 to nodes.length - 1 do
        begin
          Caption := EmptyStr;
          Description := EmptyStr;
          Url := EmptyStr;

          node := nodes[idx].selectSingleNode('title');
          if Assigned(node) then
            Caption := node.Text;
          node := nodes[idx].selectSingleNode('description');
          if Assigned(node) then
            Description := node.Text;
          node := nodes[idx].selectSingleNode('link');
          if Assigned(node) then
            Url := node.Text;

          AGUI.AddHelpItem(Tch2HIRSSEntry.Create(AURL, Url, Caption, Description, AURL.OpenLocation) as Ich2HelpItem, AParent);
        end;

        if nodes.length > 0 then
        begin
          node := channels[channelidx].selectSingleNode('title');
          if Assigned(node) then
            Caption := node.Text;

          node := channels[channelidx].selectSingleNode('link');
          if Assigned(node) then
            AGUI.AddHelpItem(Tch2HIRSSEntry.Create(AURL, node.text, 'All Results for "' + Caption + '"', '', AURL.OpenLocation) as Ich2HelpItem, AParent);
        end;
      end;
    end
    else
      raise Exception.Create('Could not download RSS data for ' + Caption);
  except
    on E: Exception do
      ShowMessage(e.Message);
  end;
end;

function Tch2ProviderRSSSearch.GetDescription: String;
begin
  Result := 'Use the helpkeyword inside an url to get a RSS-Feed'
end;

function Tch2ProviderRSSSearch.GetGUID: TGUID;
const
  g : TGUID = '{C93841D1-48AA-4D8B-BD98-65D1E934D8F3}';
begin
  Result := g;
end;

function Tch2ProviderRSSSearch.GetName: String;
begin
  Result := 'RSS Search'
end;

function Tch2ProviderRSSSearch.GetPriority: Integer;
begin
  Result := FPriority;
end;

function Tch2ProviderRSSSearch.GetURL(AIndex: Integer): Tch2RSSURL;
begin
  Result := Tch2RSSURL(FURLs[AIndex]);
end;

procedure Tch2ProviderRSSSearch.LoadSettings;
var
  Reg : TRegistry;
  sl : TStringList;
  s : String;
  url : Tch2RSSURL;
begin
  inherited;

  sl := TStringList.Create;
  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID], true) then
    begin
      if Reg.ValueExists(Settings_Value_Priority) then
        FPriority := reg.ReadInteger(Settings_Value_Priority)
      else
        FPriority := 0;

      if Reg.ValueExists(Settings_Value_ForeColor) then
        FForeColor := reg.ReadInteger(Settings_Value_ForeColor)
      else
        FForeColor := clNone;

      if Reg.ValueExists(Settings_Value_BackColor) then
        FBackColor := reg.ReadInteger(Settings_Value_BackColor)
      else
        FBackColor := clNone;

      Reg.CloseKey;
    end;

    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID] + Settings_Key_URLs, true) then
    begin
      Reg.GetKeyNames(sl);
      Reg.CloseKey;
    end;

    for s in sl do
    begin
      if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID] + Settings_Key_URLs + '\' + s, false) then
      begin
        url := Tch2RSSURL.Create;
        url.Load(Reg);
        FURLs.Add(url);
        Reg.CloseKey;
      end;
    end;

  finally
    sl.Free;
    Reg.Free;
  end;

end;

procedure Tch2ProviderRSSSearch.ProvideHelp(AKeyword: String; AGUI: Ich2GUI);
var
  kw : AnsiString;
  c : AnsiChar;
  EncodedKeyword : String;
  o : Pointer;
  u : Tch2RSSURL absolute o;
  parent : Pointer;
begin
  kw := AKeyword;

  EncodedKeyword := '';
  for c in kw do
    EncodedKeyword := EncodedKeyword + '%' + IntToHex(Ord(c), 2);

  for o in URLs do
  begin
    parent := AGUI.AddHelpItem(Tch2HIURL.Create(u, AKeyword) as Ich2HelpItem);

    DoRSSSearch(u, AGUI, EncodedKeyword, parent);
  end;
end;

procedure Tch2ProviderRSSSearch.SaveSettings;
var
  Reg : TRegistry;
  sl : TStringList;
  s : String;
  idx : Integer;
begin
  sl := TStringList.Create;
  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID], true) then
    begin
      Reg.WriteInteger(Settings_Value_Priority, FPriority);
      Reg.WriteInteger(Settings_Value_ForeColor, FForeColor);
      Reg.WriteInteger(Settings_Value_BackColor, FBackColor);

      Reg.CloseKey;
    end;

    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID] + Settings_Key_URLs, true) then
    begin
      Reg.GetKeyNames(sl);

      for s in sl do
      begin
        Reg.DeleteKey(ch2Main.RegRootKeyProvider[GetGUID] + Settings_Key_URLs + '\' + s);
      end;

      Reg.CloseKey;
    end;

    for idx := 0 to FURLs.Count - 1 do
    begin
      if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID] + Settings_Key_URLs + '\' + IntToStr(idx), true) then
      begin
        URL[idx].Save(Reg);
        Reg.CloseKey;
      end;
    end;

  finally
    sl.Free;
    Reg.Free;
  end;

end;

{ Tch2RSSURL }

constructor Tch2RSSURL.Create;
begin
  BackColor := clNone;
  ForeColor := clNone;
  OpenLocation := olDefaultBrowser;
  CreateGUID(GUID);
end;

procedure Tch2RSSURL.Load(ARegistry: TRegistry);
begin
  if ARegistry.ValueExists(Settings_Value_ForeColor) then
    ForeColor := ARegistry.ReadInteger(Settings_Value_ForeColor)
  else
    ForeColor := clNone;

  if ARegistry.ValueExists(Settings_Value_BackColor) then
    BackColor := ARegistry.ReadInteger(Settings_Value_BackColor)
  else
    BackColor := clNone;

  if ARegistry.ValueExists(Settings_Value_Name) then
    Name := ARegistry.ReadString(Settings_Value_Name)
  else
    Name := EmptyStr;

  if ARegistry.ValueExists(Settings_Value_URL) then
    URL := ARegistry.ReadString(Settings_Value_URL)
  else
    URL := EmptyStr;

  if ARegistry.ValueExists(Settings_Value_OpenLocation) then
    OpenLocation := Tch2URLOpenLocation(ARegistry.ReadInteger(Settings_Value_OpenLocation))
  else
    OpenLocation := olDefaultBrowser;

  if ARegistry.ValueExists(Settings_Value_GUID) then
    GUID := StringToGUID(ARegistry.ReadString(Settings_Value_GUID));
end;

procedure Tch2RSSURL.Save(ARegistry: TRegistry);
begin
  ARegistry.WriteInteger(Settings_Value_ForeColor, ForeColor);
  ARegistry.WriteInteger(Settings_Value_BackColor, BackColor);

  ARegistry.WriteString(Settings_Value_Name, Name);
  ARegistry.WriteString(Settings_Value_URL, URL);

  ARegistry.WriteInteger(Settings_Value_OpenLocation, Integer(OpenLocation));

  ARegistry.WriteString(Settings_Value_GUID, GUIDToString(GUID));
end;

{ Tch2FormConfigRSSSearch }

procedure Tch2FormConfigRSSSearch.btn_AddClick(Sender: TObject);
var
  url : Tch2RSSURL;
begin
  url := Tch2RSSURL.Create;
  url.Name := IfThen(ed_Name.Text = '', 'NewURL', ed_Name.Text);
  url.URL := IfThen(ed_URL.Text = '', 'http://', ed_URL.Text);
  url.ForeColor := cob_URLForeColor.Selected;
  url.BackColor := cob_URLBackColor.Selected;
  FProvider.URLs.Add(url);
  with lv.Items.Add do
  begin
    Data := url;
    Caption := url.Name;
    SubItems.Add(url.URL);
    Selected := true;
  end;
end;

procedure Tch2FormConfigRSSSearch.btn_DelClick(Sender: TObject);
begin
  if Assigned(lv.Selected) then
  begin
    FProvider.URLs.Remove(lv.Selected.Data);
    lv.DeleteSelected;
  end;
end;

procedure Tch2FormConfigRSSSearch.cob_URLBackColorChange(Sender: TObject);
begin
  if Assigned(lv.Selected) then
    Tch2RSSURL(lv.Selected.Data).BackColor := cob_URLBackColor.Selected;
end;

procedure Tch2FormConfigRSSSearch.cob_URLForeColorChange(Sender: TObject);
begin
  if Assigned(lv.Selected) then
    Tch2RSSURL(lv.Selected.Data).ForeColor := cob_URLForeColor.Selected;
end;

procedure Tch2FormConfigRSSSearch.com_LocationChange(Sender: TObject);
begin
  if Assigned(lv.Selected) then
    Tch2RSSURL(lv.Selected.Data).OpenLocation := Tch2URLOpenLocation(com_Location.Items.Objects[com_Location.ItemIndex]);
end;

procedure Tch2FormConfigRSSSearch.ed_NameChange(Sender: TObject);
begin
  if Assigned(lv.Selected) then
  begin
    Tch2RSSURL(lv.Selected.Data).Name := ed_Name.Text;
    lv.Selected.Caption := ed_Name.Text;
  end;
end;

procedure Tch2FormConfigRSSSearch.ed_PrioChange(Sender: TObject);
begin
  FProvider.FPriority := ed_Prio.Value;
end;

procedure Tch2FormConfigRSSSearch.ed_URLChange(Sender: TObject);
begin
  if Assigned(lv.Selected) then
  begin
    Tch2RSSURL(lv.Selected.Data).URL := ed_URL.Text;
    lv.Selected.SubItems[0] := ed_URL.Text;
  end;
end;

class function Tch2FormConfigRSSSearch.Execute(
  AProvider: Tch2ProviderRSSSearch): Boolean;
var
  form : Tch2FormConfigRSSSearch;
begin
  form := Tch2FormConfigRSSSearch.Create(nil);
  try
    form.FProvider := AProvider;
    Result := IsPositiveResult(form.ShowModal);
  finally
    form.Free;
  end;
end;

procedure Tch2FormConfigRSSSearch.FormShow(Sender: TObject);
var
  o : Pointer;
  url : Tch2RSSURL absolute o;
  l : Tch2URLOpenLocation;
begin
  for o in FProvider.URLs do
  begin
    with lv.Items.Add do
    begin
      Caption := url.Name;
      SubItems.Add(url.URL);
      Data := url;
    end;
  end;

  for l := Low(Tch2URLOpenLocation) to High(Tch2URLOpenLocation) do
    com_Location.AddItem(ch2URLOpenLocationTexts[l], TObject(l));
  com_Location.ItemIndex := com_Location.Items.IndexOfObject(TObject(olDefaultBrowser));

  ed_Prio.Value := FProvider.FPriority;
end;

procedure Tch2FormConfigRSSSearch.LVAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  DefaultDraw := true;

  if Tch2RSSURL(Item.Data).ForeColor <> clNone then
    lv.Canvas.Font.Color := Tch2RSSURL(Item.Data).ForeColor;

  if Tch2RSSURL(Item.Data).BackColor <> clNone then
    lv.Canvas.Brush.Color := Tch2RSSURL(Item.Data).BackColor;
end;

procedure Tch2FormConfigRSSSearch.LVSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then
  begin
    with Tch2RSSURL(Item.Data) do
    begin
      ed_Name.Text := Name;
      ed_URL.Text := URL;
      cob_URLForeColor.Selected := ForeColor;
      cob_URLBackColor.Selected := BackColor;
      com_Location.ItemIndex := com_Location.Items.IndexOfObject(TObject(OpenLocation));
    end;
  end
  else
  begin
    ed_Name.Text := EmptyStr;
    ed_URL.Text := EmptyStr;
    cob_URLForeColor.Selected := clNone;
    cob_URLBackColor.Selected := clNone;
    com_Location.ItemIndex := com_Location.Items.IndexOfObject(TObject(olDefaultBrowser));
  end;
end;

{ Tch2HIURL }

constructor Tch2HIURL.Create(AURL: Tch2RSSURL; AKeyword: AnsiString);
begin
  FURL := AURL;
end;

function Tch2HIURL.GetBackColor: TColor;
begin
  Result := FURL.BackColor;
end;

function Tch2HIURL.GetCaption: String;
begin
  Result := FURL.Name;
end;

function Tch2HIURL.GetDescription: String;
begin
  Result := '';
end;

function Tch2HIURL.GetFlags: Tch2HelpItemFlags;
begin
  Result := [ifSaveStats];

  if FURL.BackColor <> clNone then Include(Result, ifHasBackColor);
  if FURL.ForeColor <> clNone then Include(Result, ifHasForeColor);
end;

function Tch2HIURL.GetFontStyles: TFontStyles;
begin
  Result := [];
end;

function Tch2HIURL.GetForeColor: TColor;
begin
  Result := FURL.ForeColor;
end;

function Tch2HIURL.GetGUID: TGUID;
begin
  Result := FURL.GUID;
end;

procedure Tch2HIURL.ShowHelp;
begin
end;

{ Tch2HIRSSEntry }

constructor Tch2HIRSSEntry.Create(ARSSUrl : Tch2RSSURL; AURL, ACaption, ADescription: String; AOpenLocation : Tch2URLOpenLocation);
begin
  FURL := AURL;
  FCaption := ACaption;
  FDescription := ADescription;
  FOpenLocation := AOpenLocation;
  FRSSUrl := ARSSUrl;
end;

function Tch2HIRSSEntry.GetBackColor: TColor;
begin
  Result := FRSSUrl.BackColor;
end;

function Tch2HIRSSEntry.GetCaption: String;
begin
  Result := FCaption;
end;

function Tch2HIRSSEntry.GetDescription: String;
begin
  Result := FDescription;
end;

function Tch2HIRSSEntry.GetFlags: Tch2HelpItemFlags;
begin
  Result := [ifProvidesHelp];

  if FRSSUrl.ForeColor <> clNone then Include(Result, ifHasForeColor);
  if FRSSUrl.BackColor <> clNone then Include(Result, ifHasBackColor);
end;

function Tch2HIRSSEntry.GetFontStyles: TFontStyles;
begin
  Result := [];
end;

function Tch2HIRSSEntry.GetForeColor: TColor;
begin
  Result := FRSSUrl.ForeColor;
end;

function Tch2HIRSSEntry.GetGUID: TGUID;
const
  g : TGUID = '{971832BD-A7E3-40E9-84AD-1193DA34655F}';
begin
  Result := g;
end;

procedure Tch2HIRSSEntry.ShowHelp;
begin
  ch2Main.ShowURL(FURL, FOpenLocation);
end;

initialization
  ch2Main.RegisterProvider(Tch2ProviderRSSSearch.Create as Ich2Provider);

end.
