unit uch2ProviderStaticWebsearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uch2Main, Registry, StdCtrls, ExtCtrls, Spin, ImgList, ComCtrls,
  ToolWin, Contnrs, StrUtils, uch2FrameHelpItemDecoration;

type
  Tch2StatWebURL = class
  public
    Name : String;
    URL : String;
    Deco : Tch2HelpItemDecoration;
    OpenLocation : Tch2URLOpenLocation;

    constructor Create();

    procedure Load(ARegistry : TRegistry);
    procedure Save(ARegistry : TRegistry);
  end;

  Tch2ProviderStaticWebsearch = class(TInterfacedObject, Ich2Provider)
  private
    FPriority : Integer;
    FURLs: TObjectList;
    function GetURL(AIndex: Integer): Tch2StatWebURL;

    procedure LoadSettings;
    procedure SaveSettings;
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
    property URL[AIndex : Integer] : Tch2StatWebURL read GetURL;
  end;

  Tch2FormConfigStaticWebsearch = class(TForm)
    Panel1: TPanel;
    btn_OK: TButton;
    GroupBox2: TGroupBox;
    LV: TListView;
    Panel2: TPanel;
    ToolBar1: TToolBar;
    btn_Add: TToolButton;
    btn_Del: TToolButton;
    Label2: TLabel;
    Label3: TLabel;
    ed_Name: TEdit;
    ed_URL: TEdit;
    com_Location: TComboBox;
    Label9: TLabel;
    frame_Deco: Tch2FrameHelpItemDecoration;
    Panel3: TPanel;
    Label1: TLabel;
    ed_Prio: TSpinEdit;
    procedure ed_NameChange(Sender: TObject);
    procedure ed_URLChange(Sender: TObject);
    procedure LVSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure btn_AddClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btn_DelClick(Sender: TObject);
    procedure ed_PrioChange(Sender: TObject);
    procedure com_LocationChange(Sender: TObject);
  private
    FProvider : Tch2ProviderStaticWebsearch;

    procedure OnDecoChange(Sender : TObject);
  public
    class function Execute(AProvider : Tch2ProviderStaticWebsearch) : Boolean;
  end;

implementation

uses uch2Tools, uch2Data;

{$R *.dfm}

const
  Settings_Value_Priority = 'Priority';
  Settings_Value_Name = 'Name';
  Settings_Value_URL = 'URL';
  Settings_Key_URLs = '\URLs';
  Settings_Value_OpenLocation = 'OpenIn';

type
  Tch2HIURL = class(TInterfacedObject, Ich2HelpItem)
  private
    FURL : Tch2StatWebURL;
    FKeyword : String;
  public
    constructor Create(AURL : Tch2StatWebURL; AKeyword : String);

    {$REGION 'Ich2HelpItem'}
    function GetGUID : TGUID;
    function GetCaption : String;
    function GetDescription : String;
    function GetDecoration : Tch2HelpItemDecoration;
    function GetFlags : Tch2HelpItemFlags;
    procedure ShowHelp;
    {$ENDREGION}
  end;

{ Tch2ProviderStaticWebsearch }

procedure Tch2ProviderStaticWebsearch.AfterConstruction;
begin
  FURLs := TObjectList.Create(true);

  LoadSettings;
end;

procedure Tch2ProviderStaticWebsearch.BeforeDestruction;
begin
  SaveSettings;

  FURLs.Free;

  inherited;
end;

procedure Tch2ProviderStaticWebsearch.Configure;
begin
  Tch2FormConfigStaticWebsearch.Execute(Self);
  SaveSettings;
end;

function Tch2ProviderStaticWebsearch.GetDescription: String;
begin
  Result := 'Open a webpage by the composition of your url and the keyword';
end;

function Tch2ProviderStaticWebsearch.GetGUID: TGUID;
const
  g : TGUID = '{DB121BA8-E497-4050-BD59-32CB7204B6CF}';
begin
  Result := g;
end;

function Tch2ProviderStaticWebsearch.GetName: String;
begin
  Result := 'Static Websearch';
end;

function Tch2ProviderStaticWebsearch.GetPriority: Integer;
begin
  Result := FPriority;
end;

function Tch2ProviderStaticWebsearch.GetURL(AIndex: Integer): Tch2StatWebURL;
begin
  Result := Tch2StatWebURL(FURLs[AIndex]);
end;

procedure Tch2ProviderStaticWebsearch.LoadSettings;
var
  Reg : TRegistry;
  sl : TStringList;
  s : String;
  url : Tch2StatWebURL;
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
        url := Tch2StatWebURL.Create;
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

procedure Tch2ProviderStaticWebsearch.ProvideHelp(AKeyword: String;
  AGUI: Ich2GUI);
var
  o : Pointer;
  u : Tch2StatWebURL absolute o;
begin
  for o in FURLs do
  begin
    AGUI.AddHelpItem(Tch2HIURL.Create(u, AKeyword) as Ich2HelpItem);
  end;
end;

procedure Tch2ProviderStaticWebsearch.SaveSettings;
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

{ Tch2StatWebURL }

constructor Tch2StatWebURL.Create;
begin
  OpenLocation := olDefaultBrowser;
end;

procedure Tch2StatWebURL.Load(ARegistry: TRegistry);
begin
  Deco.LoadFromRegistry(ARegistry);

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
end;

procedure Tch2StatWebURL.Save(ARegistry: TRegistry);
begin
  Deco.SaveToRegistry(ARegistry);

  ARegistry.WriteString(Settings_Value_Name, Name);
  ARegistry.WriteString(Settings_Value_URL, URL);

  ARegistry.WriteInteger(Settings_Value_OpenLocation, Integer(OpenLocation));
end;

procedure Tch2FormConfigStaticWebsearch.btn_AddClick(Sender: TObject);
var
  url : Tch2StatWebURL;
begin
  url := Tch2StatWebURL.Create;
  url.Name := IfThen(ed_Name.Text = '', 'NewURL', ed_Name.Text);
  url.URL := IfThen(ed_URL.Text = '', 'http://', ed_URL.Text);
  url.Deco := frame_Deco.Decoration;
  FProvider.URLs.Add(url);
  with lv.Items.Add do
  begin
    Data := url;
    Caption := url.Name;
    SubItems.Add(url.URL);
    Selected := true;
  end;
end;

procedure Tch2FormConfigStaticWebsearch.btn_DelClick(Sender: TObject);
begin
  if Assigned(lv.Selected) then
  begin
    FProvider.URLs.Remove(lv.Selected.Data);
    lv.DeleteSelected;
  end;
end;

procedure Tch2FormConfigStaticWebsearch.com_LocationChange(Sender: TObject);
begin
  if Assigned(lv.Selected) then
    Tch2StatWebURL(lv.Selected.Data).OpenLocation := Tch2URLOpenLocation(com_Location.Items.Objects[com_Location.ItemIndex]);
end;

procedure Tch2FormConfigStaticWebsearch.ed_NameChange(Sender: TObject);
begin
  if Assigned(lv.Selected) then
  begin
    Tch2StatWebURL(lv.Selected.Data).Name := ed_Name.Text;
    lv.Selected.Caption := ed_Name.Text;
    frame_Deco.Caption := ed_Name.Text;
  end;
end;

procedure Tch2FormConfigStaticWebsearch.ed_PrioChange(Sender: TObject);
begin
  FProvider.FPriority := ed_Prio.Value;
end;

procedure Tch2FormConfigStaticWebsearch.ed_URLChange(Sender: TObject);
begin
  if Assigned(lv.Selected) then
  begin
    Tch2StatWebURL(lv.Selected.Data).URL := ed_URL.Text;
    lv.Selected.SubItems[0] := ed_URL.Text;
  end;
end;

class function Tch2FormConfigStaticWebsearch.Execute(
  AProvider: Tch2ProviderStaticWebsearch): Boolean;
var
  form : Tch2FormConfigStaticWebsearch;
begin
  form := Tch2FormConfigStaticWebsearch.Create(nil);
  try
    form.FProvider := AProvider;
    Result := IsPositiveResult(Form.ShowModal);
  finally
    form.Free;
  end;
end;

procedure Tch2FormConfigStaticWebsearch.FormShow(Sender: TObject);
var
  o : Pointer;
  url : Tch2StatWebURL absolute o;
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

  frame_Deco.OnChange := OnDecoChange;
end;

procedure Tch2FormConfigStaticWebsearch.LVSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
  begin
    with Tch2StatWebURL(Item.Data) do
    begin
      ed_Name.Text := Name;
      ed_URL.Text := URL;
      frame_Deco.Decoration := Deco;
      frame_Deco.Caption := Name;
      com_Location.ItemIndex := com_Location.Items.IndexOfObject(TObject(OpenLocation));
    end;
  end
  else
  begin
    ed_Name.Text := EmptyStr;
    ed_URL.Text := EmptyStr;
    frame_Deco.ResetToDefault;
    com_Location.ItemIndex := com_Location.Items.IndexOfObject(TObject(olDefaultBrowser));
  end;
end;

procedure Tch2FormConfigStaticWebsearch.OnDecoChange(Sender: TObject);
begin
  if Assigned(lv.Selected) then
    Tch2StatWebURL(lv.Selected.Data).Deco := frame_Deco.Decoration;
end;

{ Tch2HIURL }

constructor Tch2HIURL.Create(AURL: Tch2StatWebURL; AKeyword : String);
begin
  FURL := AURL;
  FKeyword := AKeyword;
end;

function Tch2HIURL.GetCaption: String;
begin
  Result := FURL.Name;
end;

function Tch2HIURL.GetDecoration: Tch2HelpItemDecoration;
begin
  Result := FURL.Deco;
end;

function Tch2HIURL.GetDescription: String;
begin
  Result := '';
end;

function Tch2HIURL.GetFlags: Tch2HelpItemFlags;
begin
  Result := [ifProvidesHelp];
end;

function Tch2HIURL.GetGUID: TGUID;
const
  g : TGUID = '{E30101F2-352E-47DB-8D2E-0DBF999FB803}';
begin
  Result := g;
end;

procedure Tch2HIURL.ShowHelp;
var
  EncodedKeyWord : String;
begin
  EncodedKeyWord := ch2StrEncodeURL(FKeyword);

  ch2Main.ShowURL(StringReplace(FURL.URL, '$(HelpString)', EncodedKeyWord, [rfIgnoreCase, rfReplaceAll]), FURL.OpenLocation);
end;

initialization
  ch2Main.RegisterProvider(Tch2ProviderStaticWebsearch.Create as Ich2Provider);

end.
