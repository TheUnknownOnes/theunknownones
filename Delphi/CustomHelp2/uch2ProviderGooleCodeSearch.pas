unit uch2ProviderGooleCodeSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls, ToolWin, uch2FrameHelpItemDecoration, StdCtrls,
  Spin, ExtCtrls, uch2Main, Registry, Contnrs, StrUtils, msxml, urlmon;

type
  Tch2GCSQuery = class
    Name : String;
    Query : String;
    Deco : Tch2HelpItemDecoration;
    GUID : TGUID;
    OpenLocation : Tch2URLOpenLocation;

    constructor Create();

    procedure Load(ARegistry : TRegistry);
    procedure Save(ARegistry : TRegistry);
  end;

  Tch2ProviderGoogleCodeSearch = class(TInterfacedObject, Ich2Provider)
  private
    FPriority : Integer;
    FQueries: TObjectList;
    FFeedURL,
    FWebURL : String;

    procedure LoadSettings;
    procedure SaveSettings;
    function GetQuery(AIndex: Integer): Tch2GCSQuery;

    function DoQuery(AQuery : Tch2GCSQuery; AEncodedQuery : String; AGUI : Ich2GUI) : Boolean;
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

    property Queries : TObjectList read FQueries;
    property Query[AIndex : Integer] : Tch2GCSQuery read GetQuery;
  end;

  Tch2FormConfigGoogleCodeSearch = class(TForm)
    GroupBox2: TGroupBox;
    LV: TListView;
    Panel2: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    ed_Name: TEdit;
    ed_Query: TEdit;
    frame_Deco: Tch2FrameHelpItemDecoration;
    ToolBar1: TToolBar;
    btn_Add: TToolButton;
    btn_Del: TToolButton;
    iml_TB: TImageList;
    Label4: TLabel;
    Panel1: TPanel;
    btn_OK: TButton;
    com_Location: TComboBox;
    Label9: TLabel;
    GroupBox1: TGroupBox;
    ed_Prio: TSpinEdit;
    Label1: TLabel;
    Label5: TLabel;
    ed_FeedURL: TEdit;
    ed_WebURL: TEdit;
    Label6: TLabel;
    procedure Label4Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LVSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure btn_AddClick(Sender: TObject);
    procedure btn_DelClick(Sender: TObject);
    procedure ed_NameChange(Sender: TObject);
    procedure ed_QueryChange(Sender: TObject);
    procedure ed_PrioChange(Sender: TObject);
    procedure com_LocationChange(Sender: TObject);
    procedure ed_FeedURLChange(Sender: TObject);
    procedure ed_WebURLChange(Sender: TObject);
  private
    FProvider : Tch2ProviderGoogleCodeSearch;

    procedure OnDecoChange(Sender : TObject);
  public
    class function Execute(AProvider : Tch2ProviderGoogleCodeSearch) : Boolean;
  end;


implementation

uses uch2Tools;

{$R *.dfm}

const
  Settings_Value_Name = 'Name';
  Settings_Value_Query = 'Query';
  Settings_Value_Priority = 'Priority';
  Settings_Value_GUID = 'GUID';
  Settings_Key_Queries = '\Queries';
  Settings_Value_OpenLocation = 'OpenIn';
  Settings_Value_FeedURL = 'FeedURL';
  Settings_Value_WebURL = 'WebURL';

type
  THIQuery = class(TInterfacedObject, Ich2HelpItem)
  private
    FQuery : Tch2GCSQuery;
  public
    constructor Create(AQuery : Tch2GCSQuery);

    {$REGION 'Ich2HelpItem'}
    function GetGUID : TGUID;
    function GetCaption : String;
    function GetDescription : String;
    function GetDecoration : Tch2HelpItemDecoration;
    function GetFlags : Tch2HelpItemFlags;
    procedure ShowHelp;
    {$ENDREGION}
  end;

  THIEntry = class(TInterfacedObject, Ich2HelpItem)
  private
    FQuery : Tch2GCSQuery;
    FCaption,
    FDescription,
    FURL : String;
  public
    constructor Create(AQuery : Tch2GCSQuery; ACaption, ADescription, AURL : String);

    {$REGION 'Ich2HelpItem'}
    function GetGUID : TGUID;
    function GetCaption : String;
    function GetDescription : String;
    function GetDecoration : Tch2HelpItemDecoration;
    function GetFlags : Tch2HelpItemFlags;
    procedure ShowHelp;
    {$ENDREGION}
  end;

  THIMatch = class(TInterfacedObject, Ich2HelpItem)
  private
    FQuery : Tch2GCSQuery;
    FCaption,
    FDescription,
    FURL : String;
  public
    constructor Create(AQuery : Tch2GCSQuery; ACaption, ADescription, AURL : String);

    {$REGION 'Ich2HelpItem'}
    function GetGUID : TGUID;
    function GetCaption : String;
    function GetDescription : String;
    function GetDecoration : Tch2HelpItemDecoration;
    function GetFlags : Tch2HelpItemFlags;
    procedure ShowHelp;
    {$ENDREGION}
  end;

constructor THIQuery.Create(AQuery: Tch2GCSQuery);
begin
  FQuery := AQuery;
end;

function THIQuery.GetCaption: String;
begin
  Result := FQuery.Name;
end;

function THIQuery.GetDecoration: Tch2HelpItemDecoration;
begin
  Result := FQuery.Deco;
end;

function THIQuery.GetDescription: String;
begin
  Result := '';
end;

function THIQuery.GetFlags: Tch2HelpItemFlags;
begin
  Result := [ifSaveStats];
end;

function THIQuery.GetGUID: TGUID;
begin
  Result := FQuery.GUID;
end;

procedure THIQuery.ShowHelp;
begin

end;

procedure Tch2FormConfigGoogleCodeSearch.btn_AddClick(Sender: TObject);
var
  qry : Tch2GCSQuery;
begin
  qry := Tch2GCSQuery.Create;
  qry.Name := IfThen(ed_Name.Text = '', 'NewURL', ed_Name.Text);
  qry.Query := IfThen(ed_Query.Text = '', '$(HelpString)', ed_Query.Text);
  qry.Deco := frame_Deco.Decoration;
  FProvider.Queries.Add(qry);
  with lv.Items.Add do
  begin
    Data := qry;
    Caption := qry.Name;
    SubItems.Add(qry.Query);
    Selected := true;
  end;
end;

procedure Tch2FormConfigGoogleCodeSearch.btn_DelClick(Sender: TObject);
begin
  if Assigned(lv.Selected) then
  begin
    FProvider.Queries.Remove(lv.Selected.Data);
    lv.DeleteSelected;
  end;
end;

procedure Tch2FormConfigGoogleCodeSearch.com_LocationChange(Sender: TObject);
begin
  if Assigned(lv.Selected) then
    Tch2GCSQuery(lv.Selected.Data).OpenLocation := Tch2URLOpenLocation(com_Location.Items.Objects[com_Location.ItemIndex]);
end;

procedure Tch2FormConfigGoogleCodeSearch.ed_FeedURLChange(Sender: TObject);
begin
  FProvider.FFeedURL := ed_FeedURL.Text;
end;

procedure Tch2FormConfigGoogleCodeSearch.ed_NameChange(Sender: TObject);
begin
  if Assigned(lv.Selected) then
  begin
    Tch2GCSQuery(lv.Selected.Data).Name := ed_Name.Text;
    lv.Selected.Caption := ed_Name.Text;
    frame_Deco.Caption := ed_Name.Text;
  end;
end;

procedure Tch2FormConfigGoogleCodeSearch.ed_PrioChange(Sender: TObject);
begin
  FProvider.FPriority := ed_Prio.Value;
end;

procedure Tch2FormConfigGoogleCodeSearch.ed_QueryChange(Sender: TObject);
begin
  if Assigned(lv.Selected) then
  begin
    Tch2GCSQuery(lv.Selected.Data).Query := ed_Query.Text;
    lv.Selected.SubItems[0] := ed_Query.Text;
  end;
end;

procedure Tch2FormConfigGoogleCodeSearch.ed_WebURLChange(Sender: TObject);
begin
  FProvider.FWebURL := ed_WebURL.Text;
end;

class function Tch2FormConfigGoogleCodeSearch.Execute(
  AProvider: Tch2ProviderGoogleCodeSearch): Boolean;
var
  FForm : Tch2FormConfigGoogleCodeSearch;
begin
  FForm := Tch2FormConfigGoogleCodeSearch.Create(nil);
  try
    FForm.FProvider := AProvider;
    Result := IsPositiveResult(FForm.ShowModal);
  finally
    FForm.Free;
  end;
end;

procedure Tch2FormConfigGoogleCodeSearch.FormShow(Sender: TObject);
var
  o : Pointer;
  qry : Tch2GCSQuery absolute o;
  l : Tch2URLOpenLocation;
begin
  for o in FProvider.Queries do
  begin
    with lv.Items.Add do
    begin
      Caption := qry.Name;
      SubItems.Add(qry.Query);
      Data := qry;
    end;
  end;

  for l := Low(Tch2URLOpenLocation) to High(Tch2URLOpenLocation) do
    com_Location.AddItem(ch2URLOpenLocationTexts[l], TObject(l));
  com_Location.ItemIndex := com_Location.Items.IndexOfObject(TObject(olDefaultBrowser));

  ed_Prio.Value := FProvider.FPriority;
  ed_FeedURL.Text := FProvider.FFeedURL;
  ed_WebURL.Text := FProvider.FWebURL;

  frame_Deco.OnChange := OnDecoChange;
end;

procedure Tch2FormConfigGoogleCodeSearch.Label4Click(Sender: TObject);
begin
  ch2Main.ShowURL('http://www.google.com/codesearch', olDefaultBrowser);
end;

procedure Tch2FormConfigGoogleCodeSearch.LVSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
  begin
    with Tch2GCSQuery(Item.Data) do
    begin
      ed_Name.Text := Name;
      ed_Query.Text := Query;
      frame_Deco.Decoration := Deco;
      frame_Deco.Caption := Name;
      com_Location.ItemIndex := com_Location.Items.IndexOfObject(TObject(OpenLocation));
    end;
  end
  else
  begin
    ed_Name.Text := EmptyStr;
    ed_Query.Text := EmptyStr;
    frame_Deco.ResetToDefault;
    com_Location.ItemIndex := com_Location.Items.IndexOfObject(TObject(olDefaultBrowser));
  end;
end;

procedure Tch2FormConfigGoogleCodeSearch.OnDecoChange(Sender: TObject);
begin
  if Assigned(lv.Selected) then
    Tch2GCSQuery(lv.Selected.Data).Deco := frame_Deco.Decoration;
end;

{ Tch2GCSQuery }

constructor Tch2GCSQuery.Create;
begin
  CreateGUID(GUID);
  OpenLocation := olDefaultBrowser;
end;

procedure Tch2GCSQuery.Load(ARegistry: TRegistry);
begin
  Deco.LoadFromRegistry(ARegistry);

  if ARegistry.ValueExists(Settings_Value_Name) then
    Name := ARegistry.ReadString(Settings_Value_Name)
  else
    Name := EmptyStr;

  if ARegistry.ValueExists(Settings_Value_Query) then
    Query := ARegistry.ReadString(Settings_Value_Query)
  else
    Query := EmptyStr;

  if ARegistry.ValueExists(Settings_Value_GUID) then
    GUID := StringToGUID(ARegistry.ReadString(Settings_Value_GUID));

  if ARegistry.ValueExists(Settings_Value_OpenLocation) then
    OpenLocation := Tch2URLOpenLocation(ARegistry.ReadInteger(Settings_Value_OpenLocation))
  else
    OpenLocation := olDefaultBrowser;
end;

procedure Tch2GCSQuery.Save(ARegistry: TRegistry);
begin
  Deco.SaveToRegistry(ARegistry);

  ARegistry.WriteString(Settings_Value_Name, Name);
  ARegistry.WriteString(Settings_Value_Query, Query);

  ARegistry.WriteString(Settings_Value_GUID, GUIDToString(GUID));

  ARegistry.WriteInteger(Settings_Value_OpenLocation, Integer(OpenLocation));
end;

{ Tch2ProviderGoogleCodeSearch }

procedure Tch2ProviderGoogleCodeSearch.AfterConstruction;
begin
  inherited;

  FQueries := TObjectList.Create(true);

  FFeedURL := 'http://www.google.com/codesearch/feeds/search?max-results=20&q=';
  FWebURL := 'http://www.google.com/codesearch?q=';

  LoadSettings;
end;

procedure Tch2ProviderGoogleCodeSearch.BeforeDestruction;
begin
  SaveSettings;

  FQueries.Free;

  inherited;
end;

procedure Tch2ProviderGoogleCodeSearch.Configure;
begin
  Tch2FormConfigGoogleCodeSearch.Execute(Self);
  SaveSettings;
end;

function Tch2ProviderGoogleCodeSearch.DoQuery(AQuery: Tch2GCSQuery; AEncodedQuery : String; AGUI : Ich2GUI) : Boolean;
var
  FileName : String;
  xml : IXMLDOMDocument;
  entries : IXMLDOMNodeList;
  idxEntry,
  idxMatch : Integer;
  entry,
  match,
  node : IXMLDOMNode;
  attr : IXMLDOMNode;

  matches : IXMLDOMNodeList;
  cap, descr, url, entry_title, entry_filename : String;

  hientry : Pointer;

  sl : TStringList;

  Parent : Pointer;

  FoundOne : Boolean;

  procedure NeedParent();
  begin
    if not Assigned(Parent) then
      Parent := AGUI.AddHelpItem(THIQuery.Create(AQuery) as Ich2HelpItem)
  end;
begin
  Parent := nil;

  sl := TStringList.Create;
  result := false;
  try
    SetLength(FileName, MAX_PATH + 1);
    try
      sl.Delimiter := '/';
      sl.StrictDelimiter := true;

      url := 'http://www.google.com/codesearch/feeds/search?max-results=20&q=' + AEncodedQuery;

      if URLDownloadToCacheFile(nil, PChar(Url), PChar(FileName), MAX_PATH,
        0, nil) = s_OK then
      begin
        xml       := CoDOMDocument.Create;
        xml.async := False;
        xml.validateOnParse := False;
        xml.load(FileName);

        if xml.parseError.errorCode <> 0 then
          raise Exception.Create('Could not parse feed data for ' + AQuery.Name +
            #13#10 + 'Reason: ' + xml.parseError.reason +
            'File saved to: ' + FileName);

        entries := xml.selectNodes('/feed/entry');

        for idxEntry := 0 to entries.length - 1 do
        begin
          entry := entries[idxEntry];

          url := '';

          node := entry.selectSingleNode('link');
          if Assigned(node) then
          begin
            attr := node.attributes.getNamedItem('href');
            if Assigned(attr) then
              url := attr.text;
          end;


          entry_title := ''; entry_filename := '';

          node := entry.selectSingleNode('title');
          if Assigned(node) then
          begin
            entry_title := node.text;
          end;

          node := entry.selectSingleNode('gcs:file');
          if Assigned(Node) then
          begin
            attr := node.attributes.getNamedItem('name');
            if Assigned(attr) then
              entry_filename := attr.text;
          end;

          if entry_title = entry_filename then
          begin
            sl.DelimitedText := entry_filename;
            if sl.Count > 0 then
            begin
              cap := sl[sl.Count - 1];
              sl.Delete(sl.Count - 1);
              descr := sl.DelimitedText;
            end
            else
            begin
              cap := entry_title;
              descr := '';
            end;
          end
          else
          begin
            cap := entry_title;
            descr := entry_filename;
          end;

          NeedParent;
          hientry := AGUI.AddHelpItem(THIEntry.Create(AQuery, cap, descr, url), Parent);
          if not Result then
            Result := true;


          matches := entry.selectNodes('gcs:match');

          for idxMatch := 0 to matches.length - 1 do
          begin
            match := matches.item[idxMatch];

            cap := trim(ch2StripTags(match.text, '<', '>'));
            attr := match.attributes.getNamedItem('lineNumber');
            descr := 'Line: ' + attr.text;

            AGUI.AddHelpItem(THIMatch.Create(AQuery, cap, descr, url), hientry);
          end;
        end;

        if Result then
        begin
          NeedParent;
          AGUI.AddHelpItem(THIEntry.Create(AQuery, '-= Show in browser =-', '', 'http://www.google.com/codesearch?q=' + AEncodedQuery) as Ich2HelpItem, Parent);
        end;
      end
      else
        raise Exception.Create('Could not download feed data for ' + AQuery.Name);
    except
      on E: Exception do
        ShowMessage(e.Message);
    end;

  finally
    sl.Free;
  end;
end;

function Tch2ProviderGoogleCodeSearch.GetDescription: String;
begin
  Result := 'Search dynamically using Google Codesearch';
end;

function Tch2ProviderGoogleCodeSearch.GetGUID: TGUID;
const
  g : TGUID = '{D2CEA42F-A773-435D-BD7B-CF9905423758}';
begin
  Result := g;
end;

function Tch2ProviderGoogleCodeSearch.GetName: String;
begin
  Result := 'Google Codesearch';
end;

function Tch2ProviderGoogleCodeSearch.GetPriority: Integer;
begin
  Result := FPriority;
end;

function Tch2ProviderGoogleCodeSearch.GetQuery(AIndex: Integer): Tch2GCSQuery;
begin
  Result := Tch2GCSQuery(FQueries[AIndex]);
end;

procedure Tch2ProviderGoogleCodeSearch.LoadSettings;
var
  Reg : TRegistry;
  sl : TStringList;
  s : String;
  qry : Tch2GCSQuery;
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

      if Reg.ValueExists(Settings_Value_FeedURL) then
        FFeedURL := reg.ReadString(Settings_Value_FeedURL);

      if Reg.ValueExists(Settings_Value_WebURL) then
        FWebURL := reg.ReadString(Settings_Value_WebURL);

      Reg.CloseKey;
    end;

    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID] + Settings_Key_Queries, true) then
    begin
      Reg.GetKeyNames(sl);
      Reg.CloseKey;
    end;

    for s in sl do
    begin
      if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID] + Settings_Key_Queries + '\' + s, false) then
      begin
        qry := Tch2GCSQuery.Create;
        qry.Load(Reg);
        FQueries.Add(qry);
        Reg.CloseKey;
      end;
    end;

  finally
    sl.Free;
    Reg.Free;
  end;

end;

procedure Tch2ProviderGoogleCodeSearch.ProvideHelp(AKeyword: String;
  AGUI: Ich2GUI);
var
  EncodedQuery : String;
  o : Pointer;
  q : Tch2GCSQuery absolute o;
begin
  for o in Queries do
  begin
    EncodedQuery := ch2StrEncodeURL(StringReplace(q.Query, '$(HelpString)', AKeyword, [rfReplaceAll, rfIgnoreCase]));

    DoQuery(q, EncodedQuery, AGUI);
  end;
end;

procedure Tch2ProviderGoogleCodeSearch.SaveSettings;
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

      Reg.WriteString(Settings_Value_FeedURL, FFeedURL);
      Reg.WriteString(Settings_Value_WebURL, FWebURL);

      Reg.CloseKey;
    end;

    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID] + Settings_Key_Queries, true) then
    begin
      Reg.GetKeyNames(sl);

      for s in sl do
      begin
        Reg.DeleteKey(ch2Main.RegRootKeyProvider[GetGUID] + Settings_Key_Queries + '\' + s);
      end;

      Reg.CloseKey;
    end;

    for idx := 0 to FQueries.Count - 1 do
    begin
      if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID] + Settings_Key_Queries + '\' + IntToStr(idx), true) then
      begin
        Query[idx].Save(Reg);
        Reg.CloseKey;
      end;
    end;

  finally
    sl.Free;
    Reg.Free;
  end;

end;

{ THIEntry }

constructor THIEntry.Create(AQuery: Tch2GCSQuery; ACaption, ADescription,
  AURL: String);
begin
  FQuery := AQuery;
  FCaption := ACaption;
  FDescription := ADescription;
  FURL := AURL;
end;

function THIEntry.GetCaption: String;
begin
  Result := FCaption;
end;

function THIEntry.GetDecoration: Tch2HelpItemDecoration;
begin
  Result := FQuery.Deco;
end;

function THIEntry.GetDescription: String;
begin
  Result := FDescription;
end;

function THIEntry.GetFlags: Tch2HelpItemFlags;
begin
  Result := [ifProvidesHelp];
end;

function THIEntry.GetGUID: TGUID;
const
  g : TGUID = '{5B2EE9AC-0E2E-45DC-A782-D243595A9811}';
begin
  Result := g;
end;

procedure THIEntry.ShowHelp;
begin
  ch2Main.ShowURL(FURL, FQuery.OpenLocation);
end;

{ THIMatch }

constructor THIMatch.Create(AQuery: Tch2GCSQuery; ACaption,
  ADescription, AURL: String);
begin
  FQuery := AQuery;
  FCaption := ACaption;
  FDescription := ADescription;
  FURL := AURL;
end;

function THIMatch.GetCaption: String;
begin
  Result := FCaption;
end;

function THIMatch.GetDecoration: Tch2HelpItemDecoration;
begin
  Result := FQuery.Deco;
end;

function THIMatch.GetDescription: String;
begin
  Result := FDescription;
end;

function THIMatch.GetFlags: Tch2HelpItemFlags;
begin
  Result := [ifProvidesHelp];
end;

function THIMatch.GetGUID: TGUID;
const
  g : TGUID = '{5AEAEC5F-05E0-4B75-B793-73790A1FC7A6}';
begin
  Result := g;
end;

procedure THIMatch.ShowHelp;
begin
  ch2Main.ShowURL(FURL, FQuery.OpenLocation);
end;

initialization
  ch2Main.RegisterProvider(Tch2ProviderGoogleCodeSearch.Create);

end.
