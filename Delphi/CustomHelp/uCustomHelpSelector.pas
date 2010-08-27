{-----------------------------------------------------------------------------
 Purpose: Form to select which help should be showed 
 
 (c) by TheUnknownOnes under dwywbdbu license - see http://theunknownones.googlecode.com/svn/ for the license
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uCustomHelpSelector;

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
  HelpIntfs,
  OleCtrls,
  StdCtrls,
  ExtCtrls,
  ActiveX,
  mshtml,
  ComCtrls,
  CategoryButtons,
  msxml;

type
  TFormHelpSelector = class(TForm)
    catbtnTopics:     TCategoryButtons;
    cbFullTextSearch: TCheckBox;
    grpErrors:        TGroupBox;
    mmoErrors:        TMemo;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListBox1Compare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListBox1Editing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure catbtnTopicsDrawText(Sender: TObject; const Button: TButtonItem;
      Canvas: TCanvas; Rect: TRect; State: TButtonDrawState);
    procedure catbtnTopicsButtonClicked(Sender: TObject; const Button: TButtonItem);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbbSearchKeywordCloseUp(Sender: TObject);
  private
    FURL:       string;
    FHelpIndex: Integer;
    procedure InitList(Keywords: TStrings);
    procedure SaveExpanded;
    procedure SortCategories;
  public
    property URL: string read FURL;
    property SelectedHelpIndex: Integer read FHelpIndex;

    class function Execute(HelpString: string; Keywords: TStrings;
      out SelectedIndex: Integer; out SelectedUrl: string): Boolean; overload; static;
  end;

  //This class must not contain any local fields
  //applies only to BDS 2006
  THelpSelector = class(TInterfacedObject, IHelpSelector, IHelpSelector2)
  protected
    function SelectKeyword(Keywords: TStrings): Integer;
    function TableOfContents(Contents: TStrings): Integer;
    function SelectContext(Viewers: TStrings): Integer;
  end;

implementation

uses
  UrlMon,
  StrUtils,
  ComObj,
  ShellAPI,
  uCustomHelpMain,
  uCustomHelpIDEIntegration,
  Math,
  Registry,
  uCustomHelpKeywordRecorder,
  uCustomHelpIntfs,
  uCustomHelpConsts;

{$R *.dfm}

const
  CAPTION_INDEXCHECK    =
    'Index search performed (check me to perform fulltext search next time)';
  CAPTION_FULLTEXTCHECK =
    'Fulltext search performed (uncheck me to perform index search next time)';

type
  TCustomHelpButtonItem = class(TButtonItem)
  private
    FDesc: string;
    FURL:  string;
    FIdx:  Integer;
    FTO:   TNamespaceTrimOption;
    procedure SetDesc(const Value: string);
  public
    property URL: string read FURL write FURL;
    property Description: string read FDesc write SetDesc;
    property HelpIndex: Integer read FIdx write FIdx;
    property TrimOption: TNamespaceTrimOption read FTO write FTO;
  end;

type
  THelpViewerNodeAccess = class(TObject)
  private
    FViewer:   ICustomHelpViewer;
    FViewerID: Integer;
  end;

var
  GlobalCustomHelpViewerNode: THelpViewerNodeAccess;

procedure TFormHelpSelector.catbtnTopicsButtonClicked(Sender: TObject;
  const Button: TButtonItem);
var
  CustomButton: TCustomHelpButtonItem;
begin
  if catbtnTopics.SelectedItem <> Button then
    exit;

  if Button.Category.Collapsed then
    Button.Category.Collapsed := False
  else if Button is TCustomHelpButtonItem then
  begin
    CustomButton := TCustomHelpButtonItem(Button);
    FHelpIndex   := CustomButton.HelpIndex;
    FURL         := CustomButton.URL;
    ModalResult  := mrOk;
  end;
end;

procedure TFormHelpSelector.catbtnTopicsDrawText(Sender: TObject;
  const Button: TButtonItem; Canvas: TCanvas; Rect: TRect; State: TButtonDrawState);
var
  drawrect: TRect;
  txt:      string;
  TabPos:   Integer;
begin
  drawrect := Rect;

  TabPos := Pos(#9, TCustomHelpButtonItem(Button).Caption);

  if ((Button.Category.Caption = GROUP_LABEL_WEB_BASED) or
    (Button.Category.Caption = GROUP_LABEL_FILE_BASED)) and (TabPos > 0) then
  begin
    Canvas.Font.Style := [fsBold];
    txt := copy(TCustomHelpButtonItem(Button).Caption, 0, TabPos - 1);
    drawRect.Right := drawrect.Left + (catbtnTopics.Width div 3);
    Canvas.TextRect(drawrect, txt, [tfEndEllipsis, tfNoPrefix]);

    Canvas.Font.Style := [fsItalic];
    txt           := copy(TCustomHelpButtonItem(Button).Caption, TabPos + 1,
      Length(TCustomHelpButtonItem(Button).Caption));
    drawRect.Left := drawRect.Right;
    drawRect.Right := drawrect.Left + (catbtnTopics.Width div 3);
    Canvas.TextRect(drawrect, txt, [tfEndEllipsis, tfNoPrefix]);
  end
  else
  begin
    Canvas.Font.Style := [fsBold];
    txt := TCustomHelpButtonItem(Button).Caption;
    drawRect.Right := drawrect.Left + (catbtnTopics.Width * 2 div 3);
    Canvas.TextRect(drawrect, txt, [tfEndEllipsis, tfNoPrefix]);
  end;

  drawrect      := Rect;
  Canvas.Font.Style := [];
  txt           := TCustomHelpButtonItem(Button).Description;
  drawRect.Left := drawrect.Left + (catbtnTopics.Width * 2 div 3);
  Canvas.TextRect(drawrect, txt, [tfEndEllipsis, tfNoPrefix]);
end;

procedure TFormHelpSelector.cbbSearchKeywordCloseUp(Sender: TObject);
begin
  catbtnTopics.SetFocus;
end;

class function TFormHelpSelector.Execute(HelpString: string;
  Keywords: TStrings; out SelectedIndex: Integer; out SelectedUrl: string): Boolean;
var
  fhs: TFormHelpSelector;
begin
  Result        := False;
  SelectedIndex := -1;
  SelectedUrl   := '';

  fhs := TFormHelpSelector.Create(Application.MainForm);
  with fhs do
  begin
    try
      GlobalCustomHelpViewerNode.FViewer   := HelpViewerIntf;
      GlobalCustomHelpViewerNode.FViewerID := GlobalCustomHelp.GetViewerID;
      InitList(Keywords);

      with catbtnTopics.Categories[0] do
      begin
        if Items.Count > 0 then
        begin
          ScrollIntoView;
          catbtnTopics.SelectedItem := Items[0];
          catbtnTopics.FocusedItem  := Items[0];
        end;
      end;

      // clear keyword history before showing help select form ...
      CustomHelpKeywordRecorderIntf.Reset;
      CustomHelpKeywordRecorderIntf.SetEnabled(False);

      Caption := StringReplace(Caption, '@@HELPSTRING@@', HelpString, [rfReplaceAll]);

      if (ShowModal = mrOk) then
      begin
        Result        := True;
        SelectedIndex := SelectedHelpIndex;
        SelectedUrl   := URL;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TFormHelpSelector.SortCategories;
var
  i:      Integer;
  idx:    Integer;
  toSort: TStringList;
  s:      string;
begin
  toSort := TStringList.Create;
  try
    for i := 0 to catbtnTopics.Categories.Count - 1 do
    begin
      s := catbtnTopics.Categories[i].Caption;
      if (s = GROUP_LABEL_WEB_BASED) or (s = GROUP_LABEL_STANDARD) or
        (s = GROUP_LABEL_FILE_BASED) or StartsText(GROUP_PREFIX_RSS, s) or
        (GlobalCustomHelp._3rdPartyViewers.IndexOf(s) >= 0) then
      begin
        idx := GlobalCustomHelp.ResultOrderFromString[s];
        toSort.AddObject(Format('%.4d', [idx]), catbtnTopics.Categories[i]);
      end
      else
      begin
        toSort.AddObject(Format('%.4d',
          [GlobalCustomHelp.ResultOrderFromString[GROUP_LABEL_DUMMY_MSHELP2]]),
          catbtnTopics.Categories[i]);
      end;
    end;

    toSort.Sort;
    for i := 0 to toSort.Count - 1 do
    begin
      TButtonCategory(toSort.Objects[i]).Index := i;
    end;
  finally
    toSort.Free;
  end;
end;

procedure TFormHelpSelector.InitList(Keywords: TStrings);
var
  cat:           TButtonCategory;
  idx:           Integer;
  Caption, Description, URL, Group, Keyword: string;
  item:          TCustomHelpButtonItem;
  Reg:           TRegistry;
  CheckExpanded: Boolean;
  TrimOption:    TNamespaceTrimOption;
  ANode:         THelpViewerNodeAccess;
  ProvEnabled:   Boolean;
  Timeout,
  MaxResult:     Integer;
  AProvider:     ICustomHelpProvider;

  function GetCategoryFromLabel(ALabel: string;
    ACreate: Boolean = True): TButtonCategory;
  var
    i: Integer;
  begin
    Result := nil;

    for i := 0 to catbtnTopics.Categories.Count - 1 do
      if catbtnTopics.Categories[i].Caption = ALabel then
      begin
        Result := catbtnTopics.Categories[i];
        break;
      end;
    if not Assigned(Result) and ACreate then
    begin
      Result         := catbtnTopics.Categories.Add;
      Result.Caption := ALabel;
      if ALabel = GROUP_LABEL_WEB_BASED then
        Result.Color := GlobalCustomHelp.Color[GROUP_LABEL_WEB_BASED]
      else if ALabel = GROUP_LABEL_FILE_BASED then
        Result.Color := GlobalCustomHelp.Color[GROUP_LABEL_FILE_BASED]
      else if ALabel = GROUP_LABEL_FILE_BASED then
        Result.Color := GlobalCustomHelp.Color[GROUP_LABEL_STANDARD]
      else if ALabel = GROUP_LABEL_WINSEARCH then
        Result.Color := GlobalCustomHelp.Color[GROUP_LABEL_WINSEARCH]
      else if StartsText(GROUP_PREFIX_RSS, ALabel) then
        Result.Color := GlobalCustomHelp.Color[GROUP_PREFIX_RSS]
      else if StartsText(GROUP_PREFIX_RSS, ALabel) then
        Result.Color := GlobalCustomHelp.Color[GROUP_PREFIX_RSS]
      else if (GlobalCustomHelp._3rdPartyViewers.IndexOf(ALabel) >= 0) then
      begin
        Result.Color := GlobalCustomHelp.Color[ALabel];
      end
      else
        Result.Color := GlobalCustomHelp.Color[GROUP_LABEL_DUMMY_MSHELP2];

      Result.TextColor := clCaptionText;
      Result.Collapsed := True;
      if CheckExpanded then
        Result.Collapsed := Reg.ReadString(ALabel) = '0';
    end;
  end;

  function CustomURLExists(AURL: string): Boolean;
  var
    jdx: Integer;
  begin
    Result := False;
    if AURL = '' then
      Exit;
    AURL := URL_SEPERATOR + AURL + URL_SEPERATOR;
    for jdx := 0 to Keywords.Count - 1 do
      if Pos(AURL, Keywords[jdx]) > 0 then
      begin
        Result := True;
        Exit;
      end;
  end;

  function GetNode(Index: Integer): THelpViewerNodeAccess;
  begin
    Result := THelpViewerNodeAccess(Keywords.Objects[Index]);
  end;

  procedure GetViewerName(ANode: THelpViewerNodeAccess; var Group: string);
  begin
    if ANode.FViewer <> nil then
      Group := ANode.FViewer.GetViewerName;
  end;

begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    CheckExpanded := Reg.OpenKey(REG_ROOT_KEY + EXPANDEDITEMS_SUB_KEY, True);

    for idx := 0 to Keywords.Count - 1 do
    begin
      Keyword := Keywords[idx];
      ANode   := GetNode(idx);
      if AnsiStartsText(PROTPREFIX_CUSTOMHELP, Keyword) then
      begin
        if not TCustomHelp.DecodeURL(Keyword, Caption, Description,
          URL, Group, TrimOption, ProvEnabled, Timeout, MaxResult) then
          Continue;
      end
      else if CustomURLExists(Keyword) then
      begin
        // ignore duplicate url
        Continue;
      end
      else if AnsiStartsText(PROTPREFIX_MSHELP, Keyword) then
      begin
        if not TCustomHelp.DecodeURL(Keyword, Caption, Description,
          URL, Group, TrimOption, ProvEnabled, Timeout, MaxResult) then
          Continue;
        if not ProvEnabled then
          Continue;
        if Group = GROUP_LABEL_STANDARD then
          GetViewerName(ANode, Group);
        // replace default viewer?
        if GlobalCustomHelp.DisplayLocation <> dloMSDocumentExplorer then
          Keywords.Objects[idx] := GlobalCustomHelpViewerNode;
      end
      else if Supports(ANode.FViewer, ICustomHelpProvider, AProvider) then
      begin
        // imeexception, madCollection
        if not AProvider.TranslateHelpString(Keyword, Caption, Description,
          URL, Group) then
          Continue;
        TrimOption := nstoNoTrim;
        Keyword    := GlobalCustomHelp.EncodeURL(Caption, Description,
          URL, Group, TrimOption, True, Timeout, MaxResult);
      end
      else
      begin
        Caption     := Keyword;
        Description := '';
        URL         := Keyword;
        Group       := GROUP_LABEL_STANDARD;
        GetViewerName(ANode, Group);
        if (Group <> GROUP_LABEL_STANDARD) and
          (GlobalCustomHelp._3rdPartyViewers.IndexOf(Group) < 0) then
        begin
          GlobalCustomHelp._3rdPartyViewers.Add(Group);
          GlobalCustomHelp.WriteSettingToRegistry(SETTINGS_3RD_PARTY_VIEWERS,
            GlobalCustomHelp._3rdPartyViewers.CommaText);
        end;

        TrimOption := nstoNoTrim;
      end;

      cat          := GetCategoryFromLabel(Group);
      item         := TCustomHelpButtonItem.Create(cat.Items);
      item.Caption := Caption;
      item.Description := Description;
      item.URL     := Keyword;
      item.HelpIndex := idx;
      item.TrimOption := TrimOption;
    end;

    SortCategories;

    Reg.CloseKey;
  finally
    Reg.Free;
  end;
end;

function THelpSelector.SelectContext(Viewers: TStrings): Integer;
begin
  Result := Viewers.IndexOf(HelpViewerIntf.GetViewerName);
end;

function THelpSelector.SelectKeyword(Keywords: TStrings): Integer;
var
  URL, l: string;
  hv:     IExtendedHelpViewer;
  prv:    ICustomHelpProvider;
begin
  if not TFormHelpSelector.Execute(GlobalCustomHelp.LastHelpCallKeyword,
    Keywords, Result, URL) then
  begin
    Result := -1;
    Exit;
  end;

  with THelpViewerNodeAccess(Keywords.Objects[Result]) do
  begin
    if FViewer = HelpViewerIntf then
    begin
      GlobalCustomHelp.ShowHelp(Keywords[Result]);
      Result := -1;
      Exit;
    end;
    if GlobalCustomHelp.DecodeURL(URL, l) then
    begin
      GlobalCustomHelp.ShowHelp(URL);
      Result := -1;
      Exit;
    end;
    if Supports(FViewer, ICustomHelpProvider, prv) then
    begin
      prv.ShowCustomHelp(Keywords[Result]);
      Result := -1;
      Exit;
    end;
    if Supports(FViewer, IExtendedHelpViewer, hv) then
    begin // fix call of viewer by default help system ...
      hv.DisplayTopic(Keywords[Result]);
      Result := -1;
      Exit;
    end;
  end;
end;

function THelpSelector.TableOfContents(Contents: TStrings): Integer;
begin
  Result := Contents.IndexOf(HelpViewerIntf.GetViewerName);
end;

procedure TFormHelpSelector.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GlobalCustomHelp.PerformFullTextSearch := cbFullTextSearch.Checked;
  SaveExpanded;
  Action := caHide;
end;

procedure TFormHelpSelector.FormCreate(Sender: TObject);
begin
  cbFullTextSearch.Checked := GlobalCustomHelp.PerformFullTextSearch;
  if cbFullTextSearch.Checked then
    cbFullTextSearch.Caption := CAPTION_FULLTEXTCHECK
  else
    cbFullTextSearch.Caption := CAPTION_INDEXCHECK;

  mmoErrors.Text    := GlobalCustomHelp.LastHelpErrors;
  grpErrors.Visible := mmoErrors.Text <> '';
  if not grpErrors.Visible then
    Height := Height - grpErrors.Height;
end;

procedure TFormHelpSelector.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then
  begin
    ModalResult := mrCancel;
    Key         := 0;
  end;
end;

procedure TFormHelpSelector.FormShow(Sender: TObject);
begin
  catbtnTopics.FocusedItem := catbtnTopics.SelectedItem;
end;

procedure TFormHelpSelector.ListBox1Compare(Sender: TObject;
  Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare := CompareValue(Integer(Item1.Data), Integer(Item2.Data));
end;

procedure TFormHelpSelector.ListBox1Editing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  AllowEdit := False;
end;

procedure TFormHelpSelector.SaveExpanded;
var
  Reg: TRegistry;
  idx: Integer;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(REG_ROOT_KEY + EXPANDEDITEMS_SUB_KEY, True) then
    begin
      for idx := 0 to catbtnTopics.Categories.Count - 1 do
        Reg.WriteString(catbtnTopics.Categories[idx].Caption,
          ifthen(catbtnTopics.Categories[idx].Collapsed, '0', '1'));

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

{ TCustomHelpButtonItem }

procedure TCustomHelpButtonItem.SetDesc(const Value: string);
begin
  FDesc := Value;
  Hint  := Value;
end;

initialization
  GlobalCustomHelpViewerNode := THelpViewerNodeAccess.Create;

finalization
  GlobalCustomHelpViewerNode.Free;

end.
