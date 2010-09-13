unit uch2ProviderWindowsSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uch2Main, Spin, StdCtrls, ComCtrls, ToolWin, ADOInt,
  uch2FrameHelpItemDecoration, ExtCtrls, ImgList;

type
  Tch2ProviderWindowsSearch = class(TInterfacedObject, Ich2Provider)
  private
    FPriority : Integer;
    procedure SetPriority(APriority: Integer);
    procedure Search(ASearch, AQuery: String; AGUI: Ich2GUI);
  protected
    function GetDecoration(AName: String): Tch2HelpItemDecoration;
    function GetGUIDForName(AName: String): TGUID;
    function GetQuery(AName: String): String;
    function GetMaxCount(AName: String): Integer;
    procedure SetDecoration(AName: String; ADeco: Tch2HelpItemDecoration);
    procedure GetProviderList(const AList: TStrings);
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

  Tuch2FormProviderWindowsSearch = class(TForm)
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
    Panel1: TPanel;
    btn_OK: TButton;
    Panel3: TPanel;
    Label1: TLabel;
    ed_Prio: TSpinEdit;
    iml_TB: TImageList;
    Label4: TLabel;
    ed_maxResults: TSpinEdit;
    procedure btn_AddClick(Sender: TObject);
    procedure btn_DelClick(Sender: TObject);
    procedure LVChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure ed_NameChange(Sender: TObject);
    procedure ed_QueryChange(Sender: TObject);
    procedure btn_OKClick(Sender: TObject);
    procedure ed_maxResultsChange(Sender: TObject);
  private
    FProvider : Tch2ProviderWindowsSearch;
    procedure Init;
    function AddItemToList(ACaption: String = '<New Windows Search>'): TListItem;
    procedure OnDecoChange(ASender: TObject);
  public
    class procedure Execute(AProvider: Tch2ProviderWindowsSearch);
  end;

var
  uch2FormProviderWindowsSearch: Tuch2FormProviderWindowsSearch;

implementation

uses
  Registry, StrUtils, uch2TlbSearchAPILib, ComObj;

type
  Tch2ProviderWindowsSearchCategory = class(TInterfacedObject, Ich2HelpItem)
  private
    FProvider : Tch2ProviderWindowsSearch;
    FCaption  : String;
  public
    {$REGION 'Ich2HelpItem'}
    function GetGUID : TGUID; //used to store stats (expanded, position, ...)
    function GetCaption : String;
    function GetDescription : String;
    function GetDecoration : Tch2HelpItemDecoration;
    function GetFlags : Tch2HelpItemFlags;
    procedure ShowHelp;
    {$ENDREGION}

    constructor Create(AProvider: Tch2ProviderWindowsSearch;
                       ACaption: String);
  end;

  Tch2ProviderWindowsSearchItem = class(TInterfacedObject, Ich2HelpItem)
  private
    FProvider : Tch2ProviderWindowsSearch;
    FCaption     : String;
    FURL         : String;
    FDescription : String;
    FSearchName  : String;
  public
    {$REGION 'Ich2HelpItem'}
    function GetGUID : TGUID; //used to store stats (expanded, position, ...)
    function GetCaption : String;
    function GetDescription : String;
    function GetDecoration : Tch2HelpItemDecoration;
    function GetFlags : Tch2HelpItemFlags;
    procedure ShowHelp;
    {$ENDREGION}

    constructor Create(AProvider: Tch2ProviderWindowsSearch;
                       ACaption,
                       AURL,
                       ADescription,
                       ASearchName : String);
  end;

{$R *.dfm}

{ Tch2ProviderWindowsSearch }
const
  REG_VALUE_PRIORITY = 'Priority';
  REG_KEY_SEARCHES = 'Searches';
  REG_VALUE_QUERY = 'Query';
  REG_VALUE_GUID = 'GUID';
  REG_VALUE_MAXCOUNT = 'MaxCount';

procedure Tch2ProviderWindowsSearch.AfterConstruction;
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

procedure Tch2ProviderWindowsSearch.BeforeDestruction;
begin
  SetPriority(FPriority);
  inherited;
end;

procedure Tch2ProviderWindowsSearch.Configure;
begin
  Tuch2FormProviderWindowsSearch.Execute(self);
end;

function Tch2ProviderWindowsSearch.GetDecoration(
  AName: String): Tch2HelpItemDecoration;
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_SEARCHES+'\'+AName, true) then
    begin
      Result.LoadFromRegistry(Reg);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function Tch2ProviderWindowsSearch.GetDescription: String;
begin
  Result:='Search all indexed files on your computer.'
end;

function Tch2ProviderWindowsSearch.GetGUID: TGUID;
begin
  Result:=StringToGUID('{FA18E9E3-AF0F-413D-BC23-53E23C31B49C}');
end;

function Tch2ProviderWindowsSearch.GetGUIDForName(AName: String): TGUID;
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_SEARCHES+'\'+AName, true) then
    begin
      if not Reg.ValueExists(REG_VALUE_GUID) then
      begin
        CreateGUID(Result);
        Reg.WriteString(REG_VALUE_GUID, GUIDToString(Result));
      end
      else
        Result:=StringToGUID(Reg.ReadString(REG_VALUE_GUID));

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function Tch2ProviderWindowsSearch.GetMaxCount(AName: String): Integer;
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_SEARCHES+'\'+AName, true) then
    begin
      if not Reg.ValueExists(REG_VALUE_MAXCOUNT) then
        Result:=20
      else
        Result:=Reg.ReadInteger(REG_VALUE_MAXCOUNT);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function Tch2ProviderWindowsSearch.GetName: String;
begin
  Result:='Windows Search';
end;

function Tch2ProviderWindowsSearch.GetPriority: Integer;
begin
  Result:=FPriority;
end;

procedure Tch2ProviderWindowsSearch.GetProviderList(const AList: TStrings);
var
  Reg : TRegistry;
begin             
  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_SEARCHES, true) then
    begin
      Reg.GetKeyNames(AList);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function Tch2ProviderWindowsSearch.GetQuery(AName: String): String;
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_SEARCHES+'\'+AName, true) then
    begin
      if not Reg.ValueExists(REG_VALUE_QUERY) then
        Result:='$(HelpString)'
      else
        Result:=Reg.ReadString(REG_VALUE_QUERY);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure Tch2ProviderWindowsSearch.ProvideHelp(AKeyword: String;
  AGUI: Ich2GUI);
var
  sl : TStringList;
  s: string;
  query : String;
begin
  sl:=TStringList.Create;
  try
    GetProviderList(sl);
    for s in sl do
    begin
      query:=ReplaceText(GetQuery(s),'$(HelpString)',AKeyword);
      Search(s, query, AGUI);
    end;

  finally
    sl.Free;
  end;      
end;


procedure Tch2ProviderWindowsSearch.Search(ASearch, AQuery: String; AGUI: Ich2GUI);
var
  manager : ISearchManager;
  catalogManager : ISearchCatalogManager;
  queryHelper : ISearchQueryHelper;
  wQuery : string;
  temp : PWideChar;
  sTemp : string;
  ra: OleVariant;
  idx: Integer;

  fQuery: WideString;

  Caption, Description, Url, Group:  string;
  provEnabled: Boolean;
  HelpString:  string;
  MaxResults: Integer;

  path  : string;
  Parent : Pointer;

  dataset: ADOInt.Recordset;
  bdatabasefailed: boolean;
begin
  dataset:=nil;

  try
    try
      manager := CoCSearchManager.Create;
      if Succeeded(manager.GetCatalog('SystemIndex',catalogManager)) then
      begin
        if Succeeded(catalogManager.GetQueryHelper(queryHelper)) then
        begin
          MaxResults:=GetMaxCount(ASearch);
          queryHelper.Set_QueryMaxResults(MaxResults);
          queryHelper.Set_QuerySelectColumns('"System.ItemUrl" , "System.ItemNameDisplay", "System.ItemTypeText", "System.ItemPathDisplayNarrow"');

          fQuery:=AQuery;
          queryHelper.GenerateSQLFromUserQuery(PWideChar(fQuery),temp);
          wQuery := temp;

          queryHelper.Get_ConnectionString(temp);
          sTemp := temp;
          dataset := CreateComObject(CLASS_Recordset) as _Recordset;
          dataset.CursorLocation := adUseServer;
          dataset.Open(wQuery, stemp, adOpenForwardOnly, adLockReadOnly, adCmdText);

          if not dataset.EOF then
          begin
            dataset.MoveFirst;

            Parent:=AGUI.AddHelpItem(Tch2ProviderWindowsSearchCategory.Create(
                                      Self, ASearch));

            idx:=1;
            while (not dataset.EOF) and (idx<=MaxResults) do
            begin
              inc(idx);

              AGUI.AddHelpItem(Tch2ProviderWindowsSearchItem.Create(
                                      Self,
                                      dataset.Fields[3].Value,
                                      dataset.Fields[0].Value,
                                      dataset.Fields[2].Value,
                                      ASearch),
                                Parent);

              dataset.MoveNext;
            end;
          end;
        end
      end
    except
      on E:Exception do
      begin

        raise;
      end;
    end;
  finally
    dataset:=nil;
  end;
end;

procedure Tch2ProviderWindowsSearch.SetDecoration(AName: String;
  ADeco: Tch2HelpItemDecoration);
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_SEARCHES+'\'+AName, true) then
    begin
      ADeco.SaveToRegistry(Reg);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure Tch2ProviderWindowsSearch.SetPriority(APriority: Integer);
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

function Tuch2FormProviderWindowsSearch.AddItemToList(ACaption: String): TListItem;
var
  SavedDecoPtr : Pch2HelpItemDecoration;
begin
  New(SavedDecoPtr);
  SavedDecoPtr^:=FProvider.GetDecoration(ACaption);
  Result:=LV.Items.Add;
  Result.Caption:=ACaption;
  Result.SubItems.Add(FProvider.GetQuery(ACaption));
  Result.SubItems.Add(IntToStr(FProvider.GetMaxCount(ACaption)));
  Result.Data:=SavedDecoPtr;  
end;

procedure Tuch2FormProviderWindowsSearch.btn_AddClick(Sender: TObject);
begin
  LV.Selected:=AddItemToList;
end;

procedure Tuch2FormProviderWindowsSearch.btn_DelClick(Sender: TObject);
var
  item : TListItem;
begin
  item:=LV.Selected;
  if Assigned(Item) then
  begin
    Dispose(item.Data);
    Item.Free;
  end;
end;     

procedure Tuch2FormProviderWindowsSearch.btn_OKClick(Sender: TObject);
var
  Reg : TRegistry;
  idx: Integer;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    Reg.DeleteKey(ch2Main.RegRootKeyProvider[FProvider.GetGUID]+'\'+REG_KEY_SEARCHES);

    for idx := 0 to LV.Items.Count - 1 do
    begin
      if Reg.OpenKey(ch2Main.RegRootKeyProvider[FProvider.GetGUID]+'\'+REG_KEY_SEARCHES+'\'+lv.Items[idx].Caption, true) then
      begin
        Pch2HelpItemDecoration(lv.Items[idx].Data)^.SaveToRegistry(Reg);
        Reg.WriteString(REG_VALUE_QUERY,lv.Items[idx].SubItems[0]);
        Reg.WriteInteger(REG_VALUE_MAXCOUNT,StrToIntDef(lv.Items[idx].SubItems[1],20));
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;

  while lv.Items.Count > 0 do
  begin
    dispose(lv.Items[0].Data);
    lv.Items.Delete(0);   
  end;                 

  ModalResult:=mrOk;
end;

procedure Tuch2FormProviderWindowsSearch.OnDecoChange(ASender: TObject);
begin
  if Lv.Selected<>nil then
  begin
    Pch2HelpItemDecoration(lv.Selected.Data)^:=frame_Deco.Decoration;
  end;
end;

procedure Tuch2FormProviderWindowsSearch.ed_maxResultsChange(Sender: TObject);
begin
  if lv.Selected<>nil then
  begin
    LV.Selected.SubItems[1]:=IntToStr(ed_maxResults.Value);
  end;
end;

procedure Tuch2FormProviderWindowsSearch.ed_NameChange(Sender: TObject);
begin
  if lv.Selected<>nil then
  begin
    LV.Selected.Caption:=ed_Name.Text;
  end;
end;

procedure Tuch2FormProviderWindowsSearch.ed_QueryChange(Sender: TObject);
begin
  if lv.Selected<>nil then
  begin
    LV.Selected.SubItems[0]:=ed_Query.Text;
  end; 
end;

class procedure Tuch2FormProviderWindowsSearch.Execute(
  AProvider: Tch2ProviderWindowsSearch);
var
  Form: Tuch2FormProviderWindowsSearch;
begin
  Form:=Tuch2FormProviderWindowsSearch.Create(nil);
  try
    Form.FProvider:=AProvider;
    Form.Init;
    
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

procedure Tuch2FormProviderWindowsSearch.Init;   
var
  sl : TStringList;
  idx: Integer;        
begin
  frame_Deco.OnChange:=OnDecoChange;  
  sl:=TStringList.Create;
  try
    FProvider.GetProviderList(sl);
    for idx := 0 to sl.Count - 1 do
      AddItemToList(sl[idx]);
  finally
    sl.Free;
  end;
end;

procedure Tuch2FormProviderWindowsSearch.LVChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  if Item=LV.Selected then
  begin
    ed_Name.Text:=Item.Caption;
    ed_Query.Text:=Item.SubItems[0];
    ed_maxResults.Value:=StrToIntDef(Item.SubItems[1],20);
    frame_Deco.Decoration:=Pch2HelpItemDecoration(Item.Data)^;
  end;
end;

{ Tch2ProviderWindowsSearchCategory }

constructor Tch2ProviderWindowsSearchCategory.Create(
  AProvider: Tch2ProviderWindowsSearch; ACaption: String);
begin
  inherited Create;
  FProvider:=AProvider;
  FCaption:=ACaption;
end;

function Tch2ProviderWindowsSearchCategory.GetCaption: String;
begin
  Result:=FCaption;
end;

function Tch2ProviderWindowsSearchCategory.GetDecoration: Tch2HelpItemDecoration;
begin
  Result:=FProvider.GetDecoration(FCaption);
end;

function Tch2ProviderWindowsSearchCategory.GetDescription: String;
begin
  Result:=''
end;

function Tch2ProviderWindowsSearchCategory.GetFlags: Tch2HelpItemFlags;
begin
  Result:=[ifSaveStats];
end;

function Tch2ProviderWindowsSearchCategory.GetGUID: TGUID;
begin
  Result:=FProvider.GetGUIDForName(FCaption);
end;

procedure Tch2ProviderWindowsSearchCategory.ShowHelp;
begin
end;

{ Tch2ProviderWindowsSearchItem }

constructor Tch2ProviderWindowsSearchItem.Create(
  AProvider: Tch2ProviderWindowsSearch; ACaption, AURL, ADescription,
  ASearchName: String);
begin
  inherited Create;
  FProvider:=AProvider;
  FCaption:=ACaption;
  FURL:=AURL;
  FDescription:=ADescription;
  FSearchName:=ASearchName;
end;

function Tch2ProviderWindowsSearchItem.GetCaption: String;
begin
  Result:=FCaption;
end;

function Tch2ProviderWindowsSearchItem.GetDecoration: Tch2HelpItemDecoration;
begin
  Result:=FProvider.GetDecoration(FSearchName);
end;

function Tch2ProviderWindowsSearchItem.GetDescription: String;
begin
  Result:=FDescription;
end;

function Tch2ProviderWindowsSearchItem.GetFlags: Tch2HelpItemFlags;
begin
  Result:=[ifProvidesHelp];
end;

function Tch2ProviderWindowsSearchItem.GetGUID: TGUID;
begin
end;

procedure Tch2ProviderWindowsSearchItem.ShowHelp;
begin
  ch2Main.ShellOpen(FURL);
end;

initialization
  ch2Main.RegisterProvider(Tch2ProviderWindowsSearch.Create as Ich2Provider);


end.
