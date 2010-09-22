unit uch2ProviderMsHelp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Spin, ComCtrls, ToolWin, StdCtrls, ExtCtrls, uch2MSHelpServices,
  uch2Main, SyncObjs, uch2FrameHelpItemDecoration;

type
  Tch2ProviderMSHelp = class(TInterfacedObject, Ich2Provider)
  private
    FPrority : Integer;
    FEnabledhxSessions: TInterfaceList;
    FSessionLock:     TCriticalSection;


    function GetGUIDForNamespace(ANamespace: String): TGUID;
    function GetNamespaceEnabled(ANamespace: String): Boolean;
    procedure SetNamespaceEnabled(ANamespace: String; AEnabled: Boolean);
    function GetNamespaceSearchType(ANamespace: String): String;
    function SetNamespaceSearchType(ANamespace: String; ASearchType: String): String;
    function GetDecoration(ANamespace: String): Tch2HelpItemDecoration;
    procedure SetDecoration(ANamespace: String; ADeco: Tch2HelpItemDecoration);



    procedure InitSessions;
    function QueryInHxSession(hxSession: IHxSession;
      const HelpString: string) : IHxTopicList;
    function SearchInHxSession(hxSession: IHxSession; const HelpString: string;
      hxIndex: IHxIndex): IHxTopicList;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    class function GetNamespaces: IHxRegNamespaceList; static;
    class function CheckIndexInHxSession(hxSession: IHxSession;
      var hxIndex: IHxIndex): Boolean; static;
    class function GetNamespaceTitle(Session: IHxSession): string; static;
    class function GetNamespaceName(Session: IHxSession): string; static;

    {$REGION 'Ich2Provider'}
    function GetGUID : TGUID;
    function GetDescription : String;
    function GetName : String;

    procedure ProvideHelp(AKeyword : String; AGUI : Ich2GUI);

    procedure Configure;

    function GetPriority : Integer;
    {$ENDREGION}
  end;

  Tch2FormProviderMsHelp = class(TForm)
    GroupBox2: TGroupBox;
    lvNamespaces: TListView;
    GroupBox1: TGroupBox;
    ed_Prio: TSpinEdit;
    GroupBox3: TGroupBox;
    FrameHelpItemDeco: Tch2FrameHelpItemDecoration;
    Panel1: TPanel;
    btn_OK: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvNamespacesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ed_PrioChange(Sender: TObject);
  private
    FProvider: Tch2ProviderMSHelp;
    procedure Init;
    procedure OnDecoChange(ASender: TObject);
  public
    class procedure Execute(AProvider: Tch2ProviderMSHelp);
  end;

implementation

uses
  ActiveX, Registry, StrUtils;

const
  REG_KEY_NAMESPACES = 'Namespaces';
  REG_VALUE_ENABLED = 'Enabled';
  REG_VALUE_GUID = 'GUID';
  REG_VALUE_SEARCHTYPE = 'SearchType';
  REG_VALUE_Priority = 'Priority';

  VALUE_SEARCHTYPE_IDX = 'Index';
  VALUE_SEARCHTYPE_FULLTEXT = 'FullText';

type
  Tch2ProviderMsHelpItemCategory = class(TInterfacedObject, Ich2HelpItem)
  private
    FProvider : Tch2ProviderMSHelp;
    FCaption  : String;
    FNamespace : String;
  public
    {$REGION 'Ich2HelpItem'}
    function GetGUID : TGUID; //used to store stats (expanded, position, ...)
    function GetCaption : String;
    function GetDescription : String;
    function GetDecoration : Tch2HelpItemDecoration;
    function GetFlags : Tch2HelpItemFlags;
    procedure ShowHelp;
    {$ENDREGION}

    constructor Create(AProvider: Tch2ProviderMSHelp;
                       ACaption, ANamespace: String);
  end;

  Tch2ProviderMsHelpItemItem = class(TInterfacedObject, Ich2HelpItem)
  private
    FProvider : Tch2ProviderMSHelp;
    FNamespace   : String;
    FCaption     : String;
    FURL         : String;
    FDescription : String;
  public
    {$REGION 'Ich2HelpItem'}
    function GetGUID : TGUID; //used to store stats (expanded, position, ...)
    function GetCaption : String;
    function GetDescription : String;
    function GetDecoration : Tch2HelpItemDecoration;
    function GetFlags : Tch2HelpItemFlags;
    procedure ShowHelp;
    {$ENDREGION}

    constructor Create(AProvider: Tch2ProviderMSHelp;
                       ACaption,
                       AURL,
                       ADescription,
                       ANamespace : String);
  end;

{$R *.dfm}
{$I CustomHelp2.inc}

{ Tch2ProviderMSHelp }

function Tch2ProviderMSHelp.SearchInHxSession(hxSession: IHxSession;
  const HelpString: string; hxIndex: IHxIndex): IHxTopicList;
var
  slot:   Integer;
begin
  Result:=nil;
  FSessionLock.Acquire;
  try
    try
      slot := hxIndex.GetSlotFromString(HelpString);
      if ContainsText(hxIndex.GetStringFromSlot(slot), HelpString) then
      begin
        Result := hxIndex.GetTopicsFromSlot(slot);
      end;
    except
      //just a test :)
    end;
  finally
    FSessionLock.Release;
  end;
end;

function Tch2ProviderMSHelp.QueryInHxSession(hxSession: IHxSession;
  const HelpString: string): IHxTopicList;
begin
  Result := Nil;
  FSessionLock.Acquire;
  try
    Result := hxSession.Query(HelpString, '!DefaultFullTextSearch', HxQuery_No_Option, '');
  finally
    FSessionLock.Release;
  end;
end;

function Tch2ProviderMSHelp.GetNamespaceEnabled(ANamespace: String): Boolean;
var
  Reg : TRegistry;
begin
  inherited;

  Result := true;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_NAMESPACES+'\'+ANamespace, true) then
    begin
      if not Reg.ValueExists(REG_VALUE_ENABLED) then
      begin
        Reg.WriteBool(REG_VALUE_ENABLED, True);
      end;

      Result:=reg.ReadBool(REG_VALUE_ENABLED);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

class function Tch2ProviderMSHelp.GetNamespaceName(Session: IHxSession): string;
begin
  Result := Session.Collection.GetProperty(HxCollectionProp_NamespaceName);
end;

class function Tch2ProviderMSHelp.GetNamespaces: IHxRegNamespaceList;
begin
  try
    Result := CoHxRegistryWalker.Create.RegisteredNamespaceList[''];
  except
    raise Exception.Create('An error occured while talking to HX. Maybe there is no MS Document Explorer installed.');
  end;
end;

function Tch2ProviderMSHelp.GetNamespaceSearchType(ANamespace: String): String;
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_NAMESPACES+'\'+ANamespace, true) then
    begin
      if not Reg.ValueExists(REG_VALUE_SEARCHTYPE) then
      begin
        Reg.WriteString(REG_VALUE_SEARCHTYPE, VALUE_SEARCHTYPE_IDX);
      end;

      Result:=reg.ReadString(REG_VALUE_SEARCHTYPE);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

class function Tch2ProviderMSHelp.GetNamespaceTitle(
  Session: IHxSession): string;
var
  NamespaceName: string;
  idx:           Integer;
  nsList:        IHxRegNamespaceList;
begin
  NamespaceName := GetNamespaceName(Session);
  nsList        := GetNamespaces;
  for idx := 1 to nsList.Count do
  begin
    if nsList.Item(idx).Name = NamespaceName then
    begin
      Result := nsList.Item(idx).GetProperty(HxRegNamespaceDescription);
      break;
    end;
  end;
end;

procedure Tch2ProviderMSHelp.AfterConstruction;
var
  Reg : TRegistry;
begin
  inherited;

  FPrority := 0;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID], true) then
    begin
      if Reg.ValueExists(REG_VALUE_Priority) then
        FPrority := Reg.ReadInteger(REG_VALUE_Priority);
    end;

  finally
    Reg.Free;
  end;

  FEnabledhxSessions:=TInterfaceList.Create;
  FSessionLock:=TCriticalSection.Create;
  InitSessions;
end;

procedure Tch2ProviderMSHelp.BeforeDestruction;
begin
  inherited;
  FEnabledhxSessions.Free;
  FSessionLock.Free;
end;

class function Tch2ProviderMSHelp.CheckIndexInHxSession(hxSession: IHxSession;
  var hxIndex: IHxIndex): Boolean;
begin
  Result := Supports(hxSession.GetNavigationObject('!DefaultKeywordIndex', ''),
    IID_IHxIndex, hxIndex);
end;

procedure Tch2ProviderMSHelp.Configure;
begin
  Tch2FormProviderMsHelp.Execute(self);
  InitSessions;
end;

function Tch2ProviderMSHelp.GetDecoration(
  ANamespace: String): Tch2HelpItemDecoration;
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_NAMESPACES+'\'+ANamespace, true) then
    begin
      Result.LoadFromRegistry(Reg);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function Tch2ProviderMSHelp.GetDescription: String;
begin
  Result:='Get help from the Microsoft Help providers installed in your system';
end;

function Tch2ProviderMSHelp.GetGUID: TGUID;
begin
  Result:=StringToGUID('{83582CD6-7C5C-4325-9BA0-4B0D97FB057F}');
end;

function Tch2ProviderMSHelp.GetGUIDForNamespace(ANamespace: String): TGUID;
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_NAMESPACES+'\'+ANamespace, true) then
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

function Tch2ProviderMSHelp.GetName: String;
begin
  Result:='MsHelp 2.x';
end;

function Tch2ProviderMSHelp.GetPriority: Integer;
begin
  Result:=FPrority;
end;

procedure Tch2ProviderMSHelp.InitSessions;
var
  NamespaceList : IHxRegNamespaceList;
  idx: Integer;
  hxSession: IHxSession;
  hxIndex:   IHxIndex;
begin
  FEnabledhxSessions.Clear;
  NamespaceList:=GetNamespaces;
  for idx := 1 to NamespaceList.Count do
    if GetNamespaceEnabled(NamespaceList.Item(idx).Name) then
    begin
      try
        hxSession := CoHxSession.Create;
        hxSession.Initialize('ms-help://' + NamespaceList.Item(idx).Name, 0);

        CheckIndexInHxSession(hxSession, hxIndex);

        FEnabledhxSessions.Add(hxSession);
      except
        on E:Exception do
        begin
          ShowMessage('Error upon initializing MS Help Namespace "'+NamespaceList.Item(idx).Name+'"'+#13#10+
                      E.Message+#13#10#13#10+
                      'Namespace will be disabled.');
          SetNamespaceEnabled(NamespaceList.Item(idx).Name, False);
        end;
      end;
    end;
end;

procedure Tch2ProviderMSHelp.ProvideHelp(AKeyword: String; AGUI: Ich2GUI);
var
  intf : IInterface;
  Session: IHxSession absolute intf;
  SearchType : String;
  hxIndex: IHxIndex;
  Topics: IHxTopicList;
  Parent : Pointer;
  idx: Integer;
begin
  for intf in FEnabledhxSessions do
  begin
    Topics:=nil;
    SearchType:=GetNamespaceSearchType(GetNamespaceName(Session));
    if SameText(SearchType, VALUE_SEARCHTYPE_FULLTEXT) then
      Topics:=QueryInHxSession(Session, AKeyword)
    else
    begin
      if CheckIndexInHxSession(Session, hxIndex) then
        Topics := SearchInHxSession(Session, AKeyword, hxIndex);
    end;

    if Assigned(Topics) and (Topics.Count>0) then
    begin
      Parent:=AGUI.AddHelpItem(Tch2ProviderMsHelpItemCategory.Create(Self, GetNamespaceTitle(Session), GetNamespaceName(Session)));

      for idx := 1 to Topics.Count do
        AGUI.AddHelpItem(Tch2ProviderMsHelpItemItem.Create(Self,
           Topics.Item(idx).Title[HxTopicGetRLTitle, 0],
           Topics.Item(idx).URL,'',GetNamespaceName(Session)), Parent);

    end;
  end;
end;

procedure Tch2ProviderMSHelp.SetDecoration(ANamespace: String;
  ADeco: Tch2HelpItemDecoration);
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_NAMESPACES+'\'+ANamespace, true) then
    begin
      ADeco.SaveToRegistry(Reg);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure Tch2ProviderMSHelp.SetNamespaceEnabled(ANamespace: String;
  AEnabled: Boolean);
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_NAMESPACES+'\'+ANamespace, true) then
    begin
      Reg.WriteBool(REG_VALUE_ENABLED, AEnabled);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function Tch2ProviderMSHelp.SetNamespaceSearchType(ANamespace,
  ASearchType: String): String;
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_NAMESPACES+'\'+ANamespace, true) then
    begin
      Reg.WriteString(REG_VALUE_SEARCHTYPE, ASearchType);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

{ Tch2FormProviderMsHelp }


procedure Tch2FormProviderMsHelp.OnDecoChange(ASender: TObject);
begin
  if lvNamespaces.Selected<>nil then
  begin
    FProvider.SetDecoration(lvNamespaces.Selected.Caption, FrameHelpItemDeco.Decoration);
  end;
end;

procedure Tch2FormProviderMsHelp.ed_PrioChange(Sender: TObject);
begin
  FProvider.FPrority := ed_Prio.Value;
end;

class procedure Tch2FormProviderMsHelp.Execute(AProvider: Tch2ProviderMSHelp);
var
  Form : Tch2FormProviderMsHelp;
begin
  Form:=Tch2FormProviderMsHelp.Create(nil);
  try
    Form.FProvider:=AProvider;
    Form.Init;
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

procedure Tch2FormProviderMsHelp.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  idx: Integer;
  Reg : TRegistry;
begin
  for idx := 0 to lvNamespaces.Items.Count - 1 do
  begin
    FProvider.SetNamespaceEnabled(lvNamespaces.Items[idx].Caption,
                                  lvNamespaces.Items[idx].Checked);
  end;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[FProvider.GetGUID], true) then
    begin
      Reg.WriteInteger(REG_VALUE_Priority, FProvider.FPrority);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure Tch2FormProviderMsHelp.Init;
var
  EnumNamespaces : IEnumVariant;
  Namespaces: IHxRegNamespaceList;
  Namespace : OleVariant;
  NamespaceIntf : IHxRegNamespace;
  fetched : cardinal;

  li : TListItem;
begin
  FrameHelpItemDeco.OnChange:=onDecoChange;
  Namespaces:=FProvider.GetNamespaces;
  EnumNamespaces:=Namespaces._NewEnum as IEnumVARIANT;
  lvNamespaces.Items.BeginUpdate;
  while EnumNamespaces.Next(1, Namespace, fetched)=S_OK do
  begin
    supports(Namespace, IHxRegNamespace, NamespaceIntf);
    li:=lvNamespaces.Items.Add;
    li.Caption:=NamespaceIntf.Name;
    li.SubItems.Add(NamespaceIntf.GetProperty(HxRegNamespaceDescription));
    li.Checked:=FProvider.GetNamespaceEnabled(li.Caption);
  end;
  lvNamespaces.Items.EndUpdate;

  ed_Prio.Value := FProvider.FPrority;
end;


procedure Tch2FormProviderMsHelp.lvNamespacesChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  if Item=lvNamespaces.Selected then
  begin
    FrameHelpItemDeco.Decoration:=FProvider.GetDecoration(Item.Caption)
  end;
end;

{ Tch2ProviderMsHelpItemCategory }

constructor Tch2ProviderMsHelpItemCategory.Create(AProvider: Tch2ProviderMSHelp;
  ACaption, ANamespace: String);
begin
  inherited Create();
  FProvider:=AProvider;
  FCaption:=ACaption;
  FNamespace:=ANamespace;
end;

function Tch2ProviderMsHelpItemCategory.GetCaption: String;
begin
  Result:=FCaption;
end;

function Tch2ProviderMsHelpItemCategory.GetDecoration: Tch2HelpItemDecoration;
begin
  Result:=FProvider.GetDecoration(FNamespace);
end;

function Tch2ProviderMsHelpItemCategory.GetDescription: String;
begin
  Result:='';
end;

function Tch2ProviderMsHelpItemCategory.GetFlags: Tch2HelpItemFlags;
begin
  Result:=[ifSaveStats];

end;

function Tch2ProviderMsHelpItemCategory.GetGUID: TGUID;
begin
  result:=FProvider.GetGUIDForNamespace(FNamespace);
end;

procedure Tch2ProviderMsHelpItemCategory.ShowHelp;
begin

end;

{ Tch2ProviderMsHelpItemItem }

constructor Tch2ProviderMsHelpItemItem.Create(AProvider: Tch2ProviderMSHelp;
  ACaption, AURL, ADescription, ANamespace: String);
begin
  inherited Create();
  FProvider:=AProvider;
  FCaption:=ACaption;
  FNamespace:=ANamespace;
  FURL:=AURL;
end;

function Tch2ProviderMsHelpItemItem.GetCaption: String;
begin
  Result:=FCaption;
end;

function Tch2ProviderMsHelpItemItem.GetDecoration: Tch2HelpItemDecoration;
begin
  Result:=FProvider.GetDecoration(FNamespace);
end;

function Tch2ProviderMsHelpItemItem.GetDescription: String;
begin
  Result:=FDescription;
end;

function Tch2ProviderMsHelpItemItem.GetFlags: Tch2HelpItemFlags;
begin
  Result:=[ifProvidesHelp];
end;

function Tch2ProviderMsHelpItemItem.GetGUID: TGUID;
begin
  Result:=StringToGUID('{81DF310E-D56E-40C2-9907-83D7E4F2CB77}');
end;

procedure Tch2ProviderMsHelpItemItem.ShowHelp;
begin
  ch2Main.ShowInMSDocExplorer(FURL);
end;

initialization
  {$IFDEF ProviderMSHelp}ch2Main.RegisterProvider(Tch2ProviderMSHelp.Create as Ich2Provider);{$ENDIF}


end.
