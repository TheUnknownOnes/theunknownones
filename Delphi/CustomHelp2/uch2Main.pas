unit uch2Main;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  HelpIntfs,
  Windows,
  Menus,
  Dialogs,
  Registry,
  ToolsAPI,
  uTUOCommonIntf,
  ComCtrls,
  ShellAPI,
  Controls;

type
  Ich2Provider = interface
    ['{0B58226D-72D9-457D-9282-51CF1D87CEAB}']
    function GetName : String;

    function GetGUID() : TGUID;

    function GetSettingsGUI : TControl;

    procedure FillHelpTree(AKeyword : String; ATreeView : TTreeView);
    procedure HandleDoubleClick(ATreeNode : TTreeNode; AHelpString : String);

    procedure StartHelpSession;
    procedure StopHelpSession;

    property Name : String read GetName;
    property GUID : TGUID read GetGUID;
  end;

  Tch2HelpViewer = class(TInterfacedObject, ICustomHelpViewer)
  private
    FID : Integer;
    FKeywords: TStringList;
    FHelpString: String;
  public
    {$REGION 'ICustomHelpViewer'}
    function  GetViewerName: string;
    function  UnderstandsKeyword(const HelpString: string): Integer;
    function  GetHelpStrings(const HelpString: string): TStringList;
    function  CanShowTableOfContents : Boolean;
    procedure ShowTableOfContents;
    procedure ShowHelp(const HelpString: string);
    procedure NotifyID(const ViewerID: Integer);
    procedure SoftShutDown;
    procedure ShutDown;
    {$ENDREGION}

    procedure BuildKeywordList(AHelpString : String);
  public
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;

    property ID : Integer read FID;
    property Keywords : TStringList read FKeywords;
    property HelpString : String read FHelpString;
  end;

  Tch2Main = class
  private
    FHelpViewer : Tch2HelpViewer;
    FHelpManager : IHelpManager;

    FMenuItemConfig : TMenuItem;
    FProviders: TInterfaceList;
    FRegistry: TRegistry;

    procedure AttachToIDE;
    procedure DetachFromIDE;

    procedure OnConfigure(Sender : TObject);
    function GetProvider(AIndex: Integer): Ich2Provider;

    procedure LoadSettings;
    procedure SaveSettings;
    function GetRegRootKey: String;
    function GetRegRootKeyProvider(const AProvider: Ich2Provider): String;
  public

    constructor Create();
    destructor Destroy; override;

    procedure ShowInWelcomePage(AURL : String);

    procedure RegisterProvider(AProvider : Ich2Provider);

    property HelpViewer : Tch2HelpViewer read FHelpViewer;
    property HelpManager : IHelpManager read FHelpManager;
    property Providers : TInterfaceList read FProviders;
    property Provider[AIndex : Integer] : Ich2Provider read GetProvider;

    property Registry : TRegistry read FRegistry;
    property RegRootKey : String read GetRegRootKey;
    property RegRootKeyProvider[const AProvider : Ich2Provider] : String read GetRegRootKeyProvider;
  end;

  Tch2HelpSelector = class(TInterfacedObject, IHelpSelector)
  private
    {$REGION 'IHelpSelector'}
    function SelectKeyword(Keywords: TStrings) : Integer;
    function TableOfContents(Contents: TStrings): Integer;
    {$ENDREGION}
  public

  end;

var
  ch2Main : Tch2Main;

implementation

uses uch2HelpSelector, uch2Config;


{ Tch2HelpViewer }


procedure Tch2HelpViewer.AfterConstruction;
begin
  inherited;

  FKeywords := TStringList.Create;
end;

procedure Tch2HelpViewer.BeforeDestruction;
begin
  inherited;

  FKeywords.Free;

  ch2Main.FHelpViewer := nil;
end;

procedure Tch2HelpViewer.BuildKeywordList(AHelpString: String);
var
  slSplit : TStringList;
  Editor : IOTAEditor;
  SourceEditor : IOTASourceEditor70;
  EditView : IOTAEditView;
  idx : Integer;
  cls : TClass;

  function WordsFromSplitList(ACount : Integer; AReverse : Boolean) : String;
  var
    idxWord : Integer;
  begin
    Result := '';

    if not AReverse then
    begin
      for idxWord := 0 to ACount - 1 do
      begin
        if idxWord > 0 then
          Result := Result + slSplit.Delimiter;
        Result := Result + slSplit[idxWord];
      end;
    end
    else
    begin
      for idxWord := 1 to ACount do
      begin
        if idxWord > 1 then
          Result := slSplit.Delimiter + Result;
        Result := slSplit[slSplit.Count - idxWord] + Result;
      end;
    end;
  end;

  procedure AddKeyword(AKeyWord : String);
  begin
    if FKeywords.IndexOf(AKeyWord) = -1 then
      FKeywords.Add(AKeyWord);
  end;

  procedure AddSplitContent;
  var
    idxSplit : Integer;
  begin
    for idxSplit := 1 to slSplit.Count do
      AddKeyword(WordsFromSplitList(idxSplit, false));

    for idxSplit := 1 to slSplit.Count do
      AddKeyword(WordsFromSplitList(idxSplit, true));

    for idxSplit := 0 to slSplit.Count - 1 do
      AddKeyword(slSplit[idxSplit]);
  end;
begin
  FKeywords.Clear;
  FKeywords.Add(AHelpString);

  slSplit := TStringList.Create;
  try
    slSplit.Delimiter := '.';
    slSplit.StrictDelimiter := true;

    if Assigned((BorlandIDEServices as IOTAModuleServices).CurrentModule) then
    begin
      Editor:=(BorlandIDEServices as IOTAModuleServices).CurrentModule.CurrentEditor;

      if Supports(Editor, IOTASourceEditor70, SourceEditor) then
      begin
        EditView:=SourceEditor.EditViews[0];
        slSplit.DelimitedText := EditView.Position.RipText(['_','.'],rfBackward or rfIncludeAlphaChars or rfIncludeNumericChars)+
                                 EditView.Position.RipText(['_','.'],rfIncludeAlphaChars or rfIncludeNumericChars);
      end;
    end;

    AddSplitContent;

    slSplit.DelimitedText := AHelpString;
    AddSplitContent;

    for idx := 0 to FKeywords.Count - 1 do
    begin
      cls := GetClass(FKeywords[idx]);
      while Assigned(cls) do
      begin
        AddKeyword(cls.ClassName);
        cls := cls.ClassParent;
      end;
    end;
  finally
    slSplit.Free;
  end;


end;

function Tch2HelpViewer.CanShowTableOfContents: Boolean;
begin
  Result := false;
end;


function Tch2HelpViewer.GetHelpStrings(const HelpString: string): TStringList;
begin
  Result := TStringList.Create;
  Result.Add('U should never see me');
end;

function Tch2HelpViewer.GetViewerName: string;
begin
  Result := 'CustomHelp2 Viewer';
end;

procedure Tch2HelpViewer.NotifyID(const ViewerID: Integer);
begin
  FID := ViewerID;
end;

procedure Tch2HelpViewer.ShowHelp(const HelpString: string);
begin
  Tch2formHelpSelector.Execute;
end;

procedure Tch2HelpViewer.ShowTableOfContents;
begin
end;

procedure Tch2HelpViewer.ShutDown;
begin

end;

procedure Tch2HelpViewer.SoftShutDown;
begin

end;

function Tch2HelpViewer.UnderstandsKeyword(const HelpString: string): Integer;
var
  hs : IHelpSystem;
begin
  Result := 1;

  if GetHelpSystem(hs) then
    hs.AssignHelpSelector(Tch2HelpSelector.Create as IHelpSelector);

  BuildKeywordList(HelpString);
  FHelpString := HelpString;
end;

{ Tch2Main }

procedure Tch2Main.AttachToIDE;
begin
  FMenuItemConfig := TMenuItem.Create(nil);
  FMenuItemConfig.Caption := 'Configure CustomHelp';
  FMenuItemConfig.OnClick := OnConfigure;

  with GetTUOCommon do
  begin
    ToolsMenuItem.Add(FMenuItemConfig);
  end;
end;

constructor Tch2Main.Create;
begin
  FHelpViewer := Tch2HelpViewer.Create;

  RegisterViewer(FHelpViewer as ICustomHelpViewer, FHelpManager);

  AttachToIDE;

  FProviders := TInterfaceList.Create;

  FRegistry := TRegistry.Create(KEY_ALL_ACCESS);
  FRegistry.RootKey := HKEY_CURRENT_USER;
end;

destructor Tch2Main.Destroy;
begin
  HelpManager.Release(FHelpViewer.ID);
  DetachFromIDE;

  SaveSettings;
  FRegistry.Free;

  FProviders.Free;

  inherited;
end;

procedure Tch2Main.DetachFromIDE;
begin
  FMenuItemConfig.Free;
end;

function Tch2Main.GetProvider(AIndex: Integer): Ich2Provider;
begin
  Result := FProviders[AIndex] as Ich2Provider;
end;

function Tch2Main.GetRegRootKey: String;
begin
  Result := GetTUOCommon.RegistryRootKey + '\CustomHelp2';
end;

function Tch2Main.GetRegRootKeyProvider(const AProvider: Ich2Provider): String;
begin
  Result := RegRootKey + '\' + GUIDToString(AProvider.GUID);
end;

procedure Tch2Main.LoadSettings;
begin

end;

procedure Tch2Main.OnConfigure(Sender: TObject);
begin
  Tch2FormConfig.Execute;
end;

procedure Tch2Main.RegisterProvider(AProvider: Ich2Provider);
begin
  FProviders.Add(AProvider);
end;

procedure Tch2Main.SaveSettings;
begin

end;

procedure Tch2Main.ShowInWelcomePage(AURL: String);
begin
  ShellExecute(0, 'open', PChar(AURL), nil, nil, SW_SHOW);
end;

{ Tch2HelpSelector }

function Tch2HelpSelector.SelectKeyword(Keywords: TStrings): Integer;
begin
  if Assigned(Keywords) then
    Result := Keywords.Count - 1
  else
    Result := -1;
end;

function Tch2HelpSelector.TableOfContents(Contents: TStrings): Integer;
begin
  if Assigned(Contents) then
    Result := Contents.Count - 1
  else
    Result := -1;
end;

initialization
  ch2Main := Tch2Main.Create;

finalization
  ch2Main.Free;


end.
