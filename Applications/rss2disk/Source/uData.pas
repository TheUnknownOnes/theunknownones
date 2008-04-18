//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uData;

interface

uses
  SysUtils, Classes, JvComponentBase, JvTrayIcon, Forms,
  IniFiles, uSystools, ImageHlp, ShlObj, Windows, Controls, Menus,
  jwabits, ActiveX, Dialogs;

type
  TFeed = class
  private
    FIni : TIniFile;
     
    FName: String;
    FSource: String;
    FSaveTo: String;
    procedure SetSaveTo(const Value: String);

  public
    constructor Create();
    destructor Destroy();

    procedure LoadFromIni(AIni : TIniFile; ASection : String);
    procedure SaveToIni(AIni : TIniFile; ASection : String);

    property Name : String read FName write FName;
    property Source : String read FSource write FSource;
    property SaveTo : String read FSaveTo write SetSaveTo;
  end;

  TFeedList = class(TList)
  private
    function Get(Index: Integer): TFeed;
    procedure Put(Index: Integer; const Value: TFeed);
  public
    function Add(Item: TFeed): Integer;
    function Extract(Item: TFeed): TFeed;
    function First: TFeed;
    function IndexOf(Item: TFeed): Integer;
    procedure Insert(Index: Integer; Item: TFeed);
    function Last: TFeed;
    function Remove(Item: TFeed): Integer;
    property Items[Index: Integer]: TFeed read Get write Put; default;
  end; 

  TData = class(TDataModule)
    Tray: TJvTrayIcon;
    pum_Tray: TPopupMenu;
    mi_Close: TMenuItem;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure TrayDblClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mi_CloseClick(Sender: TObject);
  private
    FBits : IBackgroundCopyManager;

    FIni : TIniFile;

    procedure LoadFeedsFromIni;
    procedure SaveFeedsToIni;
  public
    Feeds : TFeedList;

  end;

var
  Data: TData;

implementation

{$R *.dfm}

procedure TData.DataModuleCreate(Sender: TObject);
var
  IniFile : String;
begin
  CoInitialize(nil);

  if CoCreateInstance(CLSID_BackgroundCopyManager, nil, CLSCTX_LOCAL_SERVER, IID_IBackgroundCopyManager, FBits) <> S_OK then
  begin
    MessageDlg('BITS unvailable!', mtError, [mbOK], 0);
    Application.Terminate;
  end;

  Tray.Icon.Assign(Application.Icon);

  IniFile := IncludeTrailingPathDelimiter(GetShellFolder(CSIDL_APPDATA)) +
             'rss2disk\settings.ini';

  MakeSureDirectoryPathExists(PChar(ExtractFilePath(IniFile)));

  FIni := TIniFile.Create(IniFile);

  Feeds := TFeedList.Create;
  LoadFeedsFromIni;
end;

procedure TData.DataModuleDestroy(Sender: TObject);
begin
  SaveFeedsToIni;
  
  while Feeds.Count > 0 do
  begin
    Feeds.First.Free;
    Feeds.Delete(0);
  end;
  
  Feeds.Free;
  
  if Assigned(FIni) then
    FIni.Free;

  FBits := nil;

  CoUninitialize;
end;

procedure TData.LoadFeedsFromIni;
var
  Feed : TFeed;
  idx : Integer;
begin
  for idx := 0 to Fini.ReadInteger('General', 'FeedCount', 0) - 1 do
  begin
    Feed := TFeed.Create;
    Feed.LoadFromIni(Fini, 'Feed' + IntToStr(idx));

    Feeds.Add(Feed);
  end;

end;

procedure TData.mi_CloseClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TData.SaveFeedsToIni;
var
  idx : Integer;
begin
  Fini.WriteInteger('General', 'FeedCount', Feeds.Count);

  for idx := 0 to Feeds.Count - 1 do
  begin
    Feeds[idx].SaveToIni(Fini, 'Feed' + IntToStr(idx));
  end;
end;

procedure TData.TrayDblClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Tray.ShowApplication;
  Application.MainForm.BringToFront;
  Application.BringToFront;
end;

{ TFeed }

constructor TFeed.Create();
begin
  
end;

destructor TFeed.Destroy;
begin

end;

procedure TFeed.LoadFromIni(AIni: TIniFile; ASection: String);
begin
  FName := AIni.ReadString(ASection, 'Name', 'FeedWithNoName');
  FSource := AIni.ReadString(ASection, 'Source', '');
  FSaveTo := IncludeTrailingPathDelimiter(AIni.ReadString(ASection, 'SaveTo', ''));

  MakeSureDirectoryPathExists(PChar(FSaveTo));
end;

procedure TFeed.SaveToIni(AIni: TIniFile; ASection: String);
begin
  AIni.WriteString(ASection, 'Name', FName);
  AIni.WriteString(ASection, 'Source', FSource);
  AIni.WriteString(ASection, 'SaveTo', FSaveTo);
end;

procedure TFeed.SetSaveTo(const Value: String);
begin
  FSaveTo := IncludeTrailingPathDelimiter(Value);
end;

{ TFeedList }

function TFeedList.Add(Item: TFeed): Integer;
begin
  Result := inherited Add(Item);
end;

function TFeedList.Extract(Item: TFeed): TFeed;
begin
  Result := inherited Extract(Item)
end;

function TFeedList.First: TFeed;
begin
  Result := inherited First;
end;

function TFeedList.Get(Index: Integer): TFeed;
begin
  Result := inherited Get(Index);
end;

function TFeedList.IndexOf(Item: TFeed): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TFeedList.Insert(Index: Integer; Item: TFeed);
begin
  inherited Insert(Index, Item);
end;

function TFeedList.Last: TFeed;
begin
  Result := inherited Last;
end;

procedure TFeedList.Put(Index: Integer; const Value: TFeed);
begin
  inherited Put(Index, Value);
end;

function TFeedList.Remove(Item: TFeed): Integer;
begin
  Result := inherited Remove(Item);
end;

end.
