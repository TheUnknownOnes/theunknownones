unit uch2ProvStaticWeb;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uch2Main, ComCtrls, Contnrs, Registry;

type

  TStaticWebContent = class
  private
    FGUID: TGUID;
  public
    Name : String;
    URL : String;

    constructor Create(); overload;
    constructor Create(AGUID : TGUID); overload;
    destructor Destroy; override;

    procedure LoadFromRegistry(ARegistry : TRegistry);
    procedure SaveToRegistry(ARegistry : TRegistry);

    procedure ShowHelp(AHelpString : String);

    property GUID : TGUID read FGUID;
  end;

  TProvStaticWeb = class(TInterfacedObject, Ich2Provider)
  private
    FContents: TObjectList;
    FRootNode : TTreeNode;
    FRootNodeExpanded : Boolean;
    FMyNodes : TList;

    {$REGION 'Ich2Provider'}
    function GetName : String;

    function GetGUID() : TGUID;

    function GetSettingsGUI : TControl;

    procedure StartHelpSession;
    procedure StopHelpSession;

    procedure FillHelpTree(AKeyword : String; ATreeView : TTreeView);
    procedure HandleDoubleClick(ATreeNode : TTreeNode; AHelpString : String);
    {$ENDREGION}

    function GetContent(AIndex: Integer): TStaticWebContent;
    procedure LoadSettings;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Content[AIndex : Integer] : TStaticWebContent read GetContent;
    property Contents : TObjectList read FContents;
  end;

  Tch2FrameProvStaticWeb = class(TFrame)
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

var
  FProvStaticWeb : TProvStaticWeb;


{ TProvStaticWeb }

procedure TProvStaticWeb.AfterConstruction;
begin
  inherited;

  FContents := TObjectList.Create(true);
  LoadSettings;
  FMyNodes := TList.Create;
end;

procedure TProvStaticWeb.BeforeDestruction;
begin
  inherited;

  FContents.Free;
  FMyNodes.Free;
end;

procedure TProvStaticWeb.FillHelpTree(AKeyword: String; ATreeView: TTreeView);
var
  idx : Integer;
begin
  FMyNodes.Clear;

  FRootNode := ATreeView.Items.AddChild(nil, GetName);

  for idx := 0 to FContents.Count - 1 do
  begin
    FMyNodes.Add(ATreeView.Items.AddChildObject(FRootNode, Content[idx].Name, Content[idx]));
  end;

  FRootNode.Expanded := FRootNodeExpanded;
end;

function TProvStaticWeb.GetContent(AIndex: Integer): TStaticWebContent;
begin
  Result := TStaticWebContent(FContents[AIndex]);
end;

function TProvStaticWeb.GetGUID: TGUID;
begin
  Result := StringToGUID('{682144E7-5FEC-491A-AF2B-8D9E049086E0}')
end;

function TProvStaticWeb.GetName: String;
begin
  Result := 'Static Websearch';
end;

function TProvStaticWeb.GetSettingsGUI: TControl;
begin
  Result := Tch2FrameProvStaticWeb.Create(nil);
end;

procedure TProvStaticWeb.HandleDoubleClick(ATreeNode: TTreeNode; AHelpString : String);
begin
  if FMyNodes.IndexOf(ATreeNode) > -1 then
  begin
    TStaticWebContent(ATreeNode.Data).ShowHelp(AHelpString);
  end;

end;

procedure TProvStaticWeb.LoadSettings;
type
  TDefaultContent = record
    Name : String;
    URL : String;
  end;

var
  myRoot : String;
  cont : TStaticWebContent;
  defCont : TDefaultContent;
  sl : TStringList;
  idx : Integer;

const
  DefaultContents : array[0..3] of TDefaultContent =
    ((Name: 'DP DelphiReference'; URL: 'http://www.delphipraxis.net/dp_reference.php?query=$(HelpString)'),
     (Name: 'Koders.com'; URL: 'http://www.koders.com/default.aspx?submit=Search&la=Delphi&li=*&s=$(HelpString)'),
     (Name: 'Google Codesearch'; URL: 'http://www.google.com/codesearch?btnG=Code+suchen&hl=de&as_lang=pascal&as_license_restrict=i&as_license=&as_package=&as_filename=&as_case=&as_q=$(HelpString)'),
     (Name: 'MSDN Online'; URL: 'http://search.msdn.microsoft.com/Default.aspx?locale=en-US&Query=$(HelpString)')
    );
begin
  myRoot := ch2Main.RegRootKeyProvider[Self];

  with ch2Main.Registry do
  begin
    CloseKey;

    if not OpenKey(myRoot, false) then
    begin
      //write defaults

      OpenKey(myRoot, true);

      WriteBool('RootNodeExpanded', true);
      FRootNodeExpanded := true;
      CloseKey;

      for defCont in DefaultContents do
      begin
        cont := TStaticWebContent.Create;
        FContents.Add(cont);
        cont.Name := defCont.Name;
        cont.URL := defCont.URL;
        OpenKey(myRoot + '\' + GUIDToString(cont.GUID), true);
        cont.SaveToRegistry(ch2Main.Registry);
        CloseKey;
      end;
    end
    else
    begin
      FRootNodeExpanded := ValueExists('RootNodeExpanded') and ReadBool('RootNodeExpanded');

      sl := TStringList.Create;
      GetKeyNames(sl);
      for idx := 0 to sl.Count - 1 do
      begin
        cont := TStaticWebContent.Create(StringToGUID(sl[idx]));
        FContents.Add(cont);
        OpenKey(myRoot + '\' + GUIDToString(cont.GUID), true);
        cont.LoadFromRegistry(ch2Main.Registry);
        CloseKey;
      end;

      sl.Free;
    end;

    CloseKey;

  end;
end;

procedure TProvStaticWeb.StartHelpSession;
begin

end;

procedure TProvStaticWeb.StopHelpSession;
begin
  FRootNodeExpanded := FRootNode.Expanded;
end;

{ TStaticWebContent }

constructor TStaticWebContent.Create;
begin
  CreateGUID(FGUID);
end;

constructor TStaticWebContent.Create(AGUID: TGUID);
begin
  FGUID := AGUID;
end;

destructor TStaticWebContent.Destroy;
begin
  inherited;
end;

procedure TStaticWebContent.LoadFromRegistry(ARegistry: TRegistry);
begin
  with ARegistry do
  begin
    if ValueExists('Name') then
      Name := ReadString('Name')
    else
      Name := 'NoName';

    if ValueExists('URL') then
      URL := ReadString('URL')
    else
      URL := 'http://www.google.com/q=$(HelpString)';
  end;
end;

procedure TStaticWebContent.SaveToRegistry(ARegistry: TRegistry);
begin
  with ARegistry do
  begin
    WriteString('Name', Name);
    WriteString('URL', URL);
  end;
end;

procedure TStaticWebContent.ShowHelp(AHelpString: String);
var
  hs : AnsiString;
  idx : Integer;
  Encoded : String;
begin
  Encoded := '';
  hs := AHelpString;

  for idx := 1 to Length(hs) do
  begin
    Encoded := Encoded + '%' + IntToHex(Ord(hs[idx]), 2);
  end;

  Encoded := StringReplace(URL, '$(HelpString)', Encoded, [rfReplaceAll, rfIgnoreCase]);

  ch2Main.ShowInWelcomePage(Encoded);
end;

initialization
  FProvStaticWeb := TProvStaticWeb.Create;
  ch2Main.RegisterProvider(FProvStaticWeb as Ich2Provider);

end.
