unit uch2Main;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  HelpIntfs,
  Dialogs,
  ToolsAPI;

type
  Tch2HelpViewer = class(TInterfacedObject, ICustomHelpViewer)
  private
    FID : Integer;
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

  public
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;

    property ID : Integer read FID;
  end;

  Tch2Main = class
  private
    FHelpViewer : Tch2HelpViewer;
    FHelpManager : IHelpManager;
  public

    constructor Create();
    destructor Destroy; override;

    property HelpManager : IHelpManager read FHelpManager;
  end;

  Tch2HelpSelector = class(TInterfacedObject, IHelpSelector, IHelpSelector2)
  private
    {$REGION 'IHelpSelector'}
    function SelectKeyword(Keywords: TStrings) : Integer;
    function TableOfContents(Contents: TStrings): Integer;
    {$ENDREGION}
    {$REGION 'IHelpSelector2'}
    function SelectContext(Viewers: TStrings): Integer;
    {$ENDREGION}
  public

  end;

var
  ch2Main : Tch2Main;

implementation

uses uch2HelpSelector;


{ Tch2HelpViewer }


procedure Tch2HelpViewer.AfterConstruction;
begin
  inherited;

end;

procedure Tch2HelpViewer.BeforeDestruction;
begin
  inherited;

  ch2Main.FHelpViewer := nil;
end;

function Tch2HelpViewer.CanShowTableOfContents: Boolean;
begin
  Result := false;
end;


function Tch2HelpViewer.GetHelpStrings(const HelpString: string): TStringList;
var
  hs : IHelpSystem;
begin
  Result := TStringList.Create;
  Result.Add('CustomHelp2://' + HelpString);

  if GetHelpSystem(hs) then
    hs.AssignHelpSelector(Tch2HelpSelector.Create as IHelpSelector);
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
  //ShowMessage(HelpString);
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
begin
  Result := 1;
end;

{ Tch2Main }

constructor Tch2Main.Create;
begin
  FHelpViewer := Tch2HelpViewer.Create;

  RegisterViewer(FHelpViewer as ICustomHelpViewer, FHelpManager);
end;

destructor Tch2Main.Destroy;
begin
  HelpManager.Release(FHelpViewer.ID);

  inherited;
end;

{ Tch2HelpSelector }

function Tch2HelpSelector.SelectContext(Viewers: TStrings): Integer;
begin
  Result := -1;
end;

function Tch2HelpSelector.SelectKeyword(Keywords: TStrings): Integer;
var
  idx : Integer;
begin
  Result := -1;

  for idx := Keywords.Count - 1 downto 0 do
  begin
    if not AnsiStartsStr('CustomHelp2://', Keywords[idx]) then
      Keywords.Delete(idx);
  end;

  ShowMessage(Keywords.Text);
end;

function Tch2HelpSelector.TableOfContents(Contents: TStrings): Integer;
begin
  Result := -1;
end;

initialization
  ch2Main := Tch2Main.Create;

finalization
  ch2Main.Free;


end.
