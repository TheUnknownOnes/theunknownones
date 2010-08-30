unit uCustomHelpIDEIntegration;

interface

uses
  Windows,
  WebBrowserEx,
  ExtCtrls;

function WelcomePageNavigate(AURL: WideString): Boolean;

implementation

uses
  ToolsAPI,
  ActnList,
  SysUtils,
  Dialogs,
  Forms,
  Classes;

type
  IURLModule = interface
    ['{9D215B02-6073-45DC-B007-1A2DBCE2D693}']
    procedure Close;
    function GetURL: string;                             // tested
    procedure SetURL(const AURL: string);                // tested
    procedure SourceActivated;
    function GetWindowClosingEvent: TWindowClosingEvent; // WARNING!!! do NOT CALL!!!
    procedure Proc1;
    procedure Proc2;
    procedure Proc3;
    procedure Proc4;
    procedure Proc5;
    property URL: string read GetURL write SetURL;
  end;

  IDocModule = interface
    ['{60AE6F18-62AD-4E39-A999-29504CF2632A}']
    procedure AddToProject;
    function GetFileName: string;
    procedure GetIsModified;
    function GetModuleName: string;
    procedure Save;
    procedure Show;  // doesn't seem to work properly...
    procedure ShowEditor(Visible: Boolean; const Filename: string);
    procedure GetProjectCount;
    procedure GetProject;
    procedure GetActiveProject;
    property Filename: string read GetFilename;
    property ModuleName: string read GetModuleName;
  end;

  //Thist timer makes the welcome page navigation since we must not change the
  //ide module while in the F1 keyboard loop
  TNavigateTimer = class(TTimer)
  private
    FURL: WideString;

    procedure ShowWP;
    function WPIsEnabled: Boolean;
  protected
    procedure InternalOnTimer(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    function NavigateToUrl(AURL: WideString): Boolean;
  end;

var
  nt: TNavigateTimer;

function WelcomePageNavigate(AURL: WideString): Boolean;
begin
  Result := nt.NavigateToUrl(AURL);
end;

{ TNavigateTimer }

function TNavigateTimer.WPIsEnabled: Boolean;
begin
  Result := (GetModuleHandle('startpageide100.bpl') > 0) or
    (GetModuleHandle('startpageide120.bpl') > 0) or
    (GetModuleHandle('startpageide140.bpl') > 0);
end;

procedure TNavigateTimer.ShowWP;
var
  IDEService: INTAServices;
  actList:    TCustomActionList;
  idx:        Integer;
  act:        TContainedAction;
begin
  IDEService := (BorlandIDEServices as INTAServices);
  actList    := IDEService.ActionList;

  for idx := 0 to actList.ActionCount - 1 do
  begin
    act := actList.Actions[idx];

    if act.Name = 'ViewWelcomePageCommand' then
      act.Execute;
  end;
end;

constructor TNavigateTimer.Create(AOwner: TComponent);
begin
  inherited;
  Enabled  := False;
  Interval := 100;
  OnTimer  := InternalOnTimer;
end;

function TNavigateTimer.NavigateToUrl(AURL: WideString): Boolean;
begin
  Result := WPIsEnabled;

  if Result then
  begin
    FURL         := AURL;
    self.Enabled := True;
  end;
end;

procedure TNavigateTimer.InternalOnTimer(Sender: TObject);
var
  ModuleServices: IOTAModuleServices;
  Module:    IOTAModule;
  I:         Integer;
  mIdx:      Integer;
  URLModule: IURLModule;
  DocModule: IDocModule;
begin
  Enabled := False;
  ShowWP;

  mIdx           := -1;
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  for I := 0 to ModuleServices.ModuleCount - 1 do
  begin
    Module := ModuleServices.Modules[I];
    if Supports(Module, IURLModule, URLModule) then
    begin
      if Supports(Module, IDocModule, DocModule) then
      begin
        URLModule.URL := FURL;
        mIdx          := i;
        Break;
      end;
    end;
  end;

  if (mIdx > -1) and (mIdx < ModuleServices.ModuleCount) then
    ModuleServices.Modules[mIdx].Show;
end;

initialization
  nt := TNavigateTimer.Create(nil);

finalization
  nt.Free;

end.
