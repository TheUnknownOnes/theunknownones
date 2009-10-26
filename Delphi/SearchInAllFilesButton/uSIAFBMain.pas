unit uSIAFBMain;

interface

uses
  Classes,
  ComCtrls,
  Forms,
  SysUtils,
  Graphics,
  ExtCtrls,
  ToolsApi;

type
  TSIAFB = class
  private
    FTimer : TTimer;
    FIcon : TIcon;
    FButton : TToolButton;
    function FindSearchToolbar(AParent : TComponent): TToolbar;
    function FindFindInFilesHandler(AParent : TComponent) : TNotifyEvent;

    function CreateButton(AToolbar : TToolBar) : Boolean;
    procedure FreeButton;

    procedure OnTimer(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Dialogs, Menus, ActnList;

var
  FSIAFB : TSIAFB;

{ TSIAFB }

constructor TSIAFB.Create;
begin
  FButton := nil;

  FIcon := TIcon.Create;

  FTimer := TTimer.Create(nil);
  FTimer.Interval := 1000;
  FTimer.OnTimer := OnTimer;
  FTimer.Enabled := true;
end;

function TSIAFB.CreateButton(AToolbar : TToolBar) : Boolean;
var
  Handler : TNotifyEvent;
begin
  Handler := FindFindInFilesHandler(Application);

  Result := Assigned(Handler) and (not FIcon.Empty);

  if Result then
  begin
    FButton := TToolButton.Create(nil);
    FButton.OnClick := Handler;
    FButton.Name := 'SearchInAllFilesButton';
    FButton.Hint := 'Search in all files';
    AToolbar.AutoSize := true;

    AToolbar.InsertControl(FButton);

    FButton.ImageIndex := AToolbar.Images.AddIcon(FIcon);

    FButton.Left := AToolbar.Buttons[AToolbar.ButtonCount - 1].Left + AToolbar.Buttons[AToolbar.ButtonCount - 1].Width;
  end;
end;

destructor TSIAFB.Destroy;
begin
  FreeButton;

  inherited;
end;

function TSIAFB.FindFindInFilesHandler(AParent: TComponent): TNotifyEvent;
var
  Child : TComponent;
  idx : Integer;
begin
  Result := nil;

  for idx := 0 to AParent.ComponentCount - 1 do
  begin
    Child := AParent.Components[idx];

    if (Child is TMenuItem) and (Child.Name = 'SearchFileFindItem') then
    begin
      Result := TMenuItem(Child).Action.OnExecute;

      TMenuItem(Child).GetParentMenu.Images.GetIcon(TCustomAction(TMenuItem(Child).Action).ImageIndex, FIcon);
    end
    else
      Result := FindFindInFilesHandler(Child);

    if Assigned(Result) then
      break;
  end;
end;

function TSIAFB.FindSearchToolbar(AParent: TComponent): TToolbar;
var
  Child : TComponent;
  idx : Integer;
begin
  Result := nil;

  for idx := 0 to AParent.ComponentCount - 1 do
  begin
    Child := AParent.Components[idx];

    if (Child is TToolBar) and (Child.Name = 'SearchNavToolbar') then
      Result := TToolBar(Child)
    else
      Result := FindSearchToolbar(Child);

    if Assigned(Result) then
      break;
  end;
end;

procedure TSIAFB.FreeButton;
begin
  FTimer.Free;

  if Assigned(FIcon) then
    FIcon.Free;

  if Assigned(FButton) then
  begin
    try
      TToolBar(FButton.Parent).RemoveControl(FButton);
      FButton.Free;
    except end;
  end;
end;


procedure TSIAFB.OnTimer(Sender: TObject);
var
  TB : TToolBar;
begin
  TB := FindSearchToolbar(Application);

  if Assigned(TB) and CreateButton(TB) then
    FTimer.Enabled := false;
end;

initialization
  FSIAFB := TSIAFB.Create;

finalization
  if Assigned(FSIAFB) then
    FSIAFB.Free;

end.
