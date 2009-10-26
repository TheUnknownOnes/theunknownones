unit uSIAFBMain;

interface

uses
  Classes,
  Windows,
  Messages,
  ComCtrls,
  Forms,
  SysUtils,
  Graphics,
  ExtCtrls,
  ToolsApi;

type
  TSIAFB = class
  private
    FSearchPanelHandle : HWND;
    FTimer : TTimer;
    FIcon : TIcon;
    FButton : TToolButton;

    FTimerFillEdit : TTimer;
    FFillEditCount : Integer;

    FOffHandler : TNotifyEvent;

    function FindSearchToolbar(AParent : TComponent): TToolbar;
    function FindFindInFilesHandler(AParent : TComponent) : TNotifyEvent;

    function CreateButton(AToolbar : TToolBar) : Boolean;
    procedure FreeButton;

    procedure OnButtonClick(Sender : TObject);

    procedure OnTimer(Sender : TObject);
    procedure OnFillEdit(Sender : TObject);
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

  FTimerFillEdit := TTimer.Create(nil);
  FTimerFillEdit.Interval := 100;
  FTimerFillEdit.OnTimer := OnFillEdit;
  FTimerFillEdit.Enabled := false;
end;

function TSIAFB.CreateButton(AToolbar : TToolBar) : Boolean;
begin
  FOffHandler := FindFindInFilesHandler(Application);

  Result := Assigned(FOffHandler) and (not FIcon.Empty);

  if Result then
  begin
    FButton := TToolButton.Create(nil);
    FButton.OnClick := OnButtonClick;
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
    begin
      Result := TToolBar(Child);
      FSearchPanelHandle := Result.Parent.Handle;
    end
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


procedure TSIAFB.OnButtonClick(Sender: TObject);
begin
  if Assigned(FOffHandler) then
  begin
    FFillEditCount := 0;
    FTimerFillEdit.Enabled := true;
    FOffHandler(FButton);
  end;
end;

procedure TSIAFB.OnFillEdit(Sender: TObject);
var
  SearchWindow,
  ComboBoxEdit,
  ComboBox : HWND;
  SearchEdit : HWND;
  Buffer : array[0..255] of Char;
  l : Integer;
begin
  Inc(FFillEditCount);

  if (FFillEditCount * (1000 div FTimerFillEdit.Interval)) >= 10 then //secs.
    FTimerFillEdit.Enabled := false;

  SearchWindow := FindWindow('TSrchDialog', nil);

  if SearchWindow > 0 then
  begin
    FTimerFillEdit.Enabled := false;

    ComboBox := FindWindowEx(SearchWindow, 0, 'THistoryPropComboBox', nil);
    ComboBoxEdit := FindWindowEx(ComboBox, 0, 'Edit', nil);
    SearchEdit := FindWindowEx(FSearchPanelHandle, 0, 'THistoryPropComboBox', nil);

    if ComboBoxEdit + SearchEdit > 0 then
    begin
      GetWindowText(SearchEdit, @Buffer[0], 254);
      SetWindowText(ComboBoxEdit, Buffer);
      SendMessage(ComboBoxEdit, EM_SETSEL, 0, -1);
    end;
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
