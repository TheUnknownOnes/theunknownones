unit uSIAFBMain;

interface

uses
  Classes,
  Messages,
  ComCtrls,
  Forms,
  SysUtils,
  Graphics,
  ExtCtrls,
  Menus,
  StdCtrls,
  ToolsApi,
  Controls;

type
  TSIAFB = class(TComponent)
  private
    FSearchPanel : TPanel;
    FSearchPanelToolBar : TToolBar;
    FTimer : TTimer;
    FButton : TToolButton;
    FFindInFilesMenuItem : TMenuItem;
    FSimpleSearchCombobox : TComboBox;
    FSimpleSearchOrigKeyUp : TKeyEvent;

    FFillEditCount : Integer;

    procedure StartInstallation;
    procedure StartFillEdit;

    function FindSearchPanel(AParent : TComponent): Boolean;
    function FindFindInFilesMenu(AParent : TComponent) : Boolean;

    procedure CreateButton;

    procedure OnButtonClick(Sender : TObject);

    procedure OnDoInstallation(Sender : TObject);
    procedure OnDoFillEdit(Sender : TObject);

    procedure OnSimpleSearchKeyUp(Sender : TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Dialogs, ActnList, Windows;

var
  FSIAFB : TSIAFB;
  SIAF_Shortcut  : TShortCut;

{ TSIAFB }

constructor TSIAFB.Create(AOwner: TComponent);
begin
  inherited;

  FSearchPanel := nil;
  FSearchPanelToolBar := nil;
  FButton := nil;
  FSimpleSearchOrigKeyUp := nil;

  FTimer := TTimer.Create(nil);
  FTimer.Enabled := false;

  StartInstallation;
end;

procedure TSIAFB.CreateButton();
var
  Act : TCustomAction;
  ico : TIcon;
begin
  FButton := TToolButton.Create(nil);
  FButton.OnClick := OnButtonClick;
  FButton.Name := 'SearchInAllFilesButton';
  FButton.Hint := 'Search in all files ' + ShortCutToText(SIAF_Shortcut);
  FSearchPanelToolBar.AutoSize := true;

  FSearchPanelToolBar.InsertControl(FButton);

  FButton.FreeNotification(Self);

  Act := TCustomAction(FFindInFilesMenuItem.Action);

  Ico := TIcon.Create;
  try
    FFindInFilesMenuItem.GetParentMenu.Images.GetIcon(Act.ImageIndex, ico);
    FButton.ImageIndex := FSearchPanelToolBar.Images.AddIcon(ico);
  finally
    Ico.Free;
  end;

  FButton.Left := FSearchPanelToolBar.Buttons[FSearchPanelToolBar.ButtonCount - 1].Left +
                  FSearchPanelToolBar.Buttons[FSearchPanelToolBar.ButtonCount - 1].Width;
end;

destructor TSIAFB.Destroy;
begin
  FTimer.Free;

  if Assigned(FButton) then
    FButton.Free;

  if Assigned(FSimpleSearchCombobox) then
    FSimpleSearchCombobox.OnKeyUp := FSimpleSearchOrigKeyUp;

  inherited;
end;


function TSIAFB.FindFindInFilesMenu(AParent: TComponent): Boolean;
var
  Child : TComponent;
  idx : Integer;
begin
  Result := false;

  for idx := 0 to AParent.ComponentCount - 1 do
  begin
    Child := AParent.Components[idx];

    if (Child is TMenuItem) and (Child.Name = 'SearchFileFindItem') then
    begin
      FFindInFilesMenuItem := TMenuItem(Child);
      Result := true;
    end
    else
      Result := FindFindInFilesMenu(Child);

    if Result then
      break;
  end;
end;

function TSIAFB.FindSearchPanel(AParent: TComponent) : Boolean;
var
  Child : TComponent;
  idx : Integer;

  function FindSimpleSearchCombobox : Boolean;
  var
    idy : Integer;
    Combo : TComponent;
  begin
    result := false;

    for idy := 0 to FSearchPanel.ControlCount - 1 do
    begin
      Combo := FSearchPanel.Controls[idy];

      if Combo.Name = 'SearchTextBox' then
      begin
        Result := true;
        FSimpleSearchCombobox := TComboBox(Combo);
        FSimpleSearchOrigKeyUp := FSimpleSearchCombobox.OnKeyUp;
        FSimpleSearchCombobox.OnKeyUp := OnSimpleSearchKeyUp;
      end;
    end;
  end;

begin
  Result := false;

  for idx := 0 to AParent.ComponentCount - 1 do
  begin
    Child := AParent.Components[idx];

    if (Child is TToolBar) and (Child.Name = 'SearchNavToolbar') then
    begin
      FSearchPanelToolBar := TToolBar(Child);
      FSearchPanel := TPanel(FSearchPanelToolBar.Parent);

      Result := FindSimpleSearchCombobox;
    end
    else
      Result := FindSearchPanel(Child);

    if Result then
      break;
  end;
end;


procedure TSIAFB.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (AComponent = FButton) and (Operation = opRemove) then
  begin
    FButton := nil;
  end;
end;

procedure TSIAFB.OnButtonClick(Sender: TObject);
begin
  StartFillEdit;
  FFindInFilesMenuItem.Action.Execute;
end;

procedure TSIAFB.OnDoFillEdit(Sender: TObject);
var
  SearchWindow,
  ComboBoxEdit,
  ComboBox : HWND;
begin
  Inc(FFillEditCount);

  if (FFillEditCount * (1000 div FTimer.Interval)) >= 10 then //secs.
    FTimer.Enabled := false;

  SearchWindow := FindWindow('TSrchDialog', nil);

  if SearchWindow > 0 then
  begin
    FTimer.Enabled := false;

    ComboBox := FindWindowEx(SearchWindow, 0, 'THistoryPropComboBox', nil);
    ComboBoxEdit := FindWindowEx(ComboBox, 0, 'Edit', nil);

    if ComboBoxEdit > 0 then
    begin
      SetWindowText(ComboBoxEdit, FSimpleSearchCombobox.Text);
      SendMessage(ComboBoxEdit, EM_SETSEL, 0, -1);
    end;
  end;
end;

procedure TSIAFB.OnDoInstallation(Sender: TObject);
begin
  if FindSearchPanel(Application) and
     FindFindInFilesMenu(Application) then
  begin
    CreateButton;
    FTimer.Enabled := false;
    FTimer.OnTimer := nil;
  end;
end;

procedure TSIAFB.OnSimpleSearchKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ShortCut(Key, Shift) = SIAF_Shortcut then
    FButton.Click
  else
  if Assigned(FSimpleSearchOrigKeyUp) then
    FSimpleSearchOrigKeyUp(Sender, Key, Shift);
end;

procedure TSIAFB.StartFillEdit;
begin
  FFillEditCount := 0;
  FTimer.Interval := 100;
  FTimer.OnTimer := OnDoFillEdit;
  FTimer.Enabled := true;
end;

procedure TSIAFB.StartInstallation;
begin
  FTimer.Interval := 1000;
  FTimer.OnTimer := OnDoInstallation;
  FTimer.Enabled := true;
end;

initialization
  SIAF_Shortcut := ShortCut(VK_RETURN, [ssCtrl]);
  FSIAFB := TSIAFB.Create(nil);

finalization
  if Assigned(FSIAFB) then
    FSIAFB.Free;

end.
