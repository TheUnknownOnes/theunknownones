unit uTaskBarListTab;

interface

uses
  uTaskBarList, Classes, Controls;

type
  TTaskbarListTab = class(TTaskBarListComponent)
  private
    FControl : TWinControl;
    procedure SetControl(const Value: TWinControl);
  protected
    property WinControl : TWinControl read FControl write SetControl;
  end;

  TTaskbarListFormTab = class(TTaskbarListTab)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TTaskbarListControlTab = class(TTaskbarListTab)
  published
    property WinControl;
  end;

implementation

{ TTaskbarListFormTab }

uses
  Forms;

constructor TTaskbarListFormTab.Create(AOwner: TComponent);
begin
  if not (AOwner is TCustomForm) then
    raise ETaskBarListError.Create('Owner of TTaskbarListFormTab must be a TCustomForm descendant');

  inherited;

  FControl:=TWinControl(AOwner);

  FIsSupported := (TaskbarList3 <> nil) and (not (csDesigning in ComponentState));
  FIsActive := False;
end;

{ TTaskbarListTab }

procedure TTaskbarListTab.SetControl(const Value: TWinControl);
begin
  FControl := Value;
end;

end.
