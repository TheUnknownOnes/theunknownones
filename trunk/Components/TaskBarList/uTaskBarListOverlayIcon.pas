unit uTaskBarListOverlayIcon;

interface

uses
  uTaskBarList, Graphics, Classes;

type
  TTaskBarListOverlayIcon = class(TTaskBarListComponent)
  private
    FOverlayIcon: TIcon;
    FDescription: WideString;
    procedure SetOverlayIcon(const Value: TIcon);
    procedure SetDescription(const Value: WideString);
  protected
    procedure DoInitialize; override;
    procedure DoUpdate; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Initialize;
  published
    property OverlayIcon : TIcon read FOverlayIcon write SetOverlayIcon;
    property Description : WideString read FDescription write SetDescription;

    property AutoInitialize;
  end;

implementation

{ TaskBarListOverlayIcon }

constructor TTaskBarListOverlayIcon.Create(AOwner: TComponent);
begin
  inherited;
  FOverlayIcon:=TIcon.Create;
end;

destructor TTaskBarListOverlayIcon.Destroy;
begin
  FOverlayIcon.Free;
  inherited;
end;

procedure TTaskBarListOverlayIcon.DoInitialize;
begin
  FInitialized:=True;
  DoUpdate;
end;

procedure TTaskBarListOverlayIcon.DoUpdate;
begin
  DefaultInterface.SetOverlayIcon(TaskBarEntryHandle, FOverlayIcon.Handle, @FDescription);
end;

procedure TTaskBarListOverlayIcon.Initialize;
begin
  DoInitialize;
end;

procedure TTaskBarListOverlayIcon.SetDescription(const Value: WideString);
begin
  FDescription := Value;
  PostUpdateMessage;
end;

procedure TTaskBarListOverlayIcon.SetOverlayIcon(const Value: TIcon);
begin
  FOverlayIcon.Assign(Value);
  PostUpdateMessage;
end;

end.
