//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************

unit uTaskBarListThumbButtons;

interface

uses
  SysUtils, JwaWinType, Graphics, ImgList, Controls, jwaShlObj, jwaDWMApi,
  Messages, Classes, ActnList, AppEvnts, uTaskBarList, JwaWinUser, Windows;

type
  TTaskBarListThumbButtons = class;
  TThumbBarButton = class;
  TThumbBarButtons = class;

  TThumbBarButtonFlag = (tbfDisabled,
                            tbfDismissOnClick,
                            tbfNoBackground,
                            tbfHidden);

  TThumbBarButtonFlags = set of TThumbBarButtonFlag;

  TThumbButtonActionLink = class(TActionLink)
  private
    FButton : TThumbBarButton;
  protected
    procedure AssignClient(AClient: TObject); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetHint(const Value: string); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetAction(Value: TBasicAction); override;
  end;

  TThumbBarButton = class(TCollectionItem)
  private
    FHint: String;
    FFlags: TThumbBarButtonFlags;
    FImgIdx: TImageIndex;
    FAction: TAction;
    FActionLink: TThumbButtonActionLink;
    FOnClick: TNotifyEvent;
    procedure SetHint(const Value: String);
    procedure SetFlags(const Value: TThumbBarButtonFlags);
    procedure SetImgIdx(const Value: TImageIndex);
    procedure SetAction(const Value: TAction);
  protected
    procedure DoClick;
  public
    function GetFlagsAsDW: Cardinal;
  published
 //   property ID;
    property Hint: String read FHint write SetHint;
    property Flags: TThumbBarButtonFlags read FFlags write SetFlags;
    property ImageIndex: TImageIndex read FImgIdx write SetImgIdx;
    property Action: TAction read FAction write SetAction;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;

    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  end;

  ETaskBarListError = class(Exception);

  TThumbBarButtonArray = array of TThumbButton;

  TThumbBarButtons = class(TCollection)
  private
    FOwner : TTaskBarListThumbButtons;
    FImages: TCustomImageList;
    function GetItem(Index: Integer): TThumbBarButton;
    procedure SetItem(Index: Integer; const Value: TThumbBarButton);
    procedure CheckCanAdd;
    procedure SetImages(const Value: TCustomImageList);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;

    function GetAllAsArray: TThumbBarButtonArray;
    function GetAsRecord(AButton: TThumbBarButton): THUMBBUTTON;

  public
    constructor Create(AOwner : TTaskBarListThumbButtons);

    function Add: TThumbBarButton;
    function AddItem(Item: TThumbBarButton; Index: Integer): TThumbBarButton;
    function Insert(Index: Integer): TThumbBarButton;
    property Items[Index: Integer]: TThumbBarButton read GetItem write SetItem; default;
  published
    property Images: TCustomImageList read FImages write SetImages;
  end;

  TTaskBarListThumbButtons = class(TTaskBarListComponent)
  private
    FAppEvents : TApplicationEvents;

    FThumbBarButtons : TThumbBarButtons;

    procedure SetImages(const Value: TCustomImageList);
    function GetImages: TCustomImageList;

    procedure DoHandleEvent(var Msg: TMsg; var Handled: Boolean);
  protected
    procedure DoInitialize; override;
    procedure DoUpdate; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure Initialize;

    constructor Create(AOwner: TComponent); override;
  published
    property ThumbBarButtons: TThumbBarButtons read FThumbBarButtons write FThumbBarButtons stored true;
    property ThumbBarImages: TCustomImageList read GetImages write SetImages;

    property AutoInitialize;
  end;

implementation

uses
  Dialogs, Forms;


{ TThumbBarButtons }

procedure TThumbBarButtons.CheckCanAdd;
begin
  if FOwner.Initialized then
    raise ETaskBarListError.Create('All Buttons are already added.');
end;

constructor TThumbBarButtons.Create(AOwner: TTaskBarListThumbButtons);
begin
  inherited Create(TThumbBarButton);
  FOwner:=AOwner;
end;

function TThumbBarButtons.Add: TThumbBarButton;
begin
  CheckCanAdd;
  showmessage('add');

  Result:=TThumbBarButton(inherited Add);
end;

function TThumbBarButtons.AddItem(Item: TThumbBarButton;
  Index: Integer): TThumbBarButton;
begin
  CheckCanAdd;

  if Item = nil then
    Result:=TThumbBarButton.Create(Self)
  else
    Result := Item;
  if Assigned(Result) then
  begin
    if Index < 0 then
      Index := Count - 1;
    Result.Index := Index;
  end;
end;

function TThumbBarButtons.GetAllAsArray: TThumbBarButtonArray;
var
  idx: Integer;
begin
  SetLength(Result, Self.Count);

  for idx := 0 to Self.Count - 1 do
    Result[idx]:=GetAsRecord(Self[idx]);
end;

function TThumbBarButtons.GetAsRecord(AButton: TThumbBarButton): THUMBBUTTON;
begin
  Result.dwMask:=THB_BITMAP or THB_TOOLTIP or THB_FLAGS;
  Result.iId:=AButton.ID;
  Result.iBitmap:=AButton.ImageIndex;
  StringToWideChar(AButton.Hint, @Result.szTip[0], Length(Result.szTip));
  Result.dwFlags:=AButton.GetFlagsAsDW;
end;

function TThumbBarButtons.GetItem(Index: Integer): TThumbBarButton;
begin
  Result := TThumbBarButton(inherited GetItem(Index));
end;

function TThumbBarButtons.GetOwner: TPersistent;
begin
  Result:=FOwner;
end;

function TThumbBarButtons.Insert(Index: Integer): TThumbBarButton;
begin
  CheckCanAdd;
  Result:=AddItem(nil,Index);
end;

procedure TThumbBarButtons.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
  Self.Update(nil);
end;

procedure TThumbBarButtons.SetItem(Index: Integer;
  const Value: TThumbBarButton);
begin
  inherited SetItem(Index, Value);
end;

procedure TThumbBarButtons.Update(Item: TCollectionItem);
begin
  inherited;
  TTaskBarListThumbButtons(Owner).PostUpdateMessage;
end;

{ TThumbBarButton }

constructor TThumbBarButton.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FActionLink:=TThumbButtonActionLink.Create(Self);
end;

destructor TThumbBarButton.Destroy;
begin
  FActionLink.Free;
  FActionLink:=nil;
  inherited;
end;

procedure TThumbBarButton.DoClick;
begin
  if Assigned(FActionLink) and
     Assigned(FAction) then
    FActionLink.Execute
  else
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

function TThumbBarButton.GetFlagsAsDW: Cardinal;
begin
  Result:=THBF_ENABLED;

  if tbfDisabled in self.FFlags then
    Result:=Result or THBF_DISABLED;

  if tbfDismissOnClick in self.FFlags then
    Result:=Result or THBF_DISMISSONCLICK;

  if tbfNoBackground in self.FFlags then
    Result:=Result or THBF_NOBACKGROUND;

  if tbfHidden in self.FFlags then
    Result:=Result or THBF_HIDDEN;
end;

procedure TThumbBarButton.SetAction(const Value: TAction);
begin
  FAction:=Value;
  FActionLink.Action:=FAction;
end;

procedure TThumbBarButton.SetFlags(const Value: TThumbBarButtonFlags);
begin
  FFlags := Value;
  TThumbBarButtons(Collection).Update(Self);
end;

procedure TThumbBarButton.SetImgIdx(const Value: TImageIndex);
begin
  FImgIdx := Value;
  TThumbBarButtons(Collection).Update(Self);
end;

procedure TThumbBarButton.SetHint(const Value: String);
begin
  FHint := Value;
  TThumbBarButtons(Collection).Update(Self);
end;

{ TThumbButtonActionLink }

procedure TThumbButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited;
  FButton:=TThumbBarButton(AClient);
end;

procedure TThumbButtonActionLink.SetAction(Value: TBasicAction);
begin
  inherited;
  if Assigned(Value) and (Value is TCustomAction) then
  begin
    SetEnabled(TCustomAction(Value).Enabled);
    SetHint(TCustomAction(Value).Hint);
    SetImageIndex(TCustomAction(Value).ImageIndex);
    SetVisible(TCustomAction(Value).Visible);
  end;
end;

procedure TThumbButtonActionLink.SetEnabled(Value: Boolean);
begin
  if Value then
    Exclude(Self.FButton.FFlags, tbfDisabled)
  else
    Include(Self.FButton.FFlags, tbfDisabled);

  TThumbBarButtons(FButton.Collection).Update(FButton);
end;

procedure TThumbButtonActionLink.SetHint(const Value: string);
begin
  Self.FButton.Hint:=Value;
end;

procedure TThumbButtonActionLink.SetImageIndex(Value: Integer);
begin
  self.FButton.ImageIndex:=Value;
end;

procedure TThumbButtonActionLink.SetVisible(Value: Boolean);
begin
  if Value then
    Exclude(Self.FButton.FFlags, tbfHidden)
  else
    Include(Self.FButton.FFlags, tbfHidden);

  TThumbBarButtons(FButton.Collection).Update(FButton);
end;

procedure TTaskBarListThumbButtons.SetImages(const Value: TCustomImageList);
begin
  if Assigned(ThumbBarButtons.Images) then
    ThumbBarButtons.Images.RemoveFreeNotification(self);

  ThumbBarButtons.Images:=Value;

  if Assigned(ThumbBarButtons.Images) then
    ThumbBarButtons.Images.FreeNotification(Self);
end;

function TTaskBarListThumbButtons.GetImages: TCustomImageList;
begin
  Result:=ThumbBarButtons.Images;
end;

constructor TTaskBarListThumbButtons.Create(AOwner: TComponent);
begin
  inherited;
  FThumbBarButtons:=TThumbBarButtons.Create(self);

  FAppEvents:=TApplicationEvents.Create(self);
  FAppEvents.OnMessage:=DoHandleEvent;
end;

procedure TTaskBarListThumbButtons.DoHandleEvent(var Msg: TMsg;
  var Handled: Boolean);
var
  idx : Integer;
begin
  Handled:=False;
  if (Msg.message=WM_COMMAND) and
     (HiWord(Msg.WParam) = THBN_CLICKED) then
  begin
    for idx := 0 to Self.FThumbBarButtons.Count - 1 do
    begin
      if Self.FThumbBarButtons[idx].ID=LoWord(Msg.WParam) then
      begin
        Handled:=True;
        Self.FThumbBarButtons[idx].DoClick;
        break;
      end;
    end;
  end;
end;
procedure TTaskBarListThumbButtons.DoInitialize;
var
  Buttons : TThumbBarButtonArray;
begin
  ThumbBarButtons.CheckCanAdd;
  if ThumbBarButtons.Count>0 then
  begin
    Buttons:=ThumbBarButtons.GetAllAsArray;
    if Assigned(FTaskbarList3) and CheckWin32Version(6,1) then
      FTaskbarList3.ThumbBarAddButtons(TCustomForm(Owner).Handle, Length(Buttons), @Buttons[0]);

    FInitialized:=True;

    ThumbBarButtons.Update(nil);
  end;
end;

procedure TTaskBarListThumbButtons.DoUpdate;
var
  Buttons : TThumbBarButtonArray;
begin
  Buttons:=FThumbBarButtons.GetAllAsArray;

  if Initialized then
  begin
    if Assigned(FTaskbarList3) then
    begin
      FTaskbarList3.ThumbBarUpdateButtons(TaskBarEntryHandle, Length(Buttons), @Buttons[0]);

      if Assigned(FThumbBarButtons.Images) then
        FTaskbarList3.ThumbBarSetImageList(TaskBarEntryHandle, FThumbBarButtons.FImages.Handle)
      else
        FTaskbarList3.ThumbBarSetImageList(TaskBarEntryHandle, 0);
    end;
  end;
end;

procedure TTaskBarListThumbButtons.Initialize;
begin
  DoInitialize;
end;

procedure TTaskBarListThumbButtons.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation=opRemove then
  begin
    if AComponent=ThumbBarImages then
      ThumbBarImages:=Nil;
  end;

end;

end.
