//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************

unit uTaskBarList;

interface

uses
  uTaskBarList_Intf, SysUtils, Windows, Graphics, ImgList, Controls,
  Messages, Classes, ActnList;

type
  TTaskBarList = class;
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
    procedure SetHint(const Value: string); override;
    procedure SetImageIndex(Value: Integer); override;
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

  TThumbBarButtonArray = array of THUMBBUTTON;

  TThumbBarButtons = class(TCollection)
  private
    FOwner : TTaskBarList;
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
    constructor Create(AOwner : TTaskBarList);

    function Add: TThumbBarButton;
    function AddItem(Item: TThumbBarButton; Index: Integer): TThumbBarButton;
    function Insert(Index: Integer): TThumbBarButton;
    property Items[Index: Integer]: TThumbBarButton read GetItem write SetItem; default;
  published
    property Images: TCustomImageList read FImages write SetImages;
  end;

  TTaskBarList = class(TComponent, ITaskbarList4)
  private
    FOldWndProc : TWndMethod;

    FButtonsAdded: Boolean;
    FThumbBarButtons : TThumbBarButtons;

    FTaskBarList : ITaskbarList4;
    FProgressPos: Integer;
    FProgressMax: Integer;
    procedure SetImages(const Value: TCustomImageList);
    function GetImages: TCustomImageList;
    procedure SetProgressMax(const Value: Integer);
    procedure SetProgressPos(const Value: Integer);
  protected
    function TestButtonsAdded: Boolean;
    procedure TBLWndProc(var Message: TMessage);
    procedure UpdateProgress;
  public
    property DefaultInterface: ITaskbarList4 read FTaskBarList implements ITaskbarList4;

    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    destructor Destroy; override;

    procedure InitButtons;
  published
    property ThumbBarButtons: TThumbBarButtons read FThumbBarButtons write FThumbBarButtons stored true;
    property ThumbBarImages: TCustomImageList read GetImages write SetImages;

    property ProgressMax: Integer read FProgressMax write SetProgressMax;
    property ProgressPos: Integer read FProgressPos write SetProgressPos;
  end;

implementation

uses
  Dialogs, Forms;

{ TTaskBarList }

procedure TTaskBarList.TBLWndProc(var Message: TMessage);
var
  idx : Integer;
  Handled : Boolean;
begin
  Handled:=False;
  if (Message.Msg=WM_COMMAND) and
     (Message.WParamHi = THBN_CLICKED) then
  begin
    for idx := 0 to Self.FThumbBarButtons.Count - 1 do
    begin
      if Self.FThumbBarButtons[idx].ID=Message.WParamLo then
      begin
        Handled:=True;
        Self.FThumbBarButtons[idx].DoClick;
        break;
      end;
    end;
  end;

  if (not Handled) and Assigned(FOldWndProc) then
  try
    FOldWndProc(Message);
  except
    asm nop; end;

  end;
end;

function TTaskBarList.TestButtonsAdded: Boolean;
begin
  Result:=FButtonsAdded;
end;

procedure TTaskBarList.UpdateProgress;
var
  p,m : dwInteger64;
begin
  p.Lo:=FProgressPos;
  p.Hi:=0;
  m.Lo:=FProgressMax;
  m.Hi:=0;

  FTaskBarList.SetProgressValue(TCustomForm(Owner).Handle,p,m);
end;

procedure TTaskBarList.AfterConstruction;
begin
  inherited;
end;

procedure TTaskBarList.BeforeDestruction;
begin
  if Assigned(FOldWndProc) and Assigned(Owner) then
    TWinControl(Owner).WindowProc:=FOldWndProc;
  inherited;
end;

constructor TTaskBarList.Create(AOwner: TComponent);
begin
  if not (AOwner is TCustomForm) then
    raise ETaskBarListError.Create('Owner of TTaskbarList must be a TCustomForm descendant.');

  inherited;
  FButtonsAdded:=False;
  FTaskBarList:=GetTaskbarList4Interface;
  FThumbBarButtons:=TThumbBarButtons.Create(self);
 
  if (not (csDesigning in Self.ComponentState)) then
  begin
    FOldWndProc:=TWinControl(AOwner).WindowProc;
    TWinControl(AOwner).WindowProc:=self.TBLWndProc;
  end
end;

destructor TTaskBarList.Destroy;
begin
  FTaskBarList:=nil;
  inherited;
end;

function TTaskBarList.GetImages: TCustomImageList;
begin
  Result:=ThumbBarButtons.Images;
end;

procedure TTaskBarList.InitButtons;
var
  Buttons : TThumbBarButtonArray;
begin
  ThumbBarButtons.CheckCanAdd;
  if ThumbBarButtons.Count>0 then
  begin
    Buttons:=ThumbBarButtons.GetAllAsArray;
    FTaskBarList.ThumbBarAddButtons(TCustomForm(Owner).Handle, Length(Buttons), @Buttons[0]);
    FButtonsAdded:=True;
    ThumbBarButtons.Update(nil);
  end;
end;

procedure TTaskBarList.SetImages(const Value: TCustomImageList);
begin
  ThumbBarButtons.Images:=Value;
end;

procedure TTaskBarList.SetProgressMax(const Value: Integer);
begin
  FProgressMax := Value;
  UpdateProgress;
end;

procedure TTaskBarList.SetProgressPos(const Value: Integer);
begin
  FProgressPos := Value;
  UpdateProgress;
end;

{ TThumbBarButtons }

procedure TThumbBarButtons.CheckCanAdd;
begin
  if FOwner.TestButtonsAdded then
    raise ETaskBarListError.Create('All Buttons are already added.');
end;

constructor TThumbBarButtons.Create(AOwner: TTaskBarList);
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
var
  Buttons : TThumbBarButtonArray;
begin
  inherited;
  if Assigned(Item) then
  begin
    SetLength(Buttons, 1);
    Buttons[0]:=Self.GetAsRecord(TThumbBarButton(Item));
  end
  else
    Buttons:=GetAllAsArray;

  if FOwner.FButtonsAdded then
  begin
    FOwner.FTaskBarList.ThumbBarUpdateButtons(TCustomForm(TTaskBarList(Owner).Owner).Handle, Length(Buttons), @Buttons[0]);

    if Assigned(FImages) then
      TTaskBarList(Owner).FTaskBarList.ThumbBarSetImageList(TCustomForm(TTaskBarList(Owner).Owner).Handle, FImages.Handle)
    else
      TTaskBarList(Owner).FTaskBarList.ThumbBarSetImageList(TCustomForm(TTaskBarList(Owner).Owner).Handle, 0);
  end;
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
  if Assigned(FAction) then
    FAction.Execute
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
  FAction := Value;
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

end.
