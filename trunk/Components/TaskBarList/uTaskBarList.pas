unit uTaskBarList;

interface

uses
  jwaShlObj, Classes, JwaWinType, AppEvnts, Messages, SysUtils, ComObj,
  JwaWinBase;

{$i Jedi.inc}


{$R Images.res}

type
  ETaskBarListError = class(Exception);

  TTaskBarListComponent = class(TComponent)
  private
    FHandle    : HWND;

    FMsgAutoInitialize: Cardinal;

    FMsgUpdate: Cardinal;

    FAutoInit: Boolean;
    FTaskBarEntryHandle: THandle;
    function GetTaskBarEntryHandle: THandle;

  protected
    FInitialized : Boolean;

    FTaskbarList: ITaskbarList;
    FTaskbarList2: ITaskbarList2;
    FTaskbarList3: ITaskbarList3;
    FTaskbarList4: ITaskbarList4;

    procedure WndProc(var Message: TMessage); virtual;

    procedure DoInitialize; virtual;
    procedure DoUpdate; virtual;

    procedure PostUpdateMessage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function HandleAllocated: Boolean;

    property AutoInitialize: Boolean read FAutoInit write FAutoInit default False;
    property Handle: HWND read FHandle write FHandle;

    property Initialized: Boolean read FInitialized;

    property TaskBarEntryHandle: THandle read GetTaskBarEntryHandle write FTaskBarEntryHandle;
  end;

implementation

uses
  JwaWinUser, Forms;

{ TTaskBarListComponent }
procedure TTaskBarListComponent.WndProc(var Message: TMessage);
begin
  if Message.Msg = FMsgAutoInitialize then
  begin
    if FAutoInit then
    begin
      DoInitialize;
    end;
  end
  else
  if Message.Msg = FMsgUpdate then
  begin
    if FInitialized then
      DoUpdate;
  end
  else
  begin
    Message.Result := DefWindowProc(FHandle, Message.Msg, Message.WParam, Message.LParam);
  end;
end;

function TTaskBarListComponent.HandleAllocated: Boolean;
begin
  Result := FHandle <> 0;
end;

procedure TTaskBarListComponent.PostUpdateMessage;
begin
  if HandleAllocated then
    if FInitialized then
      PostMessage(Handle, FMsgUpdate, 0, 0);
end;

constructor TTaskBarListComponent.Create(AOwner: TComponent);
var
  Obj: IInterface;
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    FHandle := Classes.AllocateHWnd(WndProc);
  end
  else
  begin
    FHandle := 0;
  end;

  Obj:=CreateComObject(CLSID_TaskbarList);

  if Obj = nil then
  begin
    FTaskbarList := nil;
  end
  else
  begin
    FTaskbarList := ITaskbarList(Obj);
    FTaskbarList.HrInit;

    FTaskbarList.QueryInterface(IID_ITaskbarList2, FTaskbarList2);
    FTaskbarList.QueryInterface(IID_ITaskbarList3, FTaskbarList3);
    FTaskbarList.QueryInterface(IID_ITaskbarList4, FTaskbarList4);
  end;

  FMsgAutoInitialize:=RegisterWindowMessage('TUO.Components.TaskBarList.AutoInit');
  FMsgUpdate:=RegisterWindowMessage('TUO.Components.TaskBarList.Update');

  if HandleAllocated then
    PostMessage(Handle, FMsgAutoInitialize, 0, 0);
end;

destructor TTaskBarListComponent.Destroy;
begin
  FTaskBarList:=nil;
  FTaskbarList2:=nil;
  FTaskbarList3:=nil;
  FTaskbarList4:=Nil;
  DeallocateHWnd(FHandle);
  inherited;
end;

procedure TTaskBarListComponent.DoInitialize;
begin
end;

procedure TTaskBarListComponent.DoUpdate;
begin
end;

function TTaskBarListComponent.GetTaskBarEntryHandle: THandle;
begin
  if FTaskBarEntryHandle <> 0 then
  begin
    Result := FTaskBarEntryHandle;
  end
  else
  begin
    {$IFNDEF Delphi2007_Up}
      Result := Application.Handle;
    {$ELSE}
      if not Application.MainFormOnTaskBar then
      begin
        Result := Application.Handle;
      end
      else
      begin
        if Application.MainForm <> nil then
          Result := Application.MainForm.Handle
        else
          Result := INVALID_HANDLE_VALUE;
      end;
    {$ENDIF}
  end;
end;

end.
