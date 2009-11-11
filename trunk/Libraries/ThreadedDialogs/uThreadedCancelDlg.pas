unit uThreadedCancelDlg;

interface

uses
  Classes, Windows, Messages;

type
  TThreadedCancelDlg = class(TThread)
  private
    FCanceled: Boolean;
    FhInst : HINST;
    FhDlg : HWND;
    FInitialDelay: Integer;
    FMsgTxt: String;
  protected
    procedure Execute; override;
    function CheckTerminated: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property Canceled: Boolean read FCanceled;
    property InitialDelay: Integer read FInitialDelay write FInitialDelay;
    property MessageText: String read FMsgTxt write FMsgTxt;
  end;

implementation

uses
  SysUtils;

{ TThreadedCancelDlg }

{$R 'ThreadedCancelDialog.res'}

const
  CD_WM_OFFSET = WM_USER + 1;

  WM_CANCELDLG_CANCEL = CD_WM_OFFSET + 1;
  WM_CANCELDLG_INIT = CD_WM_OFFSET + 2;

type
  TCancelDlgInitData = record
    MessageText : string;
  end;
  PCancelDlgInitData = ^TCancelDlgInitData;

var
  StaticBrush : THandle;


function TThreadedCancelDlg.CheckTerminated: Boolean;
begin
  Result:=self.Terminated or self.Canceled;
end;

constructor TThreadedCancelDlg.Create;
begin
  inherited Create(True);
  FhInst:=0;
  FhDlg:=0;
  FCanceled:=False;
end;

destructor TThreadedCancelDlg.Destroy;
begin

  inherited;
end;

function InternalDlgWindowProc(WinHandle: HWND; Msg: UINT; wParam : Integer; lParam: Integer) : Integer; stdcall;
var
  InitData : PCancelDlgInitData;
begin
  Result:=0;

  case Msg of
    WM_CANCELDLG_INIT : begin
                          InitData:=PCancelDlgInitData(wParam);
                          SetDlgItemText(WinHandle, 4, PChar(InitData^.MessageText));
                        end;
    WM_COMMAND :  if (HiWord(wParam) = BN_CLICKED) and
                     (LoWord(wParam) = 1) then
                  begin
                    PostMessage(WinHandle, WM_CANCELDLG_CANCEL, 0, 0);
                  end
                  else
                  if LoWord(wParam) = ID_CANCEL then
                    PostMessage(WinHandle, WM_Close, 0, 0);

    WM_CLOSE:     begin
                    PostMessage(WinHandle, WM_CANCELDLG_CANCEL, 0, 0);
                  end;
  end;
  
end;

procedure TThreadedCancelDlg.Execute;
var
  Msg : TMsg;
  InitData : TCancelDlgInitData;
begin
  sleep(FInitialDelay);
  try
    if CheckTerminated then Abort;

    FhInst:=HInstance;

    FhDlg:=CreateDialog(FhInst, MakeIntResource(1000), GetDesktopWindow, @InternalDlgWindowProc);

    InitData.MessageText:=FMsgTxt;
    PostMessage(FhDlg, WM_CANCELDLG_INIT, Integer(@InitData), 0);

    while not Self.CheckTerminated do
    begin
      if PeekMessage(Msg, FhDlg, 0, 0, PM_REMOVE) then
      begin
        if Msg.message=WM_CANCELDLG_CANCEL then
          FCanceled:=True;

        if not IsDialogMessage(FhDlg, Msg) then
        begin
          TranslateMessage(Msg);
          Dispatch(Msg);
        end;
      end;
    end;
    
  finally
    if FhDlg<>0 then
      EndDialog(FhDlg, 0);
  end;
end;

initialization
  StaticBrush:=CreateSolidBrush($FFFFFF);

finalization
  DeleteObject(StaticBrush);

end.
