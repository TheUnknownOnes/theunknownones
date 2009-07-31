unit uIMAPMailChecker;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, AxCtrls, Classes, IMAPMailCheck_TLB, StdVcl, dialogs,
  Sysutils, ShellAPI, Windows;

type
  TIMAPMailChecker = class(TAutoObject, IConnectionPointContainer, IIMAPMailChecker)
  private
    { Private-Deklarationen }
    FConnectionPoints: TConnectionPoints;
    FConnectionPoint: TConnectionPoint;
    FEvents: IIMAPMailCheckerEvents;
    { Hinweis: FEvents unterhält eine  *einzelne* Ereignissenke. Verwenden Sie 
      für den Zugriff auf mehrere Ereignissenken FConnectionPoint.SinkList und
      durchlaufen Sie die Liste der Senken. }
  public
    procedure Initialize; override;
  protected
    procedure ShowConfigDialog; safecall;
    function Get_UnseenMessages: Integer; safecall;
    procedure RunMailTool; safecall;
    { Protected-Deklarationen }
    property ConnectionPoints: TConnectionPoints read FConnectionPoints
      implements IConnectionPointContainer;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
  end;

implementation

uses ComServ, uDlgConfig, uData;

procedure TIMAPMailChecker.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IIMAPMailCheckerEvents;
end;

procedure TIMAPMailChecker.Initialize;
begin
  inherited Initialize;
  FConnectionPoints := TConnectionPoints.Create(Self);
  if AutoFactory.EventTypeInfo <> nil then
    FConnectionPoint := FConnectionPoints.CreateConnectionPoint(
      AutoFactory.EventIID, ckSingle, EventConnect)
  else FConnectionPoint := nil;
end;


procedure TIMAPMailChecker.ShowConfigDialog;
begin
  Data.Timer.Enabled := false;
  try
    Tform_Config.Execute;
  finally
    Data.Timer.Enabled := true;
  end;
end;

function TIMAPMailChecker.Get_UnseenMessages: Integer;
begin
  Result := Data.UnseenMessages;
end;

procedure TIMAPMailChecker.RunMailTool;
begin
  if FileExists(Data.Settings.MailTool) then
    ShellExecute(GetDesktopWindow, 'open', Pchar(Data.Settings.MailTool), nil, Pchar(ExtractFilePath(Data.Settings.MailTool)), SW_SHOWNORMAL)
  else
    ShowConfigDialog;
  Data.ResetUnseenMessages;
end;


initialization
  TAutoObjectFactory.Create(ComServer, TIMAPMailChecker, Class_IMAPMailChecker,
    ciMultiInstance, tmApartment);
end.
