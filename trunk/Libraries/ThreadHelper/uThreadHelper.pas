//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************

{ Examples

function MachWasLanges : Int64;
begin
  sleep(5000);
  Result := SecondOf(Now);
end;

procedure ShowResult (AResult : Int64);
begin
  ShowMessage(IntToStr(AResult));
end;

procedure TForm61.Button3Click(Sender: TObject);
begin
  TThread.Exec<Int64>(MachWasLanges, ShowResult);
end;

}

unit uThreadHelper;

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections, Windows;

type
  TThreadExecWork = (thwFunction, thwProcedure);

  TThreadExec<TType> = class(TThread)
  protected
    FFunc : TFunc<TType>;
    FFuncCallback : TProc<TType>;
    FProc,
    FProcCallback : TProc;
    FSyncCallback : Boolean;
    FWork : TThreadExecWork;
    procedure Execute; override;

    constructor BaseConstrutor(); virtual;
  public
    constructor Create(AFunc : TFunc<TType>; ACallback : TProc<TType>; ASyncCallback : Boolean); reintroduce; overload;
    constructor Create(AProc, ACallback : TProc; ASyncCallback : Boolean); reintroduce; overload;
    destructor Destroy; override;
  end;

  TThreadHelper = class helper for TThread
  public
    class function Exec(AProc : TProc; ACallback : TProc = nil; ASyncCallback : Boolean = true) : TThread; overload;
    class function Exec(AProc : TNotifyEvent; ASender : TObject = nil; ACallback : TProc = nil; ASyncCallback : Boolean = true) : TThread; overload;
    class function Exec<TType>(AFunc : TFunc<TType>; ACallback : TProc<TType>; ASyncCallback : Boolean = true) : TThread; overload;
  end;

  TThreadExecManager = class(TThread)
  protected
    FMaxRunningThreads: Integer;
    FThreads,
    FRunningThreads : TList<TThread>;
    FThreadsLock,
    FRunningThreadsLock : TCriticalSection;
    FNewThreadEvent : TEvent;

    procedure Execute; override;
    procedure Add(const AThread : TThread);
    procedure Delete(const AThread : TThread);
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;

    property MaxRunningThreads : Integer read FMaxRunningThreads write FMaxRunningThreads;
    property WaitingThreads : TList<TThread> read FThreads;
    property RunningThreads : TList<TThread> read FRunningThreads;
  end;

var
  ThreadExecManager : TThreadExecManager;

threadvar
  MyExecThread : TThread;

implementation

{ TThreadHelper }

class function TThreadHelper.Exec(AProc, ACallback: TProc;
  ASyncCallback: Boolean) : TThread;
begin
  Result := TThreadExec<Pointer>.Create(AProc, ACallback, ASyncCallback);
end;

class function TThreadHelper.Exec(AProc: TNotifyEvent; ASender: TObject;
  ACallback: TProc; ASyncCallback: Boolean) : TThread;
begin
  Result := Exec(procedure begin AProc(ASender); end, ACallback, ASyncCallback);
end;

class function TThreadHelper.Exec<TType>(AFunc: TFunc<TType>;
  ACallback: TProc<TType>; ASyncCallback: Boolean) : TThread;
begin
  Result := TThreadExec<TType>.Create(AFunc, ACallback, ASyncCallback);
end;

{ TThreadExec<TType> }

constructor TThreadExec<TType>.Create(AFunc: TFunc<TType>;
  ACallback: TProc<TType>; ASyncCallback: Boolean);
begin
  BaseConstrutor;

  FWork := thwFunction;

  FFunc := AFunc;
  FFuncCallback := ACallback;
  FSyncCallback := ASyncCallback;
end;

constructor TThreadExec<TType>.BaseConstrutor;
begin
  inherited Create(true);
  FreeOnTerminate := true;
  ThreadExecManager.Add(Self);
end;

constructor TThreadExec<TType>.Create(AProc, ACallback: TProc;
  ASyncCallback: Boolean);
begin
  BaseConstrutor;

  FWork := thwProcedure;

  FProc := AProc;
  FProcCallback := ACallback;
  ASyncCallback := ASyncCallback;
end;

destructor TThreadExec<TType>.Destroy;
begin
  ThreadExecManager.Delete(Self);
  inherited;
end;

procedure TThreadExec<TType>.Execute;
var
  temp : TType;
begin
  MyExecThread := Self;

  if FWork = thwFunction then
  begin
    temp := FFunc;

    if Assigned(FFuncCallback) then
    begin
      if FSyncCallback then
        Synchronize(procedure begin FFuncCallback(temp); end)
      else
        FFuncCallback(temp);
    end;
  end
  else
  if FWork = thwProcedure then
  begin
    FProc;

    if Assigned(FProcCallback) then
    begin
      if FSyncCallback then
        Synchronize(procedure begin FProcCallback; end)
      else
        FProcCallback;
    end;
  end;
end;

{ TThreadExecManager }

procedure TThreadExecManager.Add(const AThread: TThread);
begin
  FThreadsLock.Enter;
  try
    FThreads.Add(AThread);
  finally
    FThreadsLock.Leave;
  end;

  FNewThreadEvent.SetEvent;
end;

constructor TThreadExecManager.Create;
begin
  FThreads := TList<TThread>.Create;
  FThreadsLock := TCriticalSection.Create;
  FRunningThreads := TList<TThread>.Create;
  FRunningThreadsLock := TCriticalSection.Create;
  FMaxRunningThreads := MaxInt;

  FNewThreadEvent := TEvent.Create();

  inherited Create(false);

  FreeOnTerminate := false;
end;

procedure TThreadExecManager.Delete(const AThread: TThread);
var
   Done : Boolean;
begin
  FRunningThreadsLock.Enter;
  try
    Done := Assigned(FRunningThreads.Extract(AThread));
  finally
    FRunningThreadsLock.Leave;
  end;

  if not Done then
  begin
    FThreadsLock.Enter;
    try
      FThreads.Extract(AThread);
    finally
      FThreadsLock.Leave;
    end;
  end;
end;

destructor TThreadExecManager.Destroy;
begin
  while FThreads.Count > 0 do
    FThreads[0].Free;

  FThreads.Free;
  FThreadsLock.Free;


  while FRunningThreads.Count > 0 do
    FRunningThreads[0].Terminate;

  FRunningThreads.Free;
  FRunningThreadsLock.Free;

  FNewThreadEvent.Free;

  inherited;
end;

procedure TThreadExecManager.Execute;
var
  thread : TThread;
begin
  while not Terminated do
  begin

    while (FThreads.Count > 0) and (FRunningThreads.Count < FMaxRunningThreads) do
    begin
      FThreadsLock.Enter;
      try
        thread := FThreads[0];
        FThreads.Delete(0);
      finally
        FThreadsLock.Leave;
      end;

      FRunningThreadsLock.Enter;
      try
        thread.Resume;
        FRunningThreads.Add(thread);
      finally
        FRunningThreadsLock.Leave;
      end;
    end;

    if (FThreads.Count = 0) and (WaitForSingleObject(FNewThreadEvent.Handle, 50) = WAIT_OBJECT_0) then
      FNewThreadEvent.ResetEvent;
  end;
end;

initialization
  ThreadExecManager := TThreadExecManager.Create;

finalization
  ThreadExecManager.Terminate;
  ThreadExecManager.Free;

end.
