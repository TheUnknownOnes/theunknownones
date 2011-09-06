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
  Classes, SysUtils, SyncObjs, Generics.Collections;

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
  end;

  TThreadHelper = class helper for TThread
  public
    class procedure Exec(AProc : TProc; ACallback : TProc = nil; ASyncCallback : Boolean = true); overload;
    class procedure Exec(AProc : TNotifyEvent; ASender : TObject = nil; ACallback : TProc = nil; ASyncCallback : Boolean = true); overload;
    class procedure Exec<TType>(AFunc : TFunc<TType>; ACallback : TProc<TType>; ASyncCallback : Boolean = true); overload;
  end;

implementation

{ TThreadHelper }

class procedure TThreadHelper.Exec(AProc, ACallback: TProc;
  ASyncCallback: Boolean);
begin
  TThreadExec<Pointer>.Create(AProc, ACallback, ASyncCallback);
end;

class procedure TThreadHelper.Exec(AProc: TNotifyEvent; ASender: TObject;
  ACallback: TProc; ASyncCallback: Boolean);
begin
  Exec(procedure begin AProc(ASender); end, ACallback, ASyncCallback);
end;

class procedure TThreadHelper.Exec<TType>(AFunc: TFunc<TType>;
  ACallback: TProc<TType>; ASyncCallback: Boolean);
begin
  TThreadExec<TType>.Create(AFunc, ACallback, ASyncCallback);
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

  Resume;
end;

constructor TThreadExec<TType>.BaseConstrutor;
begin
  inherited Create(true);
  FreeOnTerminate := true;

end;

constructor TThreadExec<TType>.Create(AProc, ACallback: TProc;
  ASyncCallback: Boolean);
begin
  BaseConstrutor;

  FWork := thwProcedure;

  FProc := AProc;
  FProcCallback := ACallback;
  ASyncCallback := ASyncCallback;

  Resume;
end;

procedure TThreadExec<TType>.Execute;
var
  temp : TType;
begin
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

end.
