//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************

unit uThreadHelper;

interface

uses
  Classes, SysUtils;

type
  TThreadExecProc = class(TThread)
  protected
    FProc,
    FCallback : TProc;
    FSyncCallback : Boolean;
    procedure Execute; override;
  public
    constructor Create(AProc, ACallback : TProc; ASyncCallback : Boolean); reintroduce;
  end;

  TThreadExecFunc<TType> = class(TThread)
  protected
    FFunc : TFunc<TType>;
    FCallback : TProc<TType>;
    FSyncCallback : Boolean;
    procedure Execute; override;
  public
    constructor Create(AFunc : TFunc<TType>; ACallback : TProc<TType>; ASyncCallback : Boolean); reintroduce;
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
  TThreadExecProc.Create(AProc, ACallback, ASyncCallback);
end;

class procedure TThreadHelper.Exec(AProc: TNotifyEvent; ASender: TObject;
  ACallback: TProc; ASyncCallback: Boolean);
begin
  Exec(procedure begin AProc(ASender); end, ACallback, ASyncCallback);
end;

class procedure TThreadHelper.Exec<TType>(AFunc: TFunc<TType>;
  ACallback: TProc<TType>; ASyncCallback: Boolean);
begin
  TThreadExecFunc<TType>.Create(AFunc, ACallback, ASyncCallback);
end;

{ TThreadExecProc }

constructor TThreadExecProc.Create(AProc, ACallback: TProc;
  ASyncCallback: Boolean);
begin
  inherited Create(true);

  FProc := AProc;
  FCallback := ACallback;
  FSyncCallback := ASyncCallback;

  FreeOnTerminate := true;

  Resume;
end;

procedure TThreadExecProc.Execute;
begin
  FProc;

  if Assigned(FCallback) then
  begin
    if FSyncCallback then
      Synchronize(procedure begin FCallback; end)
    else
      FCallback;
  end;
end;

{ TThreadExecFunc<TType> }

constructor TThreadExecFunc<TType>.Create(AFunc: TFunc<TType>;
  ACallback: TProc<TType>; ASyncCallback: Boolean);
begin
  inherited Create(true);

  FFunc := AFunc;
  FCallback := ACallback;
  FSyncCallback := ASyncCallback;

  FreeOnTerminate := true;

  Resume;
end;

procedure TThreadExecFunc<TType>.Execute;
var
  temp : TType;
begin
  temp := FFunc;

  if Assigned(FCallback) then
  begin
    if FSyncCallback then
      Synchronize(procedure begin FCallback(temp); end)
    else
      FCallback(temp);
  end;
end;

end.
