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
  TThreadHelper = class helper for TThread
  public
    class procedure Exec(AProc : TProc; ACallback : TProc = nil; ASyncCallback : Boolean = true); overload;
    class procedure Exec(AProc : TNotifyEvent; ASender : TObject = nil; ACallback : TProc = nil; ASyncCallback : Boolean = true); overload;
  end;

implementation

type
  TThreadExecute = class(TThread)
  protected
    FProc,
    FCallback : TProc;
    FSyncCallback : Boolean;
    procedure Execute; override;
  public
    constructor Create(AProc, ACallback : TProc; ASyncCallback : Boolean); reintroduce;
  end;

{ TThreadHelper }

class procedure TThreadHelper.Exec(AProc, ACallback: TProc;
  ASyncCallback: Boolean);
begin
  TThreadExecute.Create(AProc, ACallback, ASyncCallback);
end;

class procedure TThreadHelper.Exec(AProc: TNotifyEvent; ASender: TObject;
  ACallback: TProc; ASyncCallback: Boolean);
begin
  Exec(procedure begin AProc(ASender); end, ACallback, ASyncCallback);
end;

{ TThreadExecute }

constructor TThreadExecute.Create(AProc, ACallback: TProc;
  ASyncCallback: Boolean);
begin
  inherited Create(true);

  FProc := AProc;
  FCallback := ACallback;
  FSyncCallback := ASyncCallback;

  FreeOnTerminate := true;

  Resume;
end;

procedure TThreadExecute.Execute;
var
  Callback : TThreadProcedure;
begin
  FProc;

  if Assigned(FCallback) then
  begin
    if FSyncCallback then
    begin
      Callback := procedure
                  begin
                    FCallback;
                  end;
      Synchronize(Callback)
    end
    else
      FCallback;
  end;
end;

end.
