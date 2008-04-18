//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uInterfacedTools;

interface

uses
  Classes, SysUtils;

type
  {$REGION 'IRunnable declarations'}
  IRunnable = interface
    ['{B334C09C-0F04-418D-81CD-FDE7AF81ADB6}']
    procedure Run();
    procedure OnException(AException : Exception);
    procedure EndOfThread(const AID : Cardinal);
  end;
  {$ENDREGION}

  {$REGION 'IRunnable global procedures declaration'}
  function RunInThread(const AObject : IRunnable;
                       APriority : TThreadPriority = tpNormal;
                       ASyncEndCallback : Boolean = true) : Cardinal;
  {$ENDREGION}


implementation

uses
  Windows, Dialogs;

type
  {$REGION 'TIRunnableThread declaration'}
  TIRunnableThread = class(TThread)
  private
    FObject : IRunnable;
    FSyncEndCallBack : Boolean;
    FException : Exception;

    procedure DoEndCallBack();
    procedure DoOnException;
  protected
    procedure Execute(); override;
  public
    constructor Create(const AObject : IRunnable; ASyncEndCallBack : Boolean = true);
  end;
  {$ENDREGION}

{$REGION 'IRunnable global procedures implementation'}
function RunInThread(const AObject : IRunnable;
                     APriority : TThreadPriority = tpNormal;
                     ASyncEndCallback : Boolean = true) : Cardinal;
var
  Thread : TIRunnableThread;
begin
  Thread:=TIRunnableThread.Create(AObject,ASyncEndCallback);
  Thread.Priority:=APriority;
  Result:=Thread.Handle;
end;
{$ENDREGION}

{$REGION 'TIRunnableThread implementation'}
constructor TIRunnableThread.Create(const AObject: IRunnable; ASyncEndCallBack : Boolean);
begin
  FObject:=AObject;
  FSyncEndCallBack:=ASyncEndCallBack;
  
  inherited Create(true);
  FreeOnTerminate:=true;
  Resume;
end;

procedure TIRunnableThread.DoEndCallBack;
begin
  FObject.EndOfThread(Self.Handle);
end;

procedure TIRunnableThread.DoOnException;
begin
  FObject.OnException(FException);
end;

procedure TIRunnableThread.Execute;
begin
  try
    FObject.Run;
  except
    on E: Exception do
    begin
      FException:=E;
      Synchronize(DoOnException);
    end;
  end;

  if FSyncEndCallBack then
    Synchronize(DoEndCallBack)
  else
    DoEndCallBack;
end;
{$ENDREGION}

end.
