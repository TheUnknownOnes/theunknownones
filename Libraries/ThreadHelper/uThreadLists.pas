//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************

unit uThreadLists;

interface

uses
  Classes,
  jwaWindows;

type
  TWaitList = class
  protected
    FList : TList;
    FLock : TCriticalSection;
    FcvEmpty,
    FcvNotEmpty : TConditionVariable;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function BeginUpdate : TList;
    procedure EndUpdate(AWakeWaitingThreads : Boolean = true;
                        AAll : Boolean = true);

    function WaitForEmpty(ATimeout : Cardinal = INFINITE) : TList;
    //returns nil if timed out or the list wasn't cleared in the desired time
    //if the result is not nil, you have to call EndUpdate
    function WaitForNotEmpty(ATimeout : Cardinal = INFINITE) : TList;
    //returns nil if timed out or the list wasn't filled in the desired time
    //if the result is not nil, you have to call EndUpdate
  end;

implementation

{ TWaitList }

function TWaitList.BeginUpdate: TList;
begin
  EnterCriticalSection(FLock);
  Result := FList;
end;

constructor TWaitList.Create;
begin
  FList := TList.Create;
  InitializeCriticalSection(FLock);
  InitializeConditionVariable(FcvEmpty);
  InitializeConditionVariable(FcvNotEmpty);
end;

destructor TWaitList.Destroy;
begin
  EndUpdate(true, true);
  DeleteCriticalSection(FLock);
  FList.Free;

  inherited;
end;

procedure TWaitList.EndUpdate(AWakeWaitingThreads, AAll: Boolean);
begin
  LeaveCriticalSection(FLock);

  if AWakeWaitingThreads then
  begin
    if AAll then
    begin
      if FList.Count = 0 then
        WakeAllConditionVariable(FcvEmpty)
      else
        WakeAllConditionVariable(FcvNotEmpty);
    end
    else
    begin
      if FList.Count = 0 then
        WakeConditionVariable(FcvEmpty)
      else
        WakeConditionVariable(FcvNotEmpty);
    end;
  end;
end;

function TWaitList.WaitForEmpty(ATimeout: Cardinal): TList;
begin
  Result := BeginUpdate;

  while Result.Count > 0 do
  begin
    if not SleepConditionVariableCS(FcvEmpty, FLock, ATimeout) then
    begin
      Result := nil;
      EndUpdate(false);
      break;
    end;
  end;

end;

function TWaitList.WaitForNotEmpty(ATimeout: Cardinal): TList;
begin
  Result := BeginUpdate;

  while Result.Count = 0 do
  begin
    if not SleepConditionVariableCS(FcvNotEmpty, FLock, ATimeout) then
    begin
      Result := nil;
      EndUpdate(false);
      break;
    end;
  end;
end;

end.
