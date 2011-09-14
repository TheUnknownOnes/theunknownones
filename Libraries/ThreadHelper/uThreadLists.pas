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
  TWaitList = class(TObject)
  protected
    FList : TList;
    FLock : TSrwLock;
    FcvEmpty,
    FcvNotEmpty : TConditionVariable;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function BeginWrite : TList;
    procedure EndWrite(AWakeWaitingThreads : Boolean = true;
                        AAll : Boolean = true);
    function BeginRead : TList;
    procedure EndRead;

    function WaitForEmpty(AWantToWrite : Boolean = true; ATimeout : Cardinal = INFINITE) : TList;
    //returns nil if timed out
    //if the result is not nil, you have to call EndWrite
    function WaitForNotEmpty(AWantToWrite : Boolean = true; ATimeout : Cardinal = INFINITE) : TList;
    //returns nil if timed out
    //if the result is not nil, you have to call EndWrite
  end;

implementation

{ TWaitList }

function TWaitList.BeginRead: TList;
begin
  AcquireSRWLockShared(FLock);
  Result := FList;
end;

function TWaitList.BeginWrite: TList;
begin
  AcquireSRWLockExclusive(FLock);
  Result := FList;
end;

constructor TWaitList.Create;
begin
  FList := TList.Create;
  InitializeSRWLock(FLock);
  InitializeConditionVariable(FcvEmpty);
  InitializeConditionVariable(FcvNotEmpty);
end;

destructor TWaitList.Destroy;
begin
  FList.Free;

  inherited;
end;

procedure TWaitList.EndRead;
begin
  ReleaseSRWLockShared(FLock);
end;

procedure TWaitList.EndWrite(AWakeWaitingThreads, AAll: Boolean);
begin
  ReleaseSRWLockExclusive(FLock);

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

function TWaitList.WaitForEmpty(AWantToWrite : Boolean; ATimeout: Cardinal): TList;
var
  Flags : Cardinal;
begin
  if AWantToWrite then
    Flags := 0
  else
    Flags := CONDITION_VARIABLE_LOCKMODE_SHARED;

  Result := BeginWrite;

  while Result.Count > 0 do
  begin
    if not SleepConditionVariableSRW(FcvEmpty, FLock, ATimeout, Flags) then
    begin
      Result := nil;
      EndWrite(false);
      break;
    end;
  end;

end;

function TWaitList.WaitForNotEmpty(AWantToWrite : Boolean; ATimeout: Cardinal): TList;
var
  Flags : Cardinal;
begin
  if AWantToWrite then
    Flags := 0
  else
    Flags := CONDITION_VARIABLE_LOCKMODE_SHARED;

  Result := BeginWrite;

  while Result.Count = 0 do
  begin
    if not SleepConditionVariableSRW(FcvNotEmpty, FLock, ATimeout, Flags) then
    begin
      Result := nil;
      EndWrite(false);
      break;
    end;
  end;
end;

end.
