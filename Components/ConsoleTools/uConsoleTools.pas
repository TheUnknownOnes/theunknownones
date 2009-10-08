//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//********************************************************** 
unit uConsoleTools;

interface

uses
  Classes,
  Controls,
  Windows,
  WideStrings,
  WideStrUtils,
  SysUtils,
  StrUtils;

type
  TConsoleColor = (ccForegroundBlue = FOREGROUND_BLUE,
                   ccForegroundGreen = FOREGROUND_GREEN,
                   ccForegroundRed = FOREGROUND_RED,
                   ccForegroundIntensive = FOREGROUND_INTENSITY,
                   ccBackgroundBlue = BACKGROUND_BLUE,
                   ccBackgroundGreen = BACKGROUND_GREEN,
                   ccBackgroundRed = BACKGROUND_RED,
                   ccBackgroundIntensive = BACKGROUND_INTENSITY);
  TConsoleColors = set of TConsoleColor;

  TConsoleProcessWatcher = class;

  TConsoleOutputProc = procedure(const AText : String) of object;

  TCustomConsoleProcess = class(TComponent)
  private
    FOnStdOutEvent: TConsoleOutputProc;
    FOnStdErrEvent: TConsoleOutputProc;
  protected
    FWatcher : TConsoleProcessWatcher;
    FPipeInputWrite,
    FPipeOutputRead,
    FPipeErrorRead,
    FPipeInputRead,
    FPipeOutputWrite,
    FPipeErrorWrite : THandle;

    FChars: TPoint;
    FShowWindow: Boolean;
    FFullScreen: Boolean;
    FCommandLine: WideString;
    FColor: TConsoleColors;
    FWindowPos: TRect;
    FWindowTitle: WideString;
    FProcessInformation: TProcessInformation;
    FSecAttr: TSecurityAttributes;
    FRunning: Boolean;
    FCurrentDir: WideString;
    FExitCode: Cardinal;
    FHookIO: Boolean;
    procedure SetChars(const Value: TPoint);
    procedure SetColor(const Value: TConsoleColors);
    procedure SetCommandLine(const Value: WideString);
    procedure SetFullScreen(const Value: Boolean);
    procedure SetShowWindow(const Value: Boolean);
    procedure SetWindowPos(const Value: TRect);
    procedure SetWindowTitle(const Value: WideString);
    procedure SetCurrentDir(const Value: WideString);
    procedure SetHookIO(const Value: Boolean);

    procedure CheckRunningAndRaiseExceptionIf;

    procedure CreatePipes(var AStartupInfos : _STARTUPINFOW);
    procedure FreePipe(var APipe : THandle);

    procedure OnProcessExit(ACode : Cardinal);
    procedure DoStdOut(const AText : String);
    procedure DoStdErr(const AText : String);
  published
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function Execute() : Boolean;

    procedure Input(AData : String);
    procedure KillProcess(ACode : Cardinal);

    property CommandLine : WideString read FCommandLine write SetCommandLine;
    property WindowTitle : WideString read FWindowTitle write SetWindowTitle;
    property ShowWindow : Boolean read FShowWindow write SetShowWindow default true;
    property WindowRect : TRect read FWindowPos write SetWindowPos;
    property Chars : TPoint read FChars write SetChars;
    property Color : TConsoleColors read FColor write SetColor;
    property FullScreen : Boolean read FFullScreen write SetFullScreen default false;
    property CurrentDir : WideString read FCurrentDir write SetCurrentDir;

    property HookIO : Boolean read FHookIO write SetHookIO default false;

    property OnStdOut : TConsoleOutputProc read FOnStdOutEvent write FOnStdOutEvent;
    property OnStdErr : TConsoleOutputProc read FOnStdErrEvent write FOnStdErrEvent;

    property ProcessInformation : TProcessInformation read FProcessInformation;
    property SecurityAttributes : TSecurityAttributes read FSecAttr write FSecAttr;
    property Running : Boolean read FRunning;
    property ExitCode : Cardinal read FExitCode;
  end;

  TConsoleProcess = class(TCustomConsoleProcess)
  published
    property CommandLine;
    property WindowTitle;
    property ShowWindow;
    property WindowRect;
    property Chars;
    property FullScreen;
    property CurrentDir;

    property HookIO;

    property OnStdOut;
    property OnStdErr;
  end;

  TConsoleProcessWatcher = class(TThread)
  private
    FProcess : TCustomConsoleProcess;
    FExitCode : Cardinal;
    FStringBuffer : AnsiString;

    procedure DoProcessExit;
    procedure DoStdOut;
    procedure DoStdErr;

    function ProcessRunning : Boolean;

    function PipeHasData(APipe : THandle) : Boolean; 
    function ReadPipe(APipe : THandle) : AnsiString;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcesse : TCustomConsoleProcess); reintroduce;
  end;

  EProcessRunning = class(Exception)
  public
    constructor Create();
  end;

function GetConsoleColorValue(const AConsoleColors : TConsoleColors) : Cardinal;

var
  Default_Security_Attributes : TSecurityAttributes;

implementation

function GetConsoleColorValue(const AConsoleColors : TConsoleColors) : Cardinal;
var
  Buffer : Cardinal;
  Color : TConsoleColor;
begin
  Buffer:=0;

  for Color in AConsoleColors do
    Buffer:=Buffer or Cardinal(Color);

  Result:=Buffer;
end;

{ TCustomConsoleProcess }

procedure TCustomConsoleProcess.CheckRunningAndRaiseExceptionIf;
begin
  if FRunning then
    raise EProcessRunning.Create;
end;

constructor TCustomConsoleProcess.Create(AOwner : TComponent);
begin
  inherited;

  FSecAttr:=Default_Security_Attributes;
  FShowWindow:=true;
  FWindowPos.TopLeft:=Point(0,0);
  FWindowPos.BottomRight:=Point(320,100);
  FChars.X:=80;
  FChars.Y:=25;
  FColor:=[ccBackgroundBlue,
           ccBackgroundGreen,
           ccBackgroundRed,
           ccBackgroundIntensive];  //black on white (best for eyes :) )

  FCommandLine:=EmptyStr;
  FCurrentDir:=EmptyStr;
  FHookIO:=false;

  FPipeInputWrite:=0;
  FPipeOutputRead:=0;
  FPipeErrorRead:=0;
  FPipeInputRead:=0;
  FPipeOutputWrite:=0;
  FPipeErrorWrite:=0;
end;

procedure TCustomConsoleProcess.CreatePipes(var AStartupInfos : _STARTUPINFOW);
begin
  AStartupInfos.dwFlags:=AStartupInfos.dwFlags or STARTF_USESTDHANDLES;

  CreatePipe(FPipeInputRead, FPipeInputWrite, @FSecAttr, 0);
  CreatePipe(FPipeOutputRead, FPipeOutputWrite, @FSecAttr, 0);
  CreatePipe(FPipeErrorRead, FPipeErrorWrite, @FSecAttr, 0);

  AStartupInfos.hStdInput:=FPipeInputRead;
  AStartupInfos.hStdOutput:=FPipeOutputWrite;
  AStartupInfos.hStdError:=FPipeErrorWrite;
end;

destructor TCustomConsoleProcess.Destroy;
begin
  
  inherited;
end;

function TCustomConsoleProcess.Execute: Boolean;
var
  SA : _STARTUPINFOW;
  CreationFlags : Cardinal;
begin
  CheckRunningAndRaiseExceptionIf;

  FRunning:=false;
  FExitCode:=0;
  FillMemory(@FProcessInformation, SizeOf(TProcessInformation), 0);

  FillMemory(@Sa, SizeOf(TStartupInfo), 0);
  Sa.cb:=SizeOf(TStartupInfo);
  Sa.lpTitle:=PWideChar(FWindowTitle);
  Sa.dwX:=FWindowPos.Left;
  Sa.dwY:=FWindowPos.Top;
  Sa.dwXSize:=FWindowPos.Right-FWindowPos.Left;
  Sa.dwYSize:=FWindowPos.Bottom-FWindowPos.Top;
  Sa.dwXCountChars:=FChars.X;
  Sa.dwYCountChars:=FChars.Y;
  Sa.dwFillAttribute:=GetConsoleColorValue(FColor);
  if FShowWindow then
    Sa.wShowWindow:=1
  else
    Sa.wShowWindow:=0;
  if FFullScreen then
    Sa.dwFlags:=STARTF_RUNFULLSCREEN
  else
    Sa.dwFlags:=0;
  Sa.dwFlags:=Sa.dwFlags or
              STARTF_USESHOWWINDOW or
              STARTF_USESIZE or
              STARTF_USEPOSITION or
              STARTF_USECOUNTCHARS or
              STARTF_USEFILLATTRIBUTE;

  if Trim(FCurrentDir)=EmptyStr then
    FCurrentDir:=ExtractFileDir(FCommandLine);

  CreationFlags:=0;
  if FHookIO then
  begin
    CreationFlags:=CreationFlags or CREATE_NEW_CONSOLE;
    CreatePipes(Sa);
  end;

  Result:=CreateProcessW(nil,
                        PWideChar(FCommandLine),
                        @FSecAttr,
                        @FSecAttr,
                        True,
                        CreationFlags,
                        nil,
                        PWideChar(FCurrentDir),
                        Sa,
                        FProcessInformation);
                        
  FRunning:=Result;

  if not Result then
    RaiseLastOSError
  else
  begin
    FWatcher:=TConsoleProcessWatcher.Create(Self);

    FreePipe(FPipeInputRead);
    FreePipe(FPipeOutputWrite);
    FreePipe(FPipeErrorWrite);
  end;
end;

procedure TCustomConsoleProcess.KillProcess(ACode: Cardinal);
begin
  if Running then
    TerminateProcess(FProcessInformation.hProcess, ACode);
end;

procedure TCustomConsoleProcess.FreePipe(var APipe: THandle);
begin
  if APipe>0 then
  begin
    CloseHandle(APipe);
    APipe:=0;
  end;
end;

procedure TCustomConsoleProcess.Input(AData: String);
var
  Written : Cardinal;
begin
  if FRunning and FHookIO then
  begin
    WriteFile(FPipeInputWrite, AData[1], Length(AData), Written, nil);
    //FlushFileBuffers(FPipeInputWrite);
  end;
end;

procedure TCustomConsoleProcess.OnProcessExit(ACode : Cardinal);
begin
  FRunning:=false;
  FExitCode:=ACode;
  FWatcher:=nil;

  FreePipe(FPipeInputWrite);
  FreePipe(FPipeOutputRead);
  FreePipe(FPipeErrorRead);
end;

procedure TCustomConsoleProcess.DoStdErr(const AText: String);
begin
  if Assigned(FOnStdErrEvent) then
    FOnStdErrEvent(AText);
end;

procedure TCustomConsoleProcess.DoStdOut(const AText: String);
begin
  if Assigned(FOnStdOutEvent) then
    FOnStdOutEvent(AText);
end;

procedure TCustomConsoleProcess.SetChars(const Value: TPoint);
begin
  CheckRunningAndRaiseExceptionIf;
  FChars := Value;
end;

procedure TCustomConsoleProcess.SetColor(const Value: TConsoleColors);
begin
  CheckRunningAndRaiseExceptionIf;
  FColor := Value;
end;

procedure TCustomConsoleProcess.SetCommandLine(const Value: WideString);
begin
  CheckRunningAndRaiseExceptionIf;
  FCommandLine := Value;
end;

procedure TCustomConsoleProcess.SetCurrentDir(const Value: WideString);
begin
  CheckRunningAndRaiseExceptionIf;
  FCurrentDir := Value;
end;

procedure TCustomConsoleProcess.SetFullScreen(const Value: Boolean);
begin
  CheckRunningAndRaiseExceptionIf;
  FFullScreen := Value;
end;

procedure TCustomConsoleProcess.SetHookIO(const Value: Boolean);
begin
  CheckRunningAndRaiseExceptionIf;
  FHookIO := Value;
end;

procedure TCustomConsoleProcess.SetShowWindow(const Value: Boolean);
begin
  CheckRunningAndRaiseExceptionIf;
  FShowWindow := Value;
end;

procedure TCustomConsoleProcess.SetWindowPos(const Value: TRect);
begin
  CheckRunningAndRaiseExceptionIf;
  FWindowPos := Value;
end;

procedure TCustomConsoleProcess.SetWindowTitle(const Value: WideString);
begin
  CheckRunningAndRaiseExceptionIf;
  FWindowTitle := Value;
end;

{ TConsoleProcessWatcher }

constructor TConsoleProcessWatcher.Create(AProcesse: TCustomConsoleProcess);
begin
  Assert(Assigned(AProcesse), 'Please supply a valid ConsoleProcess');
  FProcess:=AProcesse;

  FreeOnTerminate:=true;

  inherited Create(false);
end;

procedure TConsoleProcessWatcher.DoProcessExit;
begin
  FProcess.OnProcessExit(FExitCode);
end;

procedure TConsoleProcessWatcher.DoStdErr;
begin
  FProcess.DoStdErr(FStringBuffer);
end;

procedure TConsoleProcessWatcher.DoStdOut;
begin
  FProcess.DoStdOut(FStringBuffer);
end;

procedure TConsoleProcessWatcher.Execute;
var
  GoHome : Boolean;
begin
  GoHome:=false;

  while not (Suspended or Terminated or GoHome) do
  begin
    if PipeHasData(FProcess.FPipeOutputRead) then
    begin
      FStringBuffer:=ReadPipe(FProcess.FPipeOutputRead);
      Synchronize(DoStdOut);
      FStringBuffer:=EmptyStr;
    end;

    if PipeHasData(FProcess.FPipeErrorRead) then
    begin
      FStringBuffer:=ReadPipe(FProcess.FPipeErrorRead);
      Synchronize(DoStdErr);
      FStringBuffer:=EmptyStr;
    end;

    if not ProcessRunning then
    begin
      GetExitCodeProcess(FProcess.ProcessInformation.hProcess, FExitCode);
      Synchronize(DoProcessExit);
      
      GoHome:=true;
    end;
    
  end;
end;

function TConsoleProcessWatcher.PipeHasData(APipe: THandle): Boolean;
var
  BytesAvailable : Cardinal;
begin
  Result:=PeekNamedPipe(APipe, nil, 0, nil, @BytesAvailable, nil) and (BytesAvailable>0);
end;

function TConsoleProcessWatcher.ProcessRunning: Boolean;
begin
  Result:=WaitForSingleObject(FProcess.ProcessInformation.hProcess, 50)<>WAIT_OBJECT_0;
end;

function TConsoleProcessWatcher.ReadPipe(APipe: THandle): AnsiString;
var
  BytesAvailable,
  BytesRead : Cardinal;
begin
  Result:=EmptyStr;

  if PeekNamedPipe(APipe, nil, 0, nil, @BytesAvailable, nil) and
     (BytesAvailable>0) then
  begin
    SetLength(Result, BytesAvailable*Sizeof(Result[1]));
    ReadFile(APipe, Result[1], BytesAvailable, BytesRead, nil);
  end;
end;

{ EProcessRunning }

constructor EProcessRunning.Create;
begin
  inherited Create('The process is still running');
end;

initialization
  Default_Security_Attributes.nLength:=SizeOf(TSecurityAttributes);
  Default_Security_Attributes.bInheritHandle:=true;

end.
