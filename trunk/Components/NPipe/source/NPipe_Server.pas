//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit NPipe_Server;

interface

uses
  SysUtils, Classes, NPipe_Types, StrUtils, Dialogs, Windows;

type
  TNPipeServerThrd = class(TThread)
  private
    FLocalOnly  : Boolean;
    FTimeOut    : Cardinal;
    FPipeName   : String;
    FRawMode    : Boolean;

    FReply          : TMemoryStream;
    FData           : TMemoryStream;
    FLastException  : Exception;
    FServerComponent: TObject;
    FbytesCurrent,
    FbytesTotal     : Int64;

    FCurWorkMode    : TNPWorkMode;

    FOnError        : TNP_OnError;
    FOnIncomingData : TNP_OnIncomingData;
    FOnProgress     : TNP_OnProgress;
    FOnStartWork    : TNP_OnStartWork;
    FOnEndWork      : TNP_OnEndWork;
    FOnConnected    : TNP_OnConnected;
    FOnDisconnected : TNP_OnDisconnected;

    procedure DoError();
    procedure DoIncomingData();
    procedure DoProgress();
    procedure DoStartWork();
    procedure DoEndWork();
    procedure DoConnected();
    procedure DoDisconnected();
  public
    constructor Create( PipeName        : String;
                        LocalOnly       : Boolean;
                        Timeout         : Cardinal;
                        RawMode         : Boolean;
                        OnError         : TNP_OnError;
                        OnIncomingData  : TNP_OnIncomingData;
                        OnProgress      : TNP_OnProgress;
                        OnStartWork     : TNP_OnStartWork;
                        OnEndWork       : TNP_OnEndWork;
                        OnConnected     : TNP_OnConnected;
                        OnDisconnected  : TNP_OnDisconnected;
                        ServerComponent : TObject);
    destructor Destroy(); override;
    procedure Execute; override;
  end;

  TNPipeServer = class(TComponent)
  private
    //Properties
    FActive         : Boolean;
    FPipeName       : String;
    FLocalAccess    : Boolean;
    FTimeOut        : Cardinal;
    FRawMode        : Boolean;

    //Internal
    FServerThrd : TNPipeServerThrd;

    //Events
    FOnError        : TNP_OnError;
    FOnIncomingData : TNP_OnIncomingData;
    FOnProgress     : TNP_OnProgress;
    FOnStartWork    : TNP_OnStartWork;
    FOnEndWork      : TNP_OnEndWork;
    FOnConnected    : TNP_OnConnected;
    FOnDisconnected : TNP_OnDisconnected;

    //Access-procedures
    procedure Set_Active(Value : Boolean);
    procedure Set_PipeName(Value : String);
    procedure Set_LocalAccess(Value : Boolean);
    procedure Set_Timeout(Value : Cardinal);
    procedure Set_RawMode(Value : Boolean);

    procedure Set_OnIncomingData(Value : TNP_OnIncomingData);
    procedure Set_OnError(Value : TNP_OnError);
    procedure Set_OnProgress(Value : TNP_OnProgress);
    procedure Set_OnStartWork(Value : TNP_OnStartWork);
    procedure Set_OnEndWork(Value : TNP_OnEndWork);
    procedure Set_OnConnected(Value : TNP_OnConnected);
    procedure Set_OnDisconnected(Value : TNP_OnDisconnected);

    //Helper
    function TryToSetVar(VarName: String): Boolean;
  protected

    procedure SendError(AException : Exception);
  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active           : Boolean   read FActive      write Set_Active      default false;
    property PipeName         : String    read FPipeName    write Set_PipeName;
    property LocalAccessOnly  : Boolean   read FLocalAccess write Set_LocalAccess default true;
    property Timeout          : Cardinal  read FTimeOut     write Set_TimeOut     default NP_Default_ServerTimeout;
    property RawMode          : Boolean   read FRawMode     write Set_RawMode     default NP_Default_RawMode;

    property OnIncomingData   : TNP_OnIncomingData  read FOnIncomingData  write Set_OnIncomingData;
    property OnError          : TNP_OnError         read FOnError         write Set_OnError;
    property OnProgress       : TNP_OnProgress      read FOnProgress      write Set_OnProgress;
    property OnStartWork      : TNP_OnStartWork     read FOnStartWork     write Set_OnStartWork;
    property OnEndWork        : TNP_OnEndWork       read FOnEndWork       write Set_OnEndWork;
    property OnConnected      : TNP_OnConnected     read FOnConnected     write Set_OnConnected;
    property OnDisconnected   : TNP_OnDisconnected  read FOnDisconnected  write Set_OnDisconnected;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TUO', [TNPipeServer]);
end;

{ TNPipeServer }

constructor TNPipeServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive:=false;
  FLocalAccess:=true;
  FPipeName:=NP_Default_PipeName;
  FTimeOut:=NP_Default_ServerTimeout;
  FRawMode:=Np_Default_RawMode;
end;

destructor TNPipeServer.Destroy;
begin
  inherited;
end;

procedure TNPipeServer.SendError(AException: Exception);
begin
  if (Assigned(FOnError)) then
    FOnError(Self,AException)
  else
    raise AException;
end;

function TNPipeServer.TryToSetVar(VarName: String): Boolean;
begin
  if (FActive) then
    SendError(ENP_CannotChange.Create(Format(ENP_MSG_CannotChange,[VarName])));
  Result:=FActive xor true;
end;


//Property-Access

procedure TNPipeServer.Set_Active(Value: Boolean);
begin
  if not (csDesigning in Self.ComponentState)  then
  begin
    case Value of
      True : if not FActive then
             begin
               FServerThrd:=TNPipeServerThrd.Create(FPipeName,
                                                    FLocalAccess,
                                                    FTimeOut,
                                                    FRawMode,
                                                    FOnError,
                                                    FOnIncomingData,
                                                    FOnProgress,
                                                    FOnStartWork,
                                                    FOnEndWork,
                                                    FOnConnected,
                                                    FOnDisconnected,
                                                    Self);
             end;
      False: if Active then
             begin
               FServerThrd.Terminate;
               //FServerThrd:=Nil;
             end;
    end;
    FActive:=Value;
  end;
end;

procedure TNPipeServer.Set_LocalAccess(Value: Boolean);
begin
  if TryToSetVar('LocalAccess') then FLocalAccess := Value;
end;

procedure TNPipeServer.Set_Timeout(Value: Cardinal);
begin
  if TryToSetVar('Timeout') then FTimeOut:=Value;
end;

procedure TNPipeServer.Set_PipeName(Value: String);
begin
  if TryToSetVar('PipeName') then
    FPipeName:=IfThen(AnsiSameText(Value,EmptyStr),NP_Default_PipeName,Value);
end;

procedure TNPipeServer.Set_RawMode(Value: Boolean);
begin
  if TryToSetVar('RawMode') then FRawMode:=Value;
end;

procedure TNPipeServer.Set_OnIncomingData(Value: TNP_OnIncomingData);
begin
  if TryToSetVar('OnIncomingData') then FOnIncomingData:=Value;
end;

procedure TNPipeServer.Set_OnError(Value: TNP_OnError);
begin
  if TryToSetVar('OnError') then FOnError:=Value;
end;

procedure TNPipeServer.Set_OnProgress(Value: TNP_OnProgress);
begin
  if TryToSetVar('OnProgress') then FOnProgress:=Value;
end;

procedure TNPipeServer.Set_OnStartWork(Value: TNP_OnStartWork);
begin
  if TryToSetVar('OnStartWork') then FOnStartWork:=Value
end;

procedure TNPipeServer.Set_OnEndWork(Value: TNP_OnEndWork);
begin
  if TryToSetVar('OnEndWork') then FOnEndWork:=Value
end;

procedure TNPipeServer.Set_OnConnected(Value: TNP_OnConnected);
begin
  if TryToSetVar('OnConnected') then FOnConnected:=Value
end;

procedure TNPipeServer.Set_OnDisconnected(Value: TNP_OnDisconnected);
begin
  if TryToSetVar('OnDisconnected') then FOnDisconnected:=Value
end;


{ TNPipeServerThrd }

constructor TNPipeServerThrd.Create(PipeName: String;
                                    LocalOnly       : Boolean;
                                    Timeout         : Cardinal;
                                    RawMode         : Boolean;
                                    OnError         : TNP_OnError;
                                    OnIncomingData  : TNP_OnIncomingData;
                                    OnProgress      : TNP_OnProgress;
                                    OnStartWork     : TNP_OnStartWork;
                                    OnEndWork       : TNP_OnEndWork;
                                    OnConnected     : TNP_OnConnected;
                                    OnDisconnected  : TNP_OnDisconnected;
                                    ServerComponent : TObject);
begin
  inherited Create(true);
  FPipeName:=PipeName;
  FLocalOnly:=LocalOnly;
  FTimeOut:=Timeout;
  FRawMode:=RawMode;

  FOnError:=OnError;
  FOnIncomingData:=OnIncomingData;
  FOnProgress:=OnProgress;
  FOnStartWork:=OnStartWork;
  FOnEndWork:=OnEndWork;
  FOnConnected:=OnConnected;
  FOnDisconnected:=OnDisconnected;
  FServerComponent:=ServerComponent;

  FData:=TMemoryStream.Create;
  FReply:=TMemoryStream.Create;

  FreeOnTerminate:=true;

  Resume;
end;

destructor TNPipeServerThrd.Destroy;
begin
  FData.Free;
  FReply.Free;
  inherited;
end;

procedure TNPipeServerThrd.DoError;
begin
  if (Assigned(FOnError)) then
    FOnError(FServerComponent,FLastException)
  else
    raise FLastException;
end;

procedure TNPipeServerThrd.DoIncomingData;
begin
  if (Assigned(FOnIncomingData)) then
    FOnIncomingData(FServerComponent,FData,FReply);
end;

procedure TNPipeServerThrd.DoProgress;
begin
  if (Assigned(FOnProgress)) then
  begin
    if (FCurWorkMode=npwSending) then
      FOnProgress(FServerComponent,FbytesCurrent,nil)
    else
      FOnProgress(FServerComponent,FbytesCurrent,FData);
  end;
end;

procedure TNPipeServerThrd.DoStartWork;
begin
  if (Assigned(FOnStartWork)) then
    FOnStartWork(FServerComponent,FbytesTotal,FCurWorkMode);
end;

procedure TNPipeServerThrd.DoEndWork;
begin
  if (Assigned(FOnEndWork)) then
    FOnEndWork(FServerComponent);
end;

procedure TNPipeServerThrd.DoConnected;
begin
  if (Assigned(FOnConnected)) then
    FOnConnected(FServerComponent);
end;

procedure TNPipeServerThrd.DoDisconnected;
begin
  if (Assigned(FOnDisconnected)) then
    FOnDisconnected(FServerComponent);
end;

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TNPipeServerThrd.Execute;
var
  hPipe         : THandle;

  fConnected      : Boolean;
  bytesTransferred: Cardinal;
  bytesToSend     : Cardinal;

  Buffer          : TNPBuffer;
  StreamSize      : Int64;
  SecDesc         : TSecurityDescriptor;
  PSecAttrib      : PSecurityAttributes;
  SecAttrib       : TSecurityAttributes;

  Lappen          : TOverlapped;

begin
  try
    if not FLocalOnly then
    begin
      if not InitializeSecurityDescriptor(@SecDesc,SECURITY_DESCRIPTOR_REVISION) then
      begin
        FLastException:=ENP_InitSecurityDescriptor.Create(ENP_MSG_InitSecurityDescriptor);
        Synchronize(DoError);
      end;

      if not  SetSecurityDescriptorDacl(@SecDesc,True, Nil, False) then
      begin
        FLastException:=ENP_SetSecurityDescriptor.Create(ENP_MSG_SetSecurityDescriptor);
        Synchronize(DoError);
      end;

      SecAttrib.nLength := sizeof(SECURITY_ATTRIBUTES);
      SecAttrib.lpSecurityDescriptor := @SecDesc;
      SecAttrib.bInheritHandle := FALSE;
      PSecAttrib:=@SecAttrib;
    end
    else
      PSecAttrib:=Nil;

    if FTimeOut=0 then
      FTimeOut:=NP_Default_ServerTimeout;

    hPipe:=CreateNamedPipe(PChar('\\.\pipe\'+FPipeName),
                             PIPE_ACCESS_DUPLEX or FILE_FLAG_OVERLAPPED,
                             PIPE_TYPE_MESSAGE or
                             PIPE_WAIT or
                             PIPE_READMODE_MESSAGE,
                             PIPE_UNLIMITED_INSTANCES,
                             sizeof(buffer),
                             sizeof(buffer),
                             FTimeout, // timeout in millseconds
                             PSecAttrib);
    try
      Lappen.hEvent:=CreateEvent(nil,true,false,'PipeConnected');
      while not Terminated do
      begin
        ResetEvent(Lappen.hEvent);

        fConnected := ConnectNamedPipe(hPipe, @Lappen) or (GetLastError = ERROR_PIPE_CONNECTED);
        while not (fConnected or Terminated) do
          fConnected := WaitForSingleObject(Lappen.hEvent, 1000) <> WAIT_TIMEOUT;
        
        if fConnected then
        begin
          //yeah, we got a connection
          Synchronize(DoConnected);

          if not FRawMode then
          begin
            // get Size of Data to receive
            ReadFile(hPipe, StreamSize, SizeOf(StreamSize), bytesTransferred, nil);
            FData.SetSize(StreamSize);
            FData.Seek(0,soFromBeginning);
          end
          else
          begin
            StreamSize:=MAX_INT64;
            FData.SetSize(0);
          end;

          bytesTransferred:=0;

          //prepare and trigger the OnStartWork event
          FCurWorkMode:=npwReceiving;
          FbytesTotal:=StreamSize;
          Synchronize(DoStartWork);

          // Receive FData;
          while FData.Position<StreamSize do
          begin
            ReadFile(hPipe,Buffer,sizeof(Buffer),bytesTransferred,nil);
            if (bytesTransferred=0) then break;
            FData.Write(Buffer,bytesTransferred);
            
            Buffer:='';

            FbytesCurrent:=FData.Position;
            Synchronize(DoProgress);
            if (FData.Size>=FbytesCurrent) then
              FData.Seek(FbytesCurrent,soFromBeginning)
            else
              FData.Seek(0,soFromEnd);
          end;

          //inform the comnponent about the end of the work
          Synchronize(DoEndWork);

          //trigger the OnIncomingData event and get the reply into FReply
          FData.Seek(0,soFromBeginning);
          FReply.Clear;
          Synchronize(DoIncomingData);

          FReply.Seek(0,soFromBeginning);
          StreamSize:=FReply.Size;

          if not FRawMode then
          begin
            //write out the size of the reply we want to send
            WriteFile(hPipe,StreamSize, SizeOf(StreamSize), bytesTransferred, nil);
          end;

          //prepare and trigger the OnStartWork event
          FbytesTotal:=StreamSize;
          FCurWorkMode:=npwSending;
          Synchronize(DoStartWork);

          //write the reply to the pipe
          while FReply.Position<StreamSize do
          begin
            bytesToSend:=FReply.Read(Buffer,SizeOf(Buffer));
            WriteFile(hPipe,buffer,bytesToSend,bytesTransferred,nil);

            FbytesCurrent:=FReply.Position;
            Synchronize(DoProgress);
          end;

          Synchronize(DoEndWork);

          FlushFileBuffers(hPipe);
          DisconnectNamedPipe(hPipe);

          //cu connection
          Synchronize(DoDisconnected);
        end; //if connected
      end; //while not terminated
    finally
      FlushFileBuffers(hPipe);
      DisconnectNamedPipe(hPipe);
      CloseHandle(hPipe);
      CloseHandle(Lappen.hEvent);
    end;
  except
    on E: Exception do
    begin
        FLastException:=E;
        Synchronize(DoError);
    end;
  end; //try .. except
end;

end.
