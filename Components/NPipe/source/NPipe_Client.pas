//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit NPipe_Client;

interface

uses
  SysUtils, Classes, StrUtils, NPipe_Types, Types, Windows;

type
  TNPClientThrd = class (TThread)
  private
    FPipeName           : String;
    FPipeServer         : String;
    FSendTimeout        : Cardinal;

    FClientComponent  : TObject;
    FDataToSend       : TMemoryStream;
    FReply            : TMemoryStream;
    FLastException    : Exception;
    FbytesCurrent,
    FbytesTotal       : Int64;
    FCurWorkMode      : TNPWorkMode;

    FOnError        : TNP_OnError;
    FOnServerReply  : TNP_OnServerReply;
    FOnProgress     : TNP_OnProgress;
    FOnStartWork    : TNP_OnStartWork;
    FOnEndWork      : TNP_OnEndWork;
    FOnConnected    : TNP_OnConnected;
    FOnDisconnected : TNP_OnDisconnected;

    procedure DoError();
    procedure DoServerReply();
    procedure DoProgress();
    procedure DoStartWork();
    procedure DoEndWork();
    procedure DoConnected();
    procedure DoDisconnected();
    
    procedure On_ExitThread();
  public
    constructor Create( PipeName           : String;
                        PipeServer         : String;
                        SendTimeout        : Cardinal;
                        OnError            : TNP_OnError;
                        OnServerReply      : TNP_OnServerReply;
                        OnProgress         : TNP_OnProgress;
                        OnStartWork        : TNP_OnStartWork;
                        OnEndWork          : TNP_OnEndWork;
                        OnConnected        : TNP_OnConnected;
                        OnDisconnected     : TNP_OnDisconnected;
                        ClientComponent    : TObject;
                        DataToSend         : TMemoryStream);
    destructor Destroy(); override;
    procedure Execute(); override;
  end;

  TNPipeClient = class(TComponent)
  private
    //Properties
    FPipeName       : String;
    FPipeServer     : String;
    FBlocked        : Boolean;
    FSendTimeout    : Cardinal;

    //Internal
    FOnError        : TNP_OnError;
    FOnReply        : TNP_OnServerReply;
    FOnProgress     : TNP_OnProgress;
    FOnStartWork    : TNP_OnStartWork;
    FOnEndWork      : TNP_OnEndWork;
    FOnConnected    : TNP_OnConnected;
    FOnDisconnected : TNP_OnDisconnected;

    FThread : TNPClientThrd;

    procedure Set_PipeServer(Value : String);
    procedure Set_PipeName(Value : String);

    procedure On_ComponentError(Error: Exception);
  protected

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    procedure Send(Data: TMemoryStream); overload;
    procedure Send(Strings: TStrings); overload;
    procedure Send(Text: String); overload;
    procedure Send(Value: Integer); overload;
    procedure Send(Value: Extended); overload;
    procedure SendBuffer(const Buffer);
    
  published
    property PipeName           : String    read FPipeName        write Set_PipeName;
    property PipeServer         : String    read FPipeServer      write Set_PipeServer;
    property SendTimeout        : Cardinal  read FSendTimeout     write FSendTimeout    default NP_Default_ClientSendTimeout;

    property OnError        : TNP_OnError         read FOnError         write FOnError;
    property OnServerReply  : TNP_OnServerReply   read FOnReply         write FOnReply;
    property OnProgress     : TNP_OnProgress      read FOnProgress      write FOnProgress;
    property OnStartWork    : TNP_OnStartWork     read FOnStartWork     write FOnStartWork;
    property OnEndWork      : TNP_OnEndWork       read FOnEndWork       write FOnEndWork;
    property OnConnected    : TNP_OnConnected     read FOnConnected     write FOnConnected;
    property OnDisconnected : TNP_OnDisconnected  read FOnDisconnected  write FOnDisconnected;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TUO', [TNPipeClient]);
end;

{ TNPipeClient }

constructor TNPipeClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPipeName:=NP_Default_PipeName;
  FPipeServer:=NP_Default_PipeServer;
  FSendTimeout:=NP_Default_ClientSendTimeout;
  FBlocked:=false;
end;

destructor TNPipeClient.Destroy;
begin
  inherited;
end;

procedure TNPipeClient.On_ComponentError(Error: Exception);
begin
  if (Assigned(FOnError)) then
    FOnError(Self,Error)
  else
    raise Error;
end;

procedure TNPipeClient.Set_PipeServer(Value: String);
begin
  FPipeServer:=IfThen(AnsiSameText(Value,EmptyStr),NP_Default_PipeServer,Value);
end;

procedure TNPipeClient.Set_PipeName(Value: String);
begin
  FPipeName:=IfThen(AnsiSameText(Value,EmptyStr),NP_Default_PipeName,Value);
end;


//Send-Routines
procedure TNPipeClient.SendBuffer(const Buffer);
var
  MS : TMemoryStream;
begin
  MS:=TMemoryStream.Create;
  MS.Write(Buffer,SizeOf(Buffer));
  Send(MS);
  MS.Free;
end;

procedure TNPipeClient.Send(Value: Extended);
var
  MS : TMemoryStream;
begin
  MS:=TMemoryStream.Create;
  MS.Write(Value,SizeOf(Value));
  Send(MS);
  MS.Free;
end;

procedure TNPipeClient.Send(Value: Integer);
var
  MS : TMemoryStream;
begin
  MS:=TMemoryStream.Create;
  MS.Write(Value,SizeOf(Value));
  Send(MS);
  MS.Free;
end;

procedure TNPipeClient.Send(Text: String);
var
  SL : TStrings;
begin
  SL:=TStringList.Create;
  SL.Text:=Text;
  Send(SL);
  SL.Free;
end;

procedure TNPipeClient.Send(Strings: TStrings);
var
  MS : TMemoryStream;
begin
  MS:=TMemoryStream.Create;
  Strings.SaveToStream(MS);
  Send(MS);
  MS.Free;
end;

procedure TNPipeClient.Send(Data: TMemoryStream);
begin
  if (FBlocked) then
  begin
    On_ComponentError(ENP_ComponentBlocked.Create(ENP_MSG_ComponentBlocked));
    exit;
  end;

  if (Assigned(Data)) then
    FThread:=TNPClientThrd.Create(FPipeName,
                                  FPipeServer,
                                  FSendTimeout,
                                  FOnError,
                                  FOnReply,
                                  FOnProgress,
                                  FOnStartWork,
                                  FOnEndWork,
                                  FOnConnected,
                                  FOnDisconnected,
                                  Self,
                                  Data);
end;

{ TNPClientThrd }

constructor TNPClientThrd.Create( PipeName, PipeServer: String;
                                  SendTimeout     : Cardinal;
                                  OnError         : TNP_OnError;
                                  OnServerReply   : TNP_OnServerReply;
                                  OnProgress      : TNP_OnProgress;
                                  OnStartWork     : TNP_OnStartWork;
                                  OnEndWork       : TNP_OnEndWork;
                                  OnConnected     : TNP_OnConnected;
                                  OnDisconnected  : TNP_OnDisconnected;
                                  ClientComponent : TObject;
                                  DataToSend      : TMemoryStream);
begin
  inherited Create(true);
  FPipeName:=PipeName;
  FPipeServer:=PipeServer;
  FSendTimeout:=SendTimeout;

  FOnError:=OnError;
  FOnServerReply:=OnServerReply;
  FOnProgress:=OnProgress;
  FOnStartWork:=OnStartWork;
  FOnEndWork:=OnEndWork;
  FOnConnected:=OnConnected;
  FOnDisconnected:=OnDisconnected;
  FClientComponent:=ClientComponent;

  FDataToSend:=TMemoryStream.Create;
  DataToSend.SaveToStream(FDataToSend);
  FReply:=TMemoryStream.Create;

  FreeOnTerminate:=true;

  Resume;
end;

destructor TNPClientThrd.Destroy;
begin
  FDataToSend.free;
  FReply.Free;
  inherited;
end;

procedure TNPClientThrd.DoError;
begin
  if (Assigned(FOnError)) then
    FOnError(FClientComponent,FLastException)
  else
    raise FLastException;
end;

procedure TNPClientThrd.DoProgress;
begin
  if (Assigned(FOnProgress)) then
  begin
    if (FCurWorkMode=npwSending) then
      FOnProgress(FClientComponent,FbytesCurrent,nil)
    else
      FOnProgress(FClientComponent,FbytesCurrent,FReply);
  end;
end;

procedure TNPClientThrd.DoServerReply;
begin
  if (Assigned(FOnServerReply)) then
    FOnServerReply(FClientComponent,FReply);
end;

procedure TNPClientThrd.DoStartWork;
begin
  if (Assigned(FOnStartWork)) then
    FOnStartWork(FClientComponent,FbytesTotal,FCurWorkMode);
end;

procedure TNPClientThrd.DoEndWork;
begin
  if (Assigned(FOnEndWork)) then
    FOnEndWork(FClientComponent);
end;

procedure TNPClientThrd.DoConnected;
begin
  if (Assigned(FOnConnected)) then
    FOnConnected(FClientComponent);
end;

procedure TNPClientThrd.DoDisconnected;
begin
  if (Assigned(FOnDisconnected)) then
    FOnDisconnected(FClientComponent);
end;


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TNPClientThrd.Execute;
var
  hPipe       : THandle;
  Sendsize,
  GetSize     : Int64;
  Written,
  BytesToSend : Cardinal;
  Buffer      : TNPBuffer;

  SecDesc     : TSecurityDescriptor;
  PSecAttrib  : PSecurityAttributes;
  SecAttrib   : TSecurityAttributes;

  Mode        : DWORD;
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

  Mode:=PIPE_READMODE_MESSAGE;
  if (not WaitNamedPipe(PChar('\\'+FPipeServer+'\pipe\'+FPipeName),
            FSendTimeout)) then
  begin
    FLastException:=ENP_SendTimeout.Create(ENP_MSG_SendTimeout);
    Synchronize(DoError);
    On_ExitThread;
    exit;
  end;
  hPipe:=CreateFile(PChar('\\'+FPipeServer+'\pipe\'+FPipeName),
                      GENERIC_READ or
                      GENERIC_WRITE or
                      FILE_FLAG_OVERLAPPED,
                      0,
                      PSecAttrib,
                      OPEN_EXISTING,
                      0,
                      0);
  if (hPipe=INVALID_HANDLE_VALUE) then
  begin
    FLastException:=ENP_OpenPipe.Create(ENP_MSG_OpenPipe);
    Synchronize(DoError);
    On_ExitThread;
    exit;
  end;

  if (not SetNamedPipeHandleState(hPipe,Mode,nil, nil)) then
  begin
    FLastException:=ENP_SetPipeMode.Create(ENP_MSG_SetPipeMode);
    Synchronize(DoError);
    CloseHandle(hPipe);
    On_ExitThread;
    exit;
  end;

  try //finally
    try //except
      //yeah, we got a connection
      Synchronize(DoConnected);

      Sendsize:=FDataToSend.Size;
      FDataToSend.Seek(0,soBeginning);
      FbytesCurrent:=0;

      //write out the size of the data
      WriteFile(hPipe,Sendsize,SizeOf(Sendsize),Written,nil);

      //prepare and trigger the OnStartWork event
      FbytesTotal:=Sendsize;
      FCurWorkMode:=npwSending;
      Synchronize(DoStartWork);

      while FBytesCurrent<FDataToSend.Size do
      begin
        BytesToSend:=FDataToSend.Read(Buffer,SizeOf(Buffer));
        WriteFile(hPipe,buffer,BytesToSend,Written,nil);

        FbytesCurrent:=FDataToSend.Position;
        Synchronize(DoProgress);
      end;

      //inform the comnponent about the end of the work
      Synchronize(DoEndWork);

      FReply.Clear;

      //Read the estimated data size
      ReadFile(hPipe, GetSize, SizeOf(GetSize), Written, nil);
      FReply.SetSize(GetSize);
      FReply.Seek(0,soFromBeginning);

      //prepare and trigger the OnStartWork event
      FbytesTotal:=GetSize;
      FCurWorkMode:=npwReceiving;
      Synchronize(DoStartWork);

      //Read the server-reply into FReply
      while (FReply.Position<GetSize) do
      begin
        ReadFile(hPipe,Buffer,sizeof(Buffer),Written,nil);
        if (Written=0) then break;
        FReply.Write(Buffer,Written);

        FillMemory(@buffer,Sizeof(buffer),0);

        FbytesCurrent:=FReply.Position;
        Synchronize(DoProgress);
        if (FReply.Size>=FbytesCurrent) then
          FReply.Seek(FbytesCurrent,soFromBeginning)
        else
          FReply.Seek(0,soFromEnd);
      end;

      //inform the comnponent about the end of the work
      Synchronize(DoEndWork);

      //we got a server reply; lets tell this the component
      FReply.Seek(0,soBeginning);
      Synchronize(DoServerReply);
    except
      on E:Exception do
      begin
        FLastException:=E;
        Synchronize(DoError);
      end;
    end; //try ... except
  finally
    CloseHandle(hPipe);
    //cu connection
    Synchronize(DoDisconnected);
    On_ExitThread;
  end; //try ... except
end;

procedure TNPClientThrd.On_ExitThread;
begin
  TNPipeClient(FClientComponent).FBlocked:=false;
  Self.Terminate;
end;

end.
