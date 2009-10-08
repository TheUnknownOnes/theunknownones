//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit sipXtapiHelper;

interface

uses
  sipXtapi,
  sipXtapiEvents,
  Classes,
  SysUtils,
  dialogs,
  Windows;

type
  TsxInstance = class;
  TsxLine = class;
  TsxLineList = class;
  TsxLineListEnumerator = class;

  TsxExternalTransport = class;
  TsxExternalTransportList = class;
  TsxExternalTransportListEnumerator = class;

  TsxCall = class;
  TsxCallList = class;
  TsxCallListEnumerator = class;

  {$REGION 'TsxLine ...'}
  TsxLine = class
  protected
    FOwner : TsxInstance;
    FLine : TSIPX_Line;
  public
    constructor Create(const AParent : TsxInstance;
                       LocalURI : String);
    destructor Destroy(); override;

    property Line : TSIPX_Line read FLine;

    procedure AddAlias(const AAlias : String);
    
    procedure AddCredential(const AUserID,
                                  APassword,
                                  ARealm : String);
    function URI : String;

    function ContactID : TSIPX_Contact_ID;


    procedure RegisterWithProxy(ARegister : Boolean);

    procedure Subscribe(ATargetUrl : String;
                        AEventType : String;
                        AAcceptType : String;
                        AContactId : TSIPX_Contact_ID;
                        var ASub : TSIPX_Sub);
    procedure UnSubscribe(ASub : TSIPX_Sub);

    function CreateCall : TsxCall;
  end;

  TsxLineListEnumerator = class
  private
    FIndex: Integer;
    FList: TsxLineList;
  public
    constructor Create(ALineList: TsxLineList);
    function GetCurrent: TsxLine;
    function MoveNext: Boolean;
    property Current: TsxLine read GetCurrent;
  end;

  TsxLineList = class(TList)
  private
    function Get(Index: Integer): TsxLine;
    procedure Put(Index: Integer; const Value: TsxLine);
  public
    function Add(Item: TsxLine): Integer;
    function Extract(Item: TsxLine): TsxLine;
    function First: TsxLine;
    function GetEnumerator: TsxLineListEnumerator;
    function IndexOf(Item: TsxLine): Integer;
    procedure Insert(Index: Integer; Item: TsxLine);
    function Last: TsxLine;
    function Remove(Item: TsxLine): Integer;
    property Items[Index: Integer]: TsxLine read Get write Put; default;
  end;
  {$ENDREGION}

  {$REGION 'TsxExternalTransport ...'}
  TsxExternalTransportProcedure = function(const ADestinationIp : String;
                                           const ADestPort      : Integer;
                                           const ALocalIp       : String;
                                           const ALocalPort     : Integer;
                                           const AData          : Pointer;
                                           const ADataLen       : Cardinal;
                                           const AUserData      : Pointer) : Boolean of object;

  TsxExternalTransport = class
  protected
    FOwner : TsxInstance;
    FTranport : TSIPX_Transport;

    FOnData: TsxExternalTransportProcedure;
    
    function DoExternalTransport(const ADestinationIp  : String;
                                 const ADestPort       : Integer;
                                 const ALocalIp        : String;
                                 const ALocalPort      : Integer;
                                 const AData           : Pointer;
                                 const ADataLen        : Cardinal;
                                 const AUserData       : Pointer) : Boolean;
  published
  public
    constructor Create(AOwner : TsxInstance;
                       AIsReliable : Boolean;
                       ATransport : String;
                       ALocalIP : String;
                       ALocalPort : Integer;
                       AWriteProc : TSIPX_Transport_Write_Proc;
                       ALocalRoutingId : String;
                       AUserData  : Pointer = nil);
    destructor Destroy(); override;

    property Transport : TSIPX_Transport read FTranport;

    property OnData : TsxExternalTransportProcedure read FOnData write FOnData;

    procedure HandleMessage(const ASourceIP : String;
                            const ASourcePort : Integer;
                            const ALocalIP : String;
                            const ALocalPort : Integer;
                            const AData : Pointer;
                            const ADataLen : Cardinal);
    procedure SetRouteByUser(ARouteByUser : Boolean);
  end;

  TsxExternalTransportListEnumerator = class
  private
    FIndex: Integer;
    FList: TsxExternalTransportList;
  public
    constructor Create(ATransportList: TsxExternalTransportList);
    function GetCurrent: TsxExternalTransport;
    function MoveNext: Boolean;
    property Current: TsxExternalTransport read GetCurrent;
  end;

  TsxExternalTransportList = class(TList)
  private
    function Get(Index: Integer): TsxExternalTransport;
    procedure Put(Index: Integer; const Value: TsxExternalTransport);
  public
    function Add(Item: TsxExternalTransport): Integer;
    function Extract(Item: TsxExternalTransport): TsxExternalTransport;
    function First: TsxExternalTransport;
    function GetEnumerator: TsxExternalTransportListEnumerator;
    function IndexOf(Item: TsxExternalTransport): Integer;
    procedure Insert(Index: Integer; Item: TsxExternalTransport);
    function Last: TsxExternalTransport;
    function Remove(Item: TsxExternalTransport): Integer;
    property Items[Index: Integer]: TsxExternalTransport read Get write Put; default;
  end;
  {$ENDREGION}

  {$REGION 'TsxCall ...'}
  TsxCall = class
  protected
    FOwner : TsxInstance;
    FCall : TSIPX_Call;
  public
    constructor Create(AOwner : TsxInstance; ACall : TSIPX_Call); overload;
    constructor Create(AOwner : TsxInstance; ALine : TsxLine); overload;
    destructor Destroy(); override;

    property Call : TSIPX_Call read FCall;

    procedure Accept(AVideoDisplay : PSIPX_Video_Display = nil;
                     ASecurityAttributes : PSIPX_Security_Attributes = nil;
                     AOptions : PSIPX_Call_Options = nil);
    procedure Reject(AErrorCode : Integer = 400;
                     AErrorText : String = 'Bad Request');
    procedure Redirect(AURL : String);
    procedure Answer(ATakeFocus : Boolean = true);

    procedure Connect(AAddress : String;
                      AContact : TSIPX_Contact_ID = 0;
                      ADisplay : PSIPX_Video_Display = nil;
                      ASecurity : PSIPX_Security_Attributes = nil;
                      ATakeFocus : Boolean = true;
                      AOptions : PSIPX_Call_OPTIONS = nil;
                      ACallId : String = '');

    procedure Hold(AStopRemoteAudio : Boolean = true);
    procedure Unhold();

    function GetID : String;
    function GetLocalID : String;
    function GetRemoteID : String;
    function GetContactID : String;
    function GetConnectionID : Integer;
    function GetConference : TSIPX_Conf;
    function GetRequestURI : String;
    function GetRemoteContact : String;
    function GetRemoteUserAgent : String;

    procedure StartTone(AToneID : TSIPX_Tone_ID;
                        ALocal,
                        ARemote : Boolean);
    procedure StopTone();

    procedure AudioPlayFileStart(AFile : String;
                                 ARepeat : Boolean;
                                 ALocal : Boolean;
                                 ARemote : Boolean;
                                 AMixWithMicrophone : Boolean = false;
                                 AVolumeScaling : Single = 1);
    procedure AudioPlayFileStop();

    procedure AudioRecordFileStart(AFile : String);
    procedure AudioRecordFileStop();

    procedure PlayBufferStart(ABuffer : PChar;
                              ABufSize : Integer;
                              ABufType : TSIPX_Audio_Data_Format;
                              ARepeat : Boolean;
                              ALocal : Boolean;
                              ARemote : Boolean);
    procedure PlayBufferStop();

    procedure Subscribe(AEventType : String;
                        AAcceptType : String;
                        var ASub : TSIPX_Sub;
                        ARemoteContactIsGruu : Boolean = false);
    procedure UnSubscribe(ASub : TSIPX_Sub);

    procedure SendInfo(var AInfo : TSIPX_Info;
                       AContentType : String;
                       AContent : PChar;
                       AContentLength : Cardinal);

    procedure BlindTransfer(AAddress : String);
    procedure Transfer(ACall : TsxCall);

    procedure UpdateVideoWindow(AHWND : TSIPX_Window_Handle);
    procedure ResizeWindow(AHWND : TSIPX_Window_Handle);

    procedure GetAudioRtpSourceIds(var ASendSSRC, AReceiveSSRC : Cardinal);

    procedure LimitCodecPreferences(AAudioBandwidth : TSIPX_Audio_Bandwidth;
                                    AVideoBandwidth : TSIPX_Video_Bandwidth;
                                    AVideoCodecName : String);
  end;

  TsxCallListEnumerator = class
  private
    FIndex: Integer;
    FList: TsxCallList;
  public
    constructor Create(ACallList: TsxCallList);
    function GetCurrent: TsxCall;
    function MoveNext: Boolean;
    property Current: TsxCall read GetCurrent;
  end;

  TsxCallList = class(TList)
  private
    function Get(Index: Integer): TsxCall;
    procedure Put(Index: Integer; const Value: TsxCall);
  public
    function Add(Item: TsxCall): Integer;
    function Extract(Item: TsxCall): TsxCall;
    function First: TsxCall;
    function GetEnumerator: TsxCallListEnumerator;
    function IndexOf(Item: TsxCall): Integer;
    procedure Insert(Index: Integer; Item: TsxCall);
    function Last: TsxCall;
    function Remove(Item: TsxCall): Integer;
    property Items[Index: Integer]: TsxCall read Get write Put; default;

    function FindCall(ACall : TSIPX_Call) : TsxCall;
  end;
  {$ENDREGION}

  {$REGION 'Events for TsxInstance'}
  TsxLogProcedure = procedure(const APriority, ASource, AMessage : String) of object;
  TsxEventProcedure_Callstate = procedure(const ACallstate : TSIPX_Callstate_Info) of object;
  TsxEventProcedure_Linestate = procedure(const ALinestate : TSIPX_Linestate_Info) of object;
  TsxEventProcedure_Infostatus = procedure(const AInfostatus : TSIPX_Infostatus_Info) of object;
  TsxEventProcedure_Info = procedure(const AInfo : TSIPX_Info_Info) of object;
  TsxEventProcedure_Substatus = procedure(const ASubstatus : TSIPX_Substatus_Info) of object;
  TsxEventProcedure_Notify = procedure(const ANotify : TSIPX_Notify_Info) of object;
  TsxEventProcedure_Config = procedure(const AConfig : TSIPX_Config_Info) of object;
  TsxEventProcedure_Security = procedure(const ASecurity : TSIPX_Security_Info) of object;
  TsxEventProcedure_Media = procedure(const AMedia : TSIPX_Media_Info) of object;
  TsxEventProcedure_Keepalive = procedure(const AKeepalive : TSIPX_Keepalive_Info) of object;
  {$ENDREGION}

  TsxInstance = class
  private
    FOnLog: TsxLogProcedure;
    FOnLinestate: TsxEventProcedure_Linestate;
    FOnNotify: TsxEventProcedure_Notify;
    FOnInfostatus: TsxEventProcedure_Infostatus;
    FOnCallstate: TsxEventProcedure_Callstate;
    FOnConfig: TsxEventProcedure_Config;
    FOnMedia: TsxEventProcedure_Media;
    FOnKeepalive: TsxEventProcedure_Keepalive;
    FOnSubstatus: TsxEventProcedure_Substatus;
    FOnSecurity: TsxEventProcedure_Security;
    FOnInfo: TsxEventProcedure_Info;
    function GetInBandDTMF: Boolean;
    function GetOutOfBandDTMF: Boolean;
    procedure SetInBandDTMF(const Value: Boolean);
    procedure SetOutOfBandDTMF(const Value: Boolean);
    function GetGain: Integer;
    procedure SetGain(const Value: Integer);
    function GetMute: Boolean;
    procedure SetMute(const Value: Boolean);
    function GetEnabledSpeaker: TSpeaker_Type;
    procedure SetEnabledSpeaker(const Value: TSpeaker_Type);
    function GetAECMode: TSIPX_AEC_Mode;
    procedure SetAECMode(const Value: TSIPX_AEC_Mode);
    function GetAGCMode: Boolean;
    procedure SetAGCMode(const Value: Boolean);
    function GetNoiseReductionMode: TSIPX_Noise_Reduction_Mode;
    procedure SetNoiseReductionMode(
      const Value: TSIPX_Noise_Reduction_Mode);
  protected
    FInstance : TSIPX_Inst;

    FLines : TsxLineList;
    FTransports : TsxExternalTransportList;
    FCalls : TsxCallList;

    procedure DoOnLog(const APriority, ASource, AMessage : String);
    procedure DoOnCallstate(ACallstate : TSIPX_Callstate_Info);
    procedure DoOnLinestate(ALinestate : TSIPX_Linestate_Info);
    procedure DoOnInfostatus(AInfostatus : TSIPX_Infostatus_Info);
    procedure DoOnInfo(AInfo : TSIPX_Info_Info);
    procedure DoOnSubstatus(ASubstatus : TSIPX_Substatus_Info);
    procedure DoOnNotify(ANotify : TSIPX_Notify_Info);
    procedure DoOnConfig(AConfig : TSIPX_Config_Info);
    procedure DoOnSecurity(ASecurity : TSIPX_Security_Info);
    procedure DoOnMedia(AMedia : TSIPX_Media_Info);
    procedure DoOnKeepalive(AKeepalive : TSIPX_Keepalive_Info);
  published
  public
    constructor Create(const udpPort : Integer = DEFAULT_UDP_PORT;
                       const tcpPort : Integer = DEFAULT_TCP_PORT;
                       const tlsPort : Integer = DEFAULT_TLS_PORT;
                       const rtpPortStart : Integer = DEFAULT_RTP_START_PORT;
                       const maxConnections : Integer = DEFAULT_CONNECTIONS;
                       Identity : String = DEFAULT_IDENTITY;
                       BindToAddr : String = DEFAULT_BIND_ADDRESS;
                       UseSequentialPorts : Boolean = false;
                       TLSCertificateNickname : PChar = nil;
                       TLSCertificatePassword : PChar = nil;
                       DbLocation : PChar = nil);

    destructor Destroy(); override;

    function Reinitialize(const udpPort : Integer = DEFAULT_UDP_PORT;
                          const tcpPort : Integer = DEFAULT_TCP_PORT;
                          const tlsPort : Integer = DEFAULT_TLS_PORT;
                          const rtpPortStart : Integer = DEFAULT_RTP_START_PORT;
                          const maxConnections : Integer = DEFAULT_CONNECTIONS;
                          Identity : String = DEFAULT_IDENTITY;
                          BindToAddr : String = DEFAULT_BIND_ADDRESS;
                          UseSequentialPorts : Boolean = false;
                          TLSCertificateNickname : PChar = nil;
                          TLSCertificatePassword : PChar = nil;
                          DbLocation : PChar = nil) : Boolean;

    property Instance : TSIPX_Inst read FInstance;
    //the real sipX instance

    property Transports : TsxExternalTransportList read FTransports;

    {$REGION 'Calls'}
    property Calls : TsxCallList read FCalls;
    {$ENDREGION}

    {$REGION 'Lines'}
    property Lines : TsxLineList read FLines;
    function AddLine(AURI : String) : TsxLine;
    function LineByURI(AURI : String) : TsxLine;
    {$ENDREGION}

    {$REGION 'Config'}
    procedure EnableGIPSTracing(AEnable : Boolean);
    procedure SetUserAgentName(AName : String; AIncludePlatform : Boolean = true);
    procedure SetOutboundProxy(AProxy : String);
    procedure SetRegisterResponseWaitSeconds(ASeconds : Integer);
    procedure SetDnsSrvFailoverTimeout(ASeconds : Integer);
    procedure EnableRport(AEnable : Boolean);
    procedure SetRegisterExpiration(ASeconds : Integer);
    procedure SetSubscribeExpiration(ASeconds : Integer);

    procedure EnableStun(AServer : String;
                         APort : Integer;
                         AKeepaliveSeconds : Integer);
    procedure DisableStun();

    procedure EnableTurn(AServer : String;
                         APort : Integer;
                         AUsername, APassword : String;
                         AKeepaliveSeconds : Integer);
    procedure DisableTurn();

    procedure EnableIce();
    procedure DisableIce();

    procedure KeepAliveAdd(AContactID : TSIPX_Contact_ID;
                           AType : TSIPX_Keepalive_Type;
                           ARemoteIP : String;
                           ARemotePort : Integer;
                           ASeconds : Integer);
    procedure KeepAliveRemove(AContactID : TSIPX_Contact_ID;
                              AType : TSIPX_Keepalive_Type;
                              ARemoteIP : String;
                              ARemotePort : Integer);

    property OutOfBandDTMF : Boolean read GetOutOfBandDTMF write SetOutOfBandDTMF;
    property InBandDTMF : Boolean read GetInBandDTMF write SetInBandDTMF;

    procedure EnableRTCP(AEnable : Boolean);

    function LocalSipUDPPort : Integer;
    function LocalSipTCPPort : Integer;
    function LocalSipTLSPort : Integer;

    procedure SetAudioCodecPreferences(ABandwidth : TSIPX_Audio_Bandwidth);
    procedure SetAudioCodecByName(ACodecName : String);
    procedure GetAudioCodecPreferences(out ABandWidth : TSIPX_Audio_Bandwidth);
    function GetNumAudioCodecs : Integer;
    procedure GetAudioCodec(AIndex : Integer; ACodec : PSIPX_Audio_Codec);

    procedure SetVideoBandwidth(ABandWidth : TSIPX_Video_Bandwidth);
    procedure GetVideoCaptureDevices(var ADeviceList : TCaptureDevices);
    function GetVideoCaptureDevice() : String;
    procedure SetVideoCaptureDevice(ADevice : String);
    procedure SetVideoCodecByName(AName : String);
    function GetNumVideoCodecs : Integer;
    procedure SetVideoFormat(AFormat : TSIPX_Video_Format);
    function GetVideoCodec(AIndex : Integer) : TSIPX_Video_Codec;
    procedure SetVideoPreviewDisplay(var ADisplay : TSIPX_Video_Display);
    procedure UpdatePreviewWindow(AHWND : HWND);
    procedure SetVideoQuality(AQuality : TSIPX_Video_Quality);
    procedure SetVideoParameters(ABitRate, AFrameRate : Integer);
    procedure SetVideoBitrate(ABitRate : Integer);
    procedure SetVideoFramerate(AFrameRate : Integer);
    procedure SetVideoCpuUsage(AUsage : Integer);

    procedure GetLocalContacts(var AContactList : TSIPX_Contact_Addresses);
    procedure GetLocalFeedbackAddress(ARemoteIP : String;
                                      ARemotePort : Integer;
                                      var AContactIP : String;
                                      var AContactPort : Integer;
                                      ATimeoutMS : Integer);

    procedure SetSecurityParameters(ADBLocation, ACertNickname, APassword : String);

    procedure EnableSipShortNames(AEnabled : Boolean);
    procedure EnableSipDateHeader(AEnabled : Boolean);
    procedure EnableSipAllowHeader(AEnables : Boolean);

    procedure SetSipAcceptLanguage(ALanguage : String);
    procedure SetLocationHeader(AHeader : String);
    procedure SetConnectionIdleTimeout(ASeconds : Integer);

    procedure PrepareToHibernate();
    procedure UnHibernate();

    procedure EnableRtpOverTcp(AEnabled : Boolean);
    procedure SetVoiceQualityServer(AServer : String);
    {$ENDREGION}

    {$REGION 'Audio'}
    property Gain : Integer read GetGain write SetGain;
    property Mute : Boolean read GetMute write SetMute;
    property EnabledSpeaker : TSpeaker_Type read GetEnabledSpeaker write SetEnabledSpeaker;

    procedure SetVolume(ASpeaker : TSpeaker_Type; AVolume : Integer);
    function GetVolume(ASpeaker : TSpeaker_Type) : Integer;

    property AECMode : TSIPX_AEC_Mode read GetAECMode write SetAECMode;
    property AGCMode : Boolean read GetAGCMode write SetAGCMode;
    property NoiseReductionMode : TSIPX_Noise_Reduction_Mode read GetNoiseReductionMode write SetNoiseReductionMode;

    function GetNumInputDevices : Cardinal;
    function GetInputDevice (AIndex : Integer) : String;

    function GetNumOutputDevices : Cardinal;
    function GetOutputDevice(AIndex : Integer) : String;

    procedure SetInputDevice(ADevice : String);
    procedure SetOutputDevice(ADevice : String);
    procedure SetRingerDevice(ADevice : String);
    {$ENDREGION}



    {$REGION 'Events'}
    property OnLog : TsxLogProcedure read FOnLog write FOnLog;
    property OnCallstate : TsxEventProcedure_Callstate read FOnCallstate write FOnCallstate;
    property OnLinestate : TsxEventProcedure_Linestate read FOnLinestate write FOnLinestate;
    property OnInfostatus : TsxEventProcedure_Infostatus read FOnInfostatus write FOnInfostatus;
    property OnInfo : TsxEventProcedure_Info read FOnInfo write FOnInfo;
    property OnSubstatus : TsxEventProcedure_Substatus read FOnSubstatus write FOnSubstatus;
    property OnNotify : TsxEventProcedure_Notify read FOnNotify write FOnNotify;
    property OnConfig : TsxEventProcedure_Config read FOnConfig write FOnConfig;
    property OnSecurity : TsxEventProcedure_Security read FOnSecurity write FOnSecurity;
    property OnMedia : TsxEventProcedure_Media read FOnMedia write FOnMedia;
    property OnKeepalive : TsxEventProcedure_Keepalive read FOnKeepalive write FOnKeepalive;
    {$ENDREGION}
  end;
  PsxInstance = ^TsxInstance;

procedure sxCheckResultForError(AResult : TSIPX_Result);
procedure sxSetLogLevel(ALevel : TSIPX_Log_Level);
procedure sxSetLogFile(AFile : String);
procedure sxSetDnsSrvTimeouts(const AInitialTimeoutInSecs : Integer; const ARetries : Integer);
procedure sxEnableDnsSrv(AEnable : Boolean);

implementation

type
  TsxLogData = record
    Priority,
    Source,
    Msg : String;
  end;
  PsxLogData = ^TsxLogData;

  TsxEventData = record
    Category : TSIPX_Event_Category;
    Info,
    Userdata : Pointer;
  end;
  PsxEventData = ^TsxEventData;
  
  TsxLogEventThread = class(TThread)
  private
    FLogData : PsxLogData;
    FEventData : PsxEventData;

    procedure DoProcessLogData();
    procedure DoProcessEventData();
  protected
    procedure Execute(); override;
  public
    constructor Create; reintroduce;
    destructor Destroy(); override;

    procedure OnLog(const szPriority : PChar; const szSource : PChar; const szMessage : PChar);
    procedure OnEvent(category : TSIPX_Event_Category; pInfo: Pointer; pUserdata  : Pointer);
  end;

var
  //Global var's for this unit

  LogEventThread : TsxLogEventThread;
  //the thread which catches the logs and events and deploys them to the instances

  LogDataList, //holds PsxLogData
  EventsList,  //holds PsxEventData
  InstanceList : TThreadList;


  
procedure sxOnLog(const szPriority : PChar; const szSource : PChar; const szMessage : PChar); cdecl;
begin
  LogEventThread.OnLog(szPriority, szSource, szMessage);
end;

function sxOnEvent(category : TSIPX_Event_Category; pInfo: Pointer; pUserdata  : Pointer) : Boolean; cdecl;
begin
  LogEventThread.OnEvent(category, pInfo, pUserdata);
  Result:=true;
end;

function sxOnExternalTranport(hTransport            : TSIPX_Transport;
                              const szDestinationIp : PChar;
                              const iDestPort       : Integer;
                              const szLocalIp       : PChar;
                              const iLocalPort      : Integer;
                              const pData           : Pointer;
                              const nData           : Cardinal;
                              const pUserData       : Pointer) : Boolean;
var
  List : TList;
  Inst : TsxInstance;
  idx : Integer;
  Transp : TsxExternalTransport;
begin
  Result:=false;

  List:=InstanceList.LockList;
  try
    for idx:=0 to List.Count-1 do
    begin
      Inst:=List[idx];
      for Transp in Inst.Transports do
      begin
        if Transp.Transport=hTransport then
        begin
          Result:=Transp.DoExternalTransport(PChar(szDestinationIp),
                                             iDestPort,
                                             PChar(szLocalIp),
                                             iLocalPort,
                                             pData,
                                             nData,
                                             pUserData);
          if Result then break;
        end;
        if Result then break;
      end;
    end;
  finally
    InstanceList.UnlockList;
  end;
end;

procedure RegisterInstance(var AInstance: TsxInstance);
var
  List : TList;
begin
  List:=InstanceList.LockList;
  try
    List.Add(AInstance);
    sxCheckResultForError(
      sipxEventListenerAdd(AInstance.Instance, sxOnEvent, AInstance)
    );
  finally
    InstanceList.UnlockList;
  end;
end;

procedure UnRegisterInstance(var AInstance: TsxInstance);
var
  List : TList;
begin
  list:=InstanceList.LockList;
  try
    sipxEventListenerRemove(AInstance.Instance, sxOnEvent, AInstance);
    list.Remove(AInstance);
  finally
    InstanceList.UnlockList;
  end;
end;

procedure sxCheckResultForError(AResult : TSIPX_Result);
begin
  case AResult of
    SIPX_RESULT_FAILURE: raise Exception.Create('Generic Failure.');
    SIPX_RESULT_NOT_IMPLEMENTED: raise Exception.Create('Method/API not implemented.');
    SIPX_RESULT_OUT_OF_MEMORY: raise Exception.Create('Unable to allocate enough memory to perform operation.');
    SIPX_RESULT_INVALID_ARGS: raise Exception.Create('Invalid arguments; bad handle, argument out of range, etc.');
    SIPX_RESULT_BAD_ADDRESS: raise Exception.Create('Invalid SIP address.');
    SIPX_RESULT_OUT_OF_RESOURCES: raise Exception.Create('Out of resources (hit some max limit).');
    SIPX_RESULT_INSUFFICIENT_BUFFER: raise Exception.Create('Buffer too short for this operation.');
    SIPX_RESULT_EVAL_TIMEOUT: raise Exception.Create('The evaluation version of this product has expired.');
    SIPX_RESULT_BUSY: raise Exception.Create('The operation failed because the system was busy.');
    SIPX_RESULT_INVALID_STATE: raise Exception.Create('The operation failed because the object was in the wrong state.');
    SIPX_RESULT_MISSING_RUNTIME_FILES: raise Exception.Create('The operation failed because required runtime dependencies are missing.');
    SIPX_RESULT_TLS_DATABASE_FAILURE: raise Exception.Create('The operation failed because the certificate database did not initialize.');
    SIPX_RESULT_TLS_BAD_PASSWORD: raise Exception.Create('The operation failed because the certificate database did not accept the password.');
    SIPX_RESULT_TLS_TCP_IMPORT_FAILURE: raise Exception.Create('The operation failed because a TCP socket could not be imported by the SSL/TLS module.');
    SIPX_RESULT_NSS_FAILURE: raise Exception.Create('The operation failed due to an NSS failure.');
    SIPX_RESULT_NOT_SUPPORTED: raise Exception.Create('The operation is not supported in this build/configuration.');
    SIPX_RESULT_NETWORK_FAILURE: raise Exception.Create('The network is down or failing.');
  end;
end;

procedure sxSetLogLevel(ALevel : TSIPX_Log_Level);
begin
  sxCheckResultForError(
    sipxConfigSetLogLevel(ALevel)
  );
end;

procedure sxSetLogFile(AFile : String);
begin
  sxCheckResultForError(
    sipxConfigSetLogFile(PChar(AFile))
  );
end;

procedure sxSetDnsSrvTimeouts(const AInitialTimeoutInSecs : Integer; const ARetries : Integer);
begin
  sxCheckResultForError(
    sipxConfigSetDnsSrvTimeouts(AInitialTimeoutInSecs, ARetries)
  );
end;

procedure sxEnableDnsSrv(AEnable : Boolean);
begin
  sxCheckResultForError(
    sipxConfigEnableDnsSrv(AEnable)
  );
end;

{ TsxLogEventThread }

constructor TsxLogEventThread.Create;
begin
  inherited Create(true); 

  FreeOnTerminate:=true;

  Resume;
end;

destructor TsxLogEventThread.Destroy;
begin
  inherited;
end;

procedure TsxLogEventThread.DoProcessEventData;
var
  Inst : TsxInstance;
begin
  Inst:=TsxInstance(FEventData.Userdata);

  case FEventData.Category of
    EVENT_CATEGORY_CALLSTATE    : Inst.DoOnCallstate(PSIPX_Callstate_Info(FEventData.Info)^);
    EVENT_CATEGORY_LINESTATE    : Inst.DoOnLinestate(PSIPX_Linestate_Info(FEventData.Info)^);
    EVENT_CATEGORY_INFO_STATUS  : Inst.DoOnInfostatus(PSIPX_infoStatus_Info(FEventData.Info)^);
    EVENT_CATEGORY_INFO         : Inst.DoOnInfo(PSIPX_Info_Info(FEventData.Info)^);
    EVENT_CATEGORY_SUB_STATUS   : Inst.DoOnSubstatus(PSIPX_Substatus_Info(FEventData.Info)^);
    EVENT_CATEGORY_NOTIFY       : Inst.DoOnNotify(PSIPX_Notify_Info(FEventData.Info)^);
    EVENT_CATEGORY_CONFIG       : Inst.DoOnConfig(PSIPX_Config_Info(FEventData.Info)^);
    EVENT_CATEGORY_SECURITY     : Inst.DoOnSecurity(PSIPX_Security_Info(FEventData.Info)^);
    EVENT_CATEGORY_MEDIA        : Inst.DoOnMedia(PSIPX_Media_Info(FEventData.Info)^);
    EVENT_CATEGORY_KEEPALIVE    : Inst.DoOnKeepalive(PSIPX_Keepalive_Info(FEventData.Info)^);
  end;
end;

procedure TsxLogEventThread.DoProcessLogData;
var
  Inst : TsxInstance;
  idx : Integer;
  List : TList;
begin
  List:=InstanceList.LockList;
  try
    for idx:=0 to List.Count-1 do
    begin
      Inst:=List[idx];
      try Inst.DoOnLog(FLogData.Priority, FLogData.Source, FLogData.Msg); except end;
    end;
  finally
    InstanceList.UnlockList;
  end;
end;

procedure TsxLogEventThread.OnEvent(category: TSIPX_Event_Category; pInfo,
  pUserdata: Pointer);
var
  List : TList;
  AData : PsxEventData;
begin
  List:=EventsList.LockList;
  try
    New(AData);
    AData.Category:=category;
    Adata.Userdata:=pUserdata;
    sipxDuplicateEvent(category, pInfo, @(Adata.Info));

    List.Add(AData);
  finally
    EventsList.UnlockList;
  end;
end;

procedure TsxLogEventThread.OnLog(const szPriority, szSource,
  szMessage: PChar);
var
  List : TList;
  AData : PsxLogData;
begin
  List:=LogDataList.LockList;
  try
    New(AData);
    AData.Priority:=szPriority;
    AData.Source:=szSource;
    AData.Msg:=szMessage;

    List.Add(AData);
  finally
    LogDataList.UnlockList;
  end;
end;

procedure TsxLogEventThread.Execute;
var
  List : TList;
begin
  while not (Terminated or Suspended) do
  begin
  
    List:=LogDataList.LockList;
    try
      while List.Count>0 do
      begin
        FLogData:=List[0];
        List.Delete(0);

        Synchronize(DoProcessLogData);
        Dispose(FLogData);
      end;
    finally
      LogDataList.UnlockList;
    end;

    List:=EventsList.LockList;
    try
      while List.Count>0 do
      begin
        FEventData:=List[0];
        List.Delete(0);
        Synchronize(DoProcessEventData);

        sipxFreeDuplicatedEvent(fEventData.Category, FEventData.Info);
        Dispose(FEventData);
      end;
    finally
      EventsList.UnlockList;
    end;

    sleep(20);
  end;
end;

{ TsxInstance }

function TsxInstance.AddLine(AURI : String): TsxLine;
begin
  Result:=TsxLine.Create(Self, AURI);
end;

constructor TsxInstance.Create(const udpPort, tcpPort, tlsPort,
  rtpPortStart, maxConnections: Integer; Identity, BindToAddr: String;
  UseSequentialPorts: Boolean; TLSCertificateNickname,
  TLSCertificatePassword, DbLocation: PChar);
begin
  FInstance:=nil;

  FLines:=TsxLineList.Create;
  FCalls:=TsxCallList.Create;
  FTransports:=TsxExternalTransportList.Create;

  sxCheckResultForError(
    sipxInitialize(FInstance,
                   udpPort,
                   tcpPort,
                   tlsPort,
                   rtpPortStart,
                   maxConnections,
                   Identity,
                   BindToAddr,
                   UseSequentialPorts,
                   TLSCertificateNickname,
                   TLSCertificatePassword,
                   DbLocation)
  );

  RegisterInstance(Self);
end;

destructor TsxInstance.Destroy;
begin
  UnRegisterInstance(Self);

  while FCalls.Count>0 do
    FCalls[0].Free;
  FCalls.Free;

  while FLines.Count>0 do
    FLines[0].Free;
  FLines.Free;

  while FTransports.Count>0 do
    FTransports[0].Free;
  FTransports.Free;

  sipxUnInitialize(FInstance, true);

  inherited;
end;

procedure TsxInstance.DisableIce;
begin
  sxCheckResultForError(
    sipxconfigDisableIce(FInstance)
  );
end;

procedure TsxInstance.DisableStun;
begin
  sxCheckResultForError(
    sipxConfigDisableStun(FInstance)
  );
end;

procedure TsxInstance.DisableTurn;
begin
  sxCheckResultForError(
    sipxConfigDisableTurn(FInstance)
  );
end;

procedure TsxInstance.DoOnCallstate(ACallstate: TSIPX_Callstate_Info);
begin
  if Assigned(FOnCallstate) then
    FOnCallstate(ACallstate);
end;

procedure TsxInstance.DoOnConfig(AConfig: TSIPX_Config_Info);
begin
  if Assigned(FOnConfig) then
    FOnConfig(AConfig);
end;

procedure TsxInstance.DoOnInfo(AInfo: TSIPX_Info_Info);
begin
  if Assigned(FOnInfo) then
    FOnInfo(AInfo);
end;

procedure TsxInstance.DoOnInfostatus(AInfostatus: TSIPX_Infostatus_Info);
begin
  if Assigned(FOnInfostatus) then
    FOnInfostatus(AInfostatus);
end;

procedure TsxInstance.DoOnKeepalive(AKeepalive: TSIPX_Keepalive_Info);
begin
  if Assigned(FOnKeepalive) then
    FOnKeepalive(AKeepalive);
end;

procedure TsxInstance.DoOnLinestate(ALinestate: TSIPX_Linestate_Info);
begin
  if Assigned(FOnLinestate) then
    FOnLinestate(ALinestate);
end;

procedure TsxInstance.DoOnLog(const APriority, ASource, AMessage: String);
begin
  if Assigned(FOnLog) then
    FOnLog(APriority, ASource, AMessage);
end;

procedure TsxInstance.DoOnMedia(AMedia: TSIPX_Media_Info);
begin
  if Assigned(FOnMedia) then
    FOnMedia(AMedia);
end;

procedure TsxInstance.DoOnNotify(ANotify: TSIPX_Notify_Info);
begin
  if Assigned(FOnNotify) then
    FOnNotify(ANotify);
end;

procedure TsxInstance.DoOnSecurity(ASecurity: TSIPX_Security_Info);
begin
  if Assigned(FOnSecurity) then
    FOnSecurity(ASecurity);
end;

procedure TsxInstance.DoOnSubstatus(ASubstatus: TSIPX_Substatus_Info);
begin
  if Assigned(FOnSubstatus) then
    FOnSubstatus(ASubstatus);
end;

procedure TsxInstance.EnableGIPSTracing(AEnable: Boolean);
begin
  sxCheckResultForError(
    sipxConfigEnableGIPSTracing(FInstance, AEnable)
  );
end;

procedure TsxInstance.EnableIce;
begin
  sxCheckResultForError(
    sipxConfigEnableIce(FInstance)
  );
end;

procedure TsxInstance.EnableRport(AEnable: Boolean);
begin
  sxCheckResultForError(
    sipxConfigEnableRport(FInstance, AEnable)
  );
end;

procedure TsxInstance.EnableRTCP(AEnable: Boolean);
begin
  sxCheckResultForError(
    sipxConfigEnableRTCP(FInstance, AEnable)
  );
end;

procedure TsxInstance.EnableRtpOverTcp(AEnabled: Boolean);
begin
  sxCheckResultForError(
    sipxConfigEnableRtpOverTcp(FInstance, AEnabled)
  );
end;

procedure TsxInstance.EnableSipAllowHeader(AEnables: Boolean);
begin
  sxCheckResultForError(
    sipxConfigEnableSipAllowHeader(FInstance, AEnables)
  );
end;

procedure TsxInstance.EnableSipDateHeader(AEnabled: Boolean);
begin
  sxCheckResultForError(
    sipxConfigEnableSipDateHeader(FInstance, AEnabled)
  );
end;

procedure TsxInstance.EnableSipShortNames(AEnabled: Boolean);
begin
  sxCheckResultForError(
    sipxConfigEnableSipShortNames(FInstance, AEnabled)
  );
end;

procedure TsxInstance.EnableStun(AServer: String; APort,
  AKeepaliveSeconds: Integer);
begin
  sxCheckResultForError(
    sipxConfigEnableStun(FInstance, PChar(AServer), APort, AKeepaliveSeconds)
  );
end;

procedure TsxInstance.EnableTurn(AServer: String; APort: Integer; AUsername,
  APassword: String; AKeepaliveSeconds: Integer);
begin
  sxCheckResultForError(
    sipxConfigEnableTurn(FInstance,
                         PChar(AServer),
                         APort,
                         PChar(AUsername),
                         PChar(APassword),
                         AKeepaliveSeconds)
  );
end;

function TsxInstance.GetAECMode: TSIPX_AEC_Mode;
begin
  sxCheckResultForError(
    sipxAudioGetAECMode(FInstance, Result)
  );
end;

function TsxInstance.GetAGCMode: Boolean;
begin
  sxCheckResultForError(
    sipxAudioGetAGCMode(FInstance, Result)
  );
end;

procedure TsxInstance.GetAudioCodec(AIndex : Integer; ACodec : PSIPX_Audio_Codec);
begin
  sxCheckResultForError(
    sipxConfigGetAudioCodec(FInstance, AIndex, ACodec)
  );
end;

procedure TsxInstance.GetAudioCodecPreferences(
  out ABandWidth: TSIPX_Audio_Bandwidth);
begin
  sxCheckResultForError(
    sipxConfigGetAudioCodecPreferences(FInstance, ABandWidth)
  );
end;

function TsxInstance.GetEnabledSpeaker: TSpeaker_Type;
begin
  sxCheckResultForError(
    sipxAudioGetEnabledSpeaker(FInstance, Result)
  );
end;

function TsxInstance.GetGain: Integer;
begin
  sxCheckResultForError(
    sipxAudioGetGain(FInstance, Result)
  );
end;

function TsxInstance.GetMute: Boolean;
begin
  sxCheckResultForError(
    sipxAudioIsMuted(FInstance, Result)
  );
end;

function TsxInstance.GetInBandDTMF: Boolean;
begin
  sxCheckResultForError(
    sipxConfigIsInBandDTMFEnabled(FInstance, Result)
  );
end;

function TsxInstance.GetInputDevice(AIndex: Integer): String;
var
  Buffer : Pchar;
begin
  sxCheckResultForError(
    sipxAudioGetInputDevice(FInstance, AIndex, Buffer)
  );
  
  Result:=Buffer;
end;

procedure TsxInstance.GetLocalContacts(var AContactList: TSIPX_Contact_Addresses);
var
  Len : Cardinal;
begin
  sxCheckResultForError(
    sipxConfigGetLocalContacts(FInstance,
                               AContactList,
                               Length(AContactList),
                               Len)
  );

  SetLength(AContactList, Len);
end;

procedure TsxInstance.GetLocalFeedbackAddress(ARemoteIP: String;
  ARemotePort: Integer; var AContactIP: String; var AContactPort: Integer;
  ATimeoutMS: Integer);
const
  MAX_IP_LEN = 64;
var
  Buffer : array[0..MAX_IP_LEN-1] of Char;
begin
  sxCheckResultForError(
    sipxConfigGetLocalFeedbackAddress(FInstance,
                                      PChar(ARemoteIP),
                                      ARemotePort,
                                      Buffer,
                                      MAX_IP_LEN,
                                      AContactPort,
                                      ATimeoutMS)
  );

  AContactIP:=Buffer;
end;

function TsxInstance.GetNoiseReductionMode: TSIPX_Noise_Reduction_Mode;
begin
  sxCheckResultForError(
    sipxAudioGetNoiseReductionMode(FInstance, Result)
  );
end;

function TsxInstance.GetNumAudioCodecs: Integer;
begin
  sxCheckResultForError(
    sipxConfigGetNumAudioCodecs(FInstance, @Result)
  );
end;

function TsxInstance.GetNumInputDevices: Cardinal;
begin
  sxCheckResultForError(
    sipxAudioGetNumInputDevices(FInstance, Result)
  );
end;

function TsxInstance.GetNumOutputDevices: Cardinal;
begin
  sxCheckResultForError(
    sipxAudioGetNumOutputDevices(FInstance, Result)
  );
end;

function TsxInstance.GetNumVideoCodecs: Integer;
begin
  sxCheckResultForError(
    sipxConfigGetNumVideoCodecs(FInstance, Result)
  );
end;

function TsxInstance.GetOutOfBandDTMF: Boolean;
begin
  sxCheckResultForError(
    sipxConfigIsOutOfBandDTMFEnabled(FInstance, Result)
  );
end;

function TsxInstance.GetOutputDevice(AIndex: Integer): String;
var
  Buffer : PChar;
begin
  sxCheckResultForError(
    sipxAudioGetOutputDevice(FInstance, AIndex, Buffer)
  );

  Result:=Buffer;
end;

function TsxInstance.GetVideoCaptureDevice: String;
var
  Buffer : array[0..MAX_CAPTUREDEVICE_LEN-1] of char;
begin
  sxCheckResultForError(
    sipxConfigGetVideoCaptureDevice(FInstance, Buffer, MAX_CAPTUREDEVICE_LEN)
  );
  
  Result:=Buffer;
end;

procedure TsxInstance.GetVideoCaptureDevices(
  var ADeviceList: TCaptureDevices);
begin
  {sxCheckResultForError(
    sipxConfigGetVideoCaptureDevices(FInstance, @ADeviceList, MAX_CAPTUREDEVICE_LEN, Length(ADeviceList))
  );}
end;

function TsxInstance.GetVideoCodec(AIndex : Integer): TSIPX_Video_Codec;
begin
  sxCheckResultForError(
    sipxConfigGetVideoCodec(FInstance, AIndex, Result)
  );
end;

function TsxInstance.GetVolume(ASpeaker: TSpeaker_Type): Integer;
begin
  sxCheckResultForError(
    sipxAudioGetVolume(FInstance, ASpeaker, Result)
  );
end;

procedure TsxInstance.KeepAliveAdd(AContactID: TSIPX_Contact_ID;
  AType: TSIPX_Keepalive_Type; ARemoteIP: String; ARemotePort,
  ASeconds: Integer);
begin
  sxCheckResultForError(
    sipxConfigKeepAliveAdd(FInstance,
                           AContactID,
                           AType,
                           PChar(ARemoteIP),
                           ARemotePort,
                           ASeconds)
  );
end;

procedure TsxInstance.KeepAliveRemove(AContactID: TSIPX_Contact_ID;
  AType: TSIPX_Keepalive_Type; ARemoteIP: String; ARemotePort: Integer);
begin
  sxCheckResultForError(
    sipxConfigKeepAliveRemove(FInstance,
                              AContactID,
                              AType,
                              PChar(ARemoteIP),
                              ARemotePort)
  );
end;

function TsxInstance.LineByURI(AURI : String): TsxLine;
var
  hLine : TSIPX_Line;
  Line : TsxLine;
begin
  Result:=nil;
  if sipxLineFindByURI(FInstance, PChar(AURI), hLine)=SIPX_RESULT_SUCCESS then
  begin
    for Line in Lines do
    begin
      if Line.Line=hLine then
      begin
        Result:=Line;
        break;
      end;
    end;
  end;
end;

function TsxInstance.LocalSipTCPPort: Integer;
begin
  sxCheckResultForError(
    sipxConfigGetLocalSipTcpPort(FInstance, Result)
  );
end;

function TsxInstance.LocalSipTLSPort: Integer;
begin
  sxCheckResultForError(
    sipxConfigGetLocalSipTlsPort(FInstance, Result)
  );
end;

function TsxInstance.LocalSipUDPPort: Integer;
begin
  sxCheckResultForError(
    sipxConfigGetLocalSipUdpPort(FInstance, Result)
  );
end;

procedure TsxInstance.PrepareToHibernate;
begin
  sxCheckResultForError(
    sipxConfigPrepareToHibernate(FInstance)
  );
end;

function TsxInstance.Reinitialize(const udpPort, tcpPort, tlsPort,
  rtpPortStart, maxConnections: Integer; Identity, BindToAddr: String;
  UseSequentialPorts: Boolean; TLSCertificateNickname,
  TLSCertificatePassword, DbLocation: PChar): Boolean;
begin
  Result:=true;

  sxCheckResultForError(
    sipxReInitialize(FInstance,
                     udpPort,
                     tcpPort,
                     tlsPort,
                     rtpPortStart,
                     maxConnections,
                     Identity,
                     BindToAddr,
                     UseSequentialPorts,
                     TLSCertificateNickname,
                     TLSCertificatePassword,
                     DbLocation)
  );
end;

procedure TsxInstance.SetAECMode(const Value: TSIPX_AEC_Mode);
begin
  sxCheckResultForError(
    sipxAudioSetAECMode(FInstance, Value)
  );
end;

procedure TsxInstance.SetAGCMode(const Value: Boolean);
begin
  sxCheckResultForError(
    sipxAudioSetAGCMode(FInstance, Value)
  );
end;

procedure TsxInstance.SetAudioCodecByName(ACodecName: String);
begin
  sxCheckResultForError(
    sipxConfigSetAudioCodecByName(FInstance, Pchar(ACodecName))
  );
end;

procedure TsxInstance.SetAudioCodecPreferences(
  ABandwidth: TSIPX_Audio_Bandwidth);
begin
  sxCheckResultForError(
    sipxConfigSetAudioCodecPreferences(FInstance, ABandwidth)
  );
end;

procedure TsxInstance.SetGain(const Value: Integer);
begin
  sxCheckResultForError(
    sipxAudioSetGain(FInstance, Value)
  );
end;

procedure TsxInstance.SetMute(const Value: Boolean);
begin
  sxCheckResultForError(
    sipxAudioMute(FInstance, Value)
  );
end;

procedure TsxInstance.SetNoiseReductionMode(
  const Value: TSIPX_Noise_Reduction_Mode);
begin
  sxCheckResultForError(
    sipxAudioSetNoiseReductionMode(FInstance, Value)
  );
end;

procedure TsxInstance.SetConnectionIdleTimeout(ASeconds: Integer);
begin
  sxCheckResultForError(
    sipxConfigSetConnectionIdleTimeout(FInstance, ASeconds)
  );
end;

procedure TsxInstance.SetDnsSrvFailoverTimeout(ASeconds: Integer);
begin
  sxCheckResultForError(
    sipxConfigSetDnsSrvFailoverTimeout(FInstance, ASeconds)
  );
end;

procedure TsxInstance.SetEnabledSpeaker(const Value: TSpeaker_Type);
begin
  sxCheckResultForError(
    sipxAudioEnableSpeaker(FInstance, Value)
  );
end;

procedure TsxInstance.SetInBandDTMF(const Value: Boolean);
begin
  sxCheckResultForError(
    sipxConfigEnableInBandDTMF(FInstance, Value)
  );
end;

procedure TsxInstance.SetInputDevice(ADevice: String);
begin
  sxCheckResultForError(
    sipxAudioSetCallInputDevice(FInstance, PChar(ADevice))
  );
end;

procedure TsxInstance.SetLocationHeader(AHeader: String);
begin
  sxCheckResultForError(
    sipxConfigSetLocationHeader(FInstance, PChar(AHeader))
  );
end;

procedure TsxInstance.SetOutboundProxy(AProxy: String);
begin
  sxCheckResultForError(
    sipxConfigSetOutboundProxy(FInstance, PChar(AProxy))
  );
end;

procedure TsxInstance.SetOutOfBandDTMF(const Value: Boolean);
begin
  sxCheckResultForError(
    sipxConfigEnableOutOfBandDTMF(FInstance, Value)
  );
end;

procedure TsxInstance.SetOutputDevice(ADevice: String);
begin
  sxCheckResultForError(
    sipxAudioSetCallOutputDevice(FInstance, PChar(ADevice))
  );
end;

procedure TsxInstance.SetRegisterExpiration(ASeconds: Integer);
begin
  sxCheckResultForError(
    sipxConfigSetRegisterExpiration(FInstance, ASeconds)
  );
end;

procedure TsxInstance.SetRegisterResponseWaitSeconds(ASeconds: Integer);
begin
  sxCheckResultForError(
    sipxConfigSetRegisterResponseWaitSeconds(FInstance, ASeconds)
  );
end;

procedure TsxInstance.SetRingerDevice(ADevice: String);
begin
  sxCheckResultForError(
    sipxAudioSetRingerOutputDevice(FInstance, PChar(ADevice))
  );
end;

procedure TsxInstance.SetSecurityParameters(ADBLocation, ACertNickname,
  APassword: String);
begin
  sxCheckResultForError(
    sipxConfigSetSecurityParameters(FInstance,
                                    PChar(ADBLocation),
                                    Pchar(ACertNickname),
                                    PChar(APassword))
  );
end;

procedure TsxInstance.SetSipAcceptLanguage(ALanguage: String);
begin
  sxCheckResultForError(
    sipxConfigSetSipAcceptLanguage(FInstance, Pchar(ALanguage))
  );
end;

procedure TsxInstance.SetSubscribeExpiration(ASeconds: Integer);
begin
  sxCheckResultForError(
    sipxConfigSetSubscribeExpiration(FInstance, ASeconds)
  );
end;

procedure TsxInstance.SetUserAgentName(AName: String; AIncludePlatform: Boolean);
begin
  sxCheckResultForError(
    sipxConfigSetUserAgentName(FInstance, PChar(AName), AIncludePlatform)
  );
end;

procedure TsxInstance.SetVideoBandwidth(ABandWidth: TSIPX_Video_Bandwidth);
begin
  sxCheckResultForError(
    sipxConfigSetVideoBandwidth(FInstance, ABandWidth)
  );
end;

procedure TsxInstance.SetVideoBitRate(ABitrate: Integer);
begin
  sxCheckResultForError(
    sipxconfigSetVideoBitrate(FInstance, ABitrate)
  );
end;

procedure TsxInstance.SetVideoCaptureDevice(ADevice: String);
begin
  sxCheckResultForError(
    sipxConfigSetVideoCaptureDevice(FInstance, PChar(ADevice))
  );
end;

procedure TsxInstance.SetVideoCodecByName(AName: String);
begin
  sxCheckResultForError(
    sipxConfigSetVideoCodecByName(FInstance, PChar(AName))
  );
end;

procedure TsxInstance.SetVideoCpuUsage(AUsage: Integer);
begin
  sxCheckResultForError(
    sipxConfigSetVideoCpuUsage(FInstance, AUsage)
  );
end;

procedure TsxInstance.SetVideoFormat(AFormat: TSIPX_Video_Format);
begin
  sxCheckResultForError(
    sipxConfigSetVideoFormat(FInstance, AFormat)
  );
end;

procedure TsxInstance.SetVideoFramerate(AFrameRate: Integer);
begin
  sxCheckResultForError(
    sipxConfigSetVideoFramerate(FInstance, AFrameRate)
  );
end;

procedure TsxInstance.SetVideoParameters(ABitRate, AFrameRate: Integer);
begin
  sxCheckResultForError(
    sipxConfigSetVideoParameters(FInstance, ABitRate, AFrameRate)
  );
end;

procedure TsxInstance.SetVideoPreviewDisplay(
  var ADisplay: TSIPX_Video_Display);
begin
  sxCheckResultForError(
    sipxConfigSetVideoPreviewDisplay(FInstance, ADisplay)
  );
end;

procedure TsxInstance.SetVideoQuality(AQuality: TSIPX_Video_Quality);
begin
  sxCheckResultForError(
    sipxConfigSetVideoQuality(FInstance, AQuality)
  );
end;

procedure TsxInstance.SetVoiceQualityServer(AServer: String);
begin
  sxCheckResultForError(
    sipxConfigSetVoiceQualityServer(FInstance, PChar(AServer))
  );
end;

procedure TsxInstance.SetVolume(ASpeaker: TSpeaker_Type; AVolume: Integer);
begin
  sxCheckResultForError(
    sipxAudioSetVolume(FInstance, ASpeaker, AVolume)
  );
end;

procedure TsxInstance.UnHibernate;
begin
  sxCheckResultForError(
    sipxConfigUnHibernate(FInstance)
  );
end;

procedure TsxInstance.UpdatePreviewWindow(AHWND: HWND);
begin
  sxCheckResultForError(
    sipxConfigUpdatePreviewWindow(FInstance, @AHWND)
  );
end;

{TsxLineListEnumerator }

constructor TsxLineListEnumerator.Create(ALineList: TsxLineList);
begin
  FList:=ALineList;
  FIndex:=-1;
end;

function TsxLineListEnumerator.GetCurrent: TsxLine;
begin
  Result:=FList[FIndex];
end;

function TsxLineListEnumerator.MoveNext: Boolean;
begin
  Result:=FIndex+1<Flist.Count-1;

  if Result then
    Inc(FIndex);
end;

{ TsxLineList }

function TsxLineList.Add(Item: TsxLine): Integer;
begin
  Result:=inherited Add(Item)
end;

function TsxLineList.Extract(Item: TsxLine): TsxLine;
begin
  Result:=inherited Extract(Item);
end;

function TsxLineList.First: TsxLine;
begin
  Result:=inherited First;
end;

function TsxLineList.Get(Index: Integer): TsxLine;
begin
  Result:=inherited Get(Index);
end;

function TsxLineList.GetEnumerator: TsxLineListEnumerator;
begin
  Result:=TsxLineListEnumerator.Create(Self);
end;

function TsxLineList.IndexOf(Item: TsxLine): Integer;
begin
  Result:=inherited IndexOf(Item);
end;

procedure TsxLineList.Insert(Index: Integer; Item: TsxLine);
begin
  inherited Insert(Index, Item);
end;

function TsxLineList.Last: TsxLine;
begin
  Result:=inherited Last;
end;

procedure TsxLineList.Put(Index: Integer; const Value: TsxLine);
begin
  inherited Put(Index, Value);
end;

function TsxLineList.Remove(Item: TsxLine): Integer;
begin
  Result:=inherited Remove(Item);
end;

{ TsxLine }

procedure TsxLine.AddAlias(const AAlias: String);
begin
  sxCheckResultForError(
    sipxLineAddAlias(FLine, Pchar(AAlias))
  );
end;

procedure TsxLine.AddCredential(const AUserID, APassword, ARealm: String);
begin
  sxCheckResultForError(
    sipxLineAddCredential(FLine,
                          PChar(AUserID),
                          PChar(APassword),
                          PChar(ARealm))
  );
end;

procedure TsxLine.Subscribe(ATargetUrl, AEventType,
  AAcceptType: String; AContactId: TSIPX_Contact_ID; var ASub: TSIPX_Sub);
begin
  sxCheckResultForError(
    sipxConfigSubscribe(FOwner.Instance,
                        FLine,
                        PChar(ATargetUrl),
                        PChar(AEventType),
                        Pchar(AAcceptType),
                        AContactId,
                        ASub)
  );
end;

function TsxLine.ContactID: TSIPX_Contact_ID;
begin
  sxCheckResultForError(
    sipxLineGetContactId(FLine, Result)
  );
end;

constructor TsxLine.Create(const AParent: TsxInstance;
                           LocalURI : String);
begin
  FOwner:=AParent;

  sxCheckResultForError(
    sipxLineAdd(FOwner.FInstance, PChar(LocalURI), FLine)
  );

  FOwner.Lines.Add(Self);
end;

function TsxLine.CreateCall: TsxCall;
begin
  Result:=TsxCall.Create(FOwner, Self);
end;

destructor TsxLine.Destroy;
begin
  FOwner.FLines.Remove(Self);
  sxCheckResultForError(
    sipxLineRemove(FLine)
  );
  
  inherited;
end;

procedure TsxLine.RegisterWithProxy(ARegister: Boolean);
begin
  sxCheckResultForError(
    sipxLineRegister(FLine, ARegister)
  );
end;

procedure TsxLine.UnSubscribe(ASub: TSIPX_Sub);
begin
  sxCheckResultForError(
    sipxConfigUnsubscribe(ASub)
  );
end;

function TsxLine.URI: String;
const
  MAX_URI_LEN = 255;
var
  Buffer : array[0..MAX_URI_LEN-1] of Char;
  Len : Cardinal;
begin
  sxCheckResultForError(
    sipxLineGetURI(FLine, Buffer, MAX_URI_LEN, Len)
  );

  Result:=Buffer;
end;

{ TsxExternalTransport }

constructor TsxExternalTransport.Create(AOwner: TsxInstance;
  AIsReliable: Boolean; ATransport, ALocalIP: String; ALocalPort: Integer;
  AWriteProc: TSIPX_Transport_Write_Proc; ALocalRoutingId: String;
  AUserData: Pointer);
begin
  FOwner:=AOwner;

  sxCheckResultForError(
    sipxConfigExternalTransportAdd(FOwner.Instance,
                                   FTranport,
                                   AIsReliable,
                                   Pchar(ATransport),
                                   Pchar(ALocalIP),
                                   ALocalPort,
                                   sxOnExternalTranport,
                                   Pchar(ALocalRoutingId),
                                   AUserData)
  );

  FOwner.Transports.Add(Self);
end;

destructor TsxExternalTransport.Destroy;
begin
  sipxConfigExternalTransportRemove(FTranport);
  inherited;
end;


function TsxExternalTransport.DoExternalTransport(
  const ADestinationIp: String; const ADestPort: Integer;
  const ALocalIp: String; const ALocalPort: Integer; const AData: Pointer;
  const ADataLen: Cardinal; const AUserData: Pointer): Boolean;
begin
  Result:=Assigned(FOnData) and FOnData(ADestinationIp,
                                        ADestPort,
                                        ALocalIp,
                                        ALocalPort,
                                        AData,
                                        ADataLen,
                                        AUserData);
end;

procedure TsxExternalTransport.HandleMessage(const ASourceIP: String;
  const ASourcePort: Integer; const ALocalIP: String;
  const ALocalPort: Integer; const AData: Pointer;
  const ADataLen: Cardinal);
begin
  sxCheckResultForError(
    sipxConfigExternalTransportHandleMessage(FTranport,
                                             PChar(ASourceIP),
                                             ASourcePort,
                                             Pchar(ALocalIP),
                                             ALocalPort,
                                             AData,
                                             ADataLen)
  );
end;

procedure TsxExternalTransport.SetRouteByUser(ARouteByUser: Boolean);
begin
  sxCheckResultForError(
    sipxConfigExternalTransportRouteByUser(FTranport, ARouteByUser)
  );
end;

{ TsxExternalTransportListEnumerator }

constructor TsxExternalTransportListEnumerator.Create(
  ATransportList: TsxExternalTransportList);
begin
  FList:=ATransportList;
  FIndex:=-1;
end;

function TsxExternalTransportListEnumerator.GetCurrent: TsxExternalTransport;
begin
  Result:=FList[Findex];
end;

function TsxExternalTransportListEnumerator.MoveNext: Boolean;
begin
  Result:=FIndex+1<Flist.Count-1;

  if Result then
    Inc(FIndex);
end;

{ TsxExternalTransportList }

function TsxExternalTransportList.Add(Item: TsxExternalTransport): Integer;
begin
  Result:=inherited Add(Item);
end;

function TsxExternalTransportList.Extract(
  Item: TsxExternalTransport): TsxExternalTransport;
begin
  Result:=inherited Extract(Item);
end;

function TsxExternalTransportList.First: TsxExternalTransport;
begin
  Result:=inherited First();
end;

function TsxExternalTransportList.Get(
  Index: Integer): TsxExternalTransport;
begin
  Result:=inherited Get(Index);
end;

function TsxExternalTransportList.GetEnumerator: TsxExternalTransportListEnumerator;
begin
  Result:=TsxExternalTransportListEnumerator.Create(Self);
end;

function TsxExternalTransportList.IndexOf(
  Item: TsxExternalTransport): Integer;
begin
  Result:=inherited IndexOf(Item);
end;

procedure TsxExternalTransportList.Insert(Index: Integer;
  Item: TsxExternalTransport);
begin
  inherited Insert(Index, Item);
end;

function TsxExternalTransportList.Last: TsxExternalTransport;
begin
  Result:=inherited Last();
end;

procedure TsxExternalTransportList.Put(Index: Integer;
  const Value: TsxExternalTransport);
begin
  inherited Put(Index, Value);
end;

function TsxExternalTransportList.Remove(
  Item: TsxExternalTransport): Integer;
begin
  Result:=inherited Remove(Item);
end;

{ TsxCall }

constructor TsxCall.Create(AOwner: TsxInstance; ACall: TSIPX_Call);
begin
  FOwner:=AOwner;
  FCall:=ACall;

  if Assigned(FOwner.Calls.FindCall(ACall)) then
    raise Exception.Create('Duplicate call instance!');

  FOwner.Calls.Add(Self);
end;

procedure TsxCall.Accept(AVideoDisplay: PSIPX_Video_Display;
  ASecurityAttributes: PSIPX_Security_Attributes;
  AOptions: PSIPX_Call_Options);
begin
  sxCheckResultForError(
    sipxCallAccept(FCall,
                   AVideoDisplay,
                   ASecurityAttributes,
                   AOptions)
  );
end;

procedure TsxCall.Answer(ATakeFocus: Boolean);
begin
  sxCheckResultForError(
    sipxCallAnswer(FCall, ATakeFocus)
  );
end;

procedure TsxCall.AudioPlayFileStart(AFile: String; ARepeat, ALocal,
  ARemote, AMixWithMicrophone: Boolean; AVolumeScaling: Single);
begin
  sxCheckResultForError(
    sipxCallAudioPlayFileStart(FCall,
                               Pchar(AFile),
                               ARepeat,
                               ALocal,
                               ARemote,
                               AMixWithMicrophone,
                               AVolumeScaling)
  );
end;

procedure TsxCall.AudioPlayFileStop;
begin
  sxCheckResultForError(
    sipxCallAudioPlayFileStop(FCall)
  );
end;

procedure TsxCall.AudioRecordFileStart(AFile: String);
begin
  sxCheckResultForError(
    sipxCallAudioRecordFileStart(FCall, PChar(AFile))
  );
end;

procedure TsxCall.AudioRecordFileStop;
begin
  sxCheckResultForError(
    sipxCallAudioRecordFileStop(FCall)
  );
end;

procedure TsxCall.BlindTransfer(AAddress: String);
begin
  sxCheckResultForError(
    sipxCallBlindTransfer(FCall, Pchar(AAddress))
  );
end;

procedure TsxCall.Connect(AAddress: String; AContact: TSIPX_Contact_ID;
  ADisplay: PSIPX_Video_Display; ASecurity: PSIPX_Security_Attributes;
  ATakeFocus: Boolean; AOptions: PSIPX_Call_OPTIONS; ACallId: String);
begin
  sxCheckResultForError(
    sipxCallConnect(FCall,
                    Pchar(AAddress),
                    AContact,
                    ADisplay,
                    ASecurity,
                    ATakeFocus,
                    AOptions,
                    ACallId)
  );
end;

constructor TsxCall.Create(AOwner: TsxInstance; ALine: TsxLine);
var
  call : TSIPX_Call;
begin
  FCall:=0;
  
  sxCheckResultForError(
    sipxCallCreate(AOwner.Instance, ALine.Line, call)
  );
  
  Create(AOwner, call);
end;

destructor TsxCall.Destroy;
begin
  if FCall>0 then
    sipxCallDestroy(FCall);

  inherited;
end;

procedure TsxCall.GetAudioRtpSourceIds(var ASendSSRC,
  AReceiveSSRC: Cardinal);
begin
  sxCheckResultForError(
    sipxCallGetAudioRtpSourceIds(FCall, ASendSSRC, AReceiveSSRC)
  );
end;

function TsxCall.GetConference: TSIPX_Conf;
begin
  sxCheckResultForError(
    sipxCallGetConference(FCall, Result)
  );
end;

function TsxCall.GetConnectionID: Integer;
begin
  sxCheckResultForError(
    sipxCallGetConnectionId(FCall, Result)
  );
end;

function TsxCall.GetContactID: String;
const
  MAX_ID_LEN = 128;
var
  Buffer : array[0..MAX_ID_LEN-1] of Char;
begin
  sxCheckResultForError(
    sipxCallGetContactID(FCall, Buffer, MAX_ID_LEN)
  );

  Result:=Buffer;
end;

function TsxCall.GetID: String;
const
  MAX_ID_LEN = 128;
var
  Buffer : array[0..MAX_ID_LEN-1] of Char;
begin
  sxCheckResultForError(
    sipxCallGetID(FCall, Buffer, MAX_ID_LEN)
  );

  Result:=Buffer;
end;

function TsxCall.GetLocalID: String;
const
  MAX_ID_LEN = 128;
var
  Buffer : array[0..MAX_ID_LEN-1] of Char;
begin
  sxCheckResultForError(
    sipxCallGetLocalID(FCall, Buffer, MAX_ID_LEN)
  );

  Result:=Buffer;
end;

function TsxCall.GetRemoteContact: String;
const
  MAX_CONTACT_LEN = 128;
var
  Buffer : array[0..MAX_CONTACT_LEN-1] of Char;
begin
  sxCheckResultForError(
    sipxCallGetRemoteContact(FCall, Buffer, MAX_CONTACT_LEN)
  );

  Result:=Buffer;
end;

function TsxCall.GetRemoteID: String;
const
  MAX_ID_LEN = 128;
var
  Buffer : array[0..MAX_ID_LEN-1] of Char;
begin
  sxCheckResultForError(
    sipxCallGetRemoteID(FCall, Buffer, MAX_ID_LEN)
  );

  Result:=Buffer;
end;

function TsxCall.GetRemoteUserAgent: String;
const
  MAX_UA_LEN = 128;
var
  Buffer : array[0..MAX_UA_LEN-1] of Char;
begin
  sxCheckResultForError(
    sipxCallGetRemoteUserAgent(FCall, Buffer, MAX_UA_LEN)
  );

  Result:=Buffer;
end;

function TsxCall.GetRequestURI: String;
const
  MAX_URI_LEN = 255;
var
  Buffer : array[0..MAX_URI_LEN-1] of Char;
begin
  sxCheckResultForError(
    sipxCallGetRequestURI(FCall, Buffer, MAX_URI_LEN)
  );

  Result:=Buffer;
end;

procedure TsxCall.Hold(AStopRemoteAudio: Boolean);
begin
  sxCheckResultForError(
    sipxCallHold(FCall, AStopRemoteAudio)
  );
end;

procedure TsxCall.LimitCodecPreferences(
  AAudioBandwidth: TSIPX_Audio_Bandwidth;
  AVideoBandwidth: TSIPX_Video_Bandwidth; AVideoCodecName: String);
begin
  sxCheckResultForError(
    sipxCallLimitCodecPreferences(FCall,
                                  AAudioBandwidth,
                                  AVideoBandwidth,
                                  PChar(AVideoCodecName))
  );
end;

procedure TsxCall.PlayBufferStart(ABuffer: PChar; ABufSize: Integer;
  ABufType: TSIPX_Audio_Data_Format; ARepeat, ALocal, ARemote: Boolean);
begin
  sxCheckResultForError(
    sipxCallPlayBufferStart(FCall,
                            ABuffer,
                            ABufSize,
                            Integer(ABufType),
                            ARepeat,
                            ALocal,
                            ARemote)
  );
end;

procedure TsxCall.PlayBufferStop;
begin
  sxCheckResultForError(
    sipxCallPlayBufferStop(FCall)
  );
end;

procedure TsxCall.Redirect(AURL: String);
begin
  sxCheckResultForError(
    sipxCallRedirect(FCall, PChar(AURL))
  );
end;

procedure TsxCall.Reject(AErrorCode: Integer; AErrorText: String);
begin
  sxCheckResultForError(
    sipxCallReject(FCall, AErrorCode, AErrorText)
  );
end;

procedure TsxCall.ResizeWindow(AHWND: TSIPX_Window_Handle);
begin
  sxCheckResultForError(
    sipxCallResizeWindow(FCall, AHWND)
  );
end;

procedure TsxCall.SendInfo(var AInfo: TSIPX_Info; AContentType : String;
  AContent: PChar; AContentLength : Cardinal);
begin
  sxCheckResultForError(
    sipxCallSendInfo(AInfo,
                     FCall,
                     Pchar(AContentType),
                     AContent,
                     AContentLength)
  );
end;

procedure TsxCall.StartTone(AToneID: TSIPX_Tone_ID; ALocal,
  ARemote: Boolean);
begin
  sxCheckResultForError(
    sipxCallStartTone(FCall,
                      AToneID,
                      ALocal,
                      ARemote)
  );
end;

procedure TsxCall.StopTone;
begin
  sxCheckResultForError(
    sipxCallStopTone(FCall)
  );
end;

procedure TsxCall.Subscribe(AEventType, AAcceptType: String;
  var ASub: TSIPX_Sub; ARemoteContactIsGruu: Boolean);
begin
  sxCheckResultForError(
    sipxCallSubscribe(FCall,
                      PChar(AEventType),
                      Pchar(AAcceptType),
                      ASub,
                      ARemoteContactIsGruu)
  );
end;

procedure TsxCall.Transfer(ACall: TsxCall);
begin
  sxCheckResultForError(
    sipxCallTransfer(FCall, ACall.Call)
  );
end;

procedure TsxCall.Unhold;
begin
  sxCheckResultForError(
    sipxCallUnhold(FCall)
  );
end;

procedure TsxCall.UnSubscribe(ASub: TSIPX_Sub);
begin
  sxCheckResultForError(
    sipxCallUnsubscribe(ASub)
  );
end;

procedure TsxCall.UpdateVideoWindow(AHWND: TSIPX_Window_Handle);
begin
  sxCheckResultForError(
    sipxCallUpdateVideoWindow(FCall, AHWND)
  );
end;

{ TsxCallListEnumerator }

constructor TsxCallListEnumerator.Create(ACallList: TsxCallList);
begin
  FList:=ACallList;
  FIndex:=-1;
end;

function TsxCallListEnumerator.GetCurrent: TsxCall;
begin
  Result:=FList[Findex];
end;

function TsxCallListEnumerator.MoveNext: Boolean;
begin
  Result:=FIndex+1<Flist.Count-1;

  if Result then
    Inc(FIndex);
end;

{ TsxCallList }

function TsxCallList.Add(Item: TsxCall): Integer;
begin
  Result:=inherited Add(Item);
end;

function TsxCallList.Extract(Item: TsxCall): TsxCall;
begin
  Result:=inherited Extract(Item);
end;

function TsxCallList.FindCall(ACall: TSIPX_Call): TsxCall;
var
  call : TsxCall;
begin
  Result:=nil;

  for call in Self do
  begin
    if call.Call=ACall then
    begin
      Result:=call;
      break;
    end;
  end;
end;

function TsxCallList.First: TsxCall;
begin
  Result:=inherited First;
end;

function TsxCallList.Get(Index: Integer): TsxCall;
begin
  Result:=inherited Get(Index);
end;

function TsxCallList.GetEnumerator: TsxCallListEnumerator;
begin
  Result:=TsxCallListEnumerator.Create(Self);
end;

function TsxCallList.IndexOf(Item: TsxCall): Integer;
begin
  Result:=inherited IndexOf(Item);
end;

procedure TsxCallList.Insert(Index: Integer; Item: TsxCall);
begin
  inherited Insert(Index, Item);
end;

function TsxCallList.Last: TsxCall;
begin
  Result:=inherited Last;
end;

procedure TsxCallList.Put(Index: Integer; const Value: TsxCall);
begin
  inherited Put(Index, Value);
end;

function TsxCallList.Remove(Item: TsxCall): Integer;
begin
  Result:=inherited Remove(Item);
end;

initialization
  InstanceList:=TThreadList.Create;
  LogDataList:=TThreadList.Create;
  EventsList:=TThreadList.Create;

  LogEventThread:=TsxLogEventThread.Create;
  sipxConfigSetLogLevel(LOG_LEVEL_NONE);
  sipxConfigSetLogCallback(sxOnLog);

finalization
  LogEventThread.Terminate;
  InstanceList.Free;
  LogDataList.Free;
  EventsList.Free;

end.
