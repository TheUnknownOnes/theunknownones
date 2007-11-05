//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit NPipe_Types;

{$R 'resources.res'}

interface

uses
  SysUtils, Classes, Windows, Types;

type
  //Types
  TNPBuffer = array[0..32767] of char;
  TNPWorkMode = (npwSending, npwReceiving);

  //Events
  TNP_OnError = procedure ( Sender      : TObject;
                            AException  : Exception) of object;
  TNP_OnIncomingData = procedure (Sender    : TObject;
                                  Data      : TMemoryStream;
                                  var Reply : TMemoryStream) of object;
  TNP_OnServerReply = procedure ( Sender  : TObject;
                                  Reply   : TMemoryStream) of object;
  TNP_OnProgress = procedure (Sender        : TObject;
                              bytesCurrent  : Int64;
                              Data          : TMemoryStream) of object;
  TNP_OnStartWork = procedure ( Sender      :TObject;
                                bytesTotal  : Int64;
                                WorkMode    : TNPWorkMode) of object;
  TNP_OnEndWork = procedure (Sender : TObject) of object;
  TNP_OnConnected = procedure (Sender : TObject) of object;
  TNP_OnDisconnected = procedure (Sender : TObject) of object;


  //Exceptions
  ENP_CannotChange = class(Exception);
  ENP_InitSecurityDescriptor = class(Exception);
  ENP_SetSecurityDescriptor = class(Exception);

  ENP_ComponentBlocked = class(Exception);
  ENP_SendTimeout = class(Exception);
  ENP_OpenPipe = class(Exception);
  ENP_SetPipeMode = class(Exception);

const
  //Error-Messages:
  ENP_MSG_CannotChange='%s cannot be changed on running server!';
  ENP_MSG_InitSecurityDescriptor='Initialization of Security Descriptor Failed';
  ENP_MSG_SetSecurityDescriptor='Setting of Security Descriptor Failed';

  ENP_MSG_ComponentBlocked='Component currently blocked by another action';
  ENP_MSG_SendTimeout='Timeout while sending data.';
  ENP_MSG_OpenPipe='Can not open pipe.';
  ENP_MSG_SetPipeMode='Can not set pipe mode.';

  //Default-Values
  NP_Default_PipeName='MyPipe';
  NP_Default_PipeServer='.';
  NP_Default_ServerTimeout=10000;
  NP_Default_ClientSendTimeout=500;
  NP_Default_RawMode=false;

  //Consts
  MAX_INT64=$7FFFFFFFFFFFFFFF;

implementation

end.
