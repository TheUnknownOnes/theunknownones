{####################################################
 #
 # sipXtapi Wrapper for Delphi
 #
 # sipXtapi is a procduct of www.sipfoundry.org
 #
 # based on sipxtapi version 2.9.1.0
 #
 #
 # done by TheUnknownOnes.net
 #
 ####################################################
}

unit sipXtapi;

interface

uses
  Classes, Windows;

const
  DLLFile='sipXtapi.dll';

  DEFAULT_UDP_PORT =          5060;         //Default UDP port.
 	DEFAULT_TCP_PORT =          5060;         //Default TCP port.
 	DEFAULT_TLS_PORT =          5061;         //Default TLS port.
 	DEFAULT_RTP_START_PORT =    9000;         //Starting RTP port for RTP port range.
  DEFAULT_STUN_PORT =         3478;         //Default stun server port. 
 	DEFAULT_CONNECTIONS =       32;           //Default number of max sim.
 	DEFAULT_IDENTITY =          'sipx';       //sipx<IP>:UDP_PORT used as identify if lines are not defined.
 	DEFAULT_BIND_ADDRESS =      '0.0.0.0';    //Bind to the first physical interface discovered.
 	CODEC_G711_PCMU =           '258';        //ID for PCMU vocodec.
  CODEC_G711_PCMA =           '257';        //ID for PCMA vocodec.
 	CODEC_DTMF_RFC2833 =        '128';        //ID for RFC2833 DMTF (out of band DTMF codec).
 	GAIN_MIN =                  1;            //Min acceptable gain value.
 	GAIN_MAX =                  100;          //Max acceptable gain value.
 	GAIN_DEFAULT =              70;           //Nominal gain value.
 	VOLUME_MIN =                1;            //Min acceptable volume value.
 	VOLUME_MAX =                100;          //Max acceptable volume value.
 	VOLUME_DEFAULT =            70;           //Nominal volume value.
 	MAX_AUDIO_DEVICES =         16;           //Max number of input/output audio devices.
  MAX_VIDEO_DEVICES =         8;            //Max number of video capture devices.
  MAX_VIDEO_DEVICE_LENGTH =   256; 	        //Max length of video capture device string.
 	CONF_MAX_CONNECTIONS =      32;           //Max number of conference participants.
 	SIPX_MAX_IP_ADDRESSES =     32;           //Maximum number of IP addresses on the host.
  SIPX_MAX_CALLS =            64;           //Maximum number of simultaneous calls.
  SIPX_PORT_DISABLE =         -1;           //Special value that disables the transport type
  SIPX_PORT_AUTO =            -2;           //Special value that instructs sipXtapi to automatically select an open port for signaling or audio when passed to sipXinitialize.
 	SIPXTAPI_VERSION_STRING =   'SIPxua SDK %s.%s %s (built %s)'; //Version string format string.
 	SIPXTAPI_VERSION =          '2.9.1';      //sipXtapi API version -- automatically filled in during release process
 	SIPXTAPI_BUILDNUMBER =      '0';          //Default build number -- automatically filled in during release process.
 	SIPXTAPI_BUILD_WORD =       '2,9,1,0';    //Default build word -- automatically filled in during release process.
 	SIPXTAPI_FULL_VERSION =     '2.9.1.X';    //Default full version number -- automatically filled in during release process.
 	SIPXTAPI_BUILDDATE =        '2005-03-23'; //Default build date -- automatically filled in during release process.
  SIPX_MAX_ADAPTER_NAME_LENGTH = 256;       //Max length of an adapter name.
	MAX_SRTP_KEY_LENGTH  =      31;           //srtp key length
	MAX_SMIME_KEY_LENGTH =      2048;         //s/mime key length
	MAX_PKCS12_KEY_LENGTH =     4096;         //pkcs12 key length
	MAX_PASSWORD_LENGTH =       32;           //maximum password length PKI operations
 	SIPXTAPI_CODEC_NAMELEN =    32;           //Maximum length for codec name.

  MAX_CAPTUREDEVICE_LEN =     64;

type
{$REGION 'Typedefs'}
PSIPX_Inst = type Pointer;
TSIPX_Inst = type PSIPX_Inst;

TSIPX_Call = type Cardinal;
TSIPX_Calls = array of TSIPX_Call;

TSIPX_Line = type Cardinal;
TSIPX_Lines = array of TSIPX_Line;

TSIPX_Contact_ID = type Integer;

TSIPX_Window_Handle = ^HWND;

TSIPX_Conf = type Cardinal;

TSIPX_Info = type Cardinal;

TSIPX_Pub = type Cardinal;

TSIPX_Sub = type Cardinal;

TSIPX_Transport = type Cardinal;

PSIPX_Security_Attributes = type Pointer;

TCCSRC = type Cardinal;
TCCSRCs = array of TCCSRC;
PCCSRCs = ^TCCSRCs;

TEnergyLevel = type Integer;
TEnergyLevels = array of TEnergyLevel;

TCaptureDevice = String[MAX_CAPTUREDEVICE_LEN];
TCaptureDevices = array of TCaptureDevice;
PCaptureDevices = ^TCaptureDevices;

TLocalNetworkAdress = type PChar;
TLocalNetworkAdresses = array of TLocalNetworkAdress;

TAdressAdapter = type PChar;
TAdressAdapters = array of TAdressAdapter;

TSIPXLogCallback = procedure(const szPriority : PChar;  //the priority level
                             const szSource : PChar;    //the source id of the subsystem that generated the message
                             const szMessage : PChar    //the message itself
                            ); cdecl;

TSIPX_Transport_Write_Proc = function(hTransport            : TSIPX_Transport; //Handle to the external transport object. Will match a transport handle obtained via a call to sipxConfigExternalTransportAdd
                                      const szDestinationIp : PChar; //IP address which is the destination for the write.
                                      const iDestPort       : Integer; //Port value to which the data will be sent.
                                      const szLocalIp       : PChar; //IP address which is the source address for the write.
                                      const iLocalPort      : Integer; //Port value from which the data will be sent.
                                      const pData           : Pointer; //Pointer to the data to be sent.
                                      const nData           : Cardinal; //Size of the data to be sent.
                                      const pUserData       : Pointer
                                     ) : Boolean;

TfnMicAudioHook = procedure(const nSamples : Integer;   //number of 16 bit unsigned PCM samples
                            const pSamples : PShortInt  //pointer to array of samples.
                           );

TfnSpkrAudioHook = procedure(const nSamples : Integer;   //number of 16 bit unsigned PCM samples
                             const pSamples : PShortInt  //pointer to array of samples.
                            );
{$ENDREGION}

{$REGION 'Enums'}
TSIPX_Video_Display_Type = (SIPX_WINDOW_HANDLE_TYPE,  //A handle to the window for the remote video display.
                            DIRECT_SHOW_FILTER 	      //A DirectShow render filter object for handling the remote video display.)
                           );
                             
TSIPX_Result = (SIPX_RESULT_SUCCESS, 	              //Success.
                SIPX_RESULT_FAILURE, 	              //Generic Failure.
                SIPX_RESULT_NOT_IMPLEMENTED, 	      //Method/API not implemented.
                SIPX_RESULT_OUT_OF_MEMORY, 	        //Unable to allocate enough memory to perform operation.
                SIPX_RESULT_INVALID_ARGS, 	        //Invalid arguments; bad handle, argument out of range, etc.
                SIPX_RESULT_BAD_ADDRESS, 	          //Invalid SIP address.
                SIPX_RESULT_OUT_OF_RESOURCES, 	    //Out of resources (hit some max limit).
                SIPX_RESULT_INSUFFICIENT_BUFFER,    //Buffer too short for this operation.
                SIPX_RESULT_EVAL_TIMEOUT, 	        //The evaluation version of this product has expired.
                SIPX_RESULT_BUSY, 	                //The operation failed because the system was busy.
                SIPX_RESULT_INVALID_STATE,          //The operation failed because the object was in the wrong state.
                SIPX_RESULT_MISSING_RUNTIME_FILES,  //The operation failed because required runtime dependencies are missing.
                SIPX_RESULT_TLS_DATABASE_FAILURE,   //The operation failed because the certificate database did not initialize.
                SIPX_RESULT_TLS_BAD_PASSWORD, 	    //The operation failed because the certificate database did not accept the password.
                SIPX_RESULT_TLS_TCP_IMPORT_FAILURE, //The operation failed because a TCP socket could not be imported by the SSL/TLS module.
                SIPX_RESULT_NSS_FAILURE, 	          //The operation failed due to an NSS failure.
                SIPX_RESULT_NOT_SUPPORTED, 	        //The operation is not supported in this build/configuration.
                SIPX_RESULT_NETWORK_FAILURE 	      //The network is down or failing.
               );

TSIPX_Audio_Bandwidth = (AUDIO_CODEC_BW_VARIABLE,   //ID for codecs with variable bandwidth requirements.
                         AUDIO_CODEC_BW_LOW,	      //ID for codecs with low bandwidth requirements.
                         AUDIO_CODEC_BW_NORMAL,	    //ID for codecs with normal bandwidth requirements.
                         AUDIO_CODEC_BW_HIGH,	      //ID for codecs with high bandwidth requirements.
                         AUDIO_CODEC_BW_CUSTOM,	    //Possible return value for sipxConfigGetAudioCodecPreferences.
                         AUDIO_CODEC_BW_DEFAULT 	  //This ID indicates the available list of codecs was overriden by a sipxConfigSetAudioCodecByName call. Value used to signify the default bandwidth level when calling sipxCallConnect, sipxCallAccept, or sipxConferenceAdd.
                        );

TSIPX_Video_Bandwidth = (VIDEO_CODEC_BW_VARIABLE,   //ID for codecs with variable bandwidth requirements.
                         VIDEO_CODEC_BW_LOW, 	      //ID for codecs with low bandwidth requirements.
                         VIDEO_CODEC_BW_NORMAL, 	  //ID for codecs with normal bandwidth requirements.
                         VIDEO_CODEC_BW_HIGH, 	    //ID for codecs with high bandwidth requirements.
                         VIDEO_CODEC_BW_CUSTOM, 	  //Possible return value for sipxConfigGetVideoCodecPreferences.
                         VIDEO_CODEC_BW_DEFAULT 	  //This ID indicates the available list of codecs was overriden by a sipxConfigSetVideoCodecByName call. Value used to signify the default bandwidth level when calling sipxCallLimitCodecPreferences.
                        );

TSIPX_RTP_Transport = (SIPX_RTP_TRANSPORT_UNKNOWN   = $00000000,
                       SIPX_RTP_TRANSPORT_UDP       = $00000001,
                       SIPX_RTP_TRANSPORT_TCP       = $00000002,
                       SIPX_RTP_TCP_ROLE_ACTIVE     = $00000004,
                       SIPX_RTP_TCP_ROLE_PASSIVE    = $00000008,
                       SIPX_RTP_TCP_ROLE_ACTPASS    = $00000010,
                       SIPX_RTP_TCP_ROLE_CONNECTION = $00000020
                      );

TSpeaker_Type = (Speaker, Ringer);

TSIPX_Video_Quality = (VIDEO_QUALITY_LOW     = 1,  //Low quality video.
                       VIDEO_QUALITY_NORMAL  = 2,  //Normal quality video.
                       VIDEO_QUALITY_HIGH    = 3 	//High quality video.
                      );

TSIPX_Video_Format = (VIDEO_FORMAT_CIF, 	//352x288
                      VIDEO_FORMAT_QCIF, 	//176x144
                      VIDEO_FORMAT_SQCIF, //128x96
                      VIDEO_FORMAT_QVGA 	//320x240
                     );

TSIPX_Audio_Data_Format = (RAW_PCM_16   //Signed 16 bit PCM data, mono, 8KHz, no header.
                          );

TSIPX_Tone_ID = (ID_DTMF_INVALID        = 0,        //Invalid/Uninitialized DMTF Id.
                 ID_DTMF_0              = Ord('0'), //DMTF 0.
                 ID_DTMF_1              = Ord('1'),	//DMTF 1.
                 ID_DTMF_2 	            = Ord('2'), //DMTF 2.
                 ID_DTMF_3 	            = Ord('3'), //DMTF 3.
                 ID_DTMF_4 	            = Ord('4'), //DMTF 4.
                 ID_DTMF_5 	            = Ord('5'), //DMTF 5.
                 ID_DTMF_6 	            = Ord('6'), //DMTF 6.
                 ID_DTMF_7 	            = Ord('7'), //DMTF 7.
                 ID_DTMF_8 	            = Ord('8'), //DMTF 8.
                 ID_DTMF_9 	            = Ord('9'), //DMTF 9.
                 ID_DTMF_STAR 	        = Ord('*'), //DMTF *.
                 ID_DTMF_POUND 	        = Ord('#'), //DMTF #.
                 ID_DTMF_FLASH 	        = Ord('!'), //DTMF Flash.
                 ID_TONE_DIALTONE 	    = 512,      //Dialtone (Not supported with GIPS VoiceEngine).
                 ID_TONE_BUSY, 	                    //Call-busy tone (Not supported with GIPS VoiceEngine).
                 ID_TONE_RINGBACK, 	                //Remote party is ringing feedback tone (Not supported with GIPS VoiceEngine).
                 ID_TONE_RINGTONE, 	                //Default ring/alert tone (Not supported with GIPS VoiceEngine).
                 ID_TONE_CALLFAILED, 	              //Fasy Busy / call failed tone (Not supported with GIPS VoiceEngine).
                 ID_TONE_SILENCE, 	                //Silence (Not supported with GIPS VoiceEngine).
                 ID_TONE_BACKSPACE, 	              //Backspace tone (Not supported with GIPS VoiceEngine).
                 ID_TONE_CALLWAITING, 	            //Call waiting alert tone (Not supported with GIPS VoiceEngine).
                 ID_TONE_CALLHELD, 	                //Call held feedback tone (Not supported with GIPS VoiceEngine).
                 ID_TONE_LOUD_FAST_BUSY 	          //Off hook / fast busy tone (Not supported with GIPS VoiceEngine).
                );

TSIPX_Log_Level = (LOG_LEVEL_DEBUG,   //debug-level messages
                   LOG_LEVEL_INFO, 	  //informational messages
                   LOG_LEVEL_NOTICE, 	//normal, but significant, conditions
                   LOG_LEVEL_WARNING, //warning conditions
                   LOG_LEVEL_ERR, 	  //error conditions
                   LOG_LEVEL_CRIT, 	  //critical conditions
                   LOG_LEVEL_ALERT, 	//action must be taken immediately
                   LOG_LEVEL_EMERG, 	//system is unusable
                   LOG_LEVEL_NONE 	  //disable logging
                  );

TSIPX_SRTP_Level = (SRTP_LEVEL_NONE = 0,
                    SRTP_LEVEL_ENCRYPTION,
                    SRTP_LEVEL_AUTHENTICATION,
                    SRTP_LEVEL_ENCRYPTION_AND_AUTHENTICATION
                   );

TSIPX_Contact_Type = (CONTACT_LOCAL, 	    //Local address for a particular interface.
                      CONTACT_NAT_MAPPED, //NAT mapped address (e.g. STUN)
                      CONTACT_RELAY, 	    //Relay address (e.g. TURN)
                      CONTACT_CONFIG, 	  //Manually configured address.
                      CONTACT_AUTO = -1,  //Automatic contact selection; used for API parameters.
                      CONTACT_ALL = -2
                     );

TSIPX_Transport_Type = (TRANSPORT_UDP     = 1,  //Indicator for a UDP socket type.
                        TRANSPORT_TCP 	  = 0,  //Indicator for a TCP socket type.
                        TRANSPORT_TLS     = 3,	//Indicator for a TLS socket type.
                        TRANSPORT_CUSTOM  = 4 	//Indicator for a custom external transport.
                       );

TSIPX_Keepalive_Type = (SIPX_KEEPALIVE_CRLF, 	      //Send a Carriage Return/Line Feed to other side.
                        SIPX_KEEPALIVE_STUN, 	      //Send a Stun request to the other side.
                        SIPX_KEEPALIVE_SIP_PING, 	  //Send a SIP PING method request to the other side.
                        SIPX_KEEPALIVE_SIP_OPTIONS  //Send a SIP OPTIONS method request to the other side.
                       );

TSIPX_AEC_Mode = (SIPX_AEC_DISABLED,    //Disabled AEC; do not attempt to cancel or suppress echo.
                  SIPX_AEC_SUPPRESS,    //Echo suppression; attempt to suppress echo by effectively forcing a half-duplex audio channel.
                  SIPX_AEC_CANCEL,      //If you are speaking, the speaker will be silenced to avoid echo. Echo cancellation is consider a better approach/experience, however, requires more CPU consumption. Full echo cancellation; attempt to cancel echo between the the speaker and microphone.
                  SIPX_AEC_CANCEL_AUTO  //Depending on the quality of your speaker/microphone, this may result in some suppression. For example, if either the speaker or microphone distorts the signal (making it non-linear), it is becomes increasingly difficult to cancel. This is consider a full-duplex solution. Full echo cancellation; attempt to cancel echo between the the speaker and microphone; however, automatically disable echo cancellation if it appears not needed.
                 );

TSIPX_Noise_Reduction_Mode = (SIPX_NOISE_REDUCTION_DISABLED,  //Disable NR; Do not attempt to reduce background noise.
                              SIPX_NOISE_REDUCTION_LOW,       //Enable NR with least amount of aggressiveness.
                              SIPX_NOISE_REDUCTION_MEDIUM,    //Enable NR with modest amount of aggressiveness.
                              SIPX_NOISE_REDUCTION_HIGH       //Enable NR with highest amount of aggressiveness.
                             );
{$ENDREGION}

{$REGION 'Data Structures'}
TSIPX_Video_Display = packed record
  cbSize  : Integer;
  _type   : TSIPX_Video_Display_Type;
  Handle  : TSIPX_Window_Handle;
end;
PSIPX_Video_Display = ^TSIPX_Video_Display;

TSIPX_Call_Options = packed record
  cbSize            : Integer;
  bandwidthId       : TSIPX_Audio_Bandwidth;
  sendLocation      : Boolean;
  contactId         : TSIPX_Contact_ID;
  rtpTransportFlags : TSIPX_RTP_Transport;
end;
PSIPX_Call_Options = ^TSIPX_Call_Options;

TSIPX_Contact_Address = packed record
  id                   : TSIPX_Contact_ID;
  eContactType         : TSIPX_Contact_Type;
  eTransportType       : TSIPX_Transport_Type;
  cInterface           : String[32];
  clpInterface         : String[28];
  cbSize               : Integer;
  iPort                : Integer;
  cCustomTransportName : String[32];
  cCustomRouteID       : String[64];
end;
PSIPX_Contact_Address = ^TSIPX_Contact_Address;
TSIPX_Contact_Addresses = array of TSIPX_Contact_Address;

TSIPX_Audio_Codec = packed record
  cName         : String[SIPXTAPI_CODEC_NAMELEN];
  iBandWidth    : TSIPX_Audio_Bandwidth;
  iPayloadType  : Integer;
end;
PSIPX_Audio_Codec = ^TSIPX_Audio_Codec;

TSIPX_RTCP_Stats = packed record
  cbSize          : Integer;
  fraction_lost   : Word;
  cum_lost        : Cardinal;
  ext_max         : Cardinal;
  jitter          : Cardinal;
  RTT             : Integer;
  bytesSent       : Integer;
  packetsSent     : Integer;
  bytesReceived   : Integer;
  packetsReceived : Integer;
end;
PSIPX_RTCP_Stats = ^TSIPX_RTCP_Stats;

TSIPX_Video_Codec = packed record
  cName         : String[SIPXTAPI_CODEC_NAMELEN];
  iBandWidth    : TSIPX_Video_Bandwidth;
  iPayloadType  : Integer;
end;
PSIPX_Video_Codec = ^TSIPX_Video_Codec;

TSIPX_Codec_Info = packed record
  audioCodec : TSIPX_Audio_Codec; //Audio codec.
  videoCodec : TSIPX_VIDEO_CODEC; //Video codec.
  bIsEncrypted : Boolean; //SRTP enabled.
end;
PSIPX_Codec_Info = ^TSIPX_Codec_Info;

{$ENDREGION}

{$REGION 'Initialization'}

function sipxInitialize(var hInst : TSIPX_Inst;
                        const udpPort : Integer = DEFAULT_UDP_PORT;
                        const tcpPort : Integer = DEFAULT_TCP_PORT;
                        const tlsPort : Integer = DEFAULT_TLS_PORT;
                        const rtpPortStart : Integer = DEFAULT_RTP_START_PORT;
                        const maxConnections : Integer = DEFAULT_CONNECTIONS;
                        szIdentity : String = DEFAULT_IDENTITY;
                        szBindToAddr : String = DEFAULT_BIND_ADDRESS;
                        bUseSequentialPorts : Boolean = false;
                        szTLSCertificateNickname : PChar = nil;
                        szTLSCertificatePassword : PChar = nil;
                        szDbLocation : PChar = nil
                       ) : TSIPX_Result;

function sipxReInitialize(var hInst : TSIPX_Inst;
                          const udpPort : Integer = DEFAULT_UDP_PORT;
                          const tcpPort : Integer = DEFAULT_TCP_PORT;
                          const tlsPort : Integer = DEFAULT_TLS_PORT;
                          const rtpPortStart : Integer = DEFAULT_RTP_START_PORT;
                          const maxConnections : Integer = DEFAULT_CONNECTIONS;
                          szIdentity : String = DEFAULT_IDENTITY;
                          szBindToAddr : String = DEFAULT_BIND_ADDRESS;
                          bUseSequentialPorts : Boolean = false;
                          szTLSCertificateNickname : PChar = nil;
                          szTLSCertificatePassword : PChar = nil;
                          szDbLocation : PChar = nil
                         ) : TSIPX_Result;

function sipxUnInitialize(const hInst : TSIPX_Inst;
                          bForceShutdown : Boolean = false
                         ) : TSIPX_Result; cdecl; external DLLFile;

{$ENDREGION}

{$REGION 'Call Methods'}
  function sipxCallAccept(const hCall : TSIPX_Call;
                          const pDisplay : PSIPX_Video_Display;
                          const pSecurity : PSIPX_Security_Attributes;
                          options  : PSIPX_Call_Options = nil
                         ) : TSIPX_Result; cdecl; external DLLFile;
    //Accepts an inbound call and proceed immediately to alerting.

  function sipxCallReject(const hCall : TSIPX_Call;
                          const errorCode  : Integer = 400;
                          const szErrorText : String = 'Bad Request'
                         ) : TSIPX_Result;
    //Reject an inbound call(prior to alerting the user).

  function sipxCallRedirect(const hCall : TSIPX_Call;
                            const szForwardURL : PChar
                           ) : TSIPX_Result; cdecl; external DLLFile;
    //Redirect an inbound call(prior to alerting the user).

  function sipxCallAnswer(const hCall : TSIPX_Call;
                          bTakeFocus : Boolean = true
                         ) : TSIPX_Result; cdecl; external DLLFile;
    //Answer an alerting call.

  function sipxCallCreate(const hInst : TSIPX_Inst;
                          const hLine : TSIPX_Line;
                          var phCall : TSIPX_Call
                         ) : TSIPX_Result; cdecl; external DLLFile;
    //Create a new call for the purpose of creating an outbound connection/call.

  function sipxCallConnect(const hCall : TSIPX_Call;
                           const szAddress : String;
                           contactId : TSIPX_Contact_ID = 0;
                           const pDisplay : PSIPX_Video_Display = nil;
                           const pSecurity : PSIPX_Security_Attributes = nil;
                           bTakeFocus : Boolean = true;
                           options : PSIPX_Call_OPTIONS = nil;
                           const szCallId : String = ''
                          ) : TSIPX_Result;
    //Connects an idle call to the designated target address.

  function sipxCallHold(const hCall : TSIPX_Call;
                        bStopRemoteAudio : Boolean = true
                       ) : TSIPX_Result; cdecl; external DLLFile;
    //Place the specified call on hold.

  function sipxCallUnhold(const hCall : TSIPX_Call
                         ) : TSIPX_Result; cdecl; external DLLFile;
    //Take the specified call off hold.

  function sipxCallDestroy(var hCall : TSIPX_Call
                          ) : TSIPX_Result; cdecl; external DLLFile;
    //Drop/Destroy the specified call.

  function sipxCallGetID(const hCall : TSIPX_Call;
                         szId : PChar;
                         const iMaxLength : Cardinal
                        ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the SIP call ID of the call represented by the specified call handle.
    
  function sipxCallGetLocalID(const hCall : TSIPX_Call;
                              szId : PChar;
                              const iMaxLength : Cardinal
                             ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the SIP identity of the local connection.

  function sipxCallGetRemoteID(const hCall : TSIPX_Call;
                               szId : PChar;
                               const iMaxLength : Cardinal
                              ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the SIP identity of the remote connection.

  function sipxCallGetContactID(const hCall : TSIPX_Call;
                                szId : PChar;
                                const iMaxLength : Cardinal
                               ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the SIP identity of the contact connection.

  function sipxCallGetConnectionId(const hCall : TSIPX_Call;
                                   var connectionId : Integer
                                  ) : TSIPX_Result; cdecl; external DLLFile;
    //Gets the media interface connectionid.

  function sipxCallGetConference(const hCall : TSIPX_Call;
                                 var hConf : TSIPX_Conf
                                ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the conference handle for the specified call.

  function sipxCallGetRequestURI(const hCall : TSIPX_Call;
                                szUri : PChar;
                                const iMaxLength : Cardinal
                               ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the SIP request uri.

  function sipxCallGetRemoteContact(const hCall : TSIPX_Call;
                                    szContact : PChar;
                                    const iMaxLength : Cardinal
                                   ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the SIP remote contact.

  function sipxCallGetRemoteUserAgent(const hCall : TSIPX_Call;
                                      szAgent : PChar;
                                      const iMaxLength : Cardinal
                                     ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the remote user agent of the call represented by the specified call handle.

  function sipxCallStartTone(const hCall : TSIPX_Call;
                             const toneId : TSIPX_TONE_ID;
                             const bLocal : Boolean;
                             const bRemote : Boolean
                            ) : TSIPX_Result; cdecl; external DLLFile;
    //Play a tone(DTMF; dialtone; ring back; etc) to the local and/or remote party.
    
  function sipxCallStopTone(const hCall : TSIPX_Call
                           ) : TSIPX_Result; cdecl; external DLLFile;
    //Stop playing a tone(DTMF; dialtone; ring back; etc).

  function sipxCallAudioPlayFileStart(const hCall : TSIPX_Call;
                                      const szFile : PChar;
                                      const bRepeat : Boolean;
                                      const bLocal : Boolean;
                                      const bRemote : Boolean;
                                      const bMixWithMicrophone : Boolean = false;
                                      const fVolumeScaling : Single = 1
                                     ) : TSIPX_Result; cdecl; external DLLFile;
    //Play the designated file.

  function sipxCallAudioPlayFileStop(const hCall : TSIPX_Call
                                    ) : TSIPX_Result; cdecl; external DLLFile;
    //Stop playing a file started with sipxCallPlayFileStart If a sipxCallDestroy is attempted while an audio file is playing; sipxCallDestroy will fail with a SIPX_RESULT_BUSY return code.

  function sipxCallAudioRecordFileStart(const hCall : TSIPX_Call;
                                        const szFile : PChar
                                       ) : TSIPX_Result; cdecl; external DLLFile;
    //Record a call session(including other parties if this is a multi-party call / conference) to a file.

  function sipxCallAudioRecordFileStop(const hCall : TSIPX_Call
                                      ) : TSIPX_Result; cdecl; external DLLFile;
    //Stop recording a call to file.

  function sipxCallPlayBufferStart(const hCall : TSIPX_Call;
                                   const szBuffer : PChar;
                                   const bufSize : Integer;
                                   const bufType : Integer;
                                   const bRepeat : Boolean;
                                   const bLocal : Boolean;
                                   const bRemote : Boolean
                                  ) : TSIPX_Result; cdecl; external DLLFile;
    //Play the specified audio data.

  function sipxCallPlayBufferStop(const hCall : TSIPX_Call
                                 ) : TSIPX_Result; cdecl; external DLLFile;
    //Stop playing the audio started with sipxCallPlayBufferStart If a sipxCallDestroy is attempted while an audio buffer is playing; sipxCallDestroy will fail with a SIPX_RESULT_BUSY return code.

  function sipxCallSubscribe(const hCall : TSIPX_Call;
                             const szEventType : PChar;
                             const szAcceptType : PChar;
                             var phSub : TSIPX_Sub;
                             bRemoteContactIsGruu : Boolean = false
                            ) : TSIPX_Result; cdecl; external DLLFile;
    //Subscribe for NOTIFY events which may be published by the other end-point of the call.
    
  function sipxCallUnsubscribe(const hSub : TSIPX_Sub
                              ) : TSIPX_Result; cdecl; external DLLFile;
    //Unsubscribe from previously subscribed NOTIFY events.

  function sipxCallSendInfo(var phInfo : TSIPX_Info;
                            const hCall : TSIPX_Call;
                            const szContentType : PChar;
                            const szContent : PChar;
                            const nContentLength : Cardinal
                           ) : TSIPX_Result; cdecl; external DLLFile;
    //Sends an INFO event to the specified call.

  function sipxCallBlindTransfer(const hCall : TSIPX_Call;
                                 const szAddress : PChar
                                ) : TSIPX_Result; cdecl; external DLLFile;
    //Blind transfer the specified call to another party.

  function sipxCallTransfer(const hSourceCall : TSIPX_Call;
                            const hTargetCall : TSIPX_Call
                           ) : TSIPX_Result; cdecl; external DLLFile;
    //Transfer the source call to the target call.

  function sipxCallUpdateVideoWindow(const hCall : TSIPX_Call;
                                     const hWnd : TSIPX_Window_Handle
                                    ) : TSIPX_Result; cdecl; external DLLFile;
    //Updates the Video window with a new frame buffer.

  function sipxCallResizeWindow(const hCall : TSIPX_Call;
                                const hWnd : TSIPX_Window_Handle
                               ) : TSIPX_Result; cdecl; external DLLFile;
    //Resizes the video window.

  function sipxCallGetEnergyLevels(const hCall : TSIPX_Call;
                                   iInputEnergyLevel : PInteger;
                                   iOutputEnergyLevel : PInteger;
                                   const nMaxContributors : Cardinal;
                                   CCSRCs : PCCSRCs;
                                   iEnergyLevels : TEnergyLevels;
                                   var nActualContributors : Cardinal
                                  ) : TSIPX_Result; cdecl; external DLLFile;
    //Gets energy levels for a call.

  function sipxCallGetAudioRtpSourceIds(const hCall : TSIPX_Call;
                                        var iSendSSRC : Cardinal;
                                        var iReceiveSSRC : Cardinal
                                       ) : TSIPX_Result; cdecl; external DLLFile;
    //Gets the sending and receiving Audio RTP SSRC IDs.

  function sipxCallGetAudioRtcpStats(const hCall : TSIPX_Call;
                                     var pStats : TSIPX_RTCP_Stats
                                    ) : TSIPX_Result; cdecl; external DLLFile;
    //Obtain RTCP stats for the specified call.

  function sipxCallLimitCodecPreferences(const hCall : TSIPX_Call;
                                         const audioBandwidth : TSIPX_Audio_Bandwidth;
                                         const videoBandwidth : TSIPX_Video_Bandwidth;
                                         const szVideoCodecName : PChar
                                        ) : TSIPX_Result; cdecl; external DLLFile;
    //Limits the codec preferences on a per-call basis.
{$ENDREGION}

{$REGION 'Publishing Methods'}
  function sipxPublisherCreate(const hInst : TSIPX_Inst;
                               var phPub : TSIPX_Pub;
                               const szResourceId : PChar;
                               const szEventType : PChar;
                               const szContentType : PChar;
                               const pContent : PChar;
                               const nContentLength : Cardinal
                              ) : TSIPX_Result; cdecl; external DLLFile;
    //Creates a publishing context; which perfoms the processing necessary to accept SUBSCRIBE requests; and to publish NOTIFY messages to subscribers.

  function sipxPublisherDestroy(const hPub : TSIPX_Pub;
                                const szContentType : PChar;
                                const pFinalContent : PChar;
                                const nContentLength : Cardinal
                               ) : TSIPX_Result; cdecl; external DLLFile;
    //Tears down the publishing context.

  function sipxPublisherUpdate(const hPub : TSIPX_Pub;
                               const szContentType : PChar;
                               const pContent : PChar;
                               const nContentLength : Cardinal
                              ) : TSIPX_Result; cdecl; external DLLFile;
    //Publishes an updated state to specific event via NOTIFY to its subscribers.
{$ENDREGION}

{$REGION 'Conference Methods'}
  function sipxConferenceCreate(const hInst : TSIPX_Inst;
                                var phConference : TSIPX_Conf
                               ) : TSIPX_Result; cdecl; external DLLFile;
    //Create a conference handle.

  function sipxConferenceJoin(const hConf : TSIPX_Conf;
                              const hCall : TSIPX_Call
                             ) : TSIPX_Result; cdecl; external DLLFile;
    //Join(add) an existing held call into a conference.

  function sipxConferenceSplit(const hConf : TSIPX_Conf;
                               const hCall : TSIPX_Call
                              ) : TSIPX_Result; cdecl; external DLLFile;
    //Split(remove) a held call from a conference.

  function sipxConferenceAdd(const hConf : TSIPX_Conf;
                             const hLine : TSIPX_Line;
                             const szAddress : PChar;
                             var phNewCall : TSIPX_Call;
                             contactId : TSIPX_Contact_ID = 0;
                             const pDisplay : PSIPX_Video_Display = nil;
                             const pSecurity : PSIPX_Security_Attributes = nil;
                             bTakeFocus : Boolean = true;
                             options : PSIPX_Call_Options = nil
                            ) : TSIPX_Result; cdecl; external DLLFile;
    //Add a new party to an existing conference.

  function sipxConferenceRemove(const hConf : TSIPX_Conf;
                                const hCall : TSIPX_Call
                                ) : TSIPX_Result; cdecl; external DLLFile;
    //Removes a participant from conference by hanging up on them.
    
  function sipxConferenceGetCalls(const hConf : TSIPX_Conf;
                                  calls : TSIPX_Calls;
                                  const iMax : Cardinal;
                                  var nActual : Cardinal
                                 ) : TSIPX_Result; cdecl; external DLLFile;
    //Gets all of the calls participating in a conference.

  function sipxConferenceHold(const hConf : TSIPX_Conf;
                              bBridging : Boolean = true
                             ) : TSIPX_Result; cdecl; external DLLFile;
    //Places a conference on hold.

  function sipxConferenceUnhold(const hConf : TSIPX_Conf
                               ) : TSIPX_Result; cdecl; external DLLFile;
    //Removes conference members from a held state.

  function sipxConferencePlayAudioFileStart(const hConf : TSIPX_Conf;
                                            const szFile : PChar;
                                            const bRepeat : Boolean;
                                            const bLocal : Boolean;
                                            const bRemote : Boolean;
                                            const bMixWithMicrophone : Boolean = false;
                                            const fVolumeScaling : Single = 1
                                           ) : TSIPX_Result; cdecl; external DLLFile;
    //Play the designated audio file to all conference partipants and/or the local speaker.

  function sipxConferencePlayAudioFileStop(const hConf : TSIPX_Conf
                                          ) : TSIPX_Result; cdecl; external DLLFile;

  function sipxConferenceDestroy(hConf : TSIPX_Conf
                                ) : TSIPX_Result; cdecl; external DLLFile;
    //Destroys a conference.

  function sipxConferenceGetEnergyLevels(const hConf : TSIPX_Conf;
                                         iInputEnergyLevel : PInteger;
                                         iOutputEnergyLevel : PInteger
                                        ) : TSIPX_Result; cdecl; external DLLFile;
    //Gets energy levels for a conference.

  function sipxConferenceLimitCodecPreferences(const hConf : TSIPX_Conf;
                                               const audioBandwidth : TSIPX_Audio_Bandwidth;
                                               const videoBandwidth : TSIPX_Video_Bandwidth;
                                               const szVideoCodecName : PChar
                                              ) : TSIPX_Result; cdecl; external DLLFile;
    //Limits the codec preferences on a conference.
{$ENDREGION}

{$REGION 'Audio Methods'}
  function sipxAudioSetGain(const hInst : TSIPX_Inst;
                            const iLevel : Integer
                           ) : TSIPX_Result; cdecl; external DLLFile;
    //Set the local microphone gain.

  function sipxAudioGetGain(const hInst : TSIPX_Inst;
                            var iLevel : Integer
                           ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the current microphone gain.

  function sipxAudioMute(const hInst : TSIPX_Inst;
                         const bMute : Boolean
                        ) : TSIPX_Result; cdecl; external DLLFile;
    //Mute or unmute the microphone.

  function sipxAudioIsMuted(const hInst : TSIPX_Inst;
                            var bMuted : Boolean
                           ) : TSIPX_Result; cdecl; external DLLFile;
    //Gets the mute state of the microphone.

  function sipxAudioEnableSpeaker(const hInst : TSIPX_Inst;
                                  const _type : TSpeaker_Type
                                 ) : TSIPX_Result; cdecl; external DLLFile;
    //Enables one of the speaker outputs.

  function sipxAudioGetEnabledSpeaker(const hInst : TSIPX_Inst;
                                      var _type : TSpeaker_Type
                                     ) : TSIPX_Result; cdecl; external DLLFile;
    //Gets the enabled speaker selection.

  function sipxAudioSetVolume(const hInst : TSIPX_Inst;
                              const _type : TSpeaker_Type;
                              const iLevel : Integer
                             ) : TSIPX_Result; cdecl; external DLLFile;
    //Sets the audio level for the designated speaker type.

  function sipxAudioGetVolume(const hInst : TSIPX_Inst;
                              const _type : TSpeaker_Type;
                              var iLevel : Integer
                             ) : TSIPX_Result; cdecl; external DLLFile;
    //Gets the audio level for the designated speaker type.

  function sipxAudioSetAECMode(const hInst : TSIPX_Inst;
                               const mode : TSIPX_AEC_Mode
                              ) : TSIPX_Result; cdecl; external DLLFile;
    //Enables or disables Acoustic Echo Cancellation(AEC).

  function sipxAudioGetAECMode(const hInst : TSIPX_Inst;
                               var mode : TSIPX_AEC_Mode
                              ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the mode of Acoustic Echo Cancellation(AEC).

  function sipxAudioSetAGCMode(const hInst : TSIPX_Inst;
                               const bEnable : Boolean
                              ) : TSIPX_Result; cdecl; external DLLFile;
    //Enable/Disable Automatic Gain Control(AGC).

  function sipxAudioGetAGCMode(const hInst : TSIPX_Inst;
                               var bEnabled : Boolean
                              ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the enable/disable state of Automatic Gain Control(AGC).

  function sipxAudioSetNoiseReductionMode(const hInst : TSIPX_Inst;
                                          const mode : TSIPX_Noise_Reduction_Mode
                                         ) : TSIPX_Result; cdecl; external DLLFile;
    //Set the noise reduction mode/policy for suppressing background noise.

  function sipxAudioGetNoiseReductionMode(const hInst : TSIPX_Inst;
                                          var mode : TSIPX_Noise_Reduction_Mode
                                         ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the mode/policy for Noise Reduction(NR).

  function sipxAudioGetNumInputDevices(const hInst : TSIPX_Inst;
                                       var numDevices : Cardinal
                                      ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the number of input devices available on this system.

  function sipxAudioGetInputDevice(const hInst : TSIPX_Inst;
                                   const index : Integer;
                                   var szDevice : PChar
                                  ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the name/identifier for input device at position index.
    
  function sipxAudioGetNumOutputDevices(const hInst : TSIPX_Inst;
                                        var numDevices : Cardinal
                                       ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the number of output devices available on this system.

  function sipxAudioGetOutputDevice(const hInst : TSIPX_Inst;
                                    const index : Integer;
                                    var szDevice : PChar
                                   ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the name/identifier for output device at position index.

  function sipxAudioSetCallInputDevice(const hInst : TSIPX_Inst;
                                       const szDevice : PChar
                                      ) : TSIPX_Result; cdecl; external DLLFile;
    //Set the call input device(in-call microphone).

  function sipxAudioSetRingerOutputDevice(const hInst : TSIPX_Inst;
                                          const szDevice : PChar
                                         ) : TSIPX_Result; cdecl; external DLLFile;
    //Set the call ringer/alerting device.

  function sipxAudioSetCallOutputDevice(const hInst : TSIPX_Inst;
                                        const szDevice : PChar
                                       ) : TSIPX_Result; cdecl; external DLLFile;
    //Set the call output device(in-call speaker).
{$ENDREGION}

{$REGION 'Line / Identity Methods'}
  function sipxLineAdd(const hInst : TSIPX_Inst;
                       const szLineURL : PChar;
                       var phLine : TSIPX_Line;
                       contactId : TSIPX_Contact_ID = 0
                      ) : TSIPX_Result; cdecl; external DLLFile;
    //Adds a line appearance.

  function sipxLineAddAlias(const hLine : TSIPX_Line;
                            const szLineURL : PChar
                           ) : TSIPX_Result; cdecl; external DLLFile;
    //Adds an alias for a line definition.

  function sipxLineRegister(const hLine : TSIPX_Line;
                            const bRegister : Boolean
                           ) : TSIPX_Result; cdecl; external DLLFile;
    //Registers a line with the proxy server.

  function sipxLineRemove(hLine : TSIPX_Line
                         ) : TSIPX_Result; cdecl; external DLLFile;
    //Remove the designated line appearence.

  function sipxLineAddCredential(const hLine : TSIPX_Line;
                                 const szUserID : PChar;
                                 const szPasswd : PChar;
                                 const szRealm : PChar
                                ) : TSIPX_Result; cdecl; external DLLFile;
    //Adds authentication credentials to the designated line appearance.

  function sipxLineGet(const hInst : TSIPX_Inst;
                       lines : TSIPX_Lines;
                       const max : Cardinal;
                       var actual : Cardinal
                      ) : TSIPX_Result; cdecl; external DLLFile;
    //Gets the active list of line identities.

  function sipxLineGetURI(const hLine : TSIPX_Line;
                          szBuffer : PChar;
                          const nBuffer : Cardinal;
                          var nActual : Cardinal
                         ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the Line URI for the designated line handle.

  function sipxLineGetContactId(const hLine : TSIPX_Line;
                                var contactId : TSIPX_Contact_ID
                               ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the contact ID for the designated line handle.

  function sipxLineFindByURI(const hInst : TSIPX_Inst;
                             const szURI : PChar;
                             var hLine : TSIPX_Line
                            ) : TSIPX_Result; cdecl; external DLLFile;
    //Find a line definition given a URI.
{$ENDREGION}

{$REGION 'Configuration Methods'}
  function sipxConfigSetLogLevel(logLevel : TSIPX_Log_Level
                                ) : TSIPX_Result; cdecl; external DLLFile;
    //The sipxConfigEnableLog method enables logging for the sipXtapi API; media processing; call processing; SIP stack; and OS abstraction layer.

  function sipxConfigSetLogFile(const szFilename : PChar
                               ) : TSIPX_Result; cdecl; external DLLFile;
    //The sipxConfigSetlogFile method sets the filename of the log file and directs output to that file.

  function sipxConfigSetLogCallback(pCallback : TSIPXLogCallback
                                   ) : TSIPX_Result; cdecl; external DLLFile;
    //Set a callback function to collect logging information.

  function sipxConfigEnableGIPSTracing(hInst : TSIPX_Inst;
                                       bEnable : Boolean
                                      ) : TSIPX_Result; cdecl; external DLLFile;
    //Enables GIPS tracing in sipXtapi(if bundled with sipXtapi).

  function sipxConfigSetMicAudioHook(hookProc : TfnMicAudioHook
                                    ) : TSIPX_Result; cdecl; external DLLFile;
    //Designate a callback routine as a microphone replacement or supplement.

  function sipxConfigSetSpkrAudioHook(hookProc : TfnMicAudioHook
                                     ) : TSIPX_Result; cdecl; external DLLFile;
    //Designate a callback routine for post-mixing audio data(e.g.

  function sipxConfigSetUserAgentName(const hInst : TSIPX_Inst;
                                      const szName : PChar;
                                      const bIncludePlatformName : Boolean = true
                                     ) : TSIPX_Result; cdecl; external DLLFile;
    //Sets the User-Agent name to be used with outgoing SIP messages.

  function sipxConfigSetOutboundProxy(const hInst : TSIPX_Inst;
                                      const szProxy : PChar
                                     ) : TSIPX_Result; cdecl; external DLLFile;
    //Defines the SIP proxy used for outbound requests.

  function sipxConfigSetDnsSrvTimeouts(const initialTimeoutInSecs : Integer;
                                       const retries : Integer
                                      ) : TSIPX_Result; cdecl; external DLLFile;
    //Modifies the timeout values used for DNS SRV lookups.

  function sipxConfigSetRegisterResponseWaitSeconds(const hInst : TSIPX_Inst;
                                                    const seconds : Integer
                                                   ) : TSIPX_Result; cdecl; external DLLFile;
    //Specifies the time to wait for a REGISTER response before sending a LINESTATE_REGISTER_FAILED(or LINESTATE_UNREGISTER_FAILED) message.
  function sipxConfigSetDnsSrvFailoverTimeout(const hInst : TSIPX_Inst;
                                              const failoverTimeoutInSecs : Integer
                                             ) : TSIPX_Result; cdecl; external DLLFile;
    //Specifies the time to wait before trying the next DNS SRV record.

  function sipxConfigEnableRport(const hInst : TSIPX_Inst;
                                 const bEnable : Boolean
                                ) : TSIPX_Result; cdecl; external DLLFile;
    //Enable or disable the use of "rport".

  function sipxConfigSetRegisterExpiration(const hInst : TSIPX_Inst;
                                           const nRegisterExpirationSecs : Integer
                                          ) : TSIPX_Result; cdecl; external DLLFile;
    //Specifies the expiration period for registration.

  function sipxConfigSetSubscribeExpiration(const hInst : TSIPX_Inst;
                                            const nSubscribeExpirationSecs : Integer
                                           ) : TSIPX_Result; cdecl; external DLLFile;
    //Specifies the expiration period for subscription.

  function sipxConfigEnableStun(const hInst : TSIPX_Inst;
                                const szServer : PChar;
                                iServerPort : Integer;
                                iKeepAliveInSecs : Integer
                               ) : TSIPX_Result; cdecl; external DLLFile;
    //Enables STUN(Simple Traversal of UDP through NAT) support for both UDP SIP signaling and UDP audio/video(RTP).

  function sipxConfigDisableStun(const hInst : TSIPX_Inst
                                ) : TSIPX_Result; cdecl; external DLLFile;
    //Disable the use of STUN.
    
  function sipxConfigEnableTurn(const hInst : TSIPX_Inst;
                                const szServer : PChar;
                                const iServerPort : Integer;
                                const szUsername : PChar;
                                const szPassword : PChar;
                                const iKeepAliveInSecs : Integer
                               ) : TSIPX_Result; cdecl; external DLLFile;
    //Enable TURN for support for UDP audio/video(RTP).

  function sipxConfigDisableTurn(const hInst : TSIPX_Inst
                                ) : TSIPX_Result; cdecl; external DLLFile;
    //Disable the use of TURN.

  function sipxConfigEnableIce(const hInst : TSIPX_Inst
                              ) : TSIPX_Result; cdecl; external DLLFile;
    //Enables an ICE-like mechanism for determining connecticity of remote parties dynamically.

  function sipxConfigDisableIce(const hInst : TSIPX_Inst
                               ) : TSIPX_Result; cdecl; external DLLFile;
    //Disable the use of ICE.

  function sipxConfigKeepAliveAdd(const hInst : TSIPX_Inst;
                                  contactId : TSIPX_Contact_ID;
                                  _type : TSIPX_Keepalive_Type;
                                  const remoteIp : PChar;
                                  remotePort : Integer;
                                  keepAliveSecs : Integer
                                 ) : TSIPX_Result; cdecl; external DLLFile;
    //Add a signaling keep alive to a remote ip address.
    
  function sipxConfigKeepAliveRemove(const hInst : TSIPX_Inst;
                                     contactId : TSIPX_Contact_ID;
                                     _type : TSIPX_Keepalive_Type;
                                     const remoteIp : PChar;
                                     remotePort : Integer
                                    ) : TSIPX_Result; cdecl; external DLLFile;
    //Remove a signaling keepalive.

  function sipxConfigEnableOutOfBandDTMF(const hInst : TSIPX_Inst;
                                         const bEnable : Boolean
                                        ) : TSIPX_Result; cdecl; external DLLFile;
    //Enable/disable sending of out-of-band DTMF tones.
    
  function sipxConfigEnableInBandDTMF(const hInst : TSIPX_Inst;
                                      const bEnable : Boolean
                                     ) : TSIPX_Result; cdecl; external DLLFile;
    //Enable/disable sending of in-band DTMF tones.

  function sipxConfigEnableRTCP(const hInst : TSIPX_Inst;
                                const bEnable : Boolean
                               ) : TSIPX_Result; cdecl; external DLLFile;
    //Enable or disable sending RTCP reports.

  function sipxConfigEnableDnsSrv(const bEnable : Boolean
                                 ) : TSIPX_Result; cdecl; external DLLFile;
    //Enables/disables sending of DNS SRV request for all sipXtapi instances.

  function sipxConfigIsOutOfBandDTMFEnabled(const hInst : TSIPX_Inst;
                                            var bEnable : Boolean
                                           ) : TSIPX_Result; cdecl; external DLLFile;
    //Determines if sending of out-of-band DTMF tones is enabled or disabled.

  function sipxConfigIsInBandDTMFEnabled(const hInst : TSIPX_Inst;
                                         var bEnable : Boolean
                                        ) : TSIPX_Result; cdecl; external DLLFile;
    //Determines if sending of in-band DTMF tones is enabled or disabled.

  function sipxConfigGetVersion(szVersion : PChar;
                                const nBuffer : Cardinal
                               ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the sipXtapi API version string.

  function sipxConfigGetLocalSipUdpPort(hInst : TSIPX_Inst;
                                        var pPort : Integer
                                       ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the local UDP port for SIP signaling.

  function sipxConfigGetLocalSipTcpPort(hInst : TSIPX_Inst;
                                        var pPort : Integer
                                       ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the local TCP port for SIP signaling.

  function sipxConfigGetLocalSipTlsPort(hInst : TSIPX_Inst;
                                        var pPort : Integer
                                       ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the local TLS port for SIP signaling.

  function sipxConfigSetAudioCodecPreferences(const hInst : TSIPX_Inst;
                                              bandWidth : TSIPX_Audio_Bandwidth
                                             ) : TSIPX_Result; cdecl; external DLLFile;
    //Set the preferred bandwidth requirement for codec selection.

  function sipxConfigSetAudioCodecByName(const hInst : TSIPX_Inst;
                                         const szCodecName : PChar
                                        ) : TSIPX_Result; cdecl; external DLLFile;
    //Set the codec by name.

  function sipxConfigGetAudioCodecPreferences(const hInst : TSIPX_Inst;
                                              var pBandWidth : TSIPX_Audio_Bandwidth
                                             ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the current codec preference.
    
  function sipxConfigGetNumAudioCodecs(const hInst : TSIPX_Inst;
                                       pNumCodecs : PInteger
                                      ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the number of audio codecs.

  function sipxConfigGetAudioCodec(const hInst : TSIPX_Inst;
                                   const index : Integer;
                                   Codec : PSIPX_Audio_Codec
                                  ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the audio codec at a certain index in the list of codecs.

  function sipxConfigSetVideoBandwidth(const hInst : TSIPX_Inst;
                                       bandWidth : TSIPX_Video_Bandwidth
                                      ) : TSIPX_Result; cdecl; external DLLFile;
    //Set the bandwidth parameters for video codecs.Depending on the bandwidth parameter that is passed in the settings will be set to:.
    
  function sipxConfigGetVideoCaptureDevices(const hInst : TSIPX_Inst;
                                            arrSzCaptureDevices : PCaptureDevices;
                                            nDeviceStringLength : Integer;
                                            nArrayLength : Integer
                                           ) : TSIPX_Result; cdecl; external DLLFile;
    //Gets the list of video capture devices.
    
  function sipxConfigGetVideoCaptureDevice(const hInst : TSIPX_Inst;
                                           szCaptureDevice : PChar;
                                           nLength : Integer
                                          ) : TSIPX_Result; cdecl; external DLLFile;
    //Gets the current video capture device.

  function sipxConfigSetVideoCaptureDevice(const hInst : TSIPX_Inst;
                                           const szCaptureDevice : PChar
                                          ) : TSIPX_Result; cdecl; external DLLFile;
    //Sets the video capture device.

  function sipxConfigSetVideoCodecByName(const hInst : TSIPX_Inst;
                                         const szCodecName : PChar
                                        ) : TSIPX_Result; cdecl; external DLLFile;
    //Set the codec by name.

  function sipxConfigResetVideoCodecs(const hInst : TSIPX_Inst
                                     ) : TSIPX_Result; cdecl; external DLLFile;
    //Reset the codec list if it was modified by sipxConfigSetVideoCodecByName.

  function sipxConfigGetVideoCodecPreferences(const hInst : TSIPX_Inst;
                                              var pBandWidth : TSIPX_Video_Bandwidth
                                             ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the current codec preference.

  function sipxConfigGetNumVideoCodecs(const hInst : TSIPX_Inst;
                                       var pNumCodecs : Integer
                                      ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the number of video codecs.

  function sipxConfigSetVideoFormat(const hInst : TSIPX_Inst;
                                    videoFormat : TSIPX_Video_Format
                                   ) : TSIPX_Result; cdecl; external DLLFile;
    //Set the supported video format This method will limit the supported video format to either VIDEO_FORMAT_CIF(352x288) : TSIPX_Result; VIDEO_FORMAT_QCIF(176x144) : TSIPX_Result; VIDEO_FORMAT_SQCIF(128x92) : TSIPX_Result; or VIDEO_FORMAT_QVGA(320x240).

  function sipxConfigGetVideoCodec(const hInst : TSIPX_Inst;
                                   const index : Integer;
                                   var pCodec : TSIPX_Video_Codec
                                  ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the video codec at a certain index in the list of codecs.

  function sipxConfigGetLocalContacts(const hInst : TSIPX_Inst;
                                      addresses : TSIPX_Contact_Addresses;
                                      nMaxAddresses : Cardinal;
                                      var nActualAddresses : Cardinal
                                     ) : TSIPX_Result; cdecl; external DLLFile;
    //Get the local contact address available for outbound/inbound signaling and audio.

  function sipxConfigGetLocalFeedbackAddress(const hInst : TSIPX_Inst;
                                             const szRemoteIp : PChar;
                                             const iRemotePort : Integer;
                                             szContactIp : PChar;
                                             nContactIpLength : Cardinal;
                                             var iContactPort : Integer;
                                             iTimeoutMs : Integer
                                            ) : TSIPX_Result; cdecl; external DLLFile;
    //Get our local ip/port combination for the designated remote ip/port.
    
  function sipxConfigGetAllLocalNetworkIps(const arrAddresses : TLocalNetworkAdresses;
                                           const arrAddressAdapter : TAdressAdapters;
                                           var numAddresses : Integer
                                          ) : TSIPX_Result; cdecl; external DLLFile; deprecated;
    //Populates an array of IP Addresses in char* form.
    
  function sipxConfigSetSecurityParameters(const hInst : TSIPX_Inst;
                                           const szDbLocation : PChar;
                                           const szMyCertNickname : PChar;
                                           const szDbPassword : PChar
                                          ) : TSIPX_Result; cdecl; external DLLFile;
    //Set security parameters for an instance of sipXtapi.
    
  function sipxConfigEnableSipShortNames(const hInst : TSIPX_Inst;
                                         const bEnabled : Boolean
                                        ) : TSIPX_Result; cdecl; external DLLFile;
    //Enables/Disables use of short field names in sip messages.

  function sipxConfigEnableSipDateHeader(const hInst : TSIPX_Inst;
                                         const bEnabled : Boolean
                                        ) : TSIPX_Result; cdecl; external DLLFile;
    //Enables/Disables use of date header in sip messages.
    
  function sipxConfigEnableSipAllowHeader(const hInst : TSIPX_Inst;
                                          const bEnabled : Boolean
                                         ) : TSIPX_Result; cdecl; external DLLFile;
    //Enables/Disables use of allow header in sip messages.

  function sipxConfigSetSipAcceptLanguage(const hInst : TSIPX_Inst;
                                          const szLanguage : PChar
                                         ) : TSIPX_Result; cdecl; external DLLFile;
    //Sets the Accept Language used in sip messages.

  function sipxConfigSetLocationHeader(const hInst : TSIPX_Inst;
                                       const szHeader : PChar
                                      ) : TSIPX_Result; cdecl; external DLLFile;
    //Sets the location header for SIP messages.

  function sipxConfigSetConnectionIdleTimeout(const hInst : TSIPX_Inst;
                                              const idleTimeout : Integer
                                             ) : TSIPX_Result; cdecl; external DLLFile;
    //Set the connection idle timeout.
    
  function sipxConfigPrepareToHibernate(const hInst : TSIPX_Inst
                                       ) : TSIPX_Result; cdecl; external DLLFile;
    //Call this function to prepare a sipXtapi instance for a system hibernation.

  function sipxConfigUnHibernate(const hInst : TSIPX_Inst
                                ) : TSIPX_Result; cdecl; external DLLFile;
    //Call this function upon returning from a system hibernation.

  function sipxConfigEnableRtpOverTcp(const hInst : TSIPX_Inst;
                                      bEnable : Boolean
                                     ) : TSIPX_Result; cdecl; external DLLFile;
    //Enables RTP streaming over TCP.
    
  function sipxConfigSetVideoPreviewDisplay(const hInst : TSIPX_Inst;
                                            var pDisplay : TSIPX_Video_Display
                                           ) : TSIPX_Result; cdecl; external DLLFile;
    //Sets the display object for the "video preview".

  function sipxConfigUpdatePreviewWindow(const hInst : TSIPX_Inst;
                                         const hWnd : TSIPX_Window_Handle
                                        ) : TSIPX_Result; cdecl; external DLLFile;
    //Updates the Preview window with a new frame buffer.

  function sipxConfigSetVideoQuality(const hInst : TSIPX_Inst;
                                     const quality : TSIPX_Video_Quality
                                    ) : TSIPX_Result; cdecl; external DLLFile;
    //Sets the video quality.

  function sipxConfigSetVideoParameters(const hInst : TSIPX_Inst;
                                        const bitRate : Integer;
                                        const frameRate : Integer
                                       ) : TSIPX_Result; cdecl; external DLLFile;
    //Sets the bit rate and frame rate parameters for video.

  function sipxConfigSetVideoBitrate(const hInst : TSIPX_Inst;
                                     const bitRate : Integer
                                    ) : TSIPX_Result; cdecl; external DLLFile;
    //Sets the video bitrate.

  function sipxConfigSetVideoFramerate(const hInst : TSIPX_Inst;
                                       const frameRate : Integer
                                      ) : TSIPX_Result; cdecl; external DLLFile;
    //Sets the video framerate.

  function sipxConfigSetVideoCpuUsage(const hInst : TSIPX_Inst;
                                      const cpuUsage : Integer
                                     ) : TSIPX_Result; cdecl; external DLLFile;
    //Set the cpu usage.

  function sipxConfigSubscribe(const hInst : TSIPX_Inst;
                               const hLine : TSIPX_Line;
                               const szTargetUrl : PChar;
                               const szEventType : PChar;
                               const szAcceptType : PChar;
                               const contactId : TSIPX_Contact_ID;
                               var phSub : TSIPX_Sub
                              ) : TSIPX_Result; cdecl; external DLLFile;
    //Subscribe for NOTIFY events which may be published by another end-point or server.

  function sipxConfigUnsubscribe(const hSub : TSIPX_Sub
                                ) : TSIPX_Result; cdecl; external DLLFile;
    //Unsubscribe from previously subscribed NOTIFY events.

  function sipxConfigExternalTransportAdd(const hInst : TSIPX_Inst;
                                          var hTransport : TSIPX_Transport;
                                          const bIsReliable : Boolean;
                                          const szTransport : PChar;
                                          const szLocalIP : PChar;
                                          const iLocalPort : Integer;
                                          writeProc : TSIPX_Transport_Write_Proc;
                                          const szLocalRoutingId : PChar;
                                          const pUserData  : Pointer = nil
                                         ) : TSIPX_Result; cdecl; external DLLFile;
    //Associates an external transport mechanism for SIP signalling with the given instance.

  function sipxConfigExternalTransportRemove(const hTransport : TSIPX_Transport
                                            ) : TSIPX_Result; cdecl; external DLLFile;
    //Removes an external transport mechanism from the given instance.
    
  function sipxConfigExternalTransportRouteByUser(const hTransport : TSIPX_Transport;
                                                  bRouteByUser : Boolean
                                                 ) : TSIPX_Result; cdecl; external DLLFile;
    //The external transport mechanism can be configured to route by user or by destination ip/port.

  function sipxConfigExternalTransportHandleMessage(const hTransport : TSIPX_Transport;
                                                    const szSourceIP : PChar;
                                                    const iSourcePort : Integer;
                                                    const szLocalIP : PChar;
                                                    const iLocalPort : Integer;
                                                    const pData : Pointer;
                                                    const nData : Cardinal
                                                   ) : TSIPX_Result; cdecl; external DLLFile;
    //Called by the application when it receives a complete SIP message via it's external transport mechanism and want to pass it along to sipXtapi.

  function sipxConfigSetVoiceQualityServer(const hInst : TSIPX_Inst;
                                           const szServer : PChar
                                          ) : TSIPX_Result; cdecl; external DLLFile;
    //Sets the SIP target URL for voice quality reports.
{$ENDREGION}

{$REGION 'Utility Functions'}
  function sipxUtilUrlParse(const szUrl : PChar; szUsername : PChar; szHostname : PChar; iPort : PInteger) : TSIPX_Result; cdecl; external DLLFile;
    //Simple utility function to parse the username; host; and port from a URL.
  function sipxUtilUrlGetDisplayName(const szUrl : PChar; szDisplayName : PChar; nDisplayName : Cardinal) : TSIPX_Result; cdecl; external DLLFile;
    //Simple utility function to parse the display name from a SIP URL.
  function sipxUtilUrlUpdate(szUrl : PChar; var nUrl : Cardinal; const szNewUsername : PChar; const szNewHostname : PChar; const iNewPort : Integer) : TSIPX_Result; cdecl; external DLLFile;
    //Simple utility function to update a URL.
  function sipxUtilUrlGetUrlParam(const szUrl : PChar; const szParamName : PChar; nParamIndex : Cardinal; szParamValue : PChar; nParamValue : Cardinal) : TSIPX_Result; cdecl; external DLLFile;
    //Get the Nth named url parameter from the designated url.
{$ENDREGION}

implementation

function _sipxInitialize(phInst : Pointer;
                         const udpPort : Integer;
                         const tcpPort : Integer;
                         const tlsPort : Integer;
                         const rtpPortStart : Integer;
                         const maxConnections : Integer;
                         szIdentity : PChar ;
                         szBindToAddr : PChar;
                         bUseSequentialPorts : Boolean;
                         szTLSCertificateNickname : PChar;
                         szTLSCertificatePassword : PChar;
                         szDbLocation : PChar
                        ) : TSIPX_Result; cdecl; external DLLFile name 'sipxInitialize';

function sipxInitialize(var hInst : TSIPX_Inst;
                        const udpPort : Integer = DEFAULT_UDP_PORT;
                        const tcpPort : Integer = DEFAULT_TCP_PORT;
                        const tlsPort : Integer = DEFAULT_TLS_PORT;
                        const rtpPortStart : Integer = DEFAULT_RTP_START_PORT;
                        const maxConnections : Integer = DEFAULT_CONNECTIONS;
                        szIdentity : String = DEFAULT_IDENTITY;
                        szBindToAddr : String = DEFAULT_BIND_ADDRESS;
                        bUseSequentialPorts : Boolean = false;
                        szTLSCertificateNickname : PChar = nil;
                        szTLSCertificatePassword : PChar = nil;
                        szDbLocation : PChar = nil
                       ) : TSIPX_Result;
begin
  Result:=_sipxInitialize(@hInst,
                          udpPort,
                          tcpPort,
                          tlsPort,
                          rtpPortStart,
                          maxConnections,
                          PChar(szIdentity),
                          Pchar(szBindToAddr),
                          bUseSequentialPorts,
                          szTLSCertificateNickname,
                          szTLSCertificatePassword,
                          szDbLocation);
end;

function _sipxReInitialize(phInst : Pointer;
                         const udpPort : Integer;
                         const tcpPort : Integer;
                         const tlsPort : Integer;
                         const rtpPortStart : Integer;
                         const maxConnections : Integer;
                         szIdentity : PChar ;
                         szBindToAddr : PChar;
                         bUseSequentialPorts : Boolean;
                         szTLSCertificateNickname : PChar;
                         szTLSCertificatePassword : PChar;
                         szDbLocation : PChar
                        ) : TSIPX_Result; cdecl; external DLLFile name 'sipxReInitialize';

function sipxReInitialize(var hInst : TSIPX_Inst;
                            const udpPort : Integer = DEFAULT_UDP_PORT;
                            const tcpPort : Integer = DEFAULT_TCP_PORT;
                            const tlsPort : Integer = DEFAULT_TLS_PORT;
                            const rtpPortStart : Integer = DEFAULT_RTP_START_PORT;
                            const maxConnections : Integer = DEFAULT_CONNECTIONS;
                            szIdentity : String = DEFAULT_IDENTITY;
                            szBindToAddr : String = DEFAULT_BIND_ADDRESS;
                            bUseSequentialPorts : Boolean = false;
                            szTLSCertificateNickname : PChar = nil;
                            szTLSCertificatePassword : PChar = nil;
                            szDbLocation : PChar = nil
                           ) : TSIPX_Result;
begin
  Result:=_sipxReInitialize(@hInst,
                            udpPort,
                            tcpPort,
                            tlsPort,
                            rtpPortStart,
                            maxConnections,
                            PChar(szIdentity),
                            Pchar(szBindToAddr),
                            bUseSequentialPorts,
                            szTLSCertificateNickname,
                            szTLSCertificatePassword,
                            szDbLocation);
end;

function _sipxCallReject(const hCall : TSIPX_Call; const errorCode : Integer; const szErrorText : PChar) : TSIPX_Result; cdecl; external DLLFile name 'sipxCallReject';

function sipxCallReject(const hCall : TSIPX_Call; const errorCode  : Integer = 400; const szErrorText : String = 'Bad Request') : TSIPX_Result;
begin
  Result:=_sipxCallReject(hCall, errorCode, Pchar(szErrorText));
end;

function _sipxCallConnect(const hCall : TSIPX_Call; const szAddress : PChar; contactId : TSIPX_Contact_ID; const pDisplay : PSIPX_Video_Display; const pSecurity : PSIPX_Security_Attributes; bTakeFocus : Boolean; options : PSIPX_Call_OPTIONS; const szCallId : PChar) : TSIPX_Result; cdecl; external DLLFile name 'sipxCallConnect';

function sipxCallConnect(const hCall : TSIPX_Call; const szAddress : String; contactId : TSIPX_Contact_ID = 0; const pDisplay : PSIPX_Video_Display = nil; const pSecurity : PSIPX_Security_Attributes = nil; bTakeFocus : Boolean = true; options : PSIPX_Call_OPTIONS = nil; const szCallId : String = '') : TSIPX_Result;
begin
  Result:=_sipxCallConnect(hCall, Pchar(szAddress), contactId, pDisplay, pSecurity, bTakeFocus, options, PChar(szCallId));
end;

end.
