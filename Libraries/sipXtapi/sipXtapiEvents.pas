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

unit sipXtapiEvents;

interface

uses
  sipXtapi;

type
  {$REGION 'Enumerations'}
  TSIPX_Event_Category = (EVENT_CATEGORY_CALLSTATE, 	//CALLSTATE events signify a change in state of a call. States range from the notification of a new call to ringing to connection established to changes in audio state (starting sending, stop sending) to termination of a call.
                          EVENT_CATEGORY_LINESTATE, 	//LINESTATE events indicate changes in the status of a line appearance. Lines identify inbound and outbound identities and can be either provisioned (hardcoded) or configured to automatically register with a registrar. Lines also encapsulate the authentication criteria needed for dynamic registrations.
                          EVENT_CATEGORY_INFO_STATUS, //INFO_STATUS events are sent when the application requests sipXtapi to send an INFO message to another user agent.The status event includes the response for the INFO method. Application developers should look at this event to determine the outcome of the INFO message.
                          EVENT_CATEGORY_INFO, 	      //INFO events are sent to the application whenever an INFO message is received by the sipXtapi user agent. INFO messages are sent to a specific call. sipXtapi will automatically acknowledges the INFO message at the protocol layer.
                          EVENT_CATEGORY_SUB_STATUS,  //SUB_STATUS events are sent to the application layer for information on the subscription state (e.g. OK, Expired).
                          EVENT_CATEGORY_NOTIFY, 	    //NOTIFY evens are send to the application layer after a remote publisher has sent data to the application. The application layer can retrieve the data from this event.
                          EVENT_CATEGORY_CONFIG, 	    //CONFIG events signify changes in configuration.  For example, when requesting STUN support, a notification is sent with the STUN outcome (either SUCCESS or FAILURE)
                          EVENT_CATEGORY_SECURITY, 	  //SECURITY events signify occurences in call security processing. These events are only sent when using S/MIME or TLS.
                          EVENT_CATEGORY_MEDIA, 	    //MEDIA events signify changes in the audio state for sipXtapi or a particular call.
                          EVENT_CATEGORY_KEEPALIVE    //KEEPALIVE events signal when a keepalive is started/stopped/fails.)
                         );

  TSIPX_Callstate_Event = (CALLSTATE_UNKNOWN          =  0, 	  //An UNKNOWN event is generated when the state for a call is no longer known. This is generally an error condition; see the minor event for specific causes.
                           CALLSTATE_NEWCALL          =  1000, 	//The NEWCALL event indicates that a new call has been created automatically by the sipXtapi. This event is most frequently generated in response to an inbound call request.
                           CALLSTATE_DIALTONE         =  2000, 	//The DIALTONE event indicates that a new call has been created for the purpose of placing an outbound call. The application layer should determine if it needs to simulate dial tone for the end user.
                           CALLSTATE_REMOTE_OFFERING  =  2500,  //The REMOTE_OFFERING event indicates that a call setup invitation has been sent to the remote party. The invitation may or may not every receive a response. If a response is not received in a timely manor, sipXtapi will move the call into a disconnected state. If calling another sipXtapi user agent, the reciprocal state is OFFER.
                           CALLSTATE_REMOTE_ALERTING  =  3000,  //The REMOTE_ALERTING event indicates that a call setup invitation has been accepted and the end user is in the alerting state (ringing). Depending on the SIP configuration, end points, and proxy servers involved, this event should only last for 3 minutes. Afterwards, the state will automatically move to DISCONNECTED. If calling another sipXtapi user agent, the reciprocate state is ALERTING. Pay attention to the cause code for this event. If the cause code is "CALLSTATE_CAUSE_EARLY_MEDIA", the remote the party is sending early media (e.g. gateway is producing ringback or audio feedback). In this case, the user agent should not produce local ringback.
                           CALLSTATE_CONNECTED        =  4000, 	//The CONNECTED state indicates that call has been setup between the local and remote party. Network audio should be flowing provided and the microphone and speakers should be engaged.
                           CALLSTATE_BRIDGED          =  5000,  //The BRIDGED state indicates that a call is active, however, the local microphone/speaker are not engaged. If this call is part of a conference, the party will be able to talk with other BRIDGED conference parties. Application developers can still play and record media.
                           CALLSTATE_HELD             =  6000,
                           CALLSTATE_REMOTE_HELD      =  7000, 	//The HELD state indicates that a call is both locally and remotely held. No network audio is flowing and the local microphone and speaker are not engaged.
                           CALLSTATE_DISCONNECTED     =  8000, 	//The REMOTE_HELD state indicates that the remote party is on hold. The DISCONNECTED state indicates that a call was disconnected or failed to connect. Locally, the microphone and speaker are still engaged, however, no network audio is flowing. A call may move into the DISCONNECTED states from almost every other state. Please review the DISCONNECTED minor events to understand the cause.
                           CALLSTATE_OFFERING         =  9000, 	//An OFFERING state indicates that a new call invitation has been extended this user agent. Application developers should invoke sipxCallAccept(), sipxCallReject() or sipxCallRedirect() in response. Not responding will result in an implicit call sipXcallReject().
                           CALLSTATE_ALERTING         = 10000, 	//An ALERTING state indicates that an inbound call has been accepted and the application layer should alert the end user. The alerting state is limited to 3 minutes in most configurations; afterwards the call will be canceled. Applications will generally play some sort of ringing tone in response to this event.
                           CALLSTATE_DESTROYED        = 11000, 	//The DESTORYED event indicates the underlying resources have been removed for a call. This is the last event that the application will receive for any call. The call handle is invalid after this event is received.
                           CALLSTATE_TRANSFER_EVENT   = 12000 	//The transfer state indicates a state change in a transfer attempt.
                          );

  TSIPX_Callstate_Cause = (CALLSTATE_CAUSE_UNKNOWN,	                    //Unknown cause.
                           CALLSTATE_CAUSE_NORMAL, 	                    //The stage changed due to normal operation.
                           CALLSTATE_CAUSE_TRANSFERRED, 	              //A call is being transferred to this user agent from another user agent.
                           CALLSTATE_CAUSE_TRANSFER, 	                  //A call on this user agent is being transferred to another user agent.
                           CALLSTATE_CAUSE_CONFERENCE, 	                //A conference operation caused a stage change.
                           CALLSTATE_CAUSE_EARLY_MEDIA, 	              //The remote party is alerting and providing ringback audio (early media).
                           CALLSTATE_CAUSE_REQUEST_NOT_ACCEPTED, 	      //The callee rejected a request (e.g. hold)
                           CALLSTATE_CAUSE_BAD_ADDRESS, 	              //The state changed due to a bad address. This can be caused by a malformed URL or network problems with your DNS server
                           CALLSTATE_CAUSE_BUSY, 	                      //The state cahnged because the remote party is busy.
                           CALLSTATE_CAUSE_RESOURCE_LIMIT, 	            //Not enough resources are available to complete the desired operation.
                           CALLSTATE_CAUSE_NETWORK, 	                  //A network error caused the desired operation to fail.
                           CALLSTATE_CAUSE_REDIRECTED, 	                //The stage changed due to a redirection of a call.
                           CALLSTATE_CAUSE_NO_RESPONSE, 	              //No response was received from the remote party or network node.
                           CALLSTATE_CAUSE_AUTH, 	                      //Unable to authenticate due to either bad or missing credentials.
                           CALLSTATE_CAUSE_TRANSFER_INITIATED, 	        //A transfer attempt has been initiated. This event is sent when a user agent attempts either a blind or consultative transfer.
                           CALLSTATE_CAUSE_TRANSFER_ACCEPTED, 	        //A transfer attempt has been accepted by the remote transferee. This event indicates that the transferee supports transfers (REFER method). The event is fired upon a 2xx class response to the SIP REFER request.
                           CALLSTATE_CAUSE_TRANSFER_TRYING, 	          //The transfer target is attempting the transfer. This event is sent when transfer target (or proxy / B2BUA) receives the call invitation, but before the the tranfer target accepts is.
                           CALLSTATE_CAUSE_TRANSFER_RINGING, 	          //The transfer target is ringing. This event is generally only sent during blind transfer. Consultative transfer should proceed directly to TRANSFER_SUCCESS or TRANSFER_FAILURE.
                           CALLSTATE_CAUSE_TRANSFER_SUCCESS, 	          //The transfer was completed successfully. The original call to transfer target will automatically disconnect.
                           CALLSTATE_CAUSE_TRANSFER_FAILURE, 	          //The transfer failed. After a transfer fails, the application layer is responsible for recovering original call to the transferee. That call is left on hold.
                           CALLSTATE_CAUSE_REMOTE_SMIME_UNSUPPORTED, 	  //Fired if the remote party's user-agent does not support S/MIME.
                           CALLSTATE_CAUSE_SMIME_FAILURE, 	            //Fired if a local S/MIME operation failed. For more information, applications should process the SECURITY event.
                           CALLSTATE_CAUSE_SHUTDOWN, 	                  //The even was fired as part of sipXtapi shutdown.
                           CALLSTATE_CAUSE_BAD_REFER, 	                //An unusable refer was sent to this user-agent.
                           CALLSTATE_CAUSE_NO_KNOWN_INVITE, 	          //This user-agent received a request or response, but there is no known matching invite.
                           CALLSTATE_CAUSE_BYE_DURING_IDLE, 	          //A BYE message was received, however, the call is in in an idle state.
                           CALLSTATE_CAUSE_UNKNOWN_STATUS_CODE, 	      //A response was received with an unknown status code.
                           CALLSTATE_CAUSE_BAD_REDIRECT, 	              //Receive a redirect with NO contact or a RANDOM redirect.
                           CALLSTATE_CAUSE_TRANSACTION_DOES_NOT_EXIST,  //No such transaction; Accepting or Rejecting a call that is part of a transfer.
                           CALLSTATE_CAUSE_CANCEL 	                    //The event was fired in response to a cancel attempt from the remote party.
                          );

  TSIPX_Linestate_Event = (LINESTATE_UNKNOWN 	          =      0, //This is the initial Line event state.
                           LINESTATE_REGISTERING	      = 200000, //The REGISTERING event is fired when sipXtapi has successfully sent a REGISTER message, but has not yet received a success response from the registrar server.
                           LINESTATE_REGISTERED 	      = 210000, //The REGISTERED event is fired after sipXtapi has received a response from the registrar server, indicating a successful registration.
                           LINESTATE_UNREGISTERING 	    = 220000, //The UNREGISTERING event is fired when sipXtapi has successfully sent a REGISTER message with an expires=0 parameter, but has not yet received a success response from the registrar server.
                           LINESTATE_UNREGISTERED 	    = 230000, //The UNREGISTERED event is fired after sipXtapi has received a response from the registrar server, indicating a successful un-registration.
                           LINESTATE_REGISTER_FAILED 	  = 240000, //The REGISTER_FAILED event is fired to indicate a failure of REGISTRATION.It is fired in the following cases: The client could not connect to the registrar server. The registrar server challenged the client for authentication credentials, and the client failed to supply valid credentials. The registrar server did not generate a success response (status code == 200) within a timeout period.
                           LINESTATE_UNREGISTER_FAILED  = 250000, //The UNREGISTER_FAILED event is fired to indicate a failure of un-REGISTRATION.  It is fired in the following cases: The client could not connect to the registrar server. The registrar server challenged the client for authentication credentials, and the client failed to supply valid credentials. The registrar server did not generate a success response (status code == 200) within a timeout period.
                           LINESTATE_PROVISIONED 	      = 260000  //The PROVISIONED event is fired when a sipXtapi Line is added, and Registration is not requested (i.e. sipxLineAdd is called with a bRegister parameter of false.
                          );

  TSIPX_Linestate_Cause = (LINESTATE_CAUSE_UNKNOWN 	                      = 0,//No cause specified.
                           LINESTATE_REGISTERING_NORMAL 	                = Integer(LINESTATE_REGISTERING) + 1, //See LINESTATE_REGISTERING event.
                           LINESTATE_REGISTERED_NORMAL 	                  = Integer(LINESTATE_REGISTERED) + 1, //See LINESTATE_REGISTERED event.
                           LINESTATE_UNREGISTERING_NORMAL 	              = Integer(LINESTATE_UNREGISTERING) + 1, //See LINESTATE_UNREGISTERING event.
                           LINESTATE_UNREGISTERED_NORMAL 	                = Integer(LINESTATE_UNREGISTERED) + 1, //See LINESTATE_UNREGISTERED event.
                           LINESTATE_REGISTER_FAILED_COULD_NOT_CONNECT 	  = Integer(LINESTATE_REGISTER_FAILED) + 1, //Failed to register because of a connectivity problem.
                           LINESTATE_REGISTER_FAILED_NOT_AUTHORIZED 	    = Integer(LINESTATE_REGISTER_FAILED) + 2, //Failed to register because of an authorization / authentication failure.
                           LINESTATE_REGISTER_FAILED_TIMEOUT 	            = Integer(LINESTATE_REGISTER_FAILED) + 3, //Failed to register because of a timeout.
                           LINESTATE_UNREGISTER_FAILED_COULD_NOT_CONNECT  = Integer(LINESTATE_UNREGISTER_FAILED) + 1,  //Failed to unregister because of a connectivity problem.
                           LINESTATE_UNREGISTER_FAILED_NOT_AUTHORIZED 	  = Integer(LINESTATE_UNREGISTER_FAILED) + 2,  //Failed to unregister because of of an authorization / authentication failure.
                           LINESTATE_UNREGISTER_FAILED_TIMEOUT 	          = Integer(LINESTATE_UNREGISTER_FAILED) + 3,  //Failed to register because of a timeout.
                           LINESTATE_PROVISIONED_NORMAL                   = Integer(LINESTATE_PROVISIONED) + 1         //See LINESTATE_PROVISIONED event.
                          );

  TSIPX_Infostatus_Event = (INFOSTATUS_UNKNOWN        =     0,  //This is the initial value for an INFOSTATUS event.
                            INFOSTATUS_RESPONSE       = 30000,	//This event is fired if a response is received after an INFO message has been sent.
                            INFOSTATUS_NETWORK_ERROR  = 31000 	//This event is fired in case a network error was encountered while trying to send an INFO event.
                           );

  TSIPX_Config_Event = (CONFIG_UNKNOWN 	    =     0,  //Unknown configuration event.
                        CONFIG_STUN_SUCCESS = 40000,  //A STUN binding has been obtained for signaling purposes. For a SIPX_CONFIG_EVENT type of CONFIG_STUN_SUCCESS, the pData pointer of the info structure will point to a SIPX_CONTACT_ADDRESS structure.
                        CONFIG_STUN_FAILURE = 41000 	//Unable to obtain a STUN binding for signaling purposes.
                       );

  TSIPX_Security_Event = (SECURITY_UNKNOWN 	=    0, //An UNKNOWN event is generated when the state for a call is no longer known. This is generally an error condition; see the minor event for specific causes.
                          SECURITY_ENCRYPT  = 1000, //The ENCRYPT event indicates that an SMIME encryption has been attempted. See the cause code for the encryption outcome, and the info structure for more information.
                          SECURITY_DECRYPT 	= 2000, //The DECRYPT event indicates that an SMIME decryption has been attempted. See the cause code for the encryption outcome, and the info structure for more information.
                          SECURITY_TLS 	    = 4000  //TLS related security event.
                         );

  TSIPX_Security_Cause  = (SECURITY_CAUSE_UNKNOWN, 	                          //An UNKNOWN cause code is generated when the state for the security operation is no longer known. This is generally an error condition; see the info structure for details.
                           SECURITY_CAUSE_NORMAL, 	                          //Event was fired as part of the normal encryption / decryption process.
                           SECURITY_CAUSE_ENCRYPT_SUCCESS, 	                  //An S/MIME encryption succeeded.
                           SECURITY_CAUSE_ENCRYPT_FAILURE_LIB_INIT, 	        //An S/MIME encryption failed because the security library could not start.
                           SECURITY_CAUSE_ENCRYPT_FAILURE_BAD_PUBLIC_KEY, 	  //An S/MIME encryption failed because of a bad certificate / public key.
                           SECURITY_CAUSE_ENCRYPT_FAILURE_INVALID_PARAMETER,  //An S/MIME encryption failed because of an invalid parameter.
                           SECURITY_CAUSE_DECRYPT_SUCCESS, 	                  //An S/MIME decryption succeeded.
                           SECURITY_CAUSE_DECRYPT_FAILURE_DB_INIT, 	          //An S/MIME decryption failed due to a failure to initialize the certificate database.
                           SECURITY_CAUSE_DECRYPT_FAILURE_BAD_DB_PASSWORD, 	  //An S/MIME decryption failed due to an invalid certificate database password.
                           SECURITY_CAUSE_DECRYPT_FAILURE_INVALID_PARAMETER,  //An S/MIME decryption failed due to an invalid parameter.
                           SECURITY_CAUSE_DECRYPT_BAD_SIGNATURE, 	            //An S/MIME decryption operation aborted due to a bad signature.
                           SECURITY_CAUSE_DECRYPT_MISSING_SIGNATURE,	        //An S/MIME decryption operation aborted due to a missing signature.
                           SECURITY_CAUSE_DECRYPT_SIGNATURE_REJECTED, 	      //An S/MIME decryption operation aborted because the signature was rejected.
                           SECURITY_CAUSE_TLS_SERVER_CERTIFICATE, 	          //A TLS server certificate is being presented to the application for possible rejection. The application must respond to this message. If the application returns false, the certificate is rejected and the call will not complete. If the application returns true, the certificate is accepted.
                           SECURITY_CAUSE_TLS_BAD_PASSWORD, 	                //A TLS operation failed due to a bad password.
                           SECURITY_CAUSE_TLS_LIBRARY_FAILURE, 	              //A TLS operation failed.
                           SECURITY_CAUSE_REMOTE_HOST_UNREACHABLE, 	          //The remote host is not reachable.
                           SECURITY_CAUSE_TLS_CONNECTION_FAILURE, 	          //A TLS connection to the remote party failed.
                           SECURITY_CAUSE_TLS_HANDSHAKE_FAILURE, 	            //A failure occured during the TLS handshake.
                           SECURITY_CAUSE_SIGNATURE_NOTIFY, 	                //The SIGNATURE_NOTIFY event is fired when the user-agent receives a SIP message with signed SMIME as its content. The signer's certificate will be located in the info structure associated with this event. The application can choose to accept the signature, by returning 'true' in response to this message or can choose to reject the signature by returning 'false' in response to this message.
                           SECURITY_CAUSE_TLS_CERTIFICATE_REJECTED 	          //The application has rejected the server's TLS certificate.
                          );

  TSIPX_Media_Event = (MEDIA_UNKNOWN 	        = 0,      //Unknown or undefined media event, this is generally the sign of an internal error in sipXtapi.
                       MEDIA_LOCAL_START 	    = 50000,  //Local media (audio or video) is being sent to the remote party.
                       MEDIA_LOCAL_STOP, 	              //Local media (audio or video) is no longer being sent to the remote party. This may be caused by a local/remote hold operation, call tear down, or error. See the SIPX_MEDIA_CAUSE enumeration for more information.
                       MEDIA_REMOTE_START, 	            //Remote media (audio or video) is ready to be received. If no audio/video is received for longer then the idle period, a MEDIA_REMOTE_SILENT event will be fired. See sipxConfigSetConnectionIdleTimeout.
                       MEDIA_REMOTE_STOP, 	              //Remote media (audio or video) has been stopped due to a hold or call tear down.
                       MEDIA_REMOTE_SILENT, 	            //Remote media has not been received for some configured period. This generally indicates a network problem and/or a problem with the remote party. See sipxConfigSetConnectionIdleTimeout for more information.
                       MEDIA_PLAYFILE_START, 	          //A file is being played to local and/or remote parties. This event will be followed by a MEDIA_PLAYFILE_STOP when the file is manually stopped or finished playing.
                       MEDIA_PLAYFILE_STOP, 	            //A file has completed playing or was aborted.
                       MEDIA_PLAYBUFFER_START,           //A buffer is being played to local and/or remote parties. This event will be followed by a MEDIA_PLAYBUFFER_STOP when the file is manually stopped or finished playing.
                       MEDIA_PLAYBUFFER_STOP, 	          //A buffer has completed playing or was aborted.
                       MEDIA_REMOTE_DTMF, 	              //A dtmf tone was started/stopped, see the cause codes for exact status.
                       MEDIA_DEVICE_FAILURE, 	          //Fired if the media device is not present or already in use.
                       MEDIA_REMOTE_ACTIVE 	            //Media has been received.
                      );

  TSIPX_Media_Cause = (MEDIA_CAUSE_NORMAL, 	            //Normal cause; the call was likely torn down.
                       MEDIA_CAUSE_HOLD, 	              //Media state changed due to a local or remote hold operation.
                       MEDIA_CAUSE_UNHOLD, 	            //Media state changed due to a local or remote unhold operation.
                       MEDIA_CAUSE_FAILED, 	            //Media state changed due to an error condition.
                       MEDIA_CAUSE_DEVICE_UNAVAILABLE,  //Media state changed due to an error condition, (device was removed, already in use, etc).
                       MEDIA_CAUSE_INCOMPATIBLE, 	      //Incompatible destination -- We were unable to negotiate a codec.
                       MEDIA_CAUSE_DTMF_START, 	        //A DTMF tone has started.
                       MEDIA_CAUSE_DTMF_STOP 	          //A DTMF tone has stopped.
                      );

  TSIPX_Media_Type = (MEDIA_TYPE_AUDIO, //Audio media event type.
                      MEDIA_TYPE_VIDEO 	//Video media event type.
                     );

  TSIPX_Keepalive_Event = (KEEPALIVE_START, 	  //A keepalive attempt has been started. The developer is responsible for stopping all keepalives. In some cases, keepalives will be automatically stopped -- however do not rely on that.
                           KEEPALIVE_FEEDBACK,  //The keepalive process has obtained information regarding your NAT mapped address (or local address). Feedback events are sent with the mapped address from a STUN transaction or the rport results from a SIP transaction.
                           KEEPALIVE_FAILURE, 	//FAILURE events are only fired when the physical send fails. The application developer should stop the keepalive or can monitor the keepalive until the condition changes (lack of failure or feedback event).
                           KEEPALIVE_STOP 	    //A keepalive process has been stopped.
                          );

  TSIPX_KEEPALIVE_Cause = (KEEPALIVE_CAUSE_NORMAL
                          );

  TSIPX_Message_Status = (SIPX_MESSAGE_OK, 	            //The message was successfully processed (200).
                          SIPX_MESSAGE_FAILURE, 	      //The server received the message, but could or would not process it.
                          SIPX_MESSAGE_SERVER_FAILURE,  //The server encountered an error while trying to process the message.
                          SIPX_MESSAGE_GLOBAL_FAILURE 	//Fatal error encountered.
                         );

  TSIPX_Subscription_State = (SIPX_SUBSCRIPTION_PENDING,  //THe subscription is being set up, but not yet active.
                              SIPX_SUBSCRIPTION_ACTIVE, 	//The subscription is currently active.
                              SIPX_SUBSCRIPTION_FAILED, 	//The subscription is not active due to a failure.
                              SIPX_SUBSCRIPTION_EXPIRED 	//The subscription's lifetime has expired.
                             );

  TSIPX_Subscription_Cause = (SUBSCRIPTION_CAUSE_UNKNOWN  = -1, //No cause specified.
                              SUBSCRIPTION_CAUSE_NORMAL 	      //Normal cause for state change.
                             );
  {$ENDREGION}

  {$REGION 'Data Structures'}
  TSIPX_Keepalive_Info = packed record
    nSize             : Cardinal; //Size of the structure.
    event             : TSIPX_Keepalive_Event; //Keepalive event identifier.
    cause             : TSIPX_KEEPALIVE_Cause; //Keepalive cause.
    _type             : TSIPX_Keepalive_Type; //Keepalive type.
    szRemoteAddress   : PChar; //Target IP/host where you are sending the keepalives.
    remotePort        : Integer; //Target port where you are sending the keepalives.
    keepAliveSecs     : Integer; //How often keepalives are being sent.
    szFeedbackAddress : PChar; //This UA's IP address as seen by the remote side (only valid in FEEDBACK events).
    feedbackPort      : Integer; //This UA's port as seen by the remote side (only valid in FEEDBACK events).
  end;
  PSIPX_Keepalive_Info = ^TSIPX_Keepalive_Info;

  TSIPX_Media_Info = packed record
    nSize     : Cardinal; //Size of the structure.
    event     : TSIPX_Media_Event; //Media event identifier.
    cause     : TSIPX_Media_Cause; //Media cause identifier.
    mediaType : TSIPX_Media_Type; //Media type: Either MEDIA_TYPE_AUDIO or MEDIA_TYPE_VIDEO.
    hCall     : TSIPX_Call; //Associate call (or SIPX_CALL_NULL if not associated with a call).
    codec     : TSIPX_Codec_Info; //Negotiated codec; only supplied on MEDIA_LOCAL_START and MEDIA_REMOTE_START events.
    idleTime  : Integer; //Idle time (ms) for SILENT events; only supplied on MEDIA_REMOTE_SILENT events.
    toneId    : TSIPX_Tone_ID; //DTMF tone received from remote party; only supplied on MEDIA_REMOTE_DTMF_START and MEDIA_REMOTE_DTMF_STOP events).
  end;
  PSIPX_Media_Info = ^TSIPX_Media_Info;

  TSIPX_Callstate_Info = packed record
    nSize           : Cardinal; //The size of this structure.
    hCall           : TSIPX_Call;//Call handle associated with the callstate event.
    hLine           : TSIPX_Line; //Line handle associated with the callstate event.
    event           : TSIPX_Callstate_Event; //Callstate event enum code.
    cause           : TSIPX_Callstate_Cause; //Callstate cause enum code.
    hAssociatedCall : TSIPX_Call; //Call associated with this event.
  end;
  PSIPX_Callstate_Info = ^TSIPX_Callstate_Info;

  TSIPX_Linestate_Info = packed record
    nSize : Cardinal; //The size of this structure.
    hLine : TSIPX_Line; //Line handle associated with the linestate event.
    event : TSIPX_Linestate_Event; //Callstate event enum code.
    cause : TSIPX_Linestate_Cause; //Callstate cause enum code.
  end;
  PSIPX_Linestate_Info = ^TSIPX_Linestate_Info;

  TSIPX_Infostatus_Info = packed record
    nSize           : Cardinal; //the size of this structure in bytes
    hInfo           : TSIPX_Info; //the handle used to make the outbound info request.
    status          : TSIPX_Message_Status; //Emumerated status for this request acknowledgement.
    responseCode    : Integer; //Numerical status code for this request acknowledgement.
    szResponseText  : PChar; //The text of the request acknowledgement.
    event           : TSIPX_Infostatus_Event; //Event code for this INFO STATUS message.
  end;
  PSIPX_Infostatus_Info = ^TSIPX_Infostatus_Info;

  TSIPX_Info_Info = packed record
    nSize           : Cardinal; //Size of structure.
    hCall           : TSIPX_CALL; //Call handle if available.
    hLine           : TSIPX_LINE; //Line handle if available.
    szFromURL       : PChar; //the URL of the host that originated the INFO message
    szUserAgent     : PChar; //the User Agent string of the source agent
    szContentType   : PChar; //string indicating the info content type
    pContent        : PChar; //pointer to the INFO message content
    nContentLength  : Cardinal; //length of the INFO message content
  end;
  PSIPX_Info_Info = ^TSIPX_Info_Info;

  TSIPX_Substatus_Info = packed record
    nSize                 : Cardinal; //The size of this structure in bytes.
    hSub                  : TSIPX_SUB; //A handle to the subscription to which this state change occurred.
    state                 : TSIPX_Subscription_State; //Enum state value indicating the current state of the subscription.
    cause                 : TSIPX_Subscription_Cause; //Enum cause for the state change in this event.
    szSubServerUserAgent  : PChar; //The User Agent header field value from the SIP SUBSCRIBE response (may be NULL).
  end;
  PSIPX_Substatus_Info = ^TSIPX_Substatus_Info;

  TSIPX_Notify_Info = packed record
    nSize               : Cardinal; //The size of this structure in bytes.
    hSub                : TSIPX_Sub; //A handle to the subscrption which caused this NOTIFY event to be received.
    szNotiferUserAgent  : PChar; //The User-Agent header field value from the SIP NOTIFY response (may be NULL).
    szContentType       : PChar; //String indicating the info content type.
    pContent            : Pointer; //Pointer to the NOTIFY message content.
    nContentLength      : Cardinal; //Length of the NOTIFY message content in bytes.
  end;
  PSIPX_Notify_Info = ^TSIPX_Notify_Info;

  TSIPX_Config_Info = packed record
    nSize : Cardinal; //The size of this structure in bytes.
    event : TSIPX_Config_Event; //Event code -- see SIPX_CONFIG_EVENT for details.
    pData : Pointer; //Pointer to event data -- SEE SIPX_CONFIG_EVENT for details.
  end;
  PSIPX_Config_Info = ^TSIPX_Config_Info;

  TSIPX_Security_Info = packed record
    nSize             : Cardinal; //the size of this structure in bytes
    szSRTPkey         : PChar; //the negotiated SRTP key, if any.
    pCertificate      : Pointer; //pointer to the certificate blob that was used to encrypt and/or sign.
    nCertificateSize  : Integer;//size of the certificate blob
    event             : TSIPX_Security_Event; //Event code for this SECURITY_INFO message.
    cause             : TSIPX_Security_Cause; //Cause code for this SECURITY_INFO message.
    szSubjAltName     : PChar; //Populated for SECURITY_CAUSE_SIGNATURE_NOTIFY.
    callId            : PChar; //Points to a call-id string associated with the event.
    hCall             : TSIPX_Call; //A call handle associated with the event.
    remoteAddress     : PChar; //A remote address associated with the event.
  end;
  PSIPX_Security_Info = ^TSIPX_Security_Info;
  {$ENDREGION}

  {$REGION 'Typedefs'}
    TSIPX_Event_Callback_Proc = function(category   : TSIPX_Event_Category;
                                         pInfo      : Pointer;
                                         pUserdata  : Pointer
                                        ) : Boolean; cdecl;
  {$ENDREGION}

  {$REGION 'Functions'}
  function sipxDuplicateEvent(category : TSIPX_Event_Category;
                               const pEventSource : Pointer;
                               pEventCopy : PPointer
                              ) : TSIPX_Result; cdecl; external DLLFile;
    //Duplicate the event information for a sipXtapi event.

  function sipxFreeDuplicatedEvent(category : TSIPX_Event_Category;
                                   pEventCopy : Pointer
                                  ) : TSIPX_Result; cdecl; external DLLFile;
    //Frees up memory allocated as part of sipxDuplicateEvent.

  function sipxEventListenerAdd(const hInst : TSIPX_Inst;
                                pCallbackProc  : TSIPX_Event_Callback_Proc;
                                pUserData : Pointer
                               ) : TSIPX_Result; cdecl; external DLLFile;
    //Add a callback/observer for the purpose of receiving sipXtapi events.

  function sipxEventListenerRemove(const hInst : TSIPX_Inst;
                                   pCallbackProc : TSIPX_Event_Callback_Proc;
                                   pUserData : Pointer
                                  ) : TSIPX_Result; cdecl; external DLLFile;
    //Remove a sipXtapi event callback/observer.

  function sipxCallEventToString(event : TSIPX_Callstate_Event;
                                 cause : TSIPX_Callstate_Cause;
                                 szBuffer : PChar;
                                 nBuffer : Cardinal
                                ) : PChar; cdecl; external DLLFile;
    //Create a printable string version of the designated call state event ids.

  function sipxEventToString(const category : TSIPX_Event_Category ;
                             const pEvent : Pointer;
                             szBuffer : PChar;
                             nBuffer : Cardinal
                            ) : PChar; cdecl; external DLLFile;
    //Create a printable string version of the designated event.

  function sipxLineEventToString(event : TSIPX_Linestate_Event;
                                 cause : TSIPX_Linestate_Cause ;
                                 szBuffer : PChar;
                                 nBuffer : Cardinal
                                ) : PChar; cdecl; external DLLFile;
    //Create a printable string version of the designated line event ids.

  function sipxConfigEventToString(event : TSIPX_Config_Event;
                                   szBuffer : PChar;
                                   nBuffer : Cardinal
                                  ) : PChar; cdecl; external DLLFile;
    //Create a printable string version of the designated config event.

  function sipxSubStatusStateToString(state : TSIPX_Subscription_State;
                                      szBuffer : PChar;
                                      nBuffer : Cardinal
                                     ) : PChar; cdecl; external DLLFile;
    //Create a printable string version of the designated subscription status state.

  function sipxSubStatusCauseToString(cause : TSIPX_Subscription_Cause;
                                      szBuffer : PChar;
                                      nBuffer : Cardinal
                                     ) : PChar; cdecl; external DLLFile;
    //Create a printable string version of the designated subscription status cause.

  function sipxSecurityEventToString(event : TSIPX_Security_Event;
                                     szBuffer : PChar;
                                     nBuffer : Cardinal
                                    ) : PChar; cdecl; external DLLFile;
    //Create a printable string version of the designated security event.

  function sipxSecurityCauseToString(cause : TSIPX_Security_Cause;
                                     szBuffer : PChar;
                                     nBuffer : Cardinal
                                    ) : PChar; cdecl; external DLLFile;
    //Create a printable string version of the designated security cause.

  function sipxMediaEventToString(event : TSIPX_MEDIA_Event;
                                  szBuffer : PChar;
                                  nBuffer : Cardinal
                                 ) : PChar; cdecl; external DLLFile;
    //Create a printable string version of the designated media event.

  function sipxMediaCauseToString(cause : TSIPX_Media_Cause;
                                  szBuffer : PChar;
                                  nBuffer : Cardinal
                                 ) : PChar; cdecl; external DLLFile;
    //Create a printable string version of the designated media cause.
  {$ENDREGION}

implementation

end.
