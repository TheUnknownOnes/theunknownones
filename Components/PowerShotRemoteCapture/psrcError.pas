unit psrcError;

interface

uses
  Sysutils, prError, prType;

type
  EpsrcException = type Exception;

  EpsrcLibNotFound                = type EpsrcException;
  EpsrcCanNotLoadLib              = type EpsrcException;
  EpsrcCanNotFindFunctionPointer  = type EpsrcException;
  EpsrcFunctionNotDefined  = class(EpsrcException)
  public
    constructor Create(AFunctionName : String);
  end;

  EpsrcFirmwareException      = type EpsrcException;
  EpsrcSDKException           = type EpsrcException;
  EpsrcWIA_STIException       = type EpsrcException;
  EpsrcWindowsException       = type EpsrcException;
  EpsrcCOMInterfaceException  = type EpsrcException;


  procedure psCheckResponse(AResponse : prResponse);
  function psResponseErrorID(AResponse : prResponse) : prResponse;
  function psResponseErrorComponent(AResponse : prResponse) : prResponse;

implementation

function psResponseErrorID(AResponse : prResponse) : prResponse;
begin
  Result := AResponse and prERROR_ERRORID_MASK;
end;

function psResponseErrorComponent(AResponse : prResponse) : prResponse;
begin
  Result := AResponse and prERROR_COMPONENTID_MASK;
end;

procedure psCheckResponse(AResponse : prResponse);
var
  E : Exception;
begin
  if AResponse = prOK then
    exit;

  case psResponseErrorComponent(AResponse) of
    prERROR_PTP_COMPONENTID     : E := EpsrcFirmwareException.Create('');
    prERROR_PRSDK_COMPONENTID   : E := EpsrcSDKException.Create('');
    prERROR_WIA_STI_COMPONENTID : E := EpsrcWIA_STIException.Create('');
    prERROR_WINDOWS_COMPONENTID : E := EpsrcWindowsException.Create('');
    prERROR_COMIF_COMPONENTID   : E := EpsrcCOMInterfaceException.Create('');
    else
      E := EpsrcException.Create('Unknown error');
  end;

  case psResponseErrorID(AResponse) of
    prUNIMPLEMENTED         : E.Message := 'Feature not implemented';
    prINTERNAL_ERROR        : E.Message := 'Internal error';
    prMEM_ALLOC_FAILED      : E.Message := 'Memory allocation failed';
    prMEM_FREE_FAILED       : E.Message := 'Memory release failed';
    prOPERATION_CANCELLED   : E.Message := 'Operation cancelled';
    prINCOMPATIBLE_VERSION  : E.Message := 'Inkompatible version';
    prNOT_SUPPORTED         : E.Message := 'Feature not supported';
    prUNEXPECTED_EXCEPTION  : E.Message := 'Unexpected error';
    prPROTECTION_VIOLATION  : E.Message := 'Violation of a protection';
    prMISSING_SUBCOMPONENT  : E.Message := 'Can not find subcomponent';
    prSELECTION_UNAVAILABLE : E.Message := 'Selection is not available';

    prINVALID_PARAMETER : E.Message := 'Invalid function parameter';
    prINVALID_HANDLE    : E.Message := 'Invalid handle passed to function';

    prINVALID_FN_CALL      : E.Message := 'Invalid function call';
    prWAIT_TIMEOUT_ERROR   : E.Message := 'Wait timeout';
    prINSUFFICIENT_BUFFER  : E.Message := 'Insufficient buffer supplied';
    prEVENT_CALLBACK_EXIST : E.Message := 'This event callback already exists';

    prRESPONSE_Undefined                             : E.Message := 'Undefined error';
    prRESPONSE_GeneralError                          : E.Message := 'General error';
    prRESPONSE_SessionNotOpen                        : E.Message := 'Session is not open';
    prRESPONSE_InvalidTransactionID                  : E.Message := 'Invalid transaction id';
    prRESPONSE_OperationNotSupported                 : E.Message := 'This operation is not supported';
    prRESPONSE_ParameterNotSupported                 : E.Message := 'This paramter is not supported';
    prRESPONSE_IncompleteTransfer                    : E.Message := 'Transfer was incomplete';
    prRESPONSE_InvalidStorageID                      : E.Message := 'Invalid storage id supplied';
    prRESPONSE_InvalidObjectHandle                   : E.Message := 'Invalid object handle supplied';
    prRESPONSE_DevicePropNotSupported                : E.Message := 'Device doesnt support this property';
    prRESPONSE_InvalidObjectFormatCode               : E.Message := 'Invalid object format supplied';
    prRESPONSE_StoreFull                             : E.Message := 'Store is full';
    prRESPONSE_ObjectWriteProtected                  : E.Message := 'Object is write protected';
    prRESPONSE_StoreRead_Only                        : E.Message := 'Store is readonly';
    prRESPONSE_AccessDenied                          : E.Message := 'Access denied';
    prRESPONSE_NoThumbnailPresent                    : E.Message := 'No thumbnail is present';
    prRESPONSE_SelfTestFailed                        : E.Message := 'Self test failed';
    prRESPONSE_PartialDeletion                       : E.Message := 'Partial deletion';
    prRESPONSE_StoreNotAvailable                     : E.Message := 'No store is available';
    prRESPONSE_SpecificationByFormatUnsupported      : E.Message := 'Specification by format is not supported';
    prRESPONSE_NoValidObjectInfo                     : E.Message := 'No valid object info';
    prRESPONSE_InvalidCodeFormat                     : E.Message := 'The code format is invalid';
    prRESPONSE_UnknownVendorCode                     : E.Message := 'The vendor code is unknown';
    prRESPONSE_CaptureAlreadyTerminated              : E.Message := 'Capture was already terminated';
    prRESPONSE_DeviceBusy                            : E.Message := 'Device is busy';
    prRESPONSE_InvalidParentObject                   : E.Message := 'The parent object is invalid';
    prRESPONSE_InvalidDevicePropFormat               : E.Message := 'The format of the device property is invalid';
    prRESPONSE_InvalidDevicePropValue                : E.Message := 'The value of the device property is invalid';
    prRESPONSE_InvalidParameter                      : E.Message := 'Invalid paramter supplied';
    prRESPONSE_SessionAlreadyOpen                    : E.Message := 'Session is already open';
    prRESPONSE_TransactionCancelled                  : E.Message := 'Transaction was cancelled';
    prRESPONSE_SpecificationOfDestinationUnsupported : E.Message := 'Specification of the destination is not supported';

    prRESPONSE_Ex_Undefined              : E.Message := 'Undefined error';
    prRESPONSE_Ex_UnknownCommandReceived : E.Message := 'Received unknown command';
    prRESPONSE_Ex_MemAllocFailed         : E.Message := 'Memory allocation failed';
    prRESPONSE_Ex_InternalError          : E.Message := 'Internal error';
    prRESPONSE_Ex_DirIOError             : E.Message := 'Dir IO error';
    prRESPONSE_Ex_RefusedByOtherProcess  : E.Message := 'Refused by another process';
    prRESPONSE_Ex_CoverClosed            : E.Message := 'Cover was closed';
    prRESPONSE_Ex_NoRelease              : E.Message := 'No release';
    prRESPONSE_Ex_DeviceIsHot            : E.Message := 'Device is hot';
    prRESPONSE_Ex_LowBattery             : E.Message := 'Battery is low';
    prRESPONSE_Ex_AlreadyExit						 : E.Message := 'This already exists';
  end;

  raise E;
end;

{ EpsrcFunctionNotDefined }

constructor EpsrcFunctionNotDefined.Create(AFunctionName: String);
begin
  inherited Create('Function "'+AFunctionName+'" is not defined');
end;

end.

