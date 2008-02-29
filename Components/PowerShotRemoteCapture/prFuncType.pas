unit prFuncType;

interface

uses
  prType;

type

{****************************************************************************
 ****************************************************************************
 *              PowerShot RemoteCapture SDK : Function Pointers             *
 ****************************************************************************
 ****************************************************************************}

{-----------------------------------------------------------------------
	Basic Functions
------------------------------------------------------------------------ }

  prStartSDK = function() : prResponse; stdcall;

  prFinishSDK = function() : prResponse; stdcall;

  prGetDllsVersion = function(var pBufferSize : prUInt32;
                              pDllVersion : PprDllsVerInfo) : prResponse; stdcall;

{-----------------------------------------------------------------------
	Basic Camera Device Functios
------------------------------------------------------------------------ }

{ Enumeration of Camera Devices  }
  prGetDeviceList = function(var pBuffer : prUInt32;
                             pDeviceList : PprDeviceList) : prResponse; stdcall;


{ Creation/Deletion of Camera Handles  }
  prCreateCameraObject = function(var pDeviceInfo : prDeviceInfoTable;
                                  var pCameraHandle : prHandle) : prResponse; stdcall;


  prDestroyCameraObject = function(CameraHandle : prHandle) : prResponse; stdcall;


{ Connecting/Disconnecting Camera Devices  }
  prConnectCamera = function(CameraHandle : prHandle) : prResponse; stdcall;


  prDisconnectCamera = function(CameraHandle : prHandle) : prResponse; stdcall;


{ Retrieving Camera Events  }
  prSetEventCB = function(CameraHandle : prHandle;
                          Context : prContext;
                          pEventData : Pointer) : prResponse; stdcall;

  prSetEventCallBack = function(CameraHandle : prHandle;
                                Context : prContext;
                                pSetEventCB : prSetEventCB) : prResponse; stdcall;


  prClearEventCallBack = function(CameraHandle : prHandle) : prResponse; stdcall;


{ Retrieving Camera Device Performance Information  }
  prGetDeviceInfo = function(CameraHandle : prHandle;
                             var pBufferSize : prUInt32;
                             pDeviceInfo : Pointer) : prResponse; stdcall;


{-----------------------------------------------------------------------
	Remote Release Control Functions
------------------------------------------------------------------------ }

{ Basic Functions  }
  prInitiateReleaseControl = function(CameraHandle : prHandle) : prResponse; stdcall;

  prTerminateReleaseControl = function(CameraHandle : prHandle) : prResponse; stdcall;

  prRC_Release = function(CameraHandle : prHandle) : prResponse; stdcall;

  prGetFileDataCB = function(CameraHandle : prHandle;
                             ObjectHandle : prObjectHandle;
                             Context : prContext;
                             var pProgress : prProgress) : prResponse; stdcall;

  prRC_GetReleasedData = function(CameraHandle : prHandle;
                                  ObjectHandle : prObjectHandle;
                                  EventCode : prptpEventCode;
                                  TransSize : prUInt32;
                                  Context : prContext;
                                  pGetFileDataCB : prGetFileDataCB) : prResponse; stdcall;

  prRC_GetNumAvailableShot = function(CameraHandle : prHandle;
                                      var pNum : prUInt32) : prResponse; stdcall;

{ Viewfinder Function  }
  prViewFinderCB = function(CameraHandle : prHandle;
                            Context : prContext;
                            Size : prUInt32;
                            pVFData : Pointer) : prResponse; stdcall;

  prRC_StartViewFinder = function(CameraHandle : prHandle;
                                  Context : prContext;
                                  pViewFinderCB : prViewFinderCB) : prResponse; stdcall;

  prRC_TermViewFinder = function(CameraHandle : prHandle) : prResponse; stdcall;

  prRC_DoAeAfAwb = function(CameraHandle : prHandle;
                            ResetFlag : prptpAeAfAwbResetFlag) : prResponse; stdcall;

{ AF Lock Settings  }
  prRC_FocusLock = function(CameraHandle : prHandle) : prResponse; stdcall;

  prRC_FocusUnlock = function(CameraHandle : prHandle) : prResponse; stdcall;

{-----------------------------------------------------------------------
	Device Property Functions
------------------------------------------------------------------------ }

  prGetDevicePropDesc = function(CameraHandle : prHandle;
                                 DevicePropCode : prptpDevicePropCode;
                                 var pBufferSize : prUInt32;
                                 pDevicePropDesc : Pointer) : prResponse; stdcall;

  prGetDevicePropValue = function(CameraHandle : prHandle;
                                  DevicePropCode : prptpDevicePropCode;
                                  var pBufferSize : prUInt32;
                                  pDeviceProperty : Pointer) : prResponse; stdcall;

  prSetDevicePropValue = function(CameraHandle : prHandle;
                                  DevicePropCode : prptpDevicePropCode;
                                  DataSize : prUInt32;
                                  pDeviceProperty : Pointer) : prResponse; stdcall;

  prRC_GetChangedReleaseParamesList = function(CameraHandle : prHandle;
                                               var pBufferSize : prUInt32;
                                               pParamsList : Pointer) : prResponse; stdcall;

{-----------------------------------------------------------------------
	prFunctions
	  Structure of function pointers
------------------------------------------------------------------------ }

const
  prCURRENT_FUNCTABLE_VERSION = $00000001;

type
  prFunctions = packed record
    Version : prUInt32;	{ Version of this structure  }

  { Basic Functions  }
   	pStartSDK       : prStartSDK;
    pFinishSDK      : prFinishSDK;
    pGetDllsVersion : prGetDllsVersion;

  { Basic Camera Device Functios  }
    pGetDeviceList        : prGetDeviceList;
    pCreateCameraObject   : prCreateCameraObject;
    pDestroyCameraObject  : prDestroyCameraObject;
    pConnectCamera        : prConnectCamera;
    pDisconnectCamera     : prDisconnectCamera;
    pSetEventCallBack     : prSetEventCallBack;
    pClearEventCallBack   : prClearEventCallBack;
    pGetDeviceInfo        : prGetDeviceInfo;

  { Remote Release Control Functions  }
    pInitiateReleaseControl   : prInitiateReleaseControl;
    pTerminateReleaseControl  : prTerminateReleaseControl;
    pRC_Release               : prRC_Release;
    pRC_GetReleasedData       : prRC_GetReleasedData;
    pRC_GetNumAvailableShot   : prRC_GetNumAvailableShot;
    pRC_StartViewFinder       : prRC_StartViewFinder;
    pRC_TermViewFinder        : prRC_TermViewFinder;
    pRC_DoAeAfAwb             : prRC_DoAeAfAwb;
    pRC_FocusLock             : prRC_FocusLock;
    pRC_FocusUnlock           : prRC_FocusUnlock;

  { Device Property Functions  }
    pGetDevicePropDesc                : prGetDevicePropDesc;
    pGetDevicePropValue               : prGetDevicePropValue;
    pSetDevicePropValue               : prSetDevicePropValue;
    pRC_GetChangedReleaseParamesList  : prRC_GetChangedReleaseParamesList;
  end;

{------------------------------------------------------------------------
	Function Pointer to get the structure of function pointers
------------------------------------------------------------------------ }
  prGetFunctions = function(var pFunctions : prFunctions) : prResponse; stdcall;

implementation

end.
