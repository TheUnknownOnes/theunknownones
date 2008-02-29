#ifndef _PR_API_H_
#define _PR_API_H_

#include "prType.h"
#include "prFuncType.h"
#include "prError.h"

#ifdef macintosh
 #if PRAGMA_STRUCT_ALIGN
  #pragma options align=mac68k
 #endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

/****************************************************************************
 ****************************************************************************
 *              PowerShot RemoteCapture SDK : Function Definitions          *
 ****************************************************************************
 ****************************************************************************/

/*-----------------------------------------------------------------------
	Basic Functions
------------------------------------------------------------------------*/

prCAPI  PR_StartSDK(
	prVoid
);

prCAPI  PR_FinishSDK(
	prVoid
);

prCAPI  PR_GetDllsVersion( 
	prUInt32*		pBufferSize,
	prDllsVerInfo*	pDllVersion
);

prCAPI  PR_GetFunctions(
	prFunctions*	pFunctions
);

/*-----------------------------------------------------------------------
	Basic Camera Device Functios
------------------------------------------------------------------------*/

/* Enumeration of Camera Devices */
prCAPI  PR_GetDeviceList( 
	prUInt32*		pBufferSize,
	prDeviceList*	pDeviceList
);

/* Creation/Deletion of Camera Handles */
prCAPI  PR_CreateCameraObject(
	prDeviceInfoTable*	pDeviceInfo,
	prHandle*			pCameraHandle
);

prCAPI  PR_DestroyCameraObject(
	prHandle	CameraHandle
);

/* Connecting/Disconnecting Camera Devices */
prCAPI  PR_ConnectCamera(
	prHandle	CameraHandle
);

prCAPI  PR_DisconnectCamera(
	prHandle	CameraHandle
);

/* Retrieving Camera Events */
prCAPI  PR_SetEventCallBack(
	prHandle	CameraHandle,
	prContext	Context,
	prSetEventCB*	pSetEventCB
);

prCAPI  PR_ClearEventCallBack(
	prHandle	CameraHandle
);

/* Retrieving Camera Device Performance Information */
prCAPI  PR_GetDeviceInfo(
	prHandle	CameraHandle,
	prUInt32*	pBufferSize,
	prVoid*		pDeviceInfo
);

/*-----------------------------------------------------------------------
	Remote Release Control Functions
------------------------------------------------------------------------*/

/* Basic Functions */
prCAPI  PR_InitiateReleaseControl(
	prHandle	CameraHandle
);

prCAPI  PR_TerminateReleaseControl(
	prHandle	CameraHandle
);

prCAPI PR_RC_Release(
	prHandle        CameraHandle
);

prCAPI PR_RC_GetReleasedData(
	prHandle        CameraHandle,
	prObjectHandle  ObjectHandle,
	prptpEventCode  EventCode,
	prUInt32        TransSize,
	prContext		Context,
	prGetFileDataCB*	pGetFileDataCB
);

prCAPI PR_RC_GetNumAvailableShot(
	prHandle       CameraHandle,
	prUInt32*      pNum
);

/* Viewfinder Function */
prCAPI PR_RC_StartViewFinder(
	prHandle        CameraHandle,
	prContext		Context,
	prViewFinderCB*	pViewFinderCB
);

prCAPI PR_RC_TermViewFinder(
	prHandle        CameraHandle
);

prCAPI  PR_RC_DoAeAfAwb(
	prHandle				CameraHandle,
	prptpAeAfAwbResetFlag	ResetFlag
);

/* AF Lock Settings */
prCAPI  PR_RC_FocusLock(
	prHandle	CameraHandle
);

prCAPI  PR_RC_FocusUnlock(
	prHandle	CameraHandle
);

/*-----------------------------------------------------------------------
	Device Property Functions
------------------------------------------------------------------------*/

prCAPI  PR_GetDevicePropDesc(
	prHandle				CameraHandle,
	prptpDevicePropCode		DevicePropCode,
	prUInt32*				pBufferSize,
	prVoid*					pDevicePropDesc
);

prCAPI  PR_GetDevicePropValue(
	prHandle			CameraHandle,
	prptpDevicePropCode	DevicePropCode,
	prUInt32*			pBufferSize,
	prVoid*				pDeviceProperty
);

prCAPI  PR_SetDevicePropValue(
	prHandle      		CameraHandle,
	prptpDevicePropCode	DevicePropCode,
	prUInt32			DataSize,
	prVoid*				pDeviceProperty
);

prCAPI  PR_RC_GetChangedReleaseParamesList(
	prHandle        CameraHandle,
	prUInt32*      	pBufferSize,
	prVoid*         pParamsList
);

/*-----------------------------------------------------------------------*/

#ifdef macintosh
 #if PRAGMA_STRUCT_ALIGN
  #pragma options align=reset
 #endif
#endif 

#ifdef __cplusplus
}
#endif

#endif

