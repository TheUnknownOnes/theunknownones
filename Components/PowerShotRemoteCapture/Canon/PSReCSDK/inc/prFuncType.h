#ifndef _PR_FNC_TYPE_H_
#define _PR_FNC_TYPE_H_

#ifdef macintosh
 #if PRAGMA_STRUCT_ALIGN
  #pragma options align=mac68k
 #endif
#endif


/****************************************************************************
 ****************************************************************************
 *              PowerShot RemoteCapture SDK : Function Pointers             *
 ****************************************************************************
 ****************************************************************************/

/*-----------------------------------------------------------------------
	Basic Functions
------------------------------------------------------------------------*/

typedef prResponse prSTDCALL  prStartSDK(
	prVoid
);

typedef prResponse prSTDCALL  prFinishSDK(
	prVoid
);

typedef prResponse prSTDCALL  prGetDllsVersion( 
	prUInt32*       pBufferSize,
	prDllsVerInfo*  pDllVersion
);

/*-----------------------------------------------------------------------
	Basic Camera Device Functios
------------------------------------------------------------------------*/

/* Enumeration of Camera Devices */
typedef prResponse prSTDCALL  prGetDeviceList( 
	prUInt32*      pBufferSize,
	prDeviceList*  pDeviceList
);

/* Creation/Deletion of Camera Handles */
typedef prResponse prSTDCALL  prCreateCameraObject(
	prDeviceInfoTable* pDeviceInfo,
	prHandle*		   pCameraHandle
);

typedef prResponse prSTDCALL  prDestroyCameraObject(
	prHandle CameraHandle
);

/* Connecting/Disconnecting Camera Devices */
typedef prResponse prSTDCALL  prConnectCamera(
	prHandle CameraHandle
);

typedef prResponse prSTDCALL  prDisconnectCamera(
	prHandle CameraHandle
);

/* Retrieving Camera Events */
typedef prResponse prSTDCALL  prSetEventCB(
	prHandle 	CameraHandle,
	prContext 	Context,
	prVoid* 	pEventData
);

typedef prResponse prSTDCALL  prSetEventCallBack(
	prHandle       CameraHandle,
	prContext      Context,
	prSetEventCB*  pSetEventCB
);

typedef prResponse prSTDCALL  prClearEventCallBack(
	prHandle CameraHandle
);

/* Retrieving Camera Device Performance Information */
typedef prResponse prSTDCALL  prGetDeviceInfo(
	prHandle       CameraHandle,
	prUInt32*      pBufferSize,
	prVoid*        pDeviceInfo
);


/*-----------------------------------------------------------------------
	Remote Release Control Functions
------------------------------------------------------------------------*/

/* Basic Functions */
typedef prResponse prSTDCALL  prInitiateReleaseControl(
	prHandle CameraHandle
);

typedef prResponse prSTDCALL  prTerminateReleaseControl(
	prHandle CameraHandle
);

typedef prResponse prSTDCALL  prRC_Release(
	prHandle        CameraHandle
);

typedef prResponse prSTDCALL  prGetFileDataCB(
	prHandle       CameraHandle,
	prObjectHandle ObjectHandle,
	prContext      Context,
	prProgress*    pProgress
);

typedef prResponse prSTDCALL  prRC_GetReleasedData(
	prHandle       		CameraHandle,
	prObjectHandle		ObjectHandle,
	prptpEventCode		EventCode,
	prUInt32    	    TransSize,
	prContext			Context,
	prGetFileDataCB*	pGetFileDataCB
);

typedef prResponse prSTDCALL  prRC_GetNumAvailableShot(
	prHandle       CameraHandle,
	prUInt32*      pNum
);

/* Viewfinder Function */
typedef prResponse prSTDCALL  prViewFinderCB (
	prHandle CameraHandle,
	prContext Context, 
	prUInt32 Size,
	prVoid * pVFData
);

typedef prResponse prSTDCALL  prRC_StartViewFinder(
	prHandle        CameraHandle,
	prContext		Context,
	prViewFinderCB*	pViewFinderCB
);

typedef prResponse prSTDCALL  prRC_TermViewFinder(
	prHandle        CameraHandle
);

typedef prResponse prSTDCALL  prRC_DoAeAfAwb(
	prHandle              CameraHandle,
	prptpAeAfAwbResetFlag ResetFlag
);

/* AF Lock Settings */
typedef prResponse prSTDCALL  prRC_FocusLock(
	prHandle CameraHandle
);

typedef prResponse prSTDCALL  prRC_FocusUnlock(
	prHandle CameraHandle
);

/*-----------------------------------------------------------------------
	Device Property Functions
------------------------------------------------------------------------*/

typedef prResponse prSTDCALL  prGetDevicePropDesc(
	prHandle      CameraHandle,
	prptpDevicePropCode		DevicePropCode,
	prUInt32*     pBufferSize,
	prVoid*       pDevicePropDesc
);

typedef prResponse prSTDCALL  prGetDevicePropValue(
	prHandle      CameraHandle,
	prptpDevicePropCode		DevicePropCode,
	prUInt32*     pBufferSize,
	prVoid*       pDeviceProperty
);

typedef prResponse prSTDCALL  prSetDevicePropValue(
	prHandle      CameraHandle,
	prptpDevicePropCode		DevicePropCode,
	prUInt32	  DataSize,
	prVoid*       pDeviceProperty
);

typedef prResponse prSTDCALL  prRC_GetChangedReleaseParamesList(
	prHandle        CameraHandle,
	prUInt32*      	pBufferSize,
	prVoid*         pParamsList
);

/*-----------------------------------------------------------------------
	prFunctions
	  Structure of function pointers
------------------------------------------------------------------------*/

#define prCURRENT_FUNCTABLE_VERSION (0x00000001)

typedef struct {
	prUInt32		Version;	/* Version of this structure */

/* Basic Functions */
	prStartSDK*								pStartSDK;
	prFinishSDK*							pFinishSDK;
	prGetDllsVersion*						pGetDllsVersion;

/* Basic Camera Device Functios */
	prGetDeviceList*						pGetDeviceList;
	prCreateCameraObject*					pCreateCameraObject;
	prDestroyCameraObject*					pDestroyCameraObject;
	prConnectCamera*						pConnectCamera;
	prDisconnectCamera*						pDisconnectCamera;
	prSetEventCallBack*						pSetEventCallBack;
	prClearEventCallBack*					pClearEventCallBack;
	prGetDeviceInfo* 						pGetDeviceInfo;

/* Remote Release Control Functions */
	prInitiateReleaseControl* 				pInitiateReleaseControl;
	prTerminateReleaseControl* 				pTerminateReleaseControl;
	prRC_Release*							pRC_Release;
	prRC_GetReleasedData*					pRC_GetReleasedData;
	prRC_GetNumAvailableShot*				pRC_GetNumAvailableShot;
	prRC_StartViewFinder*					pRC_StartViewFinder;
	prRC_TermViewFinder*					pRC_TermViewFinder;
	prRC_DoAeAfAwb* 						pRC_DoAeAfAwb;
	prRC_FocusLock* 						pRC_FocusLock;
	prRC_FocusUnlock* 						pRC_FocusUnlock;

/* Device Property Functions */
	prGetDevicePropDesc* 					pGetDevicePropDesc;
	prGetDevicePropValue* 					pGetDevicePropValue;
	prSetDevicePropValue* 					pSetDevicePropValue;
	prRC_GetChangedReleaseParamesList* 		pRC_GetChangedReleaseParamesList;

} prFunctions;

/*------------------------------------------------------------------------
	Function Pointer to get the structure of function pointers
------------------------------------------------------------------------*/
typedef prResponse prSTDCALL  prGetFunctions(
	 prFunctions* pFunctions
);


#ifdef macintosh
 #if PRAGMA_STRUCT_ALIGN
  #pragma options align=reset
 #endif
#endif 

#endif

