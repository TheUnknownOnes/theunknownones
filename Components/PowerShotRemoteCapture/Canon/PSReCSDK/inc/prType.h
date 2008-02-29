#ifndef _PR_TYPE_H_
#define _PR_TYPE_H_

#ifdef _MSC_VER
#	if	900 <= _MSC_VER
	#	pragma message( "push, _PR_TYPE_H_PACK_, 1" )
	#	pragma pack( push, _PR_TYPE_H_PACK_, 1 )
	#	pragma warning( disable : 4250 4355 4244 4005)
#	endif	/* 900 <= _MSC_VER */
#elif defined __WATCOMC__
	#pragma pack(__push,1);
#endif

#ifdef macintosh
 #if PRAGMA_STRUCT_ALIGN
  #pragma options align=mac68k
 #endif
#endif

/****************************************************************************
 ****************************************************************************
 *            PowerShot RemoteCapture SDK : Data Type Definitions           *
 ****************************************************************************
 ****************************************************************************/

/* Basic Data Types */
typedef void				prVoid;
typedef unsigned char		prUInt8;
typedef			 char		prInt8;
typedef          char		prChar;
#ifndef macintosh
typedef unsigned short		prWChar;
#endif
typedef unsigned short		prUInt16;
typedef          short		prInt16;
typedef unsigned long		prUInt32;
typedef          long		prInt32; 
typedef          float		prFloat32;
#ifdef macintosh
	typedef UInt64				prUInt64;
#else
	typedef unsigned __int64	prUInt64;
#endif
typedef prUInt32		 	prResponse;
typedef prUInt16 			prBoolean;
typedef prUInt32			prTime;
typedef prUInt32 			prEventID;  
typedef prUInt32			prContext;

#ifdef macintosh
typedef FSSpec				prFSSpec;
#endif

/* Handles */
#ifdef macintosh
	typedef prUInt32		prHWND;
#else
	typedef HWND			prHWND;
#endif
typedef prUInt32			prHandle;
typedef prUInt32			prObjectHandle;


#ifdef macintosh
	#define prSTDCALL	
#else
	#define prSTDCALL	__stdcall
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef PRSDK_EXPORT
#define prCAPI prResponse __declspec(dllexport) prSTDCALL
#else
#define prCAPI prResponse __declspec(dllimport) prSTDCALL
#endif

#ifdef __cplusplus
}
#endif


#define prANY				1

#define	prPTP_DATA_BUFFER_SIZE					(1024*1024)

/* Definition of Opration Code */
typedef prUInt16	prOperationCode;
	#define prPTP_INITIATE_RELEASE_CONTROL				0x9008
	#define prPTP_TERMINATE_RELEASE_CONTROL				0x9009
	#define prPTP_RC_INITIATE_VIEW_FINDER				0x900B
	#define prPTP_RC_TERMINATE_VIEW_FINDER				0x900C
	#define prPTP_RC_RELEASE_DO_AE_AF_AWB				0x900D
	#define prPTP_RC_FOCUS_LOCK							0x9014
	#define prPTP_RC_FOCUS_UNLOCK						0x9015
	#define prPTP_RC_CAPTURE							0x901A
	#define prPTP_RC_GET_CHANGED_RELEASE_PARAMS_LIST	0x9020

/* Progress Message */
typedef prInt32	prProgressMsg;
	#define prMSG_DATA_HEADER						0x0001
	#define prMSG_DATA								0x0002
	#define prMSG_TERMINATION						0x0004

/* Progress Status*/
typedef prInt32	prProgressSts;

/* Event Codes */
typedef prUInt16	prptpEventCode;
	#define prPTP_DEVICE_PROP_CHANGED				0x4006		/* Deveice property has been changed. */
	#define prPTP_CAPTURE_COMPLETE					0x400D		/* Capture has finished. */
	#define prPTP_SHUTDOWN_CF_GATE_WAS_OPENED		0xC001		/* The Device has shut down due to the opening of the SD card cover.*/
	#define prPTP_RESET_HW_ERROR					0xC005		/* The device has generated a hardware error. */
	#define prPTP_ABORT_PC_EVF						0xC006		/* The Viewfinder mode has been cancelled. */
	#define prPTP_ENABLE_PC_EVF						0xC007		/* The Viewfinder mode has been enablede. */
	#define prPTP_FULL_VIEW_RELEASED				0xC008		/* Transfer timing of main image data */
	#define prPTP_THUMBNAIL_RELEASED				0xC009		/* Transfer timing of thumbnail image data */
	#define prPTP_CHANGE_BATTERY_STATUS				0xC00A		/* The power condition of the camera has changed. */
	#define prPTP_PUSHED_RELEASE_SW					0xC00B		/* User has pressed the release swtich on camera. */
	#define prPTP_RC_PROP_CHANGED					0xC00C		/* A group of properties relating to release control have been changed. */
	#define prPTP_RC_ROTATION_ANGLE_CHANGED			0xC00D		/* The angle of rotation of the camera has been changed. */
	#define prPTP_RC_CHANGED_BY_CAM_UI				0xC00E		/* An operation control on the camera has been operated.*/
	#define prCAL_SHUTDOWN							0xD001		/* Shutdown */

/* Deveice Property Codes */
typedef prUInt16	prptpDevicePropCode;
	#define prPTP_DEV_PROP_BUZZER							0xD001	/* Set on/off the device buzzer */
	#define prPTP_DEV_PROP_BATTERY_KIND						0xD002	/* Type of the battery */
	#define prPTP_DEV_PROP_BATTERY_STATUS					0xD003	/* Buttery Status */
	#define prPTP_DEV_PROP_COMP_QUALITY						0xD006	/* Image quality */
	#define prPTP_DEV_PROP_FULLVIEW_FILE_FORMAT				0xD007	/* Image type */
	#define prPTP_DEV_PROP_IMAGE_SIZE						0xD008	/* Image size */
	#define prPTP_DEV_PROP_SELFTIMER						0xD009	/* Self-timer*/
	#define prPTP_DEV_PROP_STROBE_SETTING					0xD00A	/* Strobe setting */
	#define prPTP_DEV_PROP_BEEP								0xD00B	/* Buzzer setting */
	#define prPTP_DEV_PROP_EXPOSURE_MODE					0xD00C	/* Exposure mode setting */
	#define prPTP_DEV_PROP_IMAGE_MODE						0xD00D	/* Image mode setting */
	#define prPTP_DEV_PROP_DRIVE_MODE						0xD00E	/* Drive mode */
	#define prPTP_DEV_PROP_EZOOM							0xD00F	/* Electonic zoom setting */
	#define prPTP_DEV_PROP_ML_WEI_MODE						0xD010	/* Metering method */
	#define prPTP_DEV_PROP_AF_DISTANCE						0xD011	/* Search range in the AF mode */
	#define prPTP_DEV_PROP_FOCUS_POINT_SETTING				0xD012	/* Selection mode for focusing point */
	#define prPTP_DEV_PROP_WB_SETTING						0xD013	/* White balance setting */
	#define prPTP_DEV_PROP_SLOW_SHUTTER_SETTING				0xD014	/* Slow Shutter setting */
	#define prPTP_DEV_PROP_AF_MODE							0xD015	/* Auto focus mode setting */
	#define prPTP_DEV_PROP_IMAGE_STABILIZATION				0xD016	/* Image stabilization processing */
	#define prPTP_DEV_PROP_CONTRAST							0xD017	/* Contrast */
	#define prPTP_DEV_PROP_COLOR_GAIN						0xD018	/* Color Compensation */
	#define prPTP_DEV_PROP_SHARPNESS						0xD019	/* Sharpness */
	#define prPTP_DEV_PROP_SENSITIVITY						0xD01A	/* Sensitivity */
	#define prPTP_DEV_PROP_PARAMETER_SET					0xD01B	/* Development parameter setting */
	#define prPTP_DEV_PROP_ISO								0xD01C	/* ISO value */
	#define prPTP_DEV_PROP_AV								0xD01D	/* Av value */
	#define prPTP_DEV_PROP_TV								0xD01E	/* Tv value */
	#define prPTP_DEV_PROP_EXPOSURE_COMP					0xD01F	/* Exposure compensation value */
	#define prPTP_DEV_PROP_FLASH_COMP						0xD020	/* Flash exposure compensation value */
	#define prPTP_DEV_PROP_AEB_EXPOSURE_COMP				0xD021	/* AEB exposure compensation value */
	#define prPTP_DEV_PROP_AV_OPEN							0xD023	/* Open aperture value */
	#define prPTP_DEV_PROP_AV_MAX							0xD024	/* maximum aperture value */
	#define prPTP_DEV_PROP_FOCAL_LENGTH						0xD025	/* Value corresponding to the current focal distance multiplied by FocalLengthDenominator */
	#define prPTP_DEV_PROP_FOCAL_LENGTH_TELE				0xD026	/* Value corresponding to the telescopic focal distance multiplied by FocalLengthDenominator */
	#define prPTP_DEV_PROP_FOCAL_LENGTH_WIDE				0xD027	/* Value corresponding to the wide-angle focus distance multiplied by FocalLengthDenominator */
	#define prPTP_DEV_PROP_FOCAL_LENGTH_DENOMI				0xD028	/* Focus information multiplier value */
	#define prPTP_DEV_PROP_CAPTURE_TRANSFER_MODE			0xD029	/* Image transfer mode to be applied at caputre */
	#define prPTP_DEV_PROP_ZOOM_POS							0xD02A	/* Current zoom position*/

	#define prPTP_DEV_PROP_SUPPORTED_SIZE					0xD02C	/* Support size */
	#define prPTP_DEV_PROP_SUPPORTED_THUMB_SIZE				0xD02D	/* Thumbnail size supported by the device */
	#define prPTP_DEV_PROP_FIRMWARE_VERSION					0xD031	/* Version of the camera device firmware */
	#define prPTP_DEV_PROP_CAMERA_MODEL_NAME				0xD032	/* Camera model */
	#define prPTP_DEV_PROP_OWNER_NAME						0xD033	/* Owner name */
	#define prPTP_DEV_PROP_CAMERA_TIME						0xD034	/* Current time information in the device */
	#define prPTP_DEV_PROP_CAMERA_OUTPUT					0xD036	/* Destination of image signal output in the Viewfinder mode */
	#define prPTP_DEV_PROP_DISP_AV							0xD037	/* How to display the Av value */
	#define prPTP_DEV_PROP_AV_OPEN_APEX						0xD038	/* Open aperture value */
	#define prPTP_DEV_PROP_EZOOM_SIZE						0xD039	/* Horizontal size of image to be cut out from CCD image using electronic zoom */
	#define prPTP_DEV_PROP_ML_SPOT_POS						0xD03A	/* Spot metering positon */
	#define prPTP_DEV_PROP_DISP_AV_MAX						0xD03B	/* How to display the maximin Av value */
	#define prPTP_DEV_PROP_AV_MAX_APEX						0xD03C	/* minimum aperture value */
	#define prPTP_DEV_PROP_EZOOM_START_POS					0xD03D	/* Zoom position at which the electornic zoom range starts */
	#define prPTP_DEV_PROP_FOCAL_LENGTH_OF_TELE				0xD03E	/* Focal distance at the optical telescopic end */
	#define prPTP_DEV_PROP_EZOOM_SIZE_OF_TELE				0xD03F	/* Horizontal size of image to be cut out from CCD image at the telescopic end of the electronic zoom range */
	#define prPTP_DEV_PROP_PHOTO_EFFECT						0xD040	/* Photo effect */
	#define prPTP_DEV_PROP_AF_LIGHT							0xD041	/* ON/OFF of AF assist light */
	#define prPTP_DEV_PROP_FLASH_QUANTITY					0xD042	/* Number of flash levels that can be set in the manual mode */
	#define prPTP_DEV_PROP_ROTATION_ANGLE					0xD043	/* Angle of rotation detected by the gravity sensor */
	#define prPTP_DEV_PROP_ROTATION_SENCE					0xD044	/* Whether the gravity sensor is enable or disable */
	#define prPTP_DEV_PROP_IMEGE_FILE_SIZE					0xD048	/* Image file size supported be the camera */
	#define prPTP_DEV_PROP_CAMERA_MODEL_ID					0xD049	/* Camera model ID */

/* AE,AF,AWB Reset flag */
typedef prUInt32	prptpAeAfAwbResetFlag;
	#define prptpAEAFAWB_RESET_AE		0x00000001		/* AE Reset */
	#define prptpAEAFAWB_RESET_AF		0x00000002		/* AF Reset */
	#define prptpAEAFAWB_RESET_AWB		0x00000004		/* AWB Reset */

/* Port type */
typedef prUInt16	prPorttype;
	#define	prPORTTYPE_WIA			0x0001		/*	WIA	*/
	#define	prPORTTYPE_STI			0x0002		/*	STI	*/

#define prGENERATION_CAMERA_MASK		0xff00
#define prSUB_GENERATION_CAMERA(gen)	((gen&prGENERATION_CAMERA_MASK)>>8)

/* Object type */
typedef prUInt16	prptpObjectFormatCode;
	#define prPTP_EXIF_JPEG				0x3801		/* EXIF JPEG */
	#define prPTP_CRW					0xB101		/* RAW */


/****************************************************************************
 ****************************************************************************
 *              PowerShot RemoteCapture SDK : Structers                     *
 ****************************************************************************
 ****************************************************************************/

typedef struct{
	prWChar ModuleName[512];				/* Module name (512 characters)	*/
	prWChar Version[32];					/* Version (32 characters) */
}prVerInfo;

typedef struct{
	prUInt32  Entry;						/* Number of modules included in this structure */
	prVerInfo VerInfo[prANY]; 				/* Array of file version number information of PS-ReC SDK modules */
}prDllsVerInfo;

typedef struct{
	prWChar           DeviceInternalName[512];	/* Internal devicve name (512 characters) */
	prWChar    	      ModelName[32];          	/* Camera model name (32 characters) */
	prUInt16	      Generation;				/* Camera generation number */
	prUInt32		  Reserved1;				/* Reserved */
	prUInt32	      ModelID;					/* Camera model ID */
	prUInt16		  Reserved2;				/* Reserved */
	prPorttype	      PortType;					/* Port type 0x01ÅFWIA / 0x02ÅFSTI */
	prUInt32		  Reserved3;				/* Reserved */
}prDeviceInfoTable;

typedef struct{
	prUInt32	NumList;					/* Number of camera device information included in this structure */
	prDeviceInfoTable DeviceInfo[prANY];	/* Camera device information */
}prDeviceList;

typedef struct{
	prProgressMsg lMessage;					/* Message */
	prProgressSts lStatus;					/* Status */
	prUInt32	  lPercentComplete;			/* The uint of this parameter is percent */
	prUInt32	  lOffset;					/* Offset */
	prUInt32	  lLength;					/* Size */
	prUInt32	  lReserved;				/* Reserved */
	prUInt32	  lResLength;				/* Reserved */
	prUInt8 *	  pbData;					/* Pointer to the buffer in which the transferred data is stored. */
}prProgress;

#ifdef macintosh
 #if PRAGMA_STRUCT_ALIGN
  #pragma options align=reset
 #endif
#endif 


#ifdef _MSC_VER
#	if	900 <= _MSC_VER
	#	pragma message( "pop, _PR_TYPE_H_PACK_" )
	#	pragma warning( default : 4250 4355 4244 4005) 
	#	pragma pack( pop, _PR_TYPE_H_PACK_ )
#	endif	/* 900 <= _MSC_VER */
#elif defined __WATCOMC__
	#pragma pack(__pop);
#endif


#endif

