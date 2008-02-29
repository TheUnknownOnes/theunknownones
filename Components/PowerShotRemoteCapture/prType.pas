unit prType;

interface

uses
  Windows;

{****************************************************************************
 ****************************************************************************
 *            PowerShot RemoteCapture SDK : Data Type Definitions           *
 ****************************************************************************
 ****************************************************************************}

type
  // Basic Data Types
  prUInt8     = type Byte; PprUInt8 = ^prUInt8;
  prInt8      = type Byte;
  prChar      = type Char;
  prWChar     = type WCHAR;
  prUInt16    = type Word;
  prInt16     = type Smallint;
  prUInt32    = type Longword;
  prInt32     = type Integer;
  prFloat32   = type Single;
  prUInt64    = type Int64;
  prResponse  = type prUInt32;
  prBoolean   = type prUInt16;
  prTime      = type prUInt32;
  prEventID   = type prUInt32;
  prContext   = type prUInt32;

  // Handles
  prHWND          = type HWND;
  prHandle        = type prUInt32;
  prObjectHandle  = type prUInt32;

const
  prANY = 1;
  prPTP_DATA_BUFFER_SIZE = (1024 * 1024);

{ Definition of Opration Code }
type
  prOperationCode = type prUInt16;
const
  prPTP_INITIATE_RELEASE_CONTROL				    = $9008;
	prPTP_TERMINATE_RELEASE_CONTROL				    = $9009;
	prPTP_RC_INITIATE_VIEW_FINDER				      = $900B;
	prPTP_RC_TERMINATE_VIEW_FINDER				    = $900C;
	prPTP_RC_RELEASE_DO_AE_AF_AWB				      = $900D;
	prPTP_RC_FOCUS_LOCK							          = $9014;
	prPTP_RC_FOCUS_UNLOCK						          = $9015;
	prPTP_RC_CAPTURE							            = $901A;
	prPTP_RC_GET_CHANGED_RELEASE_PARAMS_LIST  = $9020;

{ Progress Message }
type
	prProgressMsg = type prInt32;
const
	prMSG_DATA_HEADER = $0001;
	prMSG_DATA				= $0002;
	prMSG_TERMINATION	= $0004;

{ Progress Status }
type
  prProgressSts = type prInt32;

{ Event Codes }
type
  prptpEventCode  = type prUInt16;
const
	prPTP_DEVICE_PROP_CHANGED				  = $4006;		{ Deveice property has been changed. }
	prPTP_CAPTURE_COMPLETE					  = $400D;		{ Capture has finished. }
	prPTP_SHUTDOWN_CF_GATE_WAS_OPENED = $C001;		{ The Device has shut down due to the opening of the SD card cover.}
	prPTP_RESET_HW_ERROR					    = $C005;		{ The device has generated a hardware error. }
	prPTP_ABORT_PC_EVF						    = $C006;		{ The Viewfinder mode has been cancelled. }
	prPTP_ENABLE_PC_EVF						    = $C007;		{ The Viewfinder mode has been enablede. }
	prPTP_FULL_VIEW_RELEASED				  = $C008;		{ Transfer timing of main image data }
	prPTP_THUMBNAIL_RELEASED				  = $C009;		{ Transfer timing of thumbnail image data }
	prPTP_CHANGE_BATTERY_STATUS				= $C00A;		{ The power condition of the camera has changed. }
	prPTP_PUSHED_RELEASE_SW					  = $C00B;		{ User has pressed the release swtich on camera. }
	prPTP_RC_PROP_CHANGED					    = $C00C;		{ A group of properties relating to release control have been changed. }
	prPTP_RC_ROTATION_ANGLE_CHANGED		= $C00D;		{ The angle of rotation of the camera has been changed. }
	prPTP_RC_CHANGED_BY_CAM_UI			  = $C00E;		{ An operation control on the camera has been operated.}
	prCAL_SHUTDOWN							      = $D001;		{ Shutdown }

{ Device Property Codes }
type
  prptpDevicePropCode = type prUInt16;
const
	prPTP_DEV_PROP_BUZZER							    = $D001;	{ Set on/off the device buzzer }
	prPTP_DEV_PROP_BATTERY_KIND						= $D002;	{ Type of the battery }
	prPTP_DEV_PROP_BATTERY_STATUS					= $D003;	{ Buttery Status }
	prPTP_DEV_PROP_COMP_QUALITY						= $D006;	{ Image quality }
	prPTP_DEV_PROP_FULLVIEW_FILE_FORMAT		= $D007;	{ Image type }
	prPTP_DEV_PROP_IMAGE_SIZE						  = $D008;	{ Image size }
	prPTP_DEV_PROP_SELFTIMER						  = $D009;	{ Self-timer}
	prPTP_DEV_PROP_STROBE_SETTING					= $D00A;	{ Strobe setting }
	prPTP_DEV_PROP_BEEP								    = $D00B;	{ Buzzer setting }
	prPTP_DEV_PROP_EXPOSURE_MODE					= $D00C;	{ Exposure mode setting }
	prPTP_DEV_PROP_IMAGE_MODE						  = $D00D;	{ Image mode setting }
	prPTP_DEV_PROP_DRIVE_MODE						  = $D00E;	{ Drive mode }
	prPTP_DEV_PROP_EZOOM							    = $D00F;	{ Electonic zoom setting }
	prPTP_DEV_PROP_ML_WEI_MODE						= $D010;	{ Metering method }
	prPTP_DEV_PROP_AF_DISTANCE						= $D011;	{ Search range in the AF mode }
	prPTP_DEV_PROP_FOCUS_POINT_SETTING		= $D012;	{ Selection mode for focusing point }
	prPTP_DEV_PROP_WB_SETTING						  = $D013;	{ White balance setting }
	prPTP_DEV_PROP_SLOW_SHUTTER_SETTING		= $D014;	{ Slow Shutter setting }
	prPTP_DEV_PROP_AF_MODE							  = $D015;	{ Auto focus mode setting }
	prPTP_DEV_PROP_IMAGE_STABILIZATION		= $D016;	{ Image stabilization processing }
	prPTP_DEV_PROP_CONTRAST							  = $D017;	{ Contrast }
	prPTP_DEV_PROP_COLOR_GAIN						  = $D018;	{ Color Compensation }
	prPTP_DEV_PROP_SHARPNESS						  = $D019;	{ Sharpness }
	prPTP_DEV_PROP_SENSITIVITY						= $D01A;	{ Sensitivity }
	prPTP_DEV_PROP_PARAMETER_SET					= $D01B;	{ Development parameter setting }
	prPTP_DEV_PROP_ISO								    = $D01C;	{ ISO value }
	prPTP_DEV_PROP_AV								      = $D01D;	{ Av value }
	prPTP_DEV_PROP_TV								      = $D01E;	{ Tv value }
	prPTP_DEV_PROP_EXPOSURE_COMP					= $D01F;	{ Exposure compensation value }
	prPTP_DEV_PROP_FLASH_COMP						  = $D020;	{ Flash exposure compensation value }
	prPTP_DEV_PROP_AEB_EXPOSURE_COMP			= $D021;	{ AEB exposure compensation value }
	prPTP_DEV_PROP_AV_OPEN							  = $D023;	{ Open aperture value }
	prPTP_DEV_PROP_AV_MAX							    = $D024;	{ maximum aperture value }
	prPTP_DEV_PROP_FOCAL_LENGTH						= $D025;	{ Value corresponding to the current focal distance multiplied by FocalLengthDenominator }
	prPTP_DEV_PROP_FOCAL_LENGTH_TELE			= $D026;	{ Value corresponding to the telescopic focal distance multiplied by FocalLengthDenominator }
	prPTP_DEV_PROP_FOCAL_LENGTH_WIDE			= $D027;	{ Value corresponding to the wide-angle focus distance multiplied by FocalLengthDenominator }
	prPTP_DEV_PROP_FOCAL_LENGTH_DENOMI		= $D028;	{ Focus information multiplier value }
	prPTP_DEV_PROP_CAPTURE_TRANSFER_MODE  = $D029;	{ Image transfer mode to be applied at caputre }
	prPTP_DEV_PROP_ZOOM_POS							  = $D02A;	{ Current zoom position}

	prPTP_DEV_PROP_SUPPORTED_SIZE				= $D02C;	{ Support size }
	prPTP_DEV_PROP_SUPPORTED_THUMB_SIZE	= $D02D;	{ Thumbnail size supported by the device }
	prPTP_DEV_PROP_FIRMWARE_VERSION			= $D031;	{ Version of the camera device firmware }
	prPTP_DEV_PROP_CAMERA_MODEL_NAME		= $D032;	{ Camera model }
	prPTP_DEV_PROP_OWNER_NAME					  = $D033;	{ Owner name }
	prPTP_DEV_PROP_CAMERA_TIME					= $D034;	{ Current time information in the device }
	prPTP_DEV_PROP_CAMERA_OUTPUT				= $D036;	{ Destination of image signal output in the Viewfinder mode }
	prPTP_DEV_PROP_DISP_AV							= $D037;	{ How to display the Av value }
	prPTP_DEV_PROP_AV_OPEN_APEX					= $D038;	{ Open aperture value }
	prPTP_DEV_PROP_EZOOM_SIZE						= $D039;	{ Horizontal size of image to be cut out from CCD image using electronic zoom }
	prPTP_DEV_PROP_ML_SPOT_POS					= $D03A;	{ Spot metering positon }
	prPTP_DEV_PROP_DISP_AV_MAX					= $D03B;	{ How to display the maximin Av value }
	prPTP_DEV_PROP_AV_MAX_APEX					= $D03C;	{ minimum aperture value }
	prPTP_DEV_PROP_EZOOM_START_POS			= $D03D;	{ Zoom position at which the electornic zoom range starts }
	prPTP_DEV_PROP_FOCAL_LENGTH_OF_TELE = $D03E;	{ Focal distance at the optical telescopic end }
	prPTP_DEV_PROP_EZOOM_SIZE_OF_TELE		= $D03F;	{ Horizontal size of image to be cut out from CCD image at the telescopic end of the electronic zoom range }
	prPTP_DEV_PROP_PHOTO_EFFECT					= $D040;	{ Photo effect }
	prPTP_DEV_PROP_AF_LIGHT							= $D041;	{ ON/OFF of AF assist light }
	prPTP_DEV_PROP_FLASH_QUANTITY				= $D042;	{ Number of flash levels that can be set in the manual mode }
	prPTP_DEV_PROP_ROTATION_ANGLE				= $D043;	{ Angle of rotation detected by the gravity sensor }
	prPTP_DEV_PROP_ROTATION_SENCE				= $D044;	{ Whether the gravity sensor is enable or disable }
	prPTP_DEV_PROP_IMEGE_FILE_SIZE			= $D048;	{ Image file size supported be the camera }
	prPTP_DEV_PROP_CAMERA_MODEL_ID			= $D049;	{ Camera model ID }

{ AE,AF,AWB Reset flag }
type
  prptpAeAfAwbResetFlag = type prUInt32;
const
	prptpAEAFAWB_RESET_AE	  = $00000001;		{ AE Reset }
	prptpAEAFAWB_RESET_AF   = $00000002;		{ AF Reset }
	prptpAEAFAWB_RESET_AWB  = $00000004;		{ AWB Reset }

{ Port type }
type
  prPorttype = type prUInt16;
const
	prPORTTYPE_WIA = $0001;		{	WIA	}
	prPORTTYPE_STI = $0002;		{	STI	}

const
  prGENERATION_CAMERA_MASK  = $ff00;

{ Object type }
type
  prptpObjectFormatCode = type prUInt16;
const
	prPTP_EXIF_JPEG = $3801;		{ EXIF JPEG }
	prPTP_CRW				= $B101;		{ RAW }

function prSUB_GENERATION_CAMERA(gen : prUInt16) : prUInt16;

{****************************************************************************
 ****************************************************************************
 *              PowerShot RemoteCapture SDK : Structers                     *
 ****************************************************************************
 ****************************************************************************}

type
  prVerInfo = packed record
    ModuleName  : array[0..511] of prWChar;				{ Module name (512 characters)	}
	  Version     : array[0..31] of prWChar;					{ Version (32 characters) }
  end;

  prDllsVerInfo = record
    Entry   : prUInt32;						                    { Number of modules included in this structure }
  	VerInfo : array[0..prANY-1] of prVerInfo; 				{ Array of file version number information of PS-ReC SDK modules }
  end;
  PprDllsVerInfo = ^prDllsVerInfo;

  prDeviceInfoTable = packed record
    DeviceInternalName  : array[0..511] of prWChar;	{ Internal devicve name (512 characters) }
    ModelName           : array[0..31] of prWChar;          	{ Camera model name (32 characters) }
    Generation          : prUInt16;				{ Camera generation number }
    Reserved1           : prUInt32;				{ Reserved }
    ModelID             : prUInt32;					{ Camera model ID }
    Reserved2           : prUInt16;				{ Reserved }
    PortType            : prPorttype;					{ Port type 0x01ÅFWIA / 0x02ÅFSTI }
    Reserved3           : prUInt32;				{ Reserved }
  end;

  prDeviceList = packed record
    NumList     : prUInt32;					{ Number of camera device information included in this structure }
  	DeviceInfo  : array[0..prANY-1] of prDeviceInfoTable;	{ Camera device information }
  end;
  PprDeviceList = ^prDeviceList;

  prProgress = packed record
    lMessage          : prProgressMsg;					{ Message }
    lStatus           : prProgressSts;					{ Status }
    lPercentComplete  : prUInt32;			{ The uint of this parameter is percent }
    lOffset           : prUInt32;					{ Offset }
    lLength           : prUInt32;					{ Size }
    lReserved         : prUInt32;				{ Reserved }
    lResLength        : prUInt32;				{ Reserved }
    pbData            : PprUInt8;					{ Pointer to the buffer in which the transferred data is stored. }
  end;


implementation

function prSUB_GENERATION_CAMERA(gen : prUInt16) : prUInt16;
begin
  Result := (gen and prGENERATION_CAMERA_MASK) shr 8
end;

end.
