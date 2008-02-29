unit psrcType;

interface

uses
  prType;

type
  prInt64    = type Int64;
  PprUInt8 = ^prUInt8;
  PprInt8 = ^prUInt8;
  PprUInt16 = ^prUInt16;
  PprInt16 = ^prUInt16;
  PprUInt32 = ^prUInt32;
  PprInt32 = ^prUInt32;
  PprUInt64 = ^prUInt64;
  PprInt64 = ^prUInt64;
  PprWChar = ^prWChar;

  TSDKConstEnumTranslation = record
    SDKConst : Integer;
    EnumVal : Byte;
    InfoText : String;
  end;
  TSDKConstEnumTranslationArray = array of TSDKConstEnumTranslation;


  TpsOperationCode = (psop_INITIATE_RELEASE_CONTROL,
                      psop_TERMINATE_RELEASE_CONTROL,
                      psop_RC_INITIATE_VIEW_FINDER,
                      psop_RC_TERMINATE_VIEW_FINDER,
                      psop_RC_RELEASE_DO_AE_AF_AWB,
                      psop_RC_FOCUS_LOCK,
                      psop_RC_FOCUS_UNLOCK,
                      psop_RC_CAPTURE,
                      psop_RC_GET_CHANGED_RELEASE_PARAMS_LIST);
  TpsOperationCodes = set of TpsOperationCode;

const
  psOperationCodes : array[0..8] of TSDKConstEnumTranslation =
   ((SDKConst : prPTP_INITIATE_RELEASE_CONTROL;            EnumVal : Byte(psop_INITIATE_RELEASE_CONTROL);            InfoText : ''),
    (SDKConst : prPTP_TERMINATE_RELEASE_CONTROL;           EnumVal : Byte(psop_TERMINATE_RELEASE_CONTROL);           InfoText : ''),
    (SDKConst : prPTP_RC_INITIATE_VIEW_FINDER;             EnumVal : Byte(psop_RC_INITIATE_VIEW_FINDER);             InfoText : ''),
    (SDKConst : prPTP_RC_TERMINATE_VIEW_FINDER;            EnumVal : Byte(psop_RC_TERMINATE_VIEW_FINDER);            InfoText : ''),
    (SDKConst : prPTP_RC_RELEASE_DO_AE_AF_AWB;             EnumVal : Byte(psop_RC_RELEASE_DO_AE_AF_AWB);             InfoText : ''),
    (SDKConst : prPTP_RC_FOCUS_LOCK;                       EnumVal : Byte(psop_RC_FOCUS_LOCK);                       InfoText : ''),
    (SDKConst : prPTP_RC_FOCUS_UNLOCK;                     EnumVal : Byte(psop_RC_FOCUS_UNLOCK);                     InfoText : ''),
    (SDKConst : prPTP_RC_CAPTURE;                          EnumVal : Byte(psop_RC_CAPTURE);                          InfoText : ''),
    (SDKConst : prPTP_RC_GET_CHANGED_RELEASE_PARAMS_LIST;  EnumVal : Byte(psop_RC_GET_CHANGED_RELEASE_PARAMS_LIST);  InfoText : '')
    );

type
  TpsEventCode = (psec_DEVICE_PROP_CHANGED,
                  psec_CAPTURE_COMPLETE,
                  psec_SHUTDOWN_CF_GATE_WAS_OPENED,
                  psec_RESET_HW_ERROR,
                  psec_ABORT_PC_EVF,
                  psec_ENABLE_PC_EVF,
                  psec_FULL_VIEW_RELEASED ,
                  psec_THUMBNAIL_RELEASED,
                  psec_CHANGE_BATTERY_STATUS,
                  psec_PUSHED_RELEASE_SW,
                  psec_RC_PROP_CHANGED,
                  psec_RC_ROTATION_ANGLE_CHANGED,
                  psec_RC_CHANGED_BY_CAM_UI,
                  psec_CAL_SHUTDOWN);
  TpsEventCodes = set of TpsEventCode;

const
  psEventCodes : array[0..13] of TSDKConstEnumTranslation =
   ((SDKConst : prPTP_DEVICE_PROP_CHANGED;          EnumVal : Byte(psec_DEVICE_PROP_CHANGED);         InfoText : 'Deveice property has been changed. '),
    (SDKConst : prPTP_CAPTURE_COMPLETE;             EnumVal : Byte(psec_CAPTURE_COMPLETE);            InfoText : 'Capture has finished. '),
    (SDKConst : prPTP_SHUTDOWN_CF_GATE_WAS_OPENED;  EnumVal : Byte(psec_SHUTDOWN_CF_GATE_WAS_OPENED); InfoText : 'The Device has shut down due to the opening of the SD card cover.'),
    (SDKConst : prPTP_RESET_HW_ERROR;               EnumVal : Byte(psec_RESET_HW_ERROR);              InfoText : 'The device has generated a hardware error. '),
    (SDKConst : prPTP_ABORT_PC_EVF;                 EnumVal : Byte(psec_ABORT_PC_EVF);                InfoText : 'The Viewfinder mode has been cancelled. '),
    (SDKConst : prPTP_ENABLE_PC_EVF;                EnumVal : Byte(psec_ENABLE_PC_EVF);               InfoText : 'The Viewfinder mode has been enablede. '),
    (SDKConst : prPTP_FULL_VIEW_RELEASED;           EnumVal : Byte(psec_FULL_VIEW_RELEASED);          InfoText : 'Transfer timing of main image data '),
    (SDKConst : prPTP_THUMBNAIL_RELEASED;           EnumVal : Byte(psec_THUMBNAIL_RELEASED);          InfoText : 'Transfer timing of thumbnail image data '),
    (SDKConst : prPTP_CHANGE_BATTERY_STATUS;        EnumVal : Byte(psec_CHANGE_BATTERY_STATUS);       InfoText : 'The power condition of the camera has changed. '),
    (SDKConst : prPTP_PUSHED_RELEASE_SW;            EnumVal : Byte(psec_PUSHED_RELEASE_SW);           InfoText : 'User has pressed the release swtich on camera. '),
    (SDKConst : prPTP_RC_PROP_CHANGED;              EnumVal : Byte(psec_RC_PROP_CHANGED);             InfoText : 'A group of properties relating to release control have been changed. '),
    (SDKConst : prPTP_RC_ROTATION_ANGLE_CHANGED;    EnumVal : Byte(psec_RC_ROTATION_ANGLE_CHANGED);   InfoText : 'The angle of rotation of the camera has been changed. '),
    (SDKConst : prPTP_RC_CHANGED_BY_CAM_UI;         EnumVal : Byte(psec_RC_CHANGED_BY_CAM_UI);        InfoText : 'An operation control on the camera has been operated.'),
    (SDKConst : prCAL_SHUTDOWN;                     EnumVal : Byte(psec_CAL_SHUTDOWN);                InfoText : 'Shutdown ')
   );

type
  TpsPropertyCode = ( pspc_BUZZER,
                      pspc_BATTERY_KIND,
                      pspc_BATTERY_STATUS,
                      pspc_COMP_QUALITY,
                      pspc_FULLVIEW_FILE_FORMAT,
                      pspc_IMAGE_SIZE,
                      pspc_SELFTIMER,
                      pspc_STROBE_SETTING,
                      pspc_BEEP,
                      pspc_EXPOSURE_MODE,
                      pspc_IMAGE_MODE,
                      pspc_DRIVE_MODE,
                      pspc_EZOOM,
                      pspc_ML_WEI_MODE,
                      pspc_AF_DISTANCE,
                      pspc_FOCUS_POINT_SETTING,
                      pspc_WB_SETTING,
                      pspc_SLOW_SHUTTER_SETTING,
                      pspc_AF_MODE,
                      pspc_IMAGE_STABILIZATION,
                      pspc_CONTRAST,
                      pspc_COLOR_GAIN,
                      pspc_SHARPNESS,
                      pspc_SENSITIVITY,
                      pspc_PARAMETER_SET,
                      pspc_ISO,
                      pspc_AV,
                      pspc_TV,
                      pspc_EXPOSURE_COMP,
                      pspc_FLASH_COMP,
                      pspc_AEB_EXPOSURE_COMP,
                      pspc_AV_OPEN,
                      pspc_AV_MAX,
                      pspc_FOCAL_LENGTH,
                      pspc_FOCAL_LENGTH_TELE,
                      pspc_FOCAL_LENGTH_WIDE,
                      pspc_FOCAL_LENGTH_DENOMI,
                      pspc_CAPTURE_TRANSFER_MODE,
                      pspc_ZOOM_POS,
                      pspc_SUPPORTED_SIZE,
                      pspc_SUPPORTED_THUMB_SIZE,
                      pspc_FIRMWARE_VERSION,
                      pspc_CAMERA_MODEL_NAME,
                      pspc_OWNER_NAME,
                      pspc_CAMERA_TIME,
                      pspc_CAMERA_OUTPUT,
                      pspc_DISP_AV,
                      pspc_AV_OPEN_APEX,
                      pspc_EZOOM_SIZE,
                      pspc_ML_SPOT_POS,
                      pspc_DISP_AV_MAX,
                      pspc_AV_MAX_APEX,
                      pspc_EZOOM_START_POS,
                      pspc_FOCAL_LENGTH_OF_TELE,
                      pspc_EZOOM_SIZE_OF_TELE,
                      pspc_PHOTO_EFFECT,
                      pspc_AF_LIGHT,
                      pspc_FLASH_QUANTITY,
                      pspc_ROTATION_ANGLE,
                      pspc_ROTATION_SENCE,
                      pspc_IMEGE_FILE_SIZE,
                      pspc_CAMERA_MODEL_ID);
  TpsPropertyCodes = set of TpsPropertyCode;

const
  psPropertyCodes : array[0..61] of TSDKConstEnumTranslation =
   ((SDKConst : prPTP_DEV_PROP_BUZZER;                EnumVal : Byte(pspc_BUZZER);                 InfoText : 'Set on/off the device buzzer '),
    (SDKConst : prPTP_DEV_PROP_BATTERY_KIND;          EnumVal : Byte(pspc_BATTERY_KIND);           InfoText : 'Type of the battery '),
    (SDKConst : prPTP_DEV_PROP_BATTERY_STATUS;        EnumVal : Byte(pspc_BATTERY_STATUS);         InfoText : 'Buttery Status '),
    (SDKConst : prPTP_DEV_PROP_COMP_QUALITY;          EnumVal : Byte(pspc_COMP_QUALITY);           InfoText : 'Image quality '),
    (SDKConst : prPTP_DEV_PROP_FULLVIEW_FILE_FORMAT;  EnumVal : Byte(pspc_FULLVIEW_FILE_FORMAT);   InfoText : 'Image type '),
    (SDKConst : prPTP_DEV_PROP_IMAGE_SIZE;            EnumVal : Byte(pspc_IMAGE_SIZE);             InfoText : 'Image size '),
    (SDKConst : prPTP_DEV_PROP_SELFTIMER;             EnumVal : Byte(pspc_SELFTIMER);              InfoText : 'Self-timer'),
    (SDKConst : prPTP_DEV_PROP_STROBE_SETTING;        EnumVal : Byte(pspc_STROBE_SETTING);         InfoText : 'Strobe setting '),
    (SDKConst : prPTP_DEV_PROP_BEEP;                  EnumVal : Byte(pspc_BEEP);                   InfoText : 'Buzzer setting '),
    (SDKConst : prPTP_DEV_PROP_EXPOSURE_MODE;         EnumVal : Byte(pspc_EXPOSURE_MODE);          InfoText : 'Exposure mode setting '),
    (SDKConst : prPTP_DEV_PROP_IMAGE_MODE;            EnumVal : Byte(pspc_IMAGE_MODE);             InfoText : 'Image mode setting '),
    (SDKConst : prPTP_DEV_PROP_DRIVE_MODE;            EnumVal : Byte(pspc_DRIVE_MODE);             InfoText : 'Drive mode '),
    (SDKConst : prPTP_DEV_PROP_EZOOM;                 EnumVal : Byte(pspc_EZOOM);                  InfoText : 'Electonic zoom setting '),
    (SDKConst : prPTP_DEV_PROP_ML_WEI_MODE;           EnumVal : Byte(pspc_ML_WEI_MODE);            InfoText : 'Metering method '),
    (SDKConst : prPTP_DEV_PROP_AF_DISTANCE;           EnumVal : Byte(pspc_AF_DISTANCE);            InfoText : 'Search range in the AF mode '),
    (SDKConst : prPTP_DEV_PROP_FOCUS_POINT_SETTING;   EnumVal : Byte(pspc_FOCUS_POINT_SETTING);    InfoText : 'Selection mode for focusing point '),
    (SDKConst : prPTP_DEV_PROP_WB_SETTING;            EnumVal : Byte(pspc_WB_SETTING);             InfoText : 'White balance setting '),
    (SDKConst : prPTP_DEV_PROP_SLOW_SHUTTER_SETTING;  EnumVal : Byte(pspc_SLOW_SHUTTER_SETTING);   InfoText : 'Slow Shutter setting '),
    (SDKConst : prPTP_DEV_PROP_AF_MODE;               EnumVal : Byte(pspc_AF_MODE);                InfoText : 'Auto focus mode setting '),
    (SDKConst : prPTP_DEV_PROP_IMAGE_STABILIZATION;   EnumVal : Byte(pspc_IMAGE_STABILIZATION);    InfoText : 'Image stabilization processing '),
    (SDKConst : prPTP_DEV_PROP_CONTRAST;              EnumVal : Byte(pspc_CONTRAST);               InfoText : 'Contrast '),
    (SDKConst : prPTP_DEV_PROP_COLOR_GAIN;            EnumVal : Byte(pspc_COLOR_GAIN);             InfoText : 'Color Compensation '),
    (SDKConst : prPTP_DEV_PROP_SHARPNESS;             EnumVal : Byte(pspc_SHARPNESS);              InfoText : 'Sharpness '),
    (SDKConst : prPTP_DEV_PROP_SENSITIVITY;           EnumVal : Byte(pspc_SENSITIVITY);            InfoText : 'Sensitivity '),
    (SDKConst : prPTP_DEV_PROP_PARAMETER_SET;         EnumVal : Byte(pspc_PARAMETER_SET);          InfoText : 'Development parameter setting '),
    (SDKConst : prPTP_DEV_PROP_ISO;                   EnumVal : Byte(pspc_ISO);                    InfoText : 'ISO value '),
    (SDKConst : prPTP_DEV_PROP_AV;                    EnumVal : Byte(pspc_AV);                     InfoText : 'Av value '),
    (SDKConst : prPTP_DEV_PROP_TV;                    EnumVal : Byte(pspc_TV);                     InfoText : 'Tv value '),
    (SDKConst : prPTP_DEV_PROP_EXPOSURE_COMP;         EnumVal : Byte(pspc_EXPOSURE_COMP);          InfoText : 'Exposure compensation value '),
    (SDKConst : prPTP_DEV_PROP_FLASH_COMP;            EnumVal : Byte(pspc_FLASH_COMP);             InfoText : 'Flash exposure compensation value '),
    (SDKConst : prPTP_DEV_PROP_AEB_EXPOSURE_COMP;     EnumVal : Byte(pspc_AEB_EXPOSURE_COMP);      InfoText : 'AEB exposure compensation value '),
    (SDKConst : prPTP_DEV_PROP_AV_OPEN;               EnumVal : Byte(pspc_AV_OPEN);                InfoText : 'Open aperture value '),
    (SDKConst : prPTP_DEV_PROP_AV_MAX;                EnumVal : Byte(pspc_AV_MAX);                 InfoText : 'maximum aperture value '),
    (SDKConst : prPTP_DEV_PROP_FOCAL_LENGTH;          EnumVal : Byte(pspc_FOCAL_LENGTH);           InfoText : 'Value corresponding to the current focal distance multiplied by FocalLengthDenominator '),
    (SDKConst : prPTP_DEV_PROP_FOCAL_LENGTH_TELE;     EnumVal : Byte(pspc_FOCAL_LENGTH_TELE);      InfoText : 'Value corresponding to the telescopic focal distance multiplied by FocalLengthDenominator '),
    (SDKConst : prPTP_DEV_PROP_FOCAL_LENGTH_WIDE;     EnumVal : Byte(pspc_FOCAL_LENGTH_WIDE);      InfoText : 'Value corresponding to the wide-angle focus distance multiplied by FocalLengthDenominator '),
    (SDKConst : prPTP_DEV_PROP_FOCAL_LENGTH_DENOMI;   EnumVal : Byte(pspc_FOCAL_LENGTH_DENOMI);    InfoText : 'Focus information multiplier value '),
    (SDKConst : prPTP_DEV_PROP_CAPTURE_TRANSFER_MODE; EnumVal : Byte(pspc_CAPTURE_TRANSFER_MODE);  InfoText : 'Image transfer mode to be applied at caputre '),
    (SDKConst : prPTP_DEV_PROP_ZOOM_POS;              EnumVal : Byte(pspc_ZOOM_POS);               InfoText : 'Current zoom position'),
    (SDKConst : prPTP_DEV_PROP_SUPPORTED_SIZE;        EnumVal : Byte(pspc_SUPPORTED_SIZE);         InfoText : 'Support size '),
    (SDKConst : prPTP_DEV_PROP_SUPPORTED_THUMB_SIZE;  EnumVal : Byte(pspc_SUPPORTED_THUMB_SIZE);   InfoText : 'Thumbnail size supported by the device '),
    (SDKConst : prPTP_DEV_PROP_FIRMWARE_VERSION;      EnumVal : Byte(pspc_FIRMWARE_VERSION);       InfoText : 'Version of the camera device firmware '),
    (SDKConst : prPTP_DEV_PROP_CAMERA_MODEL_NAME;     EnumVal : Byte(pspc_CAMERA_MODEL_NAME);      InfoText : 'Camera model '),
    (SDKConst : prPTP_DEV_PROP_OWNER_NAME;            EnumVal : Byte(pspc_OWNER_NAME);             InfoText : 'Owner name '),
    (SDKConst : prPTP_DEV_PROP_CAMERA_TIME;           EnumVal : Byte(pspc_CAMERA_TIME);            InfoText : 'Current time information in the device '),
    (SDKConst : prPTP_DEV_PROP_CAMERA_OUTPUT;         EnumVal : Byte(pspc_CAMERA_OUTPUT);          InfoText : 'Destination of image signal output in the Viewfinder mode '),
    (SDKConst : prPTP_DEV_PROP_DISP_AV;               EnumVal : Byte(pspc_DISP_AV);                InfoText : 'How to display the Av value '),
    (SDKConst : prPTP_DEV_PROP_AV_OPEN_APEX;          EnumVal : Byte(pspc_AV_OPEN_APEX);           InfoText : 'Open aperture value '),
    (SDKConst : prPTP_DEV_PROP_EZOOM_SIZE;            EnumVal : Byte(pspc_EZOOM_SIZE);             InfoText : 'Horizontal size of image to be cut out from CCD image using electronic zoom '),
    (SDKConst : prPTP_DEV_PROP_ML_SPOT_POS;           EnumVal : Byte(pspc_ML_SPOT_POS);            InfoText : 'Spot metering positon '),
    (SDKConst : prPTP_DEV_PROP_DISP_AV_MAX;           EnumVal : Byte(pspc_DISP_AV_MAX);            InfoText : 'How to display the maximin Av value '),
    (SDKConst : prPTP_DEV_PROP_AV_MAX_APEX;           EnumVal : Byte(pspc_AV_MAX_APEX);            InfoText : 'minimum aperture value '),
    (SDKConst : prPTP_DEV_PROP_EZOOM_START_POS;       EnumVal : Byte(pspc_EZOOM_START_POS);        InfoText : 'Zoom position at which the electornic zoom range starts '),
    (SDKConst : prPTP_DEV_PROP_FOCAL_LENGTH_OF_TELE;  EnumVal : Byte(pspc_FOCAL_LENGTH_OF_TELE);   InfoText : 'Focal distance at the optical telescopic end '),
    (SDKConst : prPTP_DEV_PROP_EZOOM_SIZE_OF_TELE;    EnumVal : Byte(pspc_EZOOM_SIZE_OF_TELE);     InfoText : 'Horizontal size of image to be cut out from CCD image at the telescopic end of the electronic zoom range '),
    (SDKConst : prPTP_DEV_PROP_PHOTO_EFFECT;          EnumVal : Byte(pspc_PHOTO_EFFECT);           InfoText : 'Photo effect '),
    (SDKConst : prPTP_DEV_PROP_AF_LIGHT;              EnumVal : Byte(pspc_AF_LIGHT);               InfoText : 'ON/OFF of AF assist light '),
    (SDKConst : prPTP_DEV_PROP_FLASH_QUANTITY;        EnumVal : Byte(pspc_FLASH_QUANTITY);         InfoText : 'Number of flash levels that can be set in the manual mode '),
    (SDKConst : prPTP_DEV_PROP_ROTATION_ANGLE;        EnumVal : Byte(pspc_ROTATION_ANGLE);         InfoText : 'Angle of rotation detected by the gravity sensor '),
    (SDKConst : prPTP_DEV_PROP_ROTATION_SENCE;        EnumVal : Byte(pspc_ROTATION_SENCE);         InfoText : 'Whether the gravity sensor is enable or disable '),
    (SDKConst : prPTP_DEV_PROP_IMEGE_FILE_SIZE;       EnumVal : Byte(pspc_IMEGE_FILE_SIZE);        InfoText : 'Image file size supported be the camera '),
    (SDKConst : prPTP_DEV_PROP_CAMERA_MODEL_ID;       EnumVal : Byte(pspc_CAMERA_MODEL_ID);        InfoText : 'Camera model ID ')
   );

type
  TpsObjectFormat = (psof_EXIF_JPEG,
                     psof_CRW);
  TpsObjectFormats = set of TpsObjectFormat;

const
  psObjectFormats : array[0..1] of TSDKConstEnumTranslation =
   ((SDKConst : prPTP_EXIF_JPEG;  EnumVal : Byte(psof_EXIF_JPEG); InfoText : 'EXIF JPEG'),
    (SDKConst : prPTP_CRW;        EnumVal : Byte(psof_CRW);       InfoText : 'RAW')
   );

type
  TpsPropertyTypeCode = ( pstc_NotDefined,
                          pstc_Int8,
                          pstc_UInt8,
                          pstc_Int16,
                          pstc_UInt16,
                          pstc_Int32,
                          pstc_UInt32,
                          pstc_Int64,
                          pstc_UInt64,
                          pstc_Int128,
                          pstc_UInt128,
                          pstc_Array_Int8,
                          pstc_Array_UInt8,
                          pstc_Array_Int16,
                          pstc_Array_UInt16,
                          pstc_Array_Int32,
                          pstc_Array_UInt32,
                          pstc_Array_Int64,
                          pstc_Array_UInt64,
                          pstc_Array_Int128,
                          pstc_Array_UInt128,
                          pstc_String);
  TpsPropertyTypeCodes = set of TpsPropertyTypeCode;

const
  psPropertyTypeCodes : array[0..21] of TSDKConstEnumTranslation =
   ((SDKConst : $0000; EnumVal : Byte(pstc_NotDefined); InfoText : 'Not defined'),
    (SDKConst : $0001; EnumVal : Byte(pstc_Int8);           InfoText : 'Signed 8 bit integer'),
    (SDKConst : $0002; EnumVal : Byte(pstc_UInt8);          InfoText : 'Unsigned 8 bit integer'),
    (SDKConst : $0003; EnumVal : Byte(pstc_Int16);          InfoText : 'Signed 16 bit integer'),
    (SDKConst : $0004; EnumVal : Byte(pstc_UInt16);         InfoText : 'Unsigned 16 bit integer'),
    (SDKConst : $0005; EnumVal : Byte(pstc_Int32);          InfoText : 'Signed 32 bit integer'),
    (SDKConst : $0006; EnumVal : Byte(pstc_UInt32);         InfoText : 'Unsigned 32 bit integer'),
    (SDKConst : $0007; EnumVal : Byte(pstc_Int64);          InfoText : 'Signed 64 bit integer'),
    (SDKConst : $0008; EnumVal : Byte(pstc_UInt64);         InfoText : 'Unsigned 64 bit integer'),
    (SDKConst : $0009; EnumVal : Byte(pstc_Int128);         InfoText : 'Signed 128 bit integer'),
    (SDKConst : $000A; EnumVal : Byte(pstc_UInt128);        InfoText : 'Unsigned 128 bit integer'),
    (SDKConst : $4001; EnumVal : Byte(pstc_Array_Int8);     InfoText : 'Array of Signed 8 bit integer'),
    (SDKConst : $4002; EnumVal : Byte(pstc_Array_UInt8);    InfoText : 'Array of Unsigned 8 bit integer'),
    (SDKConst : $4003; EnumVal : Byte(pstc_Array_Int16);    InfoText : 'Array of Signed 16 bit integer'),
    (SDKConst : $4004; EnumVal : Byte(pstc_Array_UInt16);   InfoText : 'Array of Unsigned 16 bit integer'),
    (SDKConst : $4005; EnumVal : Byte(pstc_Array_Int32);    InfoText : 'Array of Signed 32 bit integer'),
    (SDKConst : $4006; EnumVal : Byte(pstc_Array_UInt32);   InfoText : 'Array of Unsigned 32 bit integer'),
    (SDKConst : $4007; EnumVal : Byte(pstc_Array_Int64);    InfoText : 'Array of Signed 64 bit integer'),
    (SDKConst : $4008; EnumVal : Byte(pstc_Array_UInt64);   InfoText : 'Array of Unsigned 64 bit integer'),
    (SDKConst : $4009; EnumVal : Byte(pstc_Array_Int128);   InfoText : 'Array of Signed 128 bit integer'),
    (SDKConst : $400A; EnumVal : Byte(pstc_Array_UInt128);  InfoText : 'Array of Unsigned 128 bit integer'),
    (SDKConst : $FFFF; EnumVal : Byte(pstc_String);         InfoText : 'String(Refer to Attached Table 1-2) Unicode character string of variable length')
   );

type
  TpsPropertyAccessMode = (psam_ReadOnly,
                           psam_ReadWrite);
  TpsPropertyAccessModes = set of TpsPropertyAccessMode;

const
  psPropertyAccessModes : array[0..1] of TSDKConstEnumTranslation =
   ((SDKConst : $00; EnumVal : Byte(psam_ReadOnly);   InfoText : 'Indicates Get(Read-Only)'),
    (SDKConst : $01; EnumVal : Byte(psam_ReadWrite);  InfoText : 'Indicates Get/Set(Read-Write)')
   );

type
  TpsPropertyFormFlag = (psff_NoForm,
                         psff_Range,
                         psff_Enum);
  TpsPropertyFormFlags = set of TpsPropertyFormFlag;

const
  psPropertyFormFlags : array[0..2] of TSDKConstEnumTranslation =
   ((SDKConst : $00; EnumVal : Byte(psff_NoForm); InfoText : 'There is no FORM field.'),
    (SDKConst : $01; EnumVal : Byte(psff_Range);  InfoText : 'The FORM field is of Range-Form type'),
    (SDKConst : $02; EnumVal : Byte(psff_Enum);   InfoText : 'The FORM field is of Enumeration-Form type')
   );

type
  TpspBuzzerValue = (psp_Buzzer_On,
                     psp_Buzzer_Off);
  TpspBuzzerValues = set of TpspBuzzerValue;

const
  psPropertyValuesBuzzer : array[0..1] of TSDKConstEnumTranslation =
   ((SDKConst : $00; EnumVal : Byte(psp_Buzzer_On); InfoText : 'Buzzer on'),
    (SDKConst : $01; EnumVal : Byte(psp_Buzzer_Off);  InfoText : 'Buzzer off')
   );

type
  TpspBattery_KindValue = (psp_Battery_Kind_Unknown,
                           psp_Battery_Kind_AC,
                           psp_Battery_Kind_Lithium_ion,
                           psp_Battery_Kind_Nickel_hydride,
                           psp_Battery_Kind_Nickel_cadmium,
                           psp_Battery_Kind_Alkaline);
  TpspBattery_KindValues = set of TpspBattery_KindValue;

const
  psPropertyValuesBattery_Kind : array[0..5] of TSDKConstEnumTranslation =
   ((SDKConst : $0000; EnumVal : Byte(psp_Battery_Kind_Unknown);        InfoText : 'Unknown'),
    (SDKConst : $0001; EnumVal : Byte(psp_Battery_Kind_AC);             InfoText : 'AC power supply'),
    (SDKConst : $0002; EnumVal : Byte(psp_Battery_Kind_Lithium_ion);    InfoText : 'Lithium ion battery'),
    (SDKConst : $0003; EnumVal : Byte(psp_Battery_Kind_Nickel_hydride); InfoText : 'Nickel hydride battery'),
    (SDKConst : $0004; EnumVal : Byte(psp_Battery_Kind_Nickel_cadmium); InfoText : 'Nickel cadmium battery'),
    (SDKConst : $0005; EnumVal : Byte(psp_Battery_Kind_Alkaline);       InfoText : 'Alkaline manganese battery')
   );

type
  TpspBattery_StatusValue = (psp_Battery_Status_Not_defined,
                             psp_Battery_Status_Normal,
                             psp_Battery_Status_Warning_Level1,
                             psp_Battery_Status_Emergency,
                             psp_Battery_Status_Warning_Level0);
  TpspBattery_StatusValues = set of TpspBattery_StatusValue;

const
  psPropertyValuesBattery_Status : array[0..4] of TSDKConstEnumTranslation =
   ((SDKConst : $00000000; EnumVal : Byte(psp_Battery_Status_Not_defined); InfoText : 'Not defined.'),
    (SDKConst : $00000001; EnumVal : Byte(psp_Battery_Status_Normal); InfoText : 'NORMAL'),
    (SDKConst : $00000002; EnumVal : Byte(psp_Battery_Status_Warning_Level1); InfoText : 'WARNING_LV1'),
    (SDKConst : $00000003; EnumVal : Byte(psp_Battery_Status_Emergency); InfoText : 'EMERGENCY'),
    (SDKConst : $00000004; EnumVal : Byte(psp_Battery_Status_Warning_Level0); InfoText : 'WARNING_LV0')
   );


{------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

type
  TpsRawCameraEventData = record
    Camera : prHandle;
    Context : prContext;
    Data : Pointer;
  end;

  TpsCameraEventDataRecord = packed record
    InterruptDataLength : prUInt32;
    ContainerType : prUInt16;
    EventCode : prUint16;
    TransactionID : prUInt32;
    Parameters : array[0..0] of prUInt32;
  end;
  PpsCameraEventDataRecord = ^TpsCameraEventDataRecord;

  TpsCameraEventData = record
    EventCode : TpsEventcode;
    Params : array of prUInt32;
  end;

{------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

  TpsRawGetFileDataRecord = record
    Camera : prHandle;
    ObjectHandle : prObjectHandle;
    Context : prContext;
    Progress : prProgress;
  end;

{------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function psTranslateToEnum(ASDKConst : Integer; const AArray : array of TSDKConstEnumTranslation) : Byte;
function psTranslateToSDKConst(AEnumVal : Byte; const AArray : array of TSDKConstEnumTranslation) : Integer;

function GetInfoTextFormEnum(AEnum : Byte; const AArray : array of TSDKConstEnumTranslation) : String;

{------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function TranslateEventData(AEventDataRecord : TpsCameraEventDataRecord) : TpsCameraEventData;

{------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function psReadBufferInt8(var ABuffer : Pointer) : prInt8;
function psReadBufferUInt8(var ABuffer : Pointer) : prUInt8;
function psReadBufferInt16(var ABuffer : Pointer) : prInt16;
function psReadBufferUInt16(var ABuffer : Pointer) : prUInt16;
function psReadBufferInt32(var ABuffer : Pointer) : prInt32;
function psReadBufferUInt32(var ABuffer : Pointer) : prUInt32;
function psReadBufferInt64(var ABuffer : Pointer) : prInt64;
function psReadBufferUInt64(var ABuffer : Pointer) : prUInt64;
function psReadBufferString(var ABuffer : Pointer) : WideString;

{------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

function psGetBaseTypeCode(ATypeCode : TpsPropertyTypeCode) : TpsPropertyTypeCode;

implementation

function GetInfoTextFormEnum(AEnum : Byte; const AArray : array of TSDKConstEnumTranslation) : String;
var
  idx : Integer;
begin

  for idx := Low(AArray) to High(AArray) do
  begin
    if AArray[idx].EnumVal = AEnum then
    begin
      Result := AArray[idx].InfoText;
      break;
    end;
  end;
end;

function psTranslateToEnum(ASDKConst : Integer; const AArray : array of TSDKConstEnumTranslation) : Byte;
var
  idx : Integer;
begin
  Result := 255;

  for idx := Low(AArray) to High(AArray) do
  begin
    if AArray[idx].SDKConst = ASDKConst then
    begin
      Result := AArray[idx].EnumVal;
      break;
    end;
  end;
end;

function psTranslateToSDKConst(AEnumVal : Byte; const AArray : array of TSDKConstEnumTranslation) : Integer;
var
  idx : Integer;
begin
  Result := MaxInt;

  for idx := Low(AArray) to High(AArray) do
  begin
    if AArray[idx].EnumVal = AEnumVal then
    begin
      Result := AArray[idx].SDKConst;
      break;
    end;
  end;
end;


function TranslateEventData(AEventDataRecord : TpsCameraEventDataRecord) : TpsCameraEventData;
var
  idx : Integer;
begin
  Result.EventCode := TpsEventCode(psTranslateToEnum(AEventDataRecord.EventCode, psEventCodes));

  SetLength(Result.Params, (AEventDataRecord.InterruptDataLength - 12) div 4);

  for idx := Low(Result.Params) to High(Result.Params) do
    Result.Params[idx] := AEventDataRecord.Parameters[idx];
end;

function psReadBufferUInt8(var ABuffer : Pointer) : prUInt8;
begin
  Result := PprUInt8(ABuffer)^;
  Inc(PprUInt8(ABuffer));
end;

function psReadBufferUInt16(var ABuffer : Pointer) : prUInt16;
begin
  Result := PprUInt16(ABuffer)^;
  Inc(PprUInt16(ABuffer));
end;

function psReadBufferUInt32(var ABuffer : Pointer) : prUInt32;
begin
  Result := PprUInt32(ABuffer)^;
  Inc(PprUInt32(ABuffer));
end;

function psReadBufferUInt64(var ABuffer : Pointer) : prUInt64;
begin
  Result := PprUInt64(ABuffer)^;
  Inc(PprUInt64(ABuffer));
end;

function psReadBufferInt8(var ABuffer : Pointer) : prInt8;
begin
  Result := PprInt8(ABuffer)^;
  Inc(PprInt8(ABuffer));
end;

function psReadBufferInt16(var ABuffer : Pointer) : prInt16;
begin
  Result := PprInt16(ABuffer)^;
  Inc(PprInt16(ABuffer));
end;

function psReadBufferInt32(var ABuffer : Pointer) : prInt32;
begin
  Result := PprInt32(ABuffer)^;
  Inc(PprInt32(ABuffer));
end;

function psReadBufferInt64(var ABuffer : Pointer) : prInt64;
begin
  Result := PprInt64(ABuffer)^;
  Inc(PprInt64(ABuffer));
end;

function psReadBufferString(var ABuffer : Pointer) : WideString;
var
  NumChar : prUInt8;
begin
  NumChar := psReadBufferUInt8(ABuffer);
  Result := WideCharToString(PWideChar(ABuffer));
  Inc(PprWChar(ABuffer), Integer(NumChar));
end;


function psGetBaseTypeCode(ATypeCode : TpsPropertyTypeCode) : TpsPropertyTypeCode;
begin
  case ATypeCode of
    pstc_NotDefined     : Result := pstc_NotDefined;
    pstc_Int8           : Result := pstc_Int8;
    pstc_UInt8          : Result := pstc_UInt8;
    pstc_Int16          : Result := pstc_Int16;
    pstc_UInt16         : Result := pstc_UInt16;
    pstc_Int32          : Result := pstc_Int32;
    pstc_UInt32         : Result := pstc_UInt32;
    pstc_Int64          : Result := pstc_Int64;
    pstc_UInt64         : Result := pstc_UInt64;
    pstc_Int128         : Result := pstc_Int128;
    pstc_UInt128        : Result := pstc_UInt128;
    pstc_Array_Int8     : Result := pstc_Int8;
    pstc_Array_UInt8    : Result := pstc_UInt8;
    pstc_Array_Int16    : Result := pstc_Int16;
    pstc_Array_UInt16   : Result := pstc_UInt16;
    pstc_Array_Int32    : Result := pstc_Int32;
    pstc_Array_UInt32   : Result := pstc_UInt32;
    pstc_Array_Int64    : Result := pstc_Int64;
    pstc_Array_UInt64   : Result := pstc_UInt64;
    pstc_Array_Int128   : Result := pstc_Int128;
    pstc_Array_UInt128  : Result := pstc_UInt128;
    pstc_String         : Result := pstc_String;
    else
      Result := pstc_NotDefined;
  end;

end;

end.
