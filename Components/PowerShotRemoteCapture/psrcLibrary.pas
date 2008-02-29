unit psrcLibrary;

interface

uses
  Windows,
  SysUtils,
  prFuncType,
  psrcError,
  prType;

const
  DEFAULT_LIBRARY_NAME = 'PRSDK.dll';

type
  TpsrcLibrary = class
  protected
    FLibName : String;
    FLibHandle : THandle;
    FFunctions : prFunctions;

    procedure LoadLib();
    procedure UnloadLib();
  public
    constructor Create(ALibraryName : String = DEFAULT_LIBRARY_NAME); virtual;
    destructor Destroy(); override;

    function StartSDK() : prResponse;
    function FinishSDK() : prResponse;
    function GetDLLsVersion(var pBufferSize : prUInt32;
                            pDllVersion : PprDllsVerInfo) : prResponse;

    function GetDeviceList(var pBufferSize : prUInt32;
                           pDeviceList : PprDeviceList) : prResponse;

    function CreateCameraObject(var pDeviceInfo : prDeviceInfoTable;
                                var pCameraHandle : prHandle) : prResponse;
    function DestroyCameraObject(CameraHandle : prHandle) : prResponse;

    function ConnectCamera(CameraHandle : prHandle) : prResponse;
    function DisconnectCamera(CameraHandle : prHandle) : prResponse;

    function SetEventCallBack(CameraHandle : prHandle;
                              Context : prContext;
                              pSetEventCB : prSetEventCB) : prResponse;
    function ClearEventCallBack(CameraHandle : prHandle) : prResponse;

    function GetDeviceInfo(CameraHandle : prHandle;
                           var pBufferSize : prUInt32;
                           pDeviceInfo : Pointer) : prResponse;

    function InitiateReleaseControl(CameraHandle : prHandle) : prResponse;
    function TerminateReleaseControl(CameraHandle : prHandle) : prResponse;
    function RC_Release(CameraHandle : prHandle) : prResponse;
    function RC_GetReleasedData(CameraHandle : prHandle;
                                ObjectHandle : prObjectHandle;
                                EventCode : prptpEventCode;
                                TransSize : prUInt32;
                                Context : prContext;
                                pGetFileDataCB : prGetFileDataCB) : prResponse;
    function RC_GetNumAvailableShot(CameraHandle : prHandle;
                                    var pNum : prUInt32) : prResponse;
    function RC_StartViewFinder(CameraHandle : prHandle;
                                Context : prContext;
                                pViewFinderCB : prViewFinderCB) : prResponse;
    function RC_TermViewFinder(CameraHandle : prHandle) : prResponse;
    function RC_DoAeAfAwb(CameraHandle : prHandle;
                          ResetFlag : prptpAeAfAwbResetFlag) : prResponse;
    function RC_FocusLock(CameraHandle : prHandle) : prResponse;
    function RC_FocusUnlock(CameraHandle : prHandle) : prResponse;


    function GetDevicePropDesc(CameraHandle : prHandle;
                               DevicePropCode : prptpDevicePropCode;
                               var pBufferSize : prUInt32;
                               pDevicePropDesc : Pointer) : prResponse;

    function GetDevicePropValue(CameraHandle : prHandle;
                                DevicePropCode : prptpDevicePropCode;
                                var pBufferSize : prUInt32;
                                pDeviceProperty : Pointer) : prResponse;

    function SetDevicePropValue(CameraHandle : prHandle;
                                DevicePropCode : prptpDevicePropCode;
                                DataSize : prUInt32;
                                pDeviceProperty : Pointer) : prResponse;

    function RC_GetChangedReleaseParamesList(CameraHandle : prHandle;
                                             var pBufferSize : prUInt32;
                                             pParamsList : Pointer) : prResponse;
  end;

implementation

{ TpsrcLibrary }

function TpsrcLibrary.ClearEventCallBack(CameraHandle: prHandle): prResponse;
begin
  if not Assigned(FFunctions.pClearEventCallBack) then
    raise EpsrcFunctionNotDefined.Create('ClearEventCallBack');

  Result := FFunctions.pClearEventCallBack(CameraHandle);
end;

function TpsrcLibrary.ConnectCamera(CameraHandle: prHandle): prResponse;
begin
  if not Assigned(FFunctions.pConnectCamera) then
    raise EpsrcFunctionNotDefined.Create('ConnectCamera');

  Result := FFunctions.pConnectCamera(CameraHandle);
end;

constructor TpsrcLibrary.Create(ALibraryName: String);
begin
  FLibName := ALibraryName;

  FLibHandle := 0;

  LoadLib;
end;

function TpsrcLibrary.CreateCameraObject(var pDeviceInfo: prDeviceInfoTable;
  var pCameraHandle: prHandle): prResponse;
begin
  if not Assigned(FFunctions.pCreateCameraObject) then
    raise EpsrcFunctionNotDefined.Create('CreateCameraObject');

  Result := FFunctions.pCreateCameraObject(pDeviceInfo, pCameraHandle);
end;

destructor TpsrcLibrary.Destroy;
begin

  inherited;
end;


function TpsrcLibrary.DestroyCameraObject(CameraHandle: prHandle): prResponse;
begin
  if not Assigned(FFunctions.pDestroyCameraObject) then
    raise EpsrcFunctionNotDefined.Create('DestroyCameraObject');

  Result := FFunctions.pDestroyCameraObject(CameraHandle);
end;

function TpsrcLibrary.DisconnectCamera(CameraHandle: prHandle): prResponse;
begin
  if not Assigned(FFunctions.pDisconnectCamera) then
    raise EpsrcFunctionNotDefined.Create('DisconnectCamera');

  Result := FFunctions.pDisconnectCamera(CameraHandle);
end;

procedure TpsrcLibrary.LoadLib;
var
  GetFunctionsFunc : prGetFunctions; 
begin
  if not FileExists(FLibName) then
    raise EpsrcLibNotFound.Create('Can not find "'+FLibName+'"');

  FLibHandle := LoadLibrary(PChar(FLibName));

  if FLibHandle = 0 then
    raise EpsrcCanNotLoadLib.Create('Can not load library "'+FLibName+'"');

  @GetFunctionsFunc := GetProcAddress(FLibHandle, 'PR_GetFunctions');

  if not Assigned(GetFunctionsFunc) then
    raise EpsrcCanNotFindFunctionPointer.Create('Can not get function pointer');

    
  FillMemory(@FFunctions, SizeOf(FFunctions), 0);
  FFunctions.Version := prCURRENT_FUNCTABLE_VERSION;

  psCheckResponse(GetFunctionsFunc(FFunctions));
end;

function TpsrcLibrary.RC_DoAeAfAwb(CameraHandle: prHandle;
  ResetFlag: prptpAeAfAwbResetFlag): prResponse;
begin
  if not Assigned(FFunctions.pRC_DoAeAfAwb) then
    raise EpsrcFunctionNotDefined.Create('RC_DoAeAfAwb');

  Result := FFunctions.pRC_DoAeAfAwb(CameraHandle, ResetFlag);
end;

function TpsrcLibrary.RC_FocusLock(CameraHandle: prHandle): prResponse;
begin
  if not Assigned(FFunctions.pRC_FocusLock) then
    raise EpsrcFunctionNotDefined.Create('RC_FocusLock');

  Result := FFunctions.pRC_FocusLock(CameraHandle);
end;

function TpsrcLibrary.RC_FocusUnlock(CameraHandle: prHandle): prResponse;
begin
  if not Assigned(FFunctions.pRC_FocusUnlock) then
    raise EpsrcFunctionNotDefined.Create('RC_FocusUnlock');

  Result := FFunctions.pRC_FocusUnlock(CameraHandle);
end;

function TpsrcLibrary.RC_GetChangedReleaseParamesList(CameraHandle: prHandle;
  var pBufferSize: prUInt32; pParamsList: Pointer): prResponse;
begin
  if not Assigned(FFunctions.pRC_GetChangedReleaseParamesList) then
    raise EpsrcFunctionNotDefined.Create('RC_GetChangedReleaseParamesList');

  Result := FFunctions.pRC_GetChangedReleaseParamesList(CameraHandle,
                                                        pBufferSize,
                                                        pParamsList);
end;

function TpsrcLibrary.RC_GetNumAvailableShot(CameraHandle: prHandle;
  var pNum: prUInt32): prResponse;
begin
  if not Assigned(FFunctions.pRC_GetNumAvailableShot) then
    raise EpsrcFunctionNotDefined.Create('RC_GetNumAvailableShot');

  Result := FFunctions.pRC_GetNumAvailableShot(CameraHandle, pNum);
end;

function TpsrcLibrary.RC_GetReleasedData(CameraHandle: prHandle;
  ObjectHandle: prObjectHandle; EventCode: prptpEventCode; TransSize: prUInt32;
  Context: prContext; pGetFileDataCB: prGetFileDataCB): prResponse;
begin
  if not Assigned(FFunctions.pRC_GetReleasedData) then
    raise EpsrcFunctionNotDefined.Create('RC_GetReleasedData');

  Result := FFunctions.pRC_GetReleasedData(CameraHandle,
                                           ObjectHandle,
                                           EventCode,
                                           TransSize,
                                           Context,
                                           pGetFileDataCB);
end;

function TpsrcLibrary.RC_Release(CameraHandle: prHandle): prResponse;
begin
  if not Assigned(FFunctions.pRC_Release) then
    raise EpsrcFunctionNotDefined.Create('RC_Release');

  Result := FFunctions.pRC_Release(CameraHandle);
end;

function TpsrcLibrary.RC_StartViewFinder(CameraHandle: prHandle;
  Context: prContext; pViewFinderCB: prViewFinderCB): prResponse;
begin
  if not Assigned(FFunctions.pRC_StartViewFinder) then
    raise EpsrcFunctionNotDefined.Create('RC_StartViewFinder'); 

  Result := FFunctions.pRC_StartViewFinder(CameraHandle,
                                           Context,
                                           pViewFinderCB);
end;

function TpsrcLibrary.RC_TermViewFinder(CameraHandle: prHandle): prResponse;
begin
  if not Assigned(FFunctions.pRC_TermViewFinder) then
    raise EpsrcFunctionNotDefined.Create('RC_TermViewFinder');

  Result := FFunctions.pRC_TermViewFinder(CameraHandle);
end;

function TpsrcLibrary.SetDevicePropValue(CameraHandle: prHandle;
  DevicePropCode: prptpDevicePropCode; DataSize: prUInt32;
  pDeviceProperty: Pointer): prResponse;
begin
  if not Assigned(FFunctions.pSetDevicePropValue) then
    raise EpsrcFunctionNotDefined.Create('SetDevicePropValue');

  Result := FFunctions.pSetDevicePropValue(CameraHandle,
                                           DevicePropCode,
                                           DataSize,
                                           pDeviceProperty);
end;

function TpsrcLibrary.SetEventCallBack(CameraHandle: prHandle;
  Context: prContext; pSetEventCB: prSetEventCB): prResponse;
begin
  if not Assigned(FFunctions.pSetEventCallBack) then
    raise EpsrcFunctionNotDefined.Create('pSetEventCallBack');

  Result := FFunctions.pSetEventCallBack(CameraHandle,
                                         Context,
                                         pSetEventCB);
end;

function TpsrcLibrary.StartSDK: prResponse;
begin
  if not Assigned(FFunctions.pStartSDK) then
    raise EpsrcFunctionNotDefined.Create('StartSDK');
    
  Result := FFunctions.pStartSDK;
end;

function TpsrcLibrary.TerminateReleaseControl(
  CameraHandle: prHandle): prResponse;
begin
  if not Assigned(FFunctions.pTerminateReleaseControl) then
    raise EpsrcFunctionNotDefined.Create('TerminateReleaseControl');

  Result := FFunctions.pTerminateReleaseControl(CameraHandle);
end;

function TpsrcLibrary.FinishSDK: prResponse;
begin
  if not Assigned(FFunctions.pFinishSDK) then
    raise EpsrcFunctionNotDefined.Create('FinishSDK');

  Result := FFunctions.pFinishSDK;
end;

function TpsrcLibrary.GetDeviceInfo(CameraHandle: prHandle;
  var pBufferSize: prUInt32; pDeviceInfo: Pointer): prResponse;
begin
  if not Assigned(FFunctions.pGetDeviceInfo) then
    raise EpsrcFunctionNotDefined.Create('GetDeviceInfo');

  Result := FFunctions.pGetDeviceInfo(CameraHandle, pBufferSize, pDeviceInfo);
end;

function TpsrcLibrary.GetDeviceList(var pBufferSize: prUInt32;
  pDeviceList: PprDeviceList): prResponse;
begin
  if not Assigned(FFunctions.pGetDeviceList) then
    raise EpsrcFunctionNotDefined.Create('GetDeviceList');

  Result := FFunctions.pGetDeviceList(pBufferSize, pDeviceList);
end;

function TpsrcLibrary.GetDevicePropDesc(CameraHandle: prHandle;
  DevicePropCode: prptpDevicePropCode; var pBufferSize: prUInt32;
  pDevicePropDesc: Pointer): prResponse;
begin
  if not Assigned(FFunctions.pGetDevicePropDesc) then
    raise EpsrcFunctionNotDefined.Create('GetDevicePropDesc');

  Result := FFunctions.pGetDevicePropDesc(CameraHandle,
                                          DevicePropCode,
                                          pBufferSize,
                                          pDevicePropDesc);
end;

function TpsrcLibrary.GetDevicePropValue(CameraHandle: prHandle;
  DevicePropCode: prptpDevicePropCode; var pBufferSize: prUInt32;
  pDeviceProperty: Pointer): prResponse;
begin
  if not Assigned(FFunctions.pGetDevicePropValue) then
    raise EpsrcFunctionNotDefined.Create('GetDevicePropValue');

  Result := FFunctions.pGetDevicePropValue(CameraHandle,
                                           DevicePropCode,
                                           pBufferSize,
                                           pDeviceProperty);
end;

function TpsrcLibrary.GetDLLsVersion(var pBufferSize: prUInt32;
  pDllVersion: PprDllsVerInfo): prResponse;
begin
  if not Assigned(FFunctions.pGetDllsVersion) then
    raise EpsrcFunctionNotDefined.Create('GetDLLsVersion');

  Result := FFunctions.pGetDllsVersion(pBufferSize, pDllVersion);
end;

function TpsrcLibrary.InitiateReleaseControl(
  CameraHandle: prHandle): prResponse;
begin
  if not Assigned(FFunctions.pInitiateReleaseControl) then
    raise EpsrcFunctionNotDefined.Create('InitiateReleaseControl');

  Result := FFunctions.pInitiateReleaseControl(CameraHandle);
end;

procedure TpsrcLibrary.UnloadLib;
begin
  if FLibHandle > 0 then
    FreeLibrary(FLibHandle);
end;

end.
