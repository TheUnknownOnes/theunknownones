unit psrcMain;

interface

uses
  psrcLibrary,
  psrcError,
  psrcType,
  prError,
  prType,
  SysUtils,
  WideStrings,
  WideStrUtils,
  Controls,
  Classes,
  Windows;

type
  TpsManager = class;
  TpsCamera = class;

{------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

  TpsPropertyValue = record
    Value : Pointer;
    Size : Integer;
  end;

  TpsProperty = class
  private
    FTypeCode: TpsPropertyTypeCode;
    FBaseTypeCode : TpsPropertyTypeCode;
    FManager : TpsManager;
    FCode : TpsPropertyCode;
    FCamera : TpsCamera;
    FAccessMode: TpsPropertyAccessMode;
    FReadOnly : Boolean;
    FAvailable : Boolean;

    FFormFlag : TpsPropertyFormFlag;

    procedure ReadDescription;

    procedure CreatePropertyValue(var AValue : TpsPropertyValue; ASize : Integer);
    procedure DestroyPropertyValue(var AValue : TpsPropertyValue);
  protected
    FFactoryDefault : TpsPropertyValue;

    FRangeMinValue,
    FRangeMaxValue,
    FRangeStep : TpsPropertyValue;
    FEnumSupportedValues : array of TpsPropertyValue;

    function GetValueBufferSize(ABuffer : Pointer) : Integer; virtual;

    procedure ReadValueBuffer(ABuffer : Pointer; var AValue : TpsPropertyValue); virtual;
    procedure ReadSingleBaseValueBuffer(ABuffer : Pointer; var AValue : TpsPropertyValue); virtual;

    function GetValid: Boolean; virtual;

  public
    constructor Create(AManager : TpsManager; ACamera : TpsCamera; APropertyCode : TpsPropertyCode); virtual;
    destructor Destroy(); override;

    property Code : TpsPropertyCode read FCode;
    property Available : Boolean read FAvailable;
    property TypeCode : TpsPropertyTypeCode read FTypeCode;
    property BaseTypeCode : TpsPropertyTypeCode read FBaseTypeCode;
    property AccessMode : TpsPropertyAccessMode read FAccessMode;
    property ReadOnly : Boolean read FReadOnly;
    property Valid : Boolean read GetValid;
    property Manager : TpsManager read FManager;
    property Camera : TpsCamera read FCamera;
    property FormFlag : TpsPropertyFormFlag read FFormFlag;
  end;

{------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

  TpsCamera = class
  private
    FProperties : array[Low(TpsPropertyCode)..High(TpsPropertyCode)] of TpsProperty;

    FConnected: Boolean;
    FReleaseControl: Boolean;
    FSupportedProperties: TpsPropertyCodes;
    FSerialNumber: WideString;
    FManufacturer: WideString;
    FSupportedOperations: TpsOperationCodes;
    FModel: WideString;
    FSupportedEvents: TpsEventCodes;
    FDeviceVersion: WideString;
    FSupportedCaptureFormats: TpsObjectFormats;
    FVendorExtensionVersion: prUInt16;
    FSupportedImageFormats: TpsObjectFormats;
    FVendorExtentsionDesc: WideString;
    FStandardVersion: prUInt16;
    FVendorExtensionID: prUInt32;
    FFunctionMode: prUInt16;
    procedure SetConnected(const Value: Boolean);
    procedure SetReleaseControl(const Value: Boolean);
    function GetPropertyBattery_Kind: TpsProperty;
  protected
    FCamera : prHandle;
    FManager : TpsManager;

    procedure SetEventCallBack;
    procedure ClearEventCallBack;

    function CanDoOperation(AOperation : TpsOperationCode) : Boolean;

    procedure ReadInfo();

    procedure ProcessEvent(AEventData : TpsCameraEventData);

    procedure FreeProperties;
  public
    constructor Create(AManager : TpsManager; ADeviceInfo : prDeviceInfoTable);
    destructor Destroy(); override;

    procedure Release();
    
    procedure HasToBeConnected;

    property Connected : Boolean read FConnected write SetConnected;
    property Handle : prHandle read FCamera;
    property ReleaseControlEnabled : Boolean read FReleaseControl write SetReleaseControl;

    property StandardVersion : prUInt16 read FStandardVersion;
    property VendorExtensionID : prUInt32 read FVendorExtensionID;
    property VendorExtensionVersion : prUInt16 read FVendorExtensionVersion;
    property VendorExtentsionDesc : WideString read FVendorExtentsionDesc;
    property FunctionalMode : prUInt16 read FFunctionMode;
    property SupportedOperations : TpsOperationCodes read FSupportedOperations;
    property SupportedEvents : TpsEventcodes read FSupportedEvents;
    property SupportedProperties : TpsPropertyCodes read FSupportedProperties;
    property SupportedCaptureFormats : TpsObjectFormats read FSupportedCaptureFormats;
    property SupportedImageFormats : TpsObjectFormats read FSupportedImageFormats;
    property Manufacturer : WideString read FManufacturer;
    property Model : WideString read FModel;
    property DeviceVersion : WideString read FDeviceVersion;
    property SerialNumber : WideString read FSerialNumber;

    property Battery_Kind : TpsProperty read GetPropertyBattery_Kind;
  end;

{------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

  TpsCameraList = class(TList)
  private
    function Get(Index: Integer): TpsCamera;
    procedure Put(Index: Integer; const Value: TpsCamera);
  public
    function Add(Item: TpsCamera): Integer;
    function Extract(Item: TpsCamera): TpsCamera;
    function First: TpsCamera;
    function IndexOf(Item: TpsCamera): Integer;
    procedure Insert(Index: Integer; Item: TpsCamera);
    function Last: TpsCamera;
    function Remove(Item: TpsCamera): Integer;
    property Items[Index: Integer]: TpsCamera read Get write Put; default;
  end;

{------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

  TpsDevice = class
  private
    FManager : TpsManager;

    FDeviceInfo : prDeviceInfoTable;

    FModelName: WideString;
    FSubGeneration: prUInt16;
    FGeneration: prUInt16;
    FModelID: prUInt32;
    FInternalName: WideString;
  public
    constructor Create(AManager : TpsManager; ADeviceInfo : prDeviceInfoTable);
    destructor Destroy(); override;

    function CreateCamera : TpsCamera;

    property InternalName : WideString read FInternalName;
    property ModelName : WideString read FModelName;
    property Generation : prUInt16 read FGeneration;
    property SubGeneration : prUInt16 read FSubGeneration;
    property ModelID : prUInt32 read FModelID;
  end;

{------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

  TpsDeviceList = class(TList)
  private
    function Get(Index: Integer): TpsDevice;
    procedure Put(Index: Integer; const Value: TpsDevice);
  public
    function Add(Item: TpsDevice): Integer;
    function Extract(Item: TpsDevice): TpsDevice;
    function First: TpsDevice;
    function IndexOf(Item: TpsDevice): Integer;
    procedure Insert(Index: Integer; Item: TpsDevice);
    function Last: TpsDevice;
    function Remove(Item: TpsDevice): Integer;
    property Items[Index: Integer]: TpsDevice read Get write Put; default;

    procedure SaveTo(const AStrings : TStrings);
  end;

{------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

  TCameraEvent = procedure(ACamera : TpsCamera; AEventParams : TpsCameraEventData) of object;

{------------------------------------------------------------------------------
-------------------------------------------------------------------------------}
  
  TpsManager = class(TComponent)
  private
    FCameraEvents : array[1..14] of TCameraEvent;
    FActive: Boolean;
    FLibFile: String;
    FAutoEnableReleaseControl: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetCameraEvent(const Index: Integer): TCameraEvent;
    procedure SetCameraEvent(const Index: Integer; const Value: TCameraEvent);
  protected
    FLib : TpsrcLibrary;
    FDeviceList : TpsDeviceList;
    FCameraList : TpsCameraList;

    FCameraEventData : TpsRawCameraEventData;
    FGetFileDataRecord : TpsRawGetFileDataRecord;

    procedure HasToBeActive;

    procedure ClearDeviceList;

    procedure ProcessCameraEvent;
    procedure ProcessGetFileData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure RefreshDeviceList;

    property Active : Boolean read FActive write SetActive default false;
    property Devices : TpsDeviceList read FDeviceList;
    property Cameras : TpsCameraList read FCameraList;
    property Lib : TpsrcLibrary read FLib;
  published
    property LibFile : String read FLibFile write FLibFile;
    property AutoEnableReleaseControl : Boolean read FAutoEnableReleaseControl write FAutoEnableReleaseControl default false;

    property OnDevicePropertyChanged : TCameraEvent                   index 1 read GetCameraEvent write SetCameraEvent;
    property OnCaptureComplete : TCameraEvent                         index 2 read GetCameraEvent write SetCameraEvent;
    property OnShutdownBecauseMemoryCardGateWasOpened : TCameraEvent  index 3 read GetCameraEvent write SetCameraEvent;
    property OnHardwareError : TCameraEvent                           index 4 read GetCameraEvent write SetCameraEvent;
    property OnViewfinderDisabled : TCameraEvent                      index 5 read GetCameraEvent write SetCameraEvent;
    property OnViewfinderEnabled : TCameraEvent                       index 6 read GetCameraEvent write SetCameraEvent;
    property OnFullViewReleased : TCameraEvent                        index 7 read GetCameraEvent write SetCameraEvent;
    property OnThumbnailReleased : TCameraEvent                       index 8 read GetCameraEvent write SetCameraEvent;
    property OnBatteryStatusChanged : TCameraEvent                    index 9 read GetCameraEvent write SetCameraEvent;
    property OnReleaseSwitchPressed : TCameraEvent                    index 10 read GetCameraEvent write SetCameraEvent;
    property OnRCPropertyChanged : TCameraEvent                       index 11 read GetCameraEvent write SetCameraEvent;
    property OnRCRotationAngleChanged : TCameraEvent                  index 12 read GetCameraEvent write SetCameraEvent;
    property OnRCChangeViaCameraUI : TCameraEvent                     index 13 read GetCameraEvent write SetCameraEvent;
    property OnShutdown : TCameraEvent                                index 14 read GetCameraEvent write SetCameraEvent;
  end;

procedure Register;

implementation

var
  TheOneAndOnlyManager : TpsManager;

procedure Register;
begin
  RegisterComponentsProc('TUO', [TpsManager]);
end;

function prSetEventCB(CameraHandle : prHandle; Context : prContext; pEventData : Pointer) : prResponse; stdcall;
begin
  Result := prOK;

  if Assigned(TheOneAndOnlyManager) then
  begin
    TheOneAndOnlyManager.FCameraEventData.Camera := CameraHandle;
    TheOneAndOnlyManager.FCameraEventData.Context := Context;
    TheOneAndOnlyManager.FCameraEventData.Data := pEventData;

    TThread.Synchronize(nil, TheOneAndOnlyManager.ProcessCameraEvent);
  end;

end;

function prGetFileDataCB(CameraHandle : prHandle;
                         ObjectHandle : prObjectHandle;
                         Context : prContext;
                         var pProgress : prProgress) : prResponse; stdcall;
begin
  Result := prOK;

  if Assigned(TheOneAndOnlyManager) then
  begin
    TheOneAndOnlyManager.FGetFileDataRecord.Camera := CameraHandle;
    TheOneAndOnlyManager.FGetFileDataRecord.Context := Context;
    TheOneAndOnlyManager.FGetFileDataRecord.ObjectHandle := ObjectHandle;
    TheOneAndOnlyManager.FGetFileDataRecord.Progress := pProgress;

    TThread.Synchronize(nil, TheOneAndOnlyManager.ProcessGetFileData);
  end;
end;

{------------------------------------------------------------------------------
-------------------------------------------------------------------------------}
{ TpsManager }

procedure TpsManager.ClearDeviceList;
begin
  while FDeviceList.Count > 0 do
  begin
    FDeviceList.First.Free;
    FDeviceList.Delete(0);
  end;
end;

constructor TpsManager.Create(AOwner: TComponent);
begin
  if not Assigned(TheOneAndOnlyManager) then
    TheOneAndOnlyManager := Self
  else
    raise Exception.Create('Only one manager per application allowed');

  inherited;

  FActive := false;
  FLib := nil;
  FAutoEnableReleaseControl := false;
  FDeviceList := TpsDeviceList.Create;
  FCameraList := TpsCameraList.Create;
end;

destructor TpsManager.Destroy;
begin
  Active := false;

  if Assigned(FDeviceList) then
  begin
    ClearDeviceList;
    FDeviceList.Free;
  end;

  if Assigned(FCameraList) then
    FCameraList.Free;

  inherited;
end;

function TpsManager.GetCameraEvent(const Index: Integer): TCameraEvent;
begin
  Result := FCameraEvents[Index];
end;


procedure TpsManager.HasToBeActive;
begin
  if not Active then
    raise Exception.Create('Manager has to be active for this action');
end;

procedure TpsManager.ProcessCameraEvent;
var
  Cam : TpsCamera;
  EventData : TpsCameraEventData;
  EventDataRec : PpsCameraEventDataRecord;
  FEvent : TCameraEvent;
begin
  Cam := TpsCamera(FCameraEventData.Context);

  EventDataRec := FCameraEventData.Data;

  EventData := TranslateEventData(EventDataRec^);

  FEvent := nil;

  if (FCameraList.IndexOf(Cam) > -1) and (Cam.Handle = FCameraEventData.Camera) then
  begin
    Cam.ProcessEvent(EventData);

    case EventData.EventCode of
      psec_DEVICE_PROP_CHANGED:          FEvent := OnDevicePropertyChanged;
      psec_CAPTURE_COMPLETE :            FEvent := OnCaptureComplete;
      psec_SHUTDOWN_CF_GATE_WAS_OPENED : FEvent := OnShutdownBecauseMemoryCardGateWasOpened;
      psec_RESET_HW_ERROR :              FEvent := OnHardwareError;
      psec_ABORT_PC_EVF :                FEvent := OnViewfinderDisabled;
      psec_ENABLE_PC_EVF :               FEvent := OnViewfinderEnabled;
      psec_FULL_VIEW_RELEASED :          FEvent := OnFullViewReleased;
      psec_THUMBNAIL_RELEASED :          FEvent := OnThumbnailReleased;
      psec_CHANGE_BATTERY_STATUS :       FEvent := OnBatteryStatusChanged;
      psec_PUSHED_RELEASE_SW :           FEvent := OnReleaseSwitchPressed;
      psec_RC_PROP_CHANGED :             FEvent := OnRCPropertyChanged;
      psec_RC_ROTATION_ANGLE_CHANGED :   FEvent := OnRCRotationAngleChanged;
      psec_RC_CHANGED_BY_CAM_UI :        FEvent := OnRCChangeViaCameraUI;
      psec_CAL_SHUTDOWN :                    FEvent := OnShutdown;
      else
        FEvent := nil;
    end;

    if Assigned(FEvent) then
      FEvent(Cam, EventData);
  end
end;

procedure TpsManager.ProcessGetFileData;
begin

end;

procedure TpsManager.RefreshDeviceList;
var
  DevList : PprDeviceList;
  BuffSize : prUInt32;
  idx : Integer;
begin
  HasToBeActive;

  ClearDeviceList;

  if psResponseErrorID(FLib.GetDeviceList(BuffSize, nil)) = prINSUFFICIENT_BUFFER then
  begin
    GetMem(DevList, BuffSize);
    try
      psCheckResponse(FLib.GetDeviceList(BuffSize, DevList));

      for idx := 0 to DevList.NumList - 1 do
      begin
        FDeviceList.Add(TpsDevice.Create(Self, DevList.DeviceInfo[idx]));
      end;
    finally
      FreeMem(DevList);
    end;
  end;

end;

procedure TpsManager.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;

    if FActive then
    begin
      if Trim(FLibFile) <> EmptyStr then
        FLib := TpsrcLibrary.Create(FLibFile)
      else
        FLib := TpsrcLibrary.Create();

      FLib.StartSDK;
    end
    else
    begin
      FLib.FinishSDK;
      FLib.Free;
    end;

  end;
end;

procedure TpsManager.SetCameraEvent(const Index: Integer;
  const Value: TCameraEvent);
begin
  FCameraEvents[Index] := Value;
end;

{------------------------------------------------------------------------------
-------------------------------------------------------------------------------}
{ TpsDevice }

constructor TpsDevice.Create(AManager : TpsManager; ADeviceInfo: prDeviceInfoTable);
begin
  FManager := AManager;

  CopyMemory(@FDeviceInfo, @ADeviceInfo, SizeOf(prDeviceInfoTable));

  FInternalName := PWideChar(@ADeviceInfo.DeviceInternalName[0]);
  FModelName := PWideChar(@ADeviceInfo.ModelName[0]);
  FGeneration := ADeviceInfo.Generation and (not prGENERATION_CAMERA_MASK);
  FSubGeneration := prSUB_GENERATION_CAMERA(ADeviceInfo.Generation);
  FModelID := ADeviceInfo.ModelID;
end;

function TpsDevice.CreateCamera: TpsCamera;
begin
  Result := TpsCamera.Create(FManager, FDeviceInfo);
end;

destructor TpsDevice.Destroy;
begin

  inherited;
end;

{------------------------------------------------------------------------------
-------------------------------------------------------------------------------}
{ TpsDeviceList }

function TpsDeviceList.Add(Item: TpsDevice): Integer;
begin
  Result := inherited Add(Item);
end;

function TpsDeviceList.Extract(Item: TpsDevice): TpsDevice;
begin
  Result := inherited Extract(Item);
end;

function TpsDeviceList.First: TpsDevice;
begin
  Result := inherited First;
end;

function TpsDeviceList.Get(Index: Integer): TpsDevice;
begin
  Result := inherited Get(Index);
end;

function TpsDeviceList.IndexOf(Item: TpsDevice): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TpsDeviceList.Insert(Index: Integer; Item: TpsDevice);
begin
  inherited Insert(Index, Item);
end;

function TpsDeviceList.Last: TpsDevice;
begin
  Result := inherited Last;
end;

procedure TpsDeviceList.Put(Index: Integer; const Value: TpsDevice);
begin
  inherited Put(Index, Value);
end;

function TpsDeviceList.Remove(Item: TpsDevice): Integer;
begin
  Result := inherited Remove(Item); 
end;

procedure TpsDeviceList.SaveTo(const AStrings: TStrings);
var
  idx : Integer;
begin
  AStrings.Clear;

  for idx := 0 to Count - 1 do
    AStrings.AddObject(Items[idx].FModelName, Items[idx]);
end;

{------------------------------------------------------------------------------
-------------------------------------------------------------------------------}
{ TpsCamera }

function TpsCamera.CanDoOperation(AOperation: TpsOperationCode): Boolean;
begin
  Result := AOperation in FSupportedOperations;
end;

procedure TpsCamera.ClearEventCallBack;
begin
  psCheckResponse(FManager.Lib.ClearEventCallBack(FCamera));
end;

constructor TpsCamera.Create(AManager : TpsManager; ADeviceInfo: prDeviceInfoTable);
begin
  FManager := AManager;

  FillMemory(@FProperties,SizeOf(FProperties), 0);

  FCamera := 0;
  FConnected := false;
  FReleaseControl := false;

  psCheckResponse(FManager.Lib.CreateCameraObject(ADeviceInfo, FCamera));

  if FCamera = 0 then
    raise Exception.Create('Can not create camera object');

  FManager.Cameras.Add(Self);

  SetEventCallBack;
end;

destructor TpsCamera.Destroy;
begin
  ClearEventCallBack;
  
  FManager.Cameras.Remove(Self);

  ReleaseControlEnabled := false;
  Connected := false;

  if FCamera > 0 then
    psCheckResponse(FManager.Lib.DestroyCameraObject(FCamera));
  
  inherited;
end;

procedure TpsCamera.FreeProperties;
var
  idx : TpsPropertyCode;
begin
  for idx := Low(FProperties) to High(FProperties) do
  begin
    if Assigned(FProperties[idx]) then
    begin
      FProperties[idx].Free;
      FProperties[idx] := nil;
    end;
  end;
end;

function TpsCamera.GetPropertyBattery_Kind: TpsProperty;
begin
  if not Assigned(FProperties[pspc_BATTERY_KIND]) then
    FProperties[pspc_BATTERY_KIND] := TpsProperty.Create(FManager, Self, pspc_BATTERY_KIND);
  Result := FProperties[pspc_BATTERY_KIND];
end;

procedure TpsCamera.HasToBeConnected;
begin
  if not Connected then
    raise Exception.Create('The camera object has to be connected for this action');
end;

procedure TpsCamera.ProcessEvent(AEventData: TpsCameraEventData);
begin

end;

procedure TpsCamera.ReadInfo;
var
  Info : Pointer;
  BuffSize : prUInt32;
  idx : Integer;
  cnt : prUInt32;

  BuffPointer : Pointer;
begin
  if psResponseErrorID(FManager.Lib.GetDeviceInfo(FCamera, BuffSize, nil)) = prINSUFFICIENT_BUFFER then
  begin
    GetMem(Info, BuffSize);
    try
      psCheckResponse(FManager.Lib.GetDeviceInfo(FCamera, BuffSize, Info));

      BuffPointer := Info;

      FStandardVersion := psReadBufferUInt16(BuffPointer);
      FVendorExtensionID := psReadBufferUInt32(BuffPointer);
      FVendorExtensionVersion := psReadBufferUInt16(BuffPointer);
      FVendorExtentsionDesc := psReadBufferString(BuffPointer);
      FFunctionMode := psReadBufferUInt16(BuffPointer);

      {$REGION 'supported operations'}
      FSupportedOperations := [];
      cnt := psReadBufferUInt32(BuffPointer);
      for idx := 0 to cnt - 1 do
        Include(FSupportedOperations, TpsOperationCode(psTranslateToEnum(psReadBufferUInt16(BuffPointer), psOperationCodes)));
      {$ENDREGION}

      {$REGION 'supported events'}
      FSupportedEvents := [];
      cnt := psReadBufferUInt32(BuffPointer);
      for idx := 0 to cnt - 1 do
        Include(FSupportedEvents, TpsEventCode(psTranslateToEnum(psReadBufferUInt16(BuffPointer), psEventCodes)));
      {$ENDREGION}

      {$REGION 'supported device properties'}
      FSupportedProperties := [];
      cnt := psReadBufferUInt32(BuffPointer);
      for idx := 0 to cnt - 1 do
        Include(FSupportedProperties, TpsPropertyCode(psTranslateToEnum(psReadBufferUInt16(BuffPointer), psPropertyCodes)));
      {$ENDREGION}

      {$REGION 'capture formats'}
      FSupportedCaptureFormats := [];
      cnt := psReadBufferUInt32(BuffPointer);
      for idx := 0 to cnt - 1 do
        Include(FSupportedCaptureFormats, TpsObjectFormat(psTranslateToEnum(psReadBufferUInt16(BuffPointer), psObjectFormats)));
      {$ENDREGION}

      {$REGION 'image formats'}
      FSupportedImageFormats := [];
      cnt := psReadBufferUInt32(BuffPointer);
      for idx := 0 to cnt - 1 do
        Include(FSupportedImageFormats, TpsObjectFormat(psTranslateToEnum(psReadBufferUInt16(BuffPointer), psObjectFormats)));
      {$ENDREGION}

      FManufacturer := psReadBufferString(BuffPointer);
      FModel := psReadBufferString(BuffPointer);
      FDeviceVersion := psReadBufferString(BuffPointer);
      FSerialNumber := psReadBufferString(BuffPointer);
    finally
      FreeMem(Info);
    end;
  end;
end;

procedure TpsCamera.Release();
begin
  HasToBeConnected;

  if CanDoOperation(psop_RC_CAPTURE) then
  begin
    psCheckResponse(FManager.Lib.RC_Release(FCamera));
  end;
end;

procedure TpsCamera.SetConnected(const Value: Boolean);
begin
  if FConnected <> Value then
  begin
    if Value then
    begin
      psCheckResponse(FManager.Lib.ConnectCamera(FCamera));
      ReadInfo;
    end
    else
    begin
      psCheckResponse(FManager.Lib.DisconnectCamera(FCamera));
      FreeProperties;
    end;

    FConnected := Value;

    if FManager.AutoEnableReleaseControl then
      ReleaseControlEnabled := Value;
  end;
end;

procedure TpsCamera.SetEventCallBack;
begin
  psCheckResponse(FManager.Lib.SetEventCallBack(FCamera, prContext(Self), prSetEventCB));
end;

procedure TpsCamera.SetReleaseControl(const Value: Boolean);
begin
  if FReleaseControl <> Value then
  begin
    HasToBeConnected;

    if Value then
    begin
      if CanDoOperation(psop_INITIATE_RELEASE_CONTROL) then
        psCheckResponse(FManager.Lib.InitiateReleaseControl(FCamera))
    end
    else
    begin
      if CanDoOperation(psop_TERMINATE_RELEASE_CONTROL) then
        psCheckResponse(FManager.Lib.TerminateReleaseControl(FCamera));
    end;
      
    FReleaseControl := Value;
  end;
end;

{------------------------------------------------------------------------------
-------------------------------------------------------------------------------}
{ TpsCameraList }

function TpsCameraList.Add(Item: TpsCamera): Integer;
begin
  Result := inherited Add(Item);
end;

function TpsCameraList.Extract(Item: TpsCamera): TpsCamera;
begin
  Result := inherited Extract(Item);
end;

function TpsCameraList.First: TpsCamera;
begin
  Result := inherited First;
end;

function TpsCameraList.Get(Index: Integer): TpsCamera;
begin
  Result := inherited Get(Index);
end;

function TpsCameraList.IndexOf(Item: TpsCamera): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TpsCameraList.Insert(Index: Integer; Item: TpsCamera);
begin
  inherited Insert(Index, Item);
end;

function TpsCameraList.Last: TpsCamera;
begin
  Result := inherited Last;
end;

procedure TpsCameraList.Put(Index: Integer; const Value: TpsCamera);
begin
  inherited Put(Index, Value);
end;

function TpsCameraList.Remove(Item: TpsCamera): Integer;
begin
  Result := inherited Remove(Item);
end;

{------------------------------------------------------------------------------
-------------------------------------------------------------------------------}
{ TpsProperty }

constructor TpsProperty.Create(AManager : TpsManager; ACamera: TpsCamera;
  APropertyCode: TpsPropertyCode);
begin
  FManager := AManager;
  FCamera := ACamera;
  FCode := APropertyCode;
  FAvailable := FCode in FCamera.SupportedProperties;
  FBaseTypeCode := psGetBaseTypeCode(FTypeCode);

  ReadDescription;
end;

procedure TpsProperty.CreatePropertyValue(var AValue: TpsPropertyValue;
  ASize: Integer);
begin
  AValue.Size := ASize;

  if ASize > 0 then
    GetMem(AValue.Value, ASize);
end;

destructor TpsProperty.Destroy;
var
  idx : Integer;
begin
  DestroyPropertyValue(FFactoryDefault);

  case FFormFlag of
    psff_Range:
      begin
        DestroyPropertyValue(FRangeMinValue);
        DestroyPropertyValue(FRangeMaxValue);
        DestroyPropertyValue(FRangeStep);
      end;
    psff_Enum:
      begin
        for idx := Low(FEnumSupportedValues) to High(FEnumSupportedValues) do
          DestroyPropertyValue(FEnumSupportedValues[idx]);
        SetLength(FEnumSupportedValues, 0);
      end;
  end;

  inherited;
end;

procedure TpsProperty.DestroyPropertyValue(var AValue: TpsPropertyValue);
begin
  if Assigned(AValue.Value) then
    FreeMem(AValue.Value, AValue.Size);

  AValue.Size := 0;
end;

function TpsProperty.GetValid: Boolean;
begin
  Result := true;
end;


function TpsProperty.GetValueBufferSize(ABuffer: Pointer): Integer;
begin
  Result := 0;
end;

procedure TpsProperty.ReadDescription;
var
  Descr : Pointer;
  BuffSize : prUInt32;
  BufferPointer : Pointer;
  idx : Integer;
  PropCodeSDK : prUInt16;
begin
  FCamera.HasToBeConnected;

  if not Available then
    exit;

  PropCodeSDK := psTranslateToSDKConst(Byte(FCode), psPropertyCodes);

  BuffSize := 1;

  if psResponseErrorID(FManager.Lib.GetDevicePropDesc(FCamera.Handle,
                                                      PropCodeSDK,
                                                      BuffSize,
                                                      nil)) = prINSUFFICIENT_BUFFER then
  begin
    GetMem(Descr, BuffSize);
    try
      psCheckResponse(FManager.Lib.GetDevicePropDesc(FCamera.Handle,
                                                     PropCodeSDK,
                                                     BuffSize,
                                                     Descr));
      BufferPointer := Descr;

      if  TpsPropertyCode(psTranslateToEnum(psReadBufferUInt16(BufferPointer), psPropertyCodes)) <> FCode then
        raise Exception.Create('Error while reading property description');
        
      FTypeCode := TpsPropertyTypeCode(psTranslateToEnum(psReadBufferUInt16(BufferPointer), psPropertyTypeCodes));
      FAccessMode := TpsPropertyAccessMode(psTranslateToEnum(psReadBufferUInt8(BufferPointer), psPropertyAccessModes));
      FReadOnly := FAccessMode = psam_ReadOnly;

      CreatePropertyValue(FFactoryDefault, GetValueBufferSize(BufferPointer));
      ReadValueBuffer(BufferPointer, FFactoryDefault);
      Inc(PByte(BufferPointer), FFactoryDefault.Size);

      Inc(PByte(BufferPointer), GetValueBufferSize(BufferPointer));

      FFormFlag := TpsPropertyFormFlag(psTranslateToEnum(psReadBufferUInt8(BufferPointer), psPropertyFormFlags));

      case FFormFlag of
        psff_Range:
          begin
            CreatePropertyValue(FRangeMinValue, GetValueBufferSize(BufferPointer));
            ReadSingleBaseValueBuffer(BufferPointer, FRangeMinValue);
            Inc(PByte(BufferPointer), FRangeMinValue.Size);

            CreatePropertyValue(FRangeMaxValue, GetValueBufferSize(BufferPointer));
            ReadSingleBaseValueBuffer(BufferPointer, FRangeMaxValue);
            Inc(PByte(BufferPointer), FRangeMaxValue.Size);

            CreatePropertyValue(FRangeStep, GetValueBufferSize(BufferPointer));
            ReadSingleBaseValueBuffer(BufferPointer, FRangeStep);
            Inc(PByte(BufferPointer), FRangeStep.Size);
          end;
        psff_Enum:
          begin
            SetLength(FEnumSupportedValues, psReadBufferUInt16(BufferPointer));
            for idx := Low(FEnumSupportedValues) to High(FEnumSupportedValues) do
            begin
              CreatePropertyValue(FEnumSupportedValues[idx], GetValueBufferSize(BufferPointer));
              ReadValueBuffer(BufferPointer, FEnumSupportedValues[idx]);
              Inc(PByte(BufferPointer), FEnumSupportedValues[idx].Size);
            end;
          end;
      end;

    finally
      FreeMem(Descr, BuffSize);
    end;
  end;

end;


procedure TpsProperty.ReadSingleBaseValueBuffer(ABuffer: Pointer;
  var AValue: TpsPropertyValue);
begin

end;

procedure TpsProperty.ReadValueBuffer(ABuffer: Pointer;
  var AValue: TpsPropertyValue);
begin
  CopyMemory(AValue.Value, ABuffer, AValue.Size);
end;

initialization
  TheOneAndOnlyManager := nil;

finalization

end.
