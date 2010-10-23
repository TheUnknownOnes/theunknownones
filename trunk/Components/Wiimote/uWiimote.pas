{-----------------------------------------------------------------------------
 Purpose: Interface the Wiimote
 Created: 13.06.2008 06:38:08
 
 (c) by TheUnknownOnes under dwywbdbu license - see http://theunknownones.googlecode.com/svn/ for the license
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uWiimote;

interface

uses
  SetupAPI,
  Classes,
  Windows,
  SysUtils;


const
  WIIMOTE_VENDOR_ID = $057E;
  WIIMOTE_PRODUCT_ID = $0306;

  WIIMOTE_REPORT_LEN = 22;

  WIIMOTE_MEMADDR_IR				= $04B00030;
  WIIMOTE_MEMADDR_IR_SENSITIVITY_1	= $04B00000;
  WIIMOTE_MEMADDR_IR_SENSITIVITY_2	= $04B0001A;
  WIIMOTE_MEMADDR_IR_MODE			= $04B00033;

  WIIMOTE_KNOWN_REPORTS : array[0..11] of Byte = ($11,
                                                  $12,
                                                  $13,
                                                  $15,
                                                  $16,
                                                  $17,
                                                  $1A,
                                                  $20,
                                                  $21,
                                                  $30,
                                                  $31,
                                                  $33);


type
  TwmRawReport = array[0..WIIMOTE_REPORT_LEN - 1] of Byte;
  PwmRawReport = ^TwmRawReport;
  TwmRawData = array of Byte;

  TwmButton = (wmbA,
               wmbB,
               wmbPlus,
               wmbMinus,
               wmbHome,
               wmbOne,
               wmbTwo,
               wmbUp,
               wmbDown,
               wmbLeft,
               wmbRight);

  TwmButtons = set of TwmButton;

  TwmLED = (wmLED1,
            wmLED2,
            wmLED3,
            wmLED4);

  TwmLEDs = set of TwmLED;

  TwmIRMode = (wmiOff       = $00,
               wmiBasic     = $01,
               wmiExtended  = $03,
               wmiFull      = $05);

  TwmIRSensitivity = (wmiSense1,
                      wmiSense2,
                      wmiSense3,
                      wmiSense4,
                      wmiSense5,
                      wmiSenseMax);


  TwmAccelCalibration = record
    X0,
    Y0,
    Z0,
    XG,
    YG,
    ZG : Byte;
  end;

  TwmReport = class
  protected
    FBuffer : TwmRawReport;

    procedure CheckBufferIndex(const AIndex : Integer);

    function GetValue(AIndex: Integer): Byte;
    procedure SetValue(AIndex: Integer; const Value: Byte);

    procedure SetBitsInValue(ASet : Boolean; AValueIndex : Integer; ABits : Byte);
    function GetBitsInValue(AValueIndex : Integer; ABits : Byte) : Boolean;
  public
    class function CreateReport(const ARawReport : TwmRawReport;
                                out ANewReport : TwmReport) : Boolean;

    constructor Create(ARawReport : TwmRawReport); overload; virtual;
    constructor Create(); overload; virtual;

    destructor Destroy(); override;

    procedure Clear; virtual;

    property ReportID : Byte index 0 read GetValue;
  end;

  TwmReportClass = class of TwmReport;


//==============================================================================


  TwmOutputReport = class(TwmReport)
  //Just a ancestor for reducing code
  //Dont send this report directly!
  protected
    procedure SetRumble(const Value: Boolean);
    function GetRumble : Boolean;
  public
    property Rumble : Boolean read GetRumble write SetRumble;
  end;


//==============================================================================


  TwmOutputReportLEDs = class(TwmOutputReport)
  //Sets the LEDs on or off
  protected
    function GetLEDsOn: TwmLEDs;
    procedure SetLEDsOn(const Value: TwmLEDs);
  public
    constructor Create(); override;

    property LEDsOn : TwmLEDs read GetLEDsOn write SetLEDsOn;
  end;


//==============================================================================


  TwmOutputReportReporting = class(TwmOutputReport)
  //Requests the specified report from the wiimote
  protected
    function GetContinuous: Boolean;
    function GetReportType: TwmReportClass;
    procedure SetContinuous(const Value: Boolean);
    procedure SetReportType(const Value: TwmReportClass);
  public
    constructor Create(); override;

    property ReportType : TwmReportClass read GetReportType write SetReportType;
    property Continuous : Boolean read GetContinuous write SetContinuous;
  end;


//==============================================================================


  TwmOutputReportIR = class(TwmOutputReport)
  protected
    procedure SetEnabled(const Value: Boolean);
    function GetEnabled : Boolean;
  public
    constructor Create(); override;

    property Enabled : Boolean read GetEnabled write SetEnabled;
  end;


//==============================================================================


  TwmOutputReportIR2 = class(TwmOutputReportIR)
  public
    constructor Create(); override;
  end; 


//==============================================================================


  TwmOutputReportStatus = class(TwmOutputReport)
  //Requests a status report
  public
    constructor Create(); override;
  end;


//==============================================================================


  TwmOutputReportMemory = class(TwmOutputReport)
  protected
    function GetAddress: Cardinal;
    procedure SetAddress(const Value: Cardinal);
  public
    property Address : Cardinal read GetAddress write SetAddress;
  end;


//==============================================================================

  TwmOutputReportWriteMemory = class(TwmOutputReportMemory)
  protected
    function GetRawData: TwmRawData;
    procedure SetRawData(const Value: TwmRawData);
  public
    constructor Create(); override;

    property Data : TwmRawData read GetRawData write SetRawData;
  end;


//==============================================================================


  TwmOutputReportReadMemory = class(TwmOutputReportMemory)
  protected
    function GetSize: Word;
    procedure SetSize(const Value: Word);
  public
    constructor Create(); override;

    property Size : Word read GetSize write SetSize; 
  end;


//==============================================================================


  TwmInputReportButtons = class(TwmReport)
  protected
    function GetButtonsDown: TwmButtons;
  public
    constructor Create(); override;

    property ButtonsDown : TwmButtons read GetButtonsDown;
  end;


//==============================================================================


  TwmInputReportStatus = class(TwmInputReportButtons)
  private
    function GetLEDsOn: TwmLEDs;
  public
    constructor Create(); override;

    property BatterLevel : Byte index 6 read GetValue;
    property LEDsOn : TwmLEDs read GetLEDsOn;
  end;


//==============================================================================


  TwmInputReportReadMemory = class(TwmInputReportButtons)
  protected
    function GetErrorInvalidAddress: Boolean;
    function GetErrorWriteOnlyMemory: Boolean;

    function GetData: TwmRawData;
  public
    constructor Create(); override;

    property ErrorInvalidAddress : Boolean read GetErrorInvalidAddress;
    property ErrorWriteOnlyMemory : Boolean read GetErrorWriteOnlyMemory;

    property Data : TwmRawData read GetData;
  end;


//==============================================================================


  TwmInputReportButtonsAccel = class(TwmInputReportButtons)
  public
    constructor Create(); override;

    property XAccl : Byte index 3 read GetValue;
    property YAccl : Byte index 4 read GetValue;
    property ZAccl : Byte index 5 read GetValue;
  end;


//==============================================================================


  TwmInputReportButtonsAccelIR = class(TwmInputReportButtonsAccel)
  protected
    FIRMode: TwmIRMode;

    function GetIRPoint(AIndex: Integer): TPoint;
    function GetIRPointCount: Integer;
    function GetIRPointSize(AIndex : Integer): Byte;
  public
    constructor Create(); override;

    //set this, before working with the values
    property IRMode : TwmIRMode read FIRMode write FIRMode;

    property IRPoint[AIndex : Integer] : TPoint read GetIRPoint;
    property IRPointSize[AIndex : Integer] : Byte read GetIRPointSize;
    property IRPointCount : Integer read GetIRPointCount;
  end;


//==============================================================================

  TwmOnReportProc = procedure(const AReport : TwmReport) of object;

//==============================================================================


  TwmDeviceConnection = class(TThread)
  protected
    FHandle : THandle;

    FReportsToWrite : TThreadList;

    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;

    FOnNewReport: TwmOnReportProc;

    FNewReport : TwmReport;

    function GetConnected: Boolean;

    procedure Execute; override;

    procedure DoWriteReports;
    procedure DoReadReports;
    procedure DoNewReport;
  public
    class function ListDevices(AList : TStrings) : Integer;

    constructor Create(); virtual;
    destructor Destroy(); override;

    function Connect(ADevicePath : String) : Boolean;
    function Disconnect : Boolean;

    function WriteReport(const AReport : TwmReport) : Boolean;

    property Connected : Boolean read GetConnected;
    property Handle : THandle read FHandle;

    property OnConnected : TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected : TNotifyEvent read FOnDisconnected write FOnDisconnected;
    property OnNewReport : TwmOnReportProc read FOnNewReport write FOnNewReport;
  end;


//==============================================================================


  TwmOnButtonProc = procedure(AButton : TwmButton) of object;


//==============================================================================


  TCustomWiimote = class(TComponent)
  protected
    FConnection : TwmDeviceConnection;

    FReadMemStack : TStringList;
    //Identifies, which memory-block is replied in the next reports

    FAccelCalibration : TwmAccelCalibration;
    FAccels : array[0..2] of Single;
    FRawAccels : array[0..2] of Byte;
    
    FButtonsDown : TwmButtons;
    FLedsOn: TwmLEDs;
    FRumble : Boolean;
    FBattery : Byte;
    FIRMode: TwmIRMode;
    FIRSense: TwmIRSensitivity;
    FIRPoints : array[0..3] of TPoint;
    FIRPointSizes : array[0..3] of Byte;
    FIRPointCount : Byte;

    FOnNewReport: TwmOnReportProc;
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FOnButtonUp: TwmOnButtonProc;
    FOnButtonDown: TwmOnButtonProc;
    FOnButtonIsDown: TwmOnButtonProc;
    FOnStatus: TNotifyEvent;

    function GetData1ForIRSense(ASensitivity : TwmIRSensitivity) : TwmRawData;
    function GetData2ForIRSense(ASensitivity : TwmIRSensitivity) : TwmRawData;

    procedure DoNewReport(const AReport : TwmReport); virtual;
    procedure DoConnect(Sender : TObject); virtual;
    procedure DoDisconnect(Sender : TObject); virtual;

    procedure ReadAccelCalibration;

    procedure ExtractReadMemory(AReport : TwmInputReportReadMemory);
    procedure ExtractAccel(AReport : TwmInputReportButtonsAccel);
    procedure ExtractButtonStates(AReport : TwmInputReportButtons);
    procedure ExtractStatusinfos(AReport : TwmInputReportStatus);
    procedure ExtractIRInfos(AReport : TwmInputReportButtonsAccelIR);

    function GetCurrentAccel(const Index: Integer): Single;
    function GetButtonsDown: TwmButtons;
    function GetConnected: Boolean;
    procedure SetRumble(AValue : Boolean);
    procedure SetLedsOn(const Value: TwmLEDs);
    procedure SetIRMode(const Value: TwmIRMode);
    function GetIRPoint(AIndex: Integer): TPoint;
    function GetIRPointSize(AIndex: Integer): Byte;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Connect(ADevicePath : String) : Boolean;
    function Disconnect() : Boolean;

    procedure RequestStatus;

    property Connected : Boolean read GetConnected;

    property Rumble : Boolean read FRumble write SetRumble;

    property AccelX : Single index 0 read GetCurrentAccel;
    property AccelY : Single index 1 read GetCurrentAccel;
    property AccelZ : Single index 2 read GetCurrentAccel;

    property ButtonsDown : TwmButtons read GetButtonsDown;

    property LedsOn : TwmLEDs read FLedsOn write SetLedsOn;

    property BatteryPercent : Byte read FBattery;

    property IRMode : TwmIRMode read FIRMode write SetIRMode;
    property IRSensitivity : TwmIRSensitivity read FIRSense write FIRSense;
    property IRPointPos[AIndex : Integer] : TPoint read GetIRPoint;
    property IRPointSize[AIndex : Integer] : Byte read GetIRPointSize;
    property IRPointCount : Byte read FIRPointCount;

    property OnNewReport : TwmOnReportProc read FOnNewReport write FOnNewReport;
    property OnStatus : TNotifyEvent read FOnStatus write FOnStatus;

    property OnConnected : TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected : TNotifyEvent read FOnDisconnected write FOnDisconnected;

    property OnButtonDown : TwmOnButtonProc read FOnButtonDown write FOnButtonDown;
    property OnButtonUp : TwmOnButtonProc read FOnButtonUp write FOnButtonUp;
    property OnButtonIsDown : TwmOnButtonProc read FOnButtonIsDown write FOnButtonIsDown;
  end;


//==============================================================================

  TWiimote = class(TCustomWiimote)
  published
    property OnNewReport;
    property OnStatus;

    property OnConnected;
    property OnDisconnected;

    property OnButtonDown;
    property OnButtonUp;
    property OnButtonIsDown;
  end;



implementation //===============================================================


//Todo: Get this from a "official" unit
type
  THID_Attributes = record
    Size : Integer;
    VendorID : Short;
    ProductID : Short;
    VersionNumber : Short;
  end;

procedure HidD_GetHidGuid(out AGUID : TGUID); stdcall; external 'hid.dll';
function HidD_GetAttributes(ADeviceHandle : THandle;
                            out AAttributes : THID_Attributes) : BOOL; stdcall; external 'hid.dll';

function HidD_GetInputReport(HidDeviceObject: THandle; Buffer: Pointer; BufferLength: ULONG): LongBool; stdcall; external 'hid.dll';
function HidD_SetOutputReport(HidDeviceObject: THandle; Buffer: Pointer; BufferLength: ULONG): LongBool; stdcall; external 'hid.dll';



//==============================================================================


{ TwmDeviceConnection }

function TwmDeviceConnection.Connect(ADevicePath: String): Boolean;
begin
  Result := (not Connected) or (Connected and Disconnect);

  if Result then
  begin
    FHandle := CreateFile(PChar(ADevicePath),
                          GENERIC_READ or GENERIC_WRITE,
                          FILE_SHARE_READ or FILE_SHARE_WRITE,
                          nil,
                          OPEN_EXISTING,
                          FILE_FLAG_OVERLAPPED,
                          0);

    Result := Connected;
  end;

  if Result and Assigned(FOnConnected) then
    FOnConnected(Self);
end;

constructor TwmDeviceConnection.Create;
begin
  inherited Create(true);
  FreeOnTerminate := true;

  FHandle := 0;

  FReportsToWrite := TThreadList.Create;

  Resume;
end;

destructor TwmDeviceConnection.Destroy;
var
  lst : TList;
  Data : PwmRawReport;
begin
  Disconnect;

  lst := FReportsToWrite.LockList;
  try
    while lst.Count > 0 do
    begin
      Data := lst[0];
      Dispose(Data);
      lst.Delete(0);
    end;
  finally
    FReportsToWrite.UnlockList;
  end;

  FReportsToWrite.Free;

  inherited;
end;

function TwmDeviceConnection.Disconnect: Boolean;
begin
  Result := not Connected;

  if not Result then
  begin
    Result := CloseHandle(FHandle);

    if Result then
      FHandle := 0;
  end;

  if Result and Assigned(FOnDisconnected) then
    FOnDisconnected(Self);
end;

procedure TwmDeviceConnection.DoNewReport;
begin
  if Assigned(FOnNewReport) then
    FOnNewReport(FNewReport);
end;

procedure TwmDeviceConnection.DoReadReports;
var
  ReportID : Byte;
  RawReport : TwmRawReport;
begin
  if Connected then
  begin
    for ReportID in WIIMOTE_KNOWN_REPORTS do
    begin
      RawReport[0] := ReportID;

      if HidD_GetInputReport(FHandle, @RawReport, WIIMOTE_REPORT_LEN) then
      begin
        TwmReport.CreateReport(RawReport, FNewReport);
        try
          Synchronize(DoNewReport);
        finally
          FNewReport.Free;
        end;
      end;
    end;
  end;
end;

procedure TwmDeviceConnection.DoWriteReports;
var
  lst : TList;
  Data : PwmRawReport;
begin
  if Connected then
  begin
    lst := FReportsToWrite.LockList;
    try
      while lst.Count > 0 do
      begin
        try
          Data := lst[0];
          HidD_SetOutputReport(FHandle, Data, WIIMOTE_REPORT_LEN);
          Dispose(Data);
        finally
          lst.Delete(0);
        end;
      end;
    finally
      FReportsToWrite.UnlockList;
    end;
  end;
end;

procedure TwmDeviceConnection.Execute;
begin
  while not Terminated do
  begin

    try
      DoWriteReports;
      DoReadReports;
    except
      Disconnect;
    end;

    Sleep(100);
  end;
end;

function TwmDeviceConnection.GetConnected: Boolean;
begin
  Result := FHandle <> 0;
end;

class function TwmDeviceConnection.ListDevices(AList: TStrings): Integer;
var
  HID_GUID : TGUID;
  AllDevices : HDEVINFO;
  DeviceData : TSPDeviceInterfaceData;
  DeviceDetailData : PSPDeviceInterfaceDetailData;
  DeviceIndex : Integer;
  DetailDataSize : Cardinal;
  FileHandle : Cardinal;
  Attributes : THID_Attributes;
begin
  Result := 0;
  
  DeviceIndex := 0;

  AList.Clear;

  //first, get the GUID for HI-devices
  HidD_GetHidGuid(HID_GUID);

  //find all HID's
  AllDevices := SetupDiGetClassDevs(@HID_GUID, nil, 0, DIGCF_DEVICEINTERFACE or DIGCF_PRESENT); 

  //init the records
  FillChar(DeviceData, SizeOf(TSPDeviceInterfaceData), 0);
  DeviceData.cbSize := SizeOf(TSPDeviceInterfaceData);

  //walk through all devices ...
  while SetupDiEnumDeviceInterfaces(AllDevices, nil, HID_GUID, DeviceIndex, DeviceData) do
  begin

    //lookup the size of the device detail data
    SetupDiGetDeviceInterfaceDetail(AllDevices,
                                    @DeviceData,
                                    nil,
                                    0,
                                    DetailDataSize,
                                    nil);

    //Alloc Mem for the device detail data                                    
    GetMem(DeviceDetailData, DetailDataSize);
    try
      DeviceDetailData^.cbSize := SizeOf(TSPDeviceInterfaceDetailData);

      //get the device details
      if SetupDiGetDeviceInterfaceDetail(AllDevices,
                                         @DeviceData,
                                         DeviceDetailData,
                                         DetailDataSize,
                                         DetailDataSize,
                                         nil) then
      begin

        //"connect" to the device
        FileHandle := CreateFile(DeviceDetailData.DevicePath,
                                 GENERIC_READ or GENERIC_WRITE,
                                 FILE_SHARE_READ or FILE_SHARE_WRITE,
                                 nil,
                                 OPEN_EXISTING,
                                 FILE_FLAG_OVERLAPPED,
                                 0);

        try
          //get the attributes of the device
          if HidD_GetAttributes(FileHandle, Attributes) then
          begin

            //if the VendorID and the ProductID matches, is it a wiimote
            if (Attributes.VendorID = WIIMOTE_VENDOR_ID) and
               (Attributes.ProductID = WIIMOTE_PRODUCT_ID) then
            begin
              if Assigned(AList) then
                AList.Add(StrPas(DeviceDetailData.DevicePath));

              Inc(Result);
            end;

          end;

        finally
          CloseHandle(FileHandle);
        end;
      end;

    finally
      FreeMem(DeviceDetailData, DetailDataSize);
    end; 

    //we want to see the next device
    Inc(DeviceIndex);
  end;

  SetupDiDestroyDeviceInfoList(AllDevices);
end;


function TwmDeviceConnection.WriteReport(const AReport: TwmReport): Boolean;
var
  lst : TList;
  Data : PwmRawReport;
begin
  Result := Connected;

  if Result then
  begin
    lst := FReportsToWrite.LockList;
    try
      New(Data);
      CopyMemory(Data, @AReport.FBuffer, WIIMOTE_REPORT_LEN);
      lst.Add(Data);
    finally
      FReportsToWrite.UnlockList;
    end;
  end;
end;


//==============================================================================


{ TwmReport }

procedure TwmReport.CheckBufferIndex(const AIndex: Integer);
begin
  if (AIndex < Low(FBuffer)) or
     (AIndex > High(FBuffer)) then
    raise Exception.Create('Index out of range');

end;

procedure TwmReport.Clear;
var
  idx : Integer;
begin
  for idx := 1 to WIIMOTE_REPORT_LEN - 1 do
    FBuffer[idx] := 0;
end;

constructor TwmReport.Create(ARawReport : TwmRawReport);
begin
  Create();

  if ARawReport[0] <> ReportID then
    raise Exception.Create('Invalid raw data for this report type');
  
  CopyMemory(@FBuffer, @ARawReport, WIIMOTE_REPORT_LEN);
end;

constructor TwmReport.Create;
begin
  FBuffer[0] := 0;
  Clear;
end;

class function TwmReport.CreateReport(const ARawReport: TwmRawReport;
  out ANewReport: TwmReport): Boolean;
begin
  Result := true;
  ANewReport := nil;

  case ARawReport[0] of
    $11 : ANewReport := TwmOutputReportLEDs.Create(ARawReport);
    $12 : ANewReport := TwmOutputReportReporting.Create(ARawReport);
    $13 : ANewReport := TwmOutputReportIR.Create(ARawReport);
    $15 : ANewReport := TwmOutputReportStatus.Create(ARawReport);
    $16 : ANewReport := TwmOutputReportWriteMemory.Create(ARawReport);
    $17 : ANewReport := TwmOutputReportReadMemory.Create(ARawReport);
    $1A : ANewReport := TwmOutputReportIR2.Create(ARawReport);
    $20 : ANewReport := TwmInputReportStatus.Create(ARawReport);
    $21 : ANewReport := TwmInputReportReadMemory.Create(ARawReport);
    $30 : ANewReport := TwmInputReportButtons.Create(ARawReport);
    $31 : ANewReport := TwmInputReportButtonsAccel.Create(ARawReport);
    $33 : ANewReport := TwmInputReportButtonsAccelIR.Create(ARawReport);
    else
    begin
      Result := false;
    end;
  end;
end;

destructor TwmReport.Destroy;
begin

  inherited;
end;

function TwmReport.GetBitsInValue(AValueIndex : Integer; ABits: Byte): Boolean;
begin
  CheckBufferIndex(AValueIndex);

  Result := (FBuffer[AValueIndex] and ABits) <> 0;
end;

function TwmReport.GetValue(AIndex: Integer): Byte;
begin
  CheckBufferIndex(AIndex);

  Result := FBuffer[AIndex];
end;

procedure TwmReport.SetBitsInValue(ASet: Boolean; AValueIndex: Integer;
  ABits: Byte);
begin
  CheckBufferIndex(AValueIndex);

  if ASet then
    FBuffer[AValueIndex] := FBuffer[AValueIndex] or ABits
  else
    FBuffer[AValueIndex] := FBuffer[AValueIndex] and (not ABits);
end;

procedure TwmReport.SetValue(AIndex: Integer; const Value: Byte);
begin
  CheckBufferIndex(AIndex);

  FBuffer[AIndex] := Value;
end;


//==============================================================================


{ TwmOutputReportLEDs }

constructor TwmOutputReportLEDs.Create();
begin
  inherited;

  FBuffer[0] := $11;
end;

function TwmOutputReportLEDs.GetLEDsOn: TwmLEDs;
begin
  Result := [];

  if GetBitsInValue(1, $10) then Include(Result, wmLED1);
  if GetBitsInValue(1, $20) then Include(Result, wmLED2);
  if GetBitsInValue(1, $40) then Include(Result, wmLED3);
  if GetBitsInValue(1, $80) then Include(Result, wmLED4);
end;

procedure TwmOutputReportLEDs.SetLEDsOn(const Value: TwmLEDs);
begin
  SetBitsInValue(wmLED1 in Value, 1, $10);
  SetBitsInValue(wmLED2 in Value, 1, $20);
  SetBitsInValue(wmLED3 in Value, 1, $40);
  SetBitsInValue(wmLED4 in Value, 1, $80);
end;

//==============================================================================


{ TwmOutputReportReporting }

constructor TwmOutputReportReporting.Create;
begin
  inherited;

  FBuffer[0] := $12;
end;

function TwmOutputReportReporting.GetContinuous: Boolean;
begin
  Result := GetBitsInValue(1, $04);
end;

function TwmOutputReportReporting.GetReportType: TwmReportClass;
begin
  case FBuffer[2] of
    $30: Result := TwmInputReportButtons;
    $20: Result := TwmInputReportStatus;
    $21: Result := TwmInputReportReadMemory;
    $31: Result := TwmInputReportButtonsAccel;
    $33: Result := TwmInputReportButtonsAccelIR;
    else
      Result := nil;
  end;
end;

procedure TwmOutputReportReporting.SetContinuous(const Value: Boolean);
begin
  SetBitsInValue(Value, 1, $04);
end;

procedure TwmOutputReportReporting.SetReportType(const Value: TwmReportClass);
begin
  if Value = TwmInputReportButtons  then
    FBuffer[2] := $30
  else
  if Value = TwmInputReportStatus then
    FBuffer[2] := $20
  else
  if Value = TwmInputReportReadMemory then
    FBuffer[2] := $21
  else
  if Value = TwmInputReportButtonsAccel then
    FBuffer[2] := $31
  else
  if Value = TwmInputReportButtonsAccelIR then
    FBuffer[2] := $33
  else
    FBuffer[2] := 0;
end;


//==============================================================================


{ TwmOutputReportIR }

constructor TwmOutputReportIR.Create;
begin
  inherited;

  FBuffer[0] := $13;
end;

function TwmOutputReportIR.GetEnabled: Boolean;
begin
  Result := GetBitsInValue(1, $04);
end;

procedure TwmOutputReportIR.SetEnabled(const Value: Boolean);
begin
  SetBitsInValue(Value, 1, $04);
end;


//==============================================================================


{ TwmOutputReportIR2 }

constructor TwmOutputReportIR2.Create;
begin
  inherited;

  FBuffer[0] := $1A;
end;


//==============================================================================


{ TwmOutputReportStatus }

constructor TwmOutputReportStatus.Create;
begin
  inherited;

  FBuffer[0] := $15;
end;


//==============================================================================


{ TwmOutputReport }

function TwmOutputReport.GetRumble: Boolean;
begin
  Result := GetBitsInValue(1, $01);
end;

procedure TwmOutputReport.SetRumble(const Value: Boolean);
begin
  SetBitsInValue(Value, 1, $01);
end;


//==============================================================================


{ TwmOutputReportWriteMemory }

constructor TwmOutputReportWriteMemory.Create;
begin
  inherited;

  FBuffer[0] := $16;
end;

function TwmOutputReportWriteMemory.GetRawData: TwmRawData;
var
  idx : Integer;
begin
  SetLength(Result, FBuffer[5]);

  for idx := Low(Result) to High(Result) do
    Result[idx] := FBuffer[6 + idx];
end;

procedure TwmOutputReportWriteMemory.SetRawData(const Value: TwmRawData);
var
  idx : Integer;
begin
  if Length(Value) > 16 then
    raise Exception.Create('Data size is limited to 16 byte');

  FBuffer[5] := Length(Value);

  for idx := Low(Value) to High(Value) do
    FBuffer[6 + idx] := Value[idx];
end;


//==============================================================================


{ TwmOutputReportReadMemory }

constructor TwmOutputReportReadMemory.Create;
begin
  inherited;

  FBuffer[0] := $17;
end;

function TwmOutputReportReadMemory.GetSize: Word;
begin
  Result := (FBuffer[5] shl 8) or
             FBuffer[6];
end;

procedure TwmOutputReportReadMemory.SetSize(const Value: Word);
begin
  FBuffer[5] := (Value and $FF00) shr 8;
  FBuffer[6] := Value and $FF;
end;

//==============================================================================

{ TwmOutputReportMemory }

function TwmOutputReportMemory.GetAddress: Cardinal;
begin
  Result := (FBuffer[1] shl 24) or
            (FBuffer[2] shl 16) or
            (FBuffer[3] shl 8) or
             FBuffer[4];
end;

procedure TwmOutputReportMemory.SetAddress(const Value: Cardinal);
begin
  SetBitsInValue(true, 1, (Value and $FF000000) shr 24);
  FBuffer[2] := (Value and $00FF0000) shr 16;
  FBuffer[3] := (Value and $0000FF00) shr 8;
  FBuffer[4] := Value and $000000FF;
end;


//==============================================================================


{ TwmInputReportButtons }

constructor TwmInputReportButtons.Create;
begin
  inherited;

  FBuffer[0] := $30;
end;

function TwmInputReportButtons.GetButtonsDown: TwmButtons;
begin
  Result := [];

  if GetBitsInValue(1, $01) then Include(Result, wmbLeft);
  if GetBitsInValue(1, $02) then Include(Result, wmbRight);
  if GetBitsInValue(1, $04) then Include(Result, wmbDown);
  if GetBitsInValue(1, $08) then Include(Result, wmbUp);
  if GetBitsInValue(1, $10) then Include(Result, wmbPlus);
  if GetBitsInValue(2, $01) then Include(Result, wmbTwo);
  if GetBitsInValue(2, $02) then Include(Result, wmbOne);
  if GetBitsInValue(2, $04) then Include(Result, wmbB);
  if GetBitsInValue(2, $08) then Include(Result, wmbA);
  if GetBitsInValue(2, $10) then Include(Result, wmbMinus);
  if GetBitsInValue(2, $80) then Include(Result, wmbHome);

end;


//==============================================================================


{ TwmInputReportStatus }

constructor TwmInputReportStatus.Create;
begin
  inherited;

  FBuffer[0] := $20;
end;

function TwmInputReportStatus.GetLEDsOn: TwmLEDs;
begin
  Result := [];

  if GetBitsInValue(3, $10) then Include(Result, wmLED1);
  if GetBitsInValue(3, $20) then Include(Result, wmLED2);
  if GetBitsInValue(3, $40) then Include(Result, wmLED3);
  if GetBitsInValue(3, $80) then Include(Result, wmLED4);
end;


//==============================================================================


{ TwmInputReportReadMemory }

constructor TwmInputReportReadMemory.Create;
begin
  inherited;

  FBuffer[0] := $21;
end;

function TwmInputReportReadMemory.GetData: TwmRawData;
var
  idx,
  Len : Integer;
begin
  Len := (fBuffer[3] shr 4) + 1;

  SetLength(Result, Len);

  for idx := Low(Result) to High(Result) do
    Result[idx] := FBuffer[6 + idx];
end;

function TwmInputReportReadMemory.GetErrorInvalidAddress: Boolean;
begin
  Result := (FBuffer[3] and $08) = $08;
end;

function TwmInputReportReadMemory.GetErrorWriteOnlyMemory: Boolean;
begin
    Result := (FBuffer[3] and $07) = $07;
end;


//==============================================================================


{ TwmInputReportButtonsAccel }

constructor TwmInputReportButtonsAccel.Create;
begin
  inherited;

  FBuffer[0] := $31;
end;


//==============================================================================


{ TwmInputReportButtonsAccelIR }

constructor TwmInputReportButtonsAccelIR.Create;
begin
  inherited;

  FBuffer[0] := $33; 
end;

function TwmInputReportButtonsAccelIR.GetIRPoint(AIndex: Integer): TPoint;
begin
  case AIndex of
    0:
    begin
      Result.X := FBuffer[6] or ((FBuffer[8] shr 4) and $03) shl 8;
      Result.Y := FBuffer[7] or ((FBuffer[8] shr 6) and $03) shl 8;
    end;

    1:
    begin
      case FIRMode of
        wmiBasic:
        begin
          Result.X := FBuffer[9] or ((FBuffer[8] shr 0) and $03) shl 8;
          Result.Y := FBuffer[10] or ((FBuffer[8] shr 2) and $03) shl 8;
        end;

        wmiExtended:
        begin
          Result.X := FBuffer[9] or ((FBuffer[11] shr 4) and $03) shl 8;
          Result.Y := FBuffer[10] or ((FBuffer[11] shr 6) and $03) shl 8;
        end;
      end;
    end;

    2:
    begin
      case FIRMode of
        wmiExtended:
        begin
          Result.X := FBuffer[12] or ((FBuffer[14] shr 4) and $03) shl 8;
          Result.Y := FBuffer[13] or ((FBuffer[14] shr 6) and $03) shl 8;
        end;
      end;
    end;

    3:
    begin
      case FIRMode of
        wmiExtended:
        begin
          Result.X := FBuffer[15] or ((FBuffer[17] shr 4) and $03) shl 8;
          Result.Y := FBuffer[16] or ((FBuffer[17] shr 6) and $03) shl 8;
        end;
      end;
    end;

  end;
end;

function TwmInputReportButtonsAccelIR.GetIRPointCount: Integer;
begin
  Result := 0;

  case FIRMode of
    wmiBasic:
    begin
      if not ((FBuffer[6] = $FF) and (FBuffer[7] = $FF)) then Inc(Result);
      if not ((FBuffer[9] = $FF) and (FBuffer[10] = $FF)) then Inc(Result);
    end;

    wmiExtended:
    begin
      if not ((FBuffer[6] = $FF) and (FBuffer[7] = $FF) and (FBuffer[8] = $FF)) then Inc(Result);
      if not ((FBuffer[9] = $FF) and (FBuffer[10] = $FF) and (FBuffer[11] = $FF)) then Inc(Result);
      if not ((FBuffer[12] = $FF) and (FBuffer[13] = $FF) and (FBuffer[14] = $FF)) then Inc(Result);
      if not ((FBuffer[15] = $FF) and (FBuffer[16] = $FF) and (FBuffer[17] = $FF)) then Inc(Result);
    end;
  end;
end;

function TwmInputReportButtonsAccelIR.GetIRPointSize(AIndex : Integer): Byte;
begin
  Result := 0;

  case FIRMode of
    wmiBasic:
    begin
      Result := 15;
    end;
    wmiExtended:
    begin
      case AIndex of
        0: Result := FBuffer[8] and $0F;
        1: Result := FBuffer[11] and $0F;
        2: Result := FBuffer[14] and $0F;
        3: Result := FBuffer[17] and $0F;
      end;
    end;
  end;
end;


//==============================================================================


{ TCustomWiimote }

function TCustomWiimote.Connect(ADevicePath: String): Boolean;
var
  Rep : TwmOutputReportReporting;
begin
  Result := FConnection.Connect(ADevicePath);

  if Result then
  begin
    IRMode := wmiOff;

    ReadAccelCalibration;

    Rep := TwmOutputReportReporting.Create;
    try
      Rep.ReportType := TwmInputReportButtonsAccelIR;
      Rep.Continuous := true;
      Rep.Rumble := FRumble;

      FConnection.WriteReport(Rep);
    finally
      Rep.Free;
    end;
  end;

end;

constructor TCustomWiimote.Create(AOwner: TComponent);
begin
  inherited;

  FIRSense := wmiSense1;

  FIRPointCount := 0;

  FButtonsDown := [];

  FReadMemStack := TStringList.Create;

  FillChar(FAccelCalibration, SizeOf(FAccelCalibration), 0);

  if not (csDesigning in ComponentState) then
  begin
    FConnection := TwmDeviceConnection.Create;
    FConnection.OnConnected := DoConnect;
    FConnection.OnDisconnected := DoDisconnect;
    FConnection.OnNewReport := DoNewReport;
  end
  else
  begin
    FConnection := nil;
  end;
end;

destructor TCustomWiimote.Destroy;
begin
  if Assigned(FConnection) then
  begin
    FConnection.Resume;
    FConnection.Terminate;
  end;

  FReadMemStack.Free;

  inherited;
end;

function TCustomWiimote.Disconnect: Boolean;
begin
  Result := FConnection.Disconnect;
end;

function TCustomWiimote.GetButtonsDown: TwmButtons;
begin
  Result := FButtonsDown;
end;

function TCustomWiimote.GetConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

function TCustomWiimote.GetCurrentAccel(const Index: Integer): Single;
begin
  Result := FAccels[Index];
end;

function TCustomWiimote.GetData1ForIRSense(
  ASensitivity: TwmIRSensitivity): TwmRawData;
begin
  SetLength(Result, 9);

  //Defaults
  Result[0] := $02; Result[1] := $00; Result[2] := $00; Result[3] := $71;
  Result[4] := $01; Result[5] := $00; Result[6] := $64; Result[7] := $00;
  Result[8] := $FE;

  case ASensitivity of
    wmiSense1:
    begin
      //Default is Sensitivity1
    end;
    wmiSense2:
    begin
      Result[6] := $96; Result[8] := $B4;
    end;
    wmiSense3:
    begin
      Result[6] := $AA; Result[8] := $64;
    end;
    wmiSense4:
    begin
      Result[6] := $C8; Result[8] := $36;
    end;
    wmiSense5:
    begin
      Result[0] := $07; Result[6] := $72; Result[8] := $20;
    end;
    wmiSenseMax:
    begin
      Result[6] := $90; Result[8] := $41;
    end;
  end;
end;

function TCustomWiimote.GetData2ForIRSense(
  ASensitivity: TwmIRSensitivity): TwmRawData;
begin
  SetLength(Result, 2);

  case ASensitivity of
    wmiSense1:
    begin
      Result[0] := $FD; Result[1] := $05;
    end;
    wmiSense2:
    begin
      Result[0] := $B3; Result[1] := $04;
    end;
    wmiSense3:
    begin
      Result[0] := $63; Result[1] := $03;
    end;
    wmiSense4:
    begin
      Result[0] := $35; Result[1] := $03;
    end;
    wmiSense5:
    begin
      Result[0] := $01; Result[1] := $03;
    end;
    wmiSenseMax:
    begin
      Result[0] := $40; Result[1] := $00;
    end;
  end;
end;

function TCustomWiimote.GetIRPoint(AIndex: Integer): TPoint;
begin
  if (AIndex >=0) and (AIndex <= IRPointCount) then
    Result := FIRPoints[AIndex]
  else
    raise Exception.Create('Invalid index');
end;

function TCustomWiimote.GetIRPointSize(AIndex: Integer): Byte;
begin
  if (AIndex >=0) and (AIndex <= IRPointCount) then
    Result := FIRPointSizes[AIndex]
  else
    raise Exception.Create('Invalid index');
end;

procedure TCustomWiimote.ReadAccelCalibration;
var
  OutRep : TwmOutputReportReadMemory;
begin
  OutRep := TwmOutputReportReadMemory.Create;
  try
    OutRep.Address := $0016;
    OutRep.Size := 7;
    OutRep.Rumble := FRumble;

    if FConnection.WriteReport(OutRep) then
      FReadMemStack.Add('AccelCalibration');
  finally
    OutRep.Free;
  end;
end;

procedure TCustomWiimote.RequestStatus;
var
  Rep : TwmOutputReportStatus;
begin
  Rep := TwmOutputReportStatus.Create;
  try
    Rep.Rumble := FRumble;
    FConnection.WriteReport(Rep);
  finally
    Rep.Free;
  end;
end;

procedure TCustomWiimote.SetIRMode(const Value: TwmIRMode);
var
  RepIR : TwmOutputReportIR;
  RepIR2 : TwmOutputReportIR2;
  RepWriteMem : TwmOutputReportWriteMemory;
  Data : TwmRawData;
  Reporting : TwmOutputReportReporting;
begin
  RepIR := TwmOutputReportIR.Create;
  RepIR2 := TwmOutputReportIR2.Create;
  RepWriteMem := TwmOutputReportWriteMemory.Create;
  try
    RepIR.Rumble := FRumble;
    RepIR2.Rumble := FRumble;
    RepWriteMem.Rumble := FRumble;

    RepIR.Enabled := Value <> wmiOff;
    RepIR2.Enabled := Value <> wmiOff;

    if FConnection.WriteReport(RepIR) and
       FConnection.WriteReport(RepIR2) then
    begin
      FIRMode := Value;

      if Value <> wmiOff then
      begin
        SetLength(Data, 1); Data[0] := $08;
        RepWriteMem.Address := WIIMOTE_MEMADDR_IR;
        RepWriteMem.Data := Data;
        FConnection.WriteReport(RepWriteMem);

        Data := GetData1ForIRSense(FIRSense);
        RepWriteMem.Address := WIIMOTE_MEMADDR_IR_SENSITIVITY_1;
        RepWriteMem.Data := Data;
        FConnection.WriteReport(RepWriteMem);

        Data := GetData2ForIRSense(FIRSense);
        RepWriteMem.Address := WIIMOTE_MEMADDR_IR_SENSITIVITY_2;
        RepWriteMem.Data := Data;
        FConnection.WriteReport(RepWriteMem);

        SetLength(Data, 1); Data[0] := Byte(Value);
        RepWriteMem.Data := Data;
        RepWriteMem.Address := WIIMOTE_MEMADDR_IR_MODE;
        FConnection.WriteReport(RepWriteMem);

        SetLength(Data, 1); Data[0] := $08;
        RepWriteMem.Address := WIIMOTE_MEMADDR_IR;
        RepWriteMem.Data := Data;
        FConnection.WriteReport(RepWriteMem);
      end;
    end;
    
  finally
    RepIR.Free;
    RepIR2.Free;
    RepWriteMem.Free;
  end;

  Reporting := TwmOutputReportReporting.Create;
  try
    Reporting.Rumble := FRumble;

    if Value <> wmiOff then
      Reporting.ReportType := TwmInputReportButtonsAccelIR
    else
      Reporting.ReportType := TwmInputReportButtonsAccel;

    FConnection.WriteReport(Reporting);
  finally
    Reporting.Free;
  end;
end;

procedure TCustomWiimote.SetLedsOn(const Value: TwmLEDs);
var
  RepLeds : TwmOutputReportLEDs;
begin
  FLedsOn := Value;

  RepLeds := TwmOutputReportLEDs.Create;
  try
    RepLeds.LEDsOn := Value;
    RepLeds.Rumble := FRumble;

    if FConnection.WriteReport(RepLeds) then
      RequestStatus;
  finally
    RepLeds.Free;
  end;
end;

procedure TCustomWiimote.SetRumble(AValue: Boolean);
begin
  FRumble := AValue;

  RequestStatus;
end;

procedure TCustomWiimote.DoConnect(Sender: TObject);
begin
  if Assigned(FOnConnected) then
    FOnConnected(Self);
end;

procedure TCustomWiimote.DoDisconnect(Sender: TObject);
begin
  if Assigned(FOnDisconnected) then
    FOnDisconnected(Self);
end;

procedure TCustomWiimote.DoNewReport(const AReport: TwmReport);
begin
  if Assigned(FOnNewReport) then
    FOnNewReport(AReport);

  if AReport is TwmInputReportReadMemory then
    ExtractReadMemory(TwmInputReportReadMemory(AReport));

  if AReport is TwmInputReportButtonsAccel then
    ExtractAccel(TwmInputReportButtonsAccel(AReport));

  if AReport is TwmInputReportButtons then
    ExtractButtonStates(TwmInputReportButtons(AReport));

  if AReport is TwmInputReportButtonsAccelIR then
    ExtractIRInfos(TwmInputReportButtonsAccelIR(AReport));

  if AReport is TwmInputReportStatus then
  begin
    ExtractStatusinfos(TwmInputReportStatus(AReport));

    if Assigned(FOnStatus) then
      FOnStatus(Self);
  end;
end;

procedure TCustomWiimote.ExtractAccel(AReport: TwmInputReportButtonsAccel);
var
  Calib0,
  CalibG : Single;
begin
  //If no calibration exists, read it
  if (FAccelCalibration.X0 = 0) and
     (FAccelCalibration.Y0 = 0) and
     (FAccelCalibration.Z0 = 0) and
     (FAccelCalibration.XG = 0) and
     (FAccelCalibration.YG = 0) and
     (FAccelCalibration.ZG = 0) then
  begin
    ReadAccelCalibration;
  end
  else
  begin
    FRawAccels[0] := AReport.XAccl;
    FRawAccels[1] := AReport.YAccl;
    FRawAccels[2] := AReport.ZAccl;

    Calib0 := FAccelCalibration.X0;
    CalibG := FAccelCalibration.XG;
    if (CalibG - Calib0) <> 0 then
      FAccels[0] := ((FRawAccels[0] - Calib0) / (CalibG - Calib0))
    else
      FAccels[0] := 0;

    Calib0 := FAccelCalibration.Y0;
    CalibG := FAccelCalibration.YG;
    if (CalibG - Calib0) <> 0 then
      FAccels[1] := ((FRawAccels[1] - Calib0) / (CalibG - Calib0))
    else
      FAccels[1] := 0;

    Calib0 := FAccelCalibration.Z0;
    CalibG := FAccelCalibration.ZG;
    if (CalibG - Calib0) <> 0 then
      FAccels[2] := ((FRawAccels[2] - Calib0) / (CalibG - Calib0))
    else
      FAccels[2] := 0;
  end;
end;

procedure TCustomWiimote.ExtractButtonStates(AReport: TwmInputReportButtons);
var
  Button : TwmButton;
  ReportButtons : TwmButtons;
begin
  ReportButtons := AReport.ButtonsDown;

  for Button := Low(TwmButton) to High(TwmButton) do
  begin

    if (not (Button in FButtonsDown)) and (Button in ReportButtons) then
    begin
      if Assigned(FOnButtonDown) then
        FOnButtonDown(Button)
    end
    else
    if (Button in FButtonsDown) and (not (Button in ReportButtons)) then
    begin
      if Assigned(FOnButtonUp) then
        FOnButtonUp(Button)
    end
    else
    if (Button in FButtonsDown) and (Button in ReportButtons) then
    begin
      if Assigned(FOnButtonIsDown) then
        FOnButtonIsDown(Button)
    end;
  end;

  FButtonsDown := ReportButtons;
end;

procedure TCustomWiimote.ExtractIRInfos(AReport: TwmInputReportButtonsAccelIR);
var
  idx : Integer;
begin
  AReport.IRMode := FIRMode;

  FIRPointCount := AReport.IRPointCount;

  for idx := 0 to FIRPointCount - 1 do
    FIRPoints[idx] := AReport.IRPoint[idx];

  for idx := 0 to FIRPointCount - 1 do
    FIRPointSizes[idx] := AReport.IRPointSize[idx];
end;

procedure TCustomWiimote.ExtractStatusInfos(AReport: TwmInputReportStatus);
begin
  FLedsOn := AReport.LedsOn;
  FBattery := AReport.BatterLevel;
end;

procedure TCustomWiimote.ExtractReadMemory(AReport: TwmInputReportReadMemory);
var
  Data : TwmRawData;
begin
  Data := AReport.Data;

  if FReadMemStack.Count > 0 then
  begin

    if SameText(FReadMemStack[0], 'AccelCalibration') then
    begin
      FAccelCalibration.X0 := Data[0];
      FAccelCalibration.Y0 := Data[1];
      FAccelCalibration.Z0 := Data[2];
      FAccelCalibration.XG := Data[4];
      FAccelCalibration.YG := Data[5];
      FAccelCalibration.ZG := Data[6];
    end;

    FReadMemStack.Delete(0);
  end;
end;

initialization
  LoadSetupApi;

finalization
  UnloadSetupApi;

end.
