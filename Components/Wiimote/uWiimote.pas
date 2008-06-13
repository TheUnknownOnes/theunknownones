{-----------------------------------------------------------------------------
 Purpose: Interface the Wiimote
 Created: 13.06.2008 06:38:08
 
 (c) by TheUnknownOnes
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

  WIIMOTE_MEMADDR_IR				= $04b00030;
  WIIMOTE_MEMADDR_IR_SENSITIVITY_1	= $04B00000;
  WIIMOTE_MEMADDR_IR_SENSITIVITY_2	= $04B0001A;
  WIIMOTE_MEMADDR_IR_MODE			= $04B00033;


type
  TwmRawReport = array[0..WIIMOTE_REPORT_LEN - 1] of Byte;
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


  TwmDeviceConnection = class
  protected
    FHandle : THandle;
    FDevicePresent: Boolean;

    function GetConnected: Boolean;
  published
  public
    class function ListDevices(AList : TStrings) : Integer;

    constructor Create(); virtual;
    destructor Destroy(); override;

    function Connect(ADevicePath : String) : Boolean;
    function Disconnect : Boolean;

    function ReadReport(out AReport : TwmReport) : Boolean;
    function WriteReport(const AReport : TwmReport) : Boolean;

    property Connected : Boolean read GetConnected;
    property Handle : THandle read FHandle;
    property DevicePresent : Boolean read FDevicePresent;
  end;


//==============================================================================





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
end;

constructor TwmDeviceConnection.Create;
begin
  FHandle := 0;
  FDevicePresent := false;
end;

destructor TwmDeviceConnection.Destroy;
begin
  Disconnect;

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
      DeviceDetailData^.cbSize := 5;

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

function TwmDeviceConnection.ReadReport(out AReport: TwmReport): Boolean;
var
  BytesRead : Cardinal;
  Lappen : TOverlapped;
  RawReport : TwmRawReport;
begin
  Result := Connected;

  if Result then
  begin
    Lappen.Offset := 0;
    Lappen.OffsetHigh := 0;
    Lappen.hEvent := CreateEvent(nil, true, true, 'TwmDeviceConnection.ReadReport');
    try
      Result := not ReadFile(FHandle,
                             RawReport,
                             WIIMOTE_REPORT_LEN,
                             BytesRead,
                             @Lappen);

      Result := GetLastError = ERROR_IO_PENDING;

      if Result then
      begin
        case WaitForSingleObject(Lappen.hEvent, 100) of
          WAIT_FAILED:
          begin
            Result := False;
            FDevicePresent := false;
          end;

          WAIT_TIMEOUT:
          begin
            CancelIo(FHandle);
            Result := false;
          end

          else
          begin
            FDevicePresent := true;

            Result := GetOverlappedResult(FHandle, Lappen, BytesRead, true);
            if Result then
              Result := TwmReport.CreateReport(RawReport, AReport);
          end;
        end;
      end;

    finally
      CloseHandle(Lappen.hEvent);
    end;

  end;
end;

function TwmDeviceConnection.WriteReport(const AReport: TwmReport): Boolean;
var
  BytesWritten : Cardinal;
  Lappen : TOverlapped;
begin
  Result := Connected;

  if Result then
  begin
    Lappen.Offset := 0;
    Lappen.OffsetHigh := 0;
    Lappen.hEvent := CreateEvent(nil, true, false, 'TwmDeviceConnection.WriteReport');
    try
      Result := WriteFile(FHandle,
                          AReport.FBuffer,
                          WIIMOTE_REPORT_LEN,
                          BytesWritten,
                          @Lappen);
    finally
      CloseHandle(Lappen.hEvent);
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

  Result := (FBuffer[AValueIndex] and ABits) = ABits;
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
      Result.Y := FBuffer[7] or ((FBuffer[8] shr 4) and $03) shl 8;
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

initialization
  LoadSetupApi;

finalization
  UnloadSetupApi;

end.
