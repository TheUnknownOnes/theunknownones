unit uPSBuzz;

interface

uses
  SetupAPI,
  Classes,
  Windows,
  Dialogs,
  SysUtils;


const
  PSB_VENDOR_ID = $054C;
  PSB_PRODUCT_ID = $1000;

  PSB_INPUT_REPORT_LEN = 6;
  PSB_OUTPUT_REPORT_LEN = 8;

type
  TpsbRawInputReport = array[0..PSB_INPUT_REPORT_LEN - 1] of Byte;
  TpsbRawOutputReport = array[0..PSB_OUTPUT_REPORT_LEN - 1] of Byte;

  TpsbHandset = (psbh1,
                 psbh2,
                 psbh3,
                 psbh4);



  TpsbButton = (psbbBuzzer = $01,
                psbbYellow = $02,
                psbbGreen  = $04,
                psbbOrange = $08,
                psbbBlue   = $10);

  TpsbButtons = set of TpsbButton;

  TpsbButtonEvent = procedure(ASender : TObject; AHandset : TpsbHandset; AButton : TpsbButton) of object;

  TpsbButtonsPerHandset = array[Low(TpsbHandset)..High(TpsbHandset)] of TpsbButtons;

//==============================================================================

  TpsbInputReport = class
  private
    FBuffer : TpsbRawInputReport;

    procedure Clear;
    function GetButtonsDown(AHandset: TpsbHandset): TpsbButtons;
  public
    class function CreateReport(const ARawReport : TpsbRawInputReport;
                                out ANewReport : TpsbInputReport) : Boolean;

    constructor Create(ARawReport : TpsbRawInputReport); overload; virtual;
    constructor Create(); overload; virtual;

    property ButtonsDown[AHandset : TpsbHandset] : TpsbButtons read GetButtonsDown;
  end;

//==============================================================================

  TpsbOutputReport = class
  private
    FBuffer : TpsbRawOutputReport;
    function GetLights(AHandset: TpsbHandset): Boolean;
    procedure SetLights(AHandset: TpsbHandset; const Value: Boolean);
  public
    constructor Create(); virtual;

    property LightOn[AHandset : TpsbHandset] : Boolean read GetLights write SetLights;
  end;

//==============================================================================

  TpsbDeviceConnection = class
  protected
    FHandle : THandle;

    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;

    function GetConnected: Boolean;
  public
    class function ListDevices(AList : TStrings) : Integer;

    constructor Create(); virtual;
    destructor Destroy(); override;

    function Connect(ADevicePath : String) : Boolean;
    function Disconnect : Boolean;

    function ReadReport(out AReport : TpsbInputReport) : Boolean;
    function WriteReport(const AReport : TpsbOutputReport) : Boolean;

    property Connected : Boolean read GetConnected;
    property Handle : THandle read FHandle;

    property OnConnected : TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected : TNotifyEvent read FOnDisconnected write FOnDisconnected;
  end;

//==============================================================================

  TpsbOnInputReportProc = procedure(const AReport : TpsbInputReport) of object;

  TpsbReportReader = class(TThread)
  protected
    FConnection : TpsbDeviceConnection;
    FOnNewReport : TpsbOnInputReportProc;

    FReport : TpsbInputReport;

    procedure Execute(); override;

    procedure SyncDoNewReport();
  public
    constructor Create(const AConnection : TpsbDeviceConnection;
                       AOnNewReport : TpsbOnInputReportProc);
  end;

//==============================================================================

  TCustomPSBuzz = class(TComponent)
  private
    function GetButtonsDown(AHandset: TpsbHandset): TpsbButtons;
  protected
    FConnection : TpsbDeviceConnection;
    FReportReader : TpsbReportReader;

    FButtonsDown : TpsbButtonsPerHandset;
    FOutRep : TpsbOutputReport;

    FOnButtonUp: TpsbButtonEvent;
    FOnConnected: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FOnButtonDown: TpsbButtonEvent;
    function GetConnected: Boolean;

    function GetLightOn(AHandset: TpsbHandset): Boolean;
    procedure SetLightOn(AHandset: TpsbHandset; const Value: Boolean);

    procedure DoConnect(Sender : TObject);
    procedure DoDisconnect(Sender : TObject);
    procedure DoNewReport(const AReport : TpsbInputReport);

    procedure DoButtonDown(AHandset : TpsbHandset; AButton : TpsbButton);
    procedure DoButtonUp(AHandset : TpsbHandset; AButton : TpsbButton);
  published
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Connect(ADevicePath : String) : Boolean;
    function Disconnect() : Boolean;

    property Connected : Boolean read GetConnected;

    property LightOn[AHandset : TpsbHandset] : Boolean read GetLightOn write SetLightOn;

    property ButtonsDown[AHandset : TpsbHandset] : TpsbButtons read GetButtonsDown;

    property OnConnect : TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnect : TNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnButtonDown : TpsbButtonEvent read FOnButtonDown write FOnButtonDown;
    property OnButtonUp : TpsbButtonEvent read FOnButtonUp write FOnButtonUp;
  end;

//==============================================================================

  TPSBuzz = class(TCustomPSBuzz)
  published
    property OnConnect;
    property OnDisconnect;
    property OnButtonDown;
    property OnButtonUp;
  end;

implementation

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

{ TpsbDeviceConnection }

function TpsbDeviceConnection.Connect(ADevicePath: String): Boolean;
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

  if Result then
  begin
    if Assigned(FOnConnected) then
      FOnConnected(Self);
  end;
end;

constructor TpsbDeviceConnection.Create;
begin
  FHandle := 0;
end;

destructor TpsbDeviceConnection.Destroy;
begin
  FOnDisconnected := nil;
  Disconnect;

  inherited;
end;

function TpsbDeviceConnection.Disconnect: Boolean;
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

function TpsbDeviceConnection.GetConnected: Boolean;
begin
  Result := FHandle <> 0;
end;

class function TpsbDeviceConnection.ListDevices(AList: TStrings): Integer;
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
            if (Attributes.VendorID = PSB_VENDOR_ID) and
               (Attributes.ProductID = PSB_PRODUCT_ID) then
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

function TpsbDeviceConnection.ReadReport(out AReport: TpsbInputReport): Boolean;
var
  BytesRead : Cardinal;
  Lappen : TOverlapped;
  RawReport : TpsbRawInputReport;
begin
  Result := Connected;

  if Result then
  begin
    Lappen.Offset := 0;
    Lappen.OffsetHigh := 0;
    Lappen.hEvent := CreateEvent(nil, true, true, 'TpsbDeviceConnection.ReadReport');
    try
      Result := not ReadFile(FHandle,
                             RawReport,
                             PSB_INPUT_REPORT_LEN,
                             BytesRead,
                             @Lappen);

      Result := GetLastError = ERROR_IO_PENDING;

      if Result then
      begin
        case WaitForSingleObject(Lappen.hEvent, 500) of
          WAIT_FAILED:
          begin
            Result := False;
            //Disconnect;
          end;

          WAIT_TIMEOUT:
          begin
            CancelIo(FHandle);
            Result := false;
            //Disconnect;
          end

          else
          begin
            Result := GetOverlappedResult(FHandle, Lappen, BytesRead, true);
            if Result then
              Result := TpsbInputReport.CreateReport(RawReport, AReport);
          end;
        end;
      end;

    finally
      CloseHandle(Lappen.hEvent);
    end;

  end;
end;

function TpsbDeviceConnection.WriteReport(
  const AReport: TpsbOutputReport): Boolean;
var
  BytesWritten : Cardinal;
  Lappen : TOverlapped;
begin
  Result := Connected;

  if Result then
  begin
    Lappen.Offset := 0;
    Lappen.OffsetHigh := 0;
    Lappen.hEvent := CreateEvent(nil, true, false, 'TpsbDeviceConnection.WriteReport');
    try
      Result := WriteFile(FHandle,
                          AReport.FBuffer,
                          PSB_OUTPUT_REPORT_LEN,
                          BytesWritten,
                          @Lappen);
    finally
      CloseHandle(Lappen.hEvent);
    end;
  end;
end;

//==============================================================================

{ TpsbInputReport }

constructor TpsbInputReport.Create(ARawReport: TpsbRawInputReport);
begin
  Create();
  
  CopyMemory(@FBuffer, @ARawReport, PSB_INPUT_REPORT_LEN);
end;

procedure TpsbInputReport.Clear;
var
  idx : Integer;
begin
  for idx := 1 to PSB_INPUT_REPORT_LEN - 1 do
    FBuffer[idx] := 0;
end;

constructor TpsbInputReport.Create;
begin
  FBuffer[0] := 0;
  Clear;
end;

class function TpsbInputReport.CreateReport(const ARawReport: TpsbRawInputReport;
  out ANewReport: TpsbInputReport): Boolean;
begin
  Result := true;
  ANewReport := TpsbInputReport.Create(ARawReport);
end;

function TpsbInputReport.GetButtonsDown(AHandset: TpsbHandset): TpsbButtons;
var
  Value : PCardinal;
  Bits : Cardinal;
  DataOffset : Byte;
begin
  Value := PCardinal(Integer(@FBuffer) + 2);

  Result := [];

  case AHandset of
    psbh1: DataOffset := 19;
    psbh2: DataOffset := 14;
    psbh3: DataOffset := 9;
    psbh4: DataOffset := 4;
    else
      DataOffset := 255;
  end;

  if DataOffset < 255 then
  begin
    Bits := Value^ shl DataOffset;
    Bits := Bits shr 27;

    if (Bits and Byte(psbbBuzzer)) = Byte(psbbBuzzer) then
      Include(Result, psbbBuzzer);
    if (Bits and Byte(psbbYellow)) = Byte(psbbYellow) then
      Include(Result, psbbYellow);
    if (Bits and Byte(psbbGreen)) = Byte(psbbGreen) then
      Include(Result, psbbGreen);
    if (Bits and Byte(psbbOrange)) = Byte(psbbOrange) then
      Include(Result, psbbOrange);
    if (Bits and Byte(psbbBlue)) = Byte(psbbBlue) then
      Include(Result, psbbBlue);
  end;
  
end;

//==============================================================================

{ TpsbOutputReport }

constructor TpsbOutputReport.Create;
begin
  FillMemory(@FBuffer, PSB_OUTPUT_REPORT_LEN, 0);
end;


function TpsbOutputReport.GetLights(AHandset: TpsbHandset): Boolean;
var
  idx : Integer;
begin
  Result := false;

  case AHandset of
    psbh1: idx := 2;
    psbh2: idx := 3;
    psbh3: idx := 4;
    psbh4: idx := 5;
    else
      idx := -1;
  end;

  if idx in [Low(FBuffer)..High(FBuffer)] then
    Result := FBuffer[idx] = $FF;
end;

procedure TpsbOutputReport.SetLights(AHandset: TpsbHandset;
  const Value: Boolean);
var
  idx : Integer;
begin
  case AHandset of
    psbh1: idx := 2;
    psbh2: idx := 3;
    psbh3: idx := 4;
    psbh4: idx := 5;
    else
      idx := -1;
  end;

  if idx in [Low(FBuffer)..High(FBuffer)] then
  begin
    if Value then
    begin
      FBuffer[idx] := $FF;
    end
    else
    begin
      FBuffer[idx] := 0;
    end;
  end;
end;

//==============================================================================

{ TpsbReportReader }

constructor TpsbReportReader.Create(const AConnection: TpsbDeviceConnection;
  AOnNewReport: TpsbOnInputReportProc);
begin
  inherited Create(true);

  FConnection := AConnection;
  FOnNewReport := AOnNewReport;

  FreeOnTerminate := true;

  Resume;
end;

procedure TpsbReportReader.Execute;
begin
  while not Terminated do
  begin

    if (not Suspended) and FConnection.Connected then
    begin
      if FConnection.ReadReport(FReport) then
      begin
        try
          Synchronize(SyncDoNewReport);
        except
        end;

        FReport.Free;
      end;
    end;

    sleep(1);
  end;
end;

procedure TpsbReportReader.SyncDoNewReport;
begin
  if Assigned(FOnNewReport) then
    FOnNewReport(FReport);
end;

//==============================================================================

{ TCustomPSBuzz }

function TCustomPSBuzz.Connect(ADevicePath: String): Boolean;
begin
  Result := FConnection.Connect(ADevicePath);
end;

constructor TCustomPSBuzz.Create(AOwner: TComponent);
begin
  inherited;

  FOutRep := TpsbOutputReport.Create;

  FConnection := TpsbDeviceConnection.Create;
  FConnection.OnConnected := DoConnect;
  FConnection.OnDisconnected := DoDisconnect;

  if not (csDesigning in ComponentState) then
    FReportReader := TpsbReportReader.Create(FConnection, DoNewReport)
  else
    FReportReader := nil;
end;

destructor TCustomPSBuzz.Destroy;
begin
  if Assigned(FReportReader) then
    FReportReader.Terminate;  

  FConnection.Free;

  FOutRep.Free;


  inherited;
end;

function TCustomPSBuzz.Disconnect: Boolean;
begin
  Result := FConnection.Disconnect;
end;

procedure TCustomPSBuzz.DoButtonDown(AHandset: TpsbHandset;
  AButton: TpsbButton);
begin
  if Assigned(FOnButtonDown) then
    FOnButtonDown(Self, AHandset, AButton);
end;

procedure TCustomPSBuzz.DoButtonUp(AHandset: TpsbHandset; AButton: TpsbButton);
begin
  if Assigned(FOnButtonUp) then
    FOnButtonUp(Self, AHandset, AButton);
end;

procedure TCustomPSBuzz.DoConnect(Sender: TObject);
var
  rep : TpsbOutputReport;
begin
  rep := TpsbOutputReport.Create;
  FConnection.WriteReport(rep);
  rep.Free;

  if Assigned(FOnConnected) then
    FOnConnected(Self);
end;

procedure TCustomPSBuzz.DoDisconnect(Sender: TObject);
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self);
end;

procedure TCustomPSBuzz.DoNewReport(const AReport: TpsbInputReport);
var
  hs :  TpsbHandset;
  b : TpsbButton;

  NewStates : TpsbButtons;
begin
  try
    for hs := Low(TpsbHandset) to High(TpsbHandset) do
    begin
      NewStates := AReport.ButtonsDown[hs];

      for b := Low(TpsbButton) to High(TpsbButton) do
      begin
        if (not (b in FButtonsDown[hs])) and (b in NewStates) then
          DoButtonDown(hs, b)
        else
        if (b in FButtonsDown[hs]) and (not (b in NewStates)) then
          DoButtonUp(hs, b);
      end;        
    end;

  finally
    for hs := Low(TpsbHandset) to High(TpsbHandset) do
      FButtonsDown[hs] := AReport.ButtonsDown[hs];
  end;
end;

function TCustomPSBuzz.GetButtonsDown(AHandset: TpsbHandset): TpsbButtons;
begin
  Result := FButtonsDown[AHandset];
end;

function TCustomPSBuzz.GetConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

function TCustomPSBuzz.GetLightOn(AHandset: TpsbHandset): Boolean;
begin
  Result := Connected and FOutRep.LightOn[AHandset];
end;

procedure TCustomPSBuzz.SetLightOn(AHandset: TpsbHandset; const Value: Boolean);
begin
  if Connected then
  begin
    FOutRep.LightOn[AHandset] := Value;
    FConnection.WriteReport(FOutRep);
  end;
end;

initialization
  LoadSetupApi;

finalization
  UnloadSetupApi;

end.
