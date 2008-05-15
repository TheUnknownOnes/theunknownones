unit uWiimote;

interface

uses
  Classes,
  Windows,
  SysUtils,
  uWiiuse;

const
  MaxWiimotes = 10;

type
  TWiimoteButton = (wmbTwo        = WIIMOTE_BUTTON_TWO,
                    wmbOne        = WIIMOTE_BUTTON_ONE,
                    wmbB          = WIIMOTE_BUTTON_B,
                    wmbA          = WIIMOTE_BUTTON_A,
                    wmbMinus      = WIIMOTE_BUTTON_MINUS,
                    wmbZAccelBit6 = WIIMOTE_BUTTON_ZACCEL_BIT6,
                    wmbZAccelBit7 = WIIMOTE_BUTTON_ZACCEL_BIT7,
                    wmbHome       = WIIMOTE_BUTTON_HOME,
                    wmbLeft       = WIIMOTE_BUTTON_LEFT,
                    wmbRight      = WIIMOTE_BUTTON_RIGHT,
                    wmbDown       = WIIMOTE_BUTTON_DOWN,
                    wmbUp         = WIIMOTE_BUTTON_UP,
                    wmbPlus       = WIIMOTE_BUTTON_PLUS,
                    wmbZAccelBit4 = WIIMOTE_BUTTON_ZACCEL_BIT4,
                    wmbZAccelBit5 = WIIMOTE_BUTTON_ZACCEL_BIT5,
                    wmbUnknown    = WIIMOTE_BUTTON_UNKNOWN,
                    wmbAll        = WIIMOTE_BUTTON_ALL);

  TNunchuckButton = (ncbZ   = NUNCHUK_BUTTON_Z,
                     ncbC   = NUNCHUK_BUTTON_C,
                     ncbAll = NUNCHUK_BUTTON_ALL);

  TClassicControllerButton = (ccbUp = CLASSIC_CTRL_BUTTON_UP,
                              ccbLeft = CLASSIC_CTRL_BUTTON_LEFT,
                              ccbZR = CLASSIC_CTRL_BUTTON_ZR,
                              ccbX = CLASSIC_CTRL_BUTTON_X,
                              ccbA = CLASSIC_CTRL_BUTTON_A,
                              ccbY = CLASSIC_CTRL_BUTTON_Y,
                              ccbB = CLASSIC_CTRL_BUTTON_B,
                              ccbZL = CLASSIC_CTRL_BUTTON_ZL,
                              ccbFullR = CLASSIC_CTRL_BUTTON_FULL_R,
                              ccbPlus = CLASSIC_CTRL_BUTTON_PLUS,
                              ccbHome = CLASSIC_CTRL_BUTTON_HOME,
                              ccbMinus = CLASSIC_CTRL_BUTTON_MINUS,
                              ccbFullL = CLASSIC_CTRL_BUTTON_FULL_L,
                              ccbDown = CLASSIC_CTRL_BUTTON_DOWN,
                              ccbRight = CLASSIC_CTRL_BUTTON_RIGHT,
                              ccbAll = CLASSIC_CTRL_BUTTON_ALL);

  TGuitarHero3Button = (gh3bStrumUp = GUITAR_HERO_3_BUTTON_STRUM_UP,
                        gh3bYellow = GUITAR_HERO_3_BUTTON_YELLOW,
                        gh3bGreen = GUITAR_HERO_3_BUTTON_GREEN,
                        gh3bBlue = GUITAR_HERO_3_BUTTON_BLUE,
                        gh3bRed = GUITAR_HERO_3_BUTTON_RED,
                        gh3bOrange = GUITAR_HERO_3_BUTTON_ORANGE,
                        gh3bPlus = GUITAR_HERO_3_BUTTON_PLUS,
                        gh3bMinus = GUITAR_HERO_3_BUTTON_MINUS,
                        gh3bStrumDown = GUITAR_HERO_3_BUTTON_STRUM_DOWN,
                        gh3bAll = GUITAR_HERO_3_BUTTON_ALL);

  TWiimoteOption = (wmoSmoothing = WIIUSE_ORIENT_THRESH,
                    wmoContinuous = WIIUSE_ORIENT_THRESH,
                    wmoOrientThresh = WIIUSE_ORIENT_THRESH,
                    wmoInitFlags = WIIUSE_INIT_FLAGS);

  TWiimoteLED = (wmlLedNone = WIIMOTE_LED_NONE,
                 wmlLed1 = WIIMOTE_LED_1,
                 wmlLed2 = WIIMOTE_LED_2,
                 wmlLed3 = WIIMOTE_LED_3,
                 wmlLed4 = WIIMOTE_LED_4);

  TWiimoteIRPosition = (wmirpAbove,
                        wmirpBelow);

  TWiimoteAspectRatio = (wmar4to3,
                         wmar16to9);

  TWiimoteIRDot = record
    Size : Integer; //0-15;
    VirtualPosition,
    RawPosition : TPoint;
  end;

  IWiimote = interface
  ['{5726525F-3A56-437D-899D-5DC3831D8926}']
    function GetID : Integer;
    function GetPWiimote : Pwiimote_t;

    function GetBatteryPercent : Single;

    function ButtonIsPressed(AButton : TWiimoteButton) : Boolean;
    function ButtonIsHeld(AButton : TWiimoteButton) : Boolean;
    function ButtonIsReleased(AButton : TWiimoteButton) : Boolean;
    function ButtonIsJustPressed(AButton : TWiimoteButton) : Boolean;

    function GetLEDOn(ALED : TWiimoteLED) : Boolean;
    procedure SetLEDOn(ALED : TWiimoteLED; AValue : Boolean);
    property LEDOn[ALED : TWiimoteLED] : Boolean read GetLEDOn write SetLEDOn;

    function GetIREnabled : Boolean;
    procedure SetIREnabled(AValue : Boolean);
    property IREnabled : Boolean read GetIREnabled write SetIREnabled;

    function GetIRVRes : TPoint;
    procedure SetIRVRes(AValue : TPoint);
    property IRVirtualResolution : TPoint read GetIRVRes write SetIRVRes;

    function GetIRPosition : TWiimoteIRPosition;
    procedure SetIRPosition(AValue : TWiimoteIRPosition);
    property IRPosition : TWiimoteIRPosition read GetIRPosition write SetIRPosition;

    function GetIRAspectRatio : TWiimoteAspectRatio;
    procedure SetIRAspectRatio(AValue : TWiimoteAspectRatio);
    property IRAspectRation : TWiimoteAspectRatio read GetIRAspectRatio write SetIRAspectRatio;

    function GetIRSensitivity : Integer;
    procedure SetIRSensitivity(AValue : Integer);
    property IRSensitivity : Integer read GetIRSensitivity write SetIRSensitivity;

    function GetIRDotCount : Byte;
    function GetIRDot(AIndex : Byte) : TWiimoteIRDot;
  end;

  TWiimoteList = class(TInterfaceList)
  private
    function Get(Index: Integer): IWiimote;
    procedure Put(Index: Integer; const Value: IWiimote);
  public
    function First: IWiimote;
    function IndexOf(const Item: IWiimote): Integer; overload;
    function IndexOf(AID : Integer): Integer; overload;
    function Add(const Item: IWiimote): Integer;
    procedure Insert(Index: Integer; const Item: IWiimote);
    function Last: IWiimote;
    function Remove(const Item: IWiimote): Integer;
    property Items[Index: Integer]: IWiimote read Get write Put; default;
  end;

  TWiimotePoller = class;

  TWiimoteGenerictEventProc = procedure(AWiimote : IWiimote) of object;

  TWiimoteManager = class(TComponent)
  protected
    FWiimotesArray : PWiimotes;
    FDevices : TWiimoteList;
    FPoller : TWiimotePoller;

    FOnGenericEvent: TWiimoteGenerictEventProc;

    FPollIntervall: Cardinal;
    FActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetPollintervall(const Value: Cardinal);

    procedure DoGenericEvent(AWiimote : IWiimote);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function SearchDevices(ATimeoutSeconds : Byte = 3) : Integer; //returns the number of devices found

    property Active : Boolean read FActive write SetActive default false;

    function PollChanges : Integer;
    procedure CheckForEvents;

    property Devices : TWiimoteList read FDevices;
  published

    property PollIntervall : Cardinal read FPollIntervall write SetPollintervall default 100;

    property OnGenericEvent : TWiimoteGenerictEventProc read FOnGenericEvent write FOnGenericEvent;
  end;

  TWiimotePoller = class(TThread)
  protected
    FEventsOccured : Boolean; 

    FManager : TWiimoteManager;
    procedure Execute; override;

    procedure SyncDoPoll;
    procedure SyncDoCheckFoEvents;
  public
    constructor Create(AManager : TWiimoteManager);
  end;

procedure Register;

implementation

type
  TWiimote = class(TInterfacedObject, IWiimote)
  protected
    FWiimote : Pwiimote_t;
  public
    constructor Create(AWiimote : Pwiimote_t); virtual;
    destructor Destroy; override;

    {$REGION 'IWiimote'}
    function GetID : Integer;
    function GetPWiimote : Pwiimote_t;

    function GetBatteryPercent : Single;

    function ButtonIsPressed(AButton : TWiimoteButton) : Boolean;
    function ButtonIsHeld(AButton : TWiimoteButton) : Boolean;
    function ButtonIsReleased(AButton : TWiimoteButton) : Boolean;
    function ButtonIsJustPressed(AButton : TWiimoteButton) : Boolean;

    function GetLEDOn(ALED : TWiimoteLED) : Boolean;
    procedure SetLEDOn(ALED : TWiimoteLED; AValue : Boolean);

    function GetIREnabled : Boolean;
    procedure SetIREnabled(AValue : Boolean);

    function GetIRVRes : TPoint;
    procedure SetIRVRes(AValue : TPoint);

    function GetIRPosition : TWiimoteIRPosition;
    procedure SetIRPosition(AValue : TWiimoteIRPosition);

    function GetIRAspectRatio : TWiimoteAspectRatio;
    procedure SetIRAspectRatio(AValue : TWiimoteAspectRatio);

    function GetIRSensitivity : Integer;
    procedure SetIRSensitivity(AValue : Integer);

    function GetIRDotCount : Byte;
    function GetIRDot(AIndex : Byte) : TWiimoteIRDot;
    {$ENDREGION}
  end;

procedure Register;
begin
  RegisterComponents('TUO', [TWiimoteManager]);
end;

{ TWiimoteManager }

procedure TWiimoteManager.CheckForEvents;
var
  idx : Integer;
  mote : Pwiimote_t;
begin
  for idx := 0 to FDevices.Count - 1 do
  begin
    mote := FDevices[idx].GetPWiimote;

    case mote.event of
      WIIUSE_EVENT: DoGenericEvent(FDevices[idx]);
    end;

  end;
end;

constructor TWiimoteManager.Create(AOwner: TComponent);
begin
  inherited;

  FPollIntervall := 100;
  FActive := false;

  FWiimotesArray := nil;

  FWiimotesArray := wiiuse_init(MaxWiimotes);

  FDevices := TWiimoteList.Create;
end;

destructor TWiimoteManager.Destroy;
begin
  Active := false;

  if Assigned(FDevices) then
    FDevices.Free;

  if Assigned(FWiimotesArray) then
    wiiuse_cleanup(FWiimotesArray, MaxWiimotes);

  inherited;
end;

procedure TWiimoteManager.DoGenericEvent(AWiimote: IWiimote);
begin
  if Assigned(FOnGenericEvent) then
    FOnGenericEvent(AWiimote);
end;

function TWiimoteManager.PollChanges : Integer;
begin
  Result := wiiuse_poll(FWiimotesArray, MaxWiimotes);
end;

function TWiimoteManager.SearchDevices(ATimeoutSeconds : Byte): Integer;
var
  idx : Integer;
  ID : Integer;
  mote : Pwiimote_t;
begin
  Result := wiiuse_find(FWiimotesArray, MaxWiimotes, ATimeoutSeconds);

  //add new devices to device list
  for idx := 0 to Result - 1 do
  begin
    if FDevices.IndexOf(FWiimotesArray^[idx].unid) = -1 then
      FDevices.Add(TWiimote.Create(@FWiimotesArray^[idx]));
  end;

  //remove devices which from list, if they can not be found
  for idx := FDevices.Count - 1 downto 0 do
  begin
    ID := FDevices[idx].GetID;
    mote := wiiuse_get_by_id(FWiimotesArray, MaxWiimotes, ID);

    if not Assigned(mote) then
      FDevices.Delete(idx);
  end;
    
end;

procedure TWiimoteManager.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
  begin
    if not (csDesigning in Self.ComponentState) then
    begin
      FActive := Value;

      if FActive then
        FPoller := TWiimotePoller.Create(Self)
      else
        FPoller.Free;
    end;
  end;
end;

procedure TWiimoteManager.SetPollintervall(const Value: Cardinal);
begin
  FPollIntervall := Value;
end;

{ TWiimoteList }

function TWiimoteList.Add(const Item: IWiimote): Integer;
begin
  Result := inherited Add(Item);
end;

function TWiimoteList.First: IWiimote;
begin
  Result := inherited First as IWiimote;
end;

function TWiimoteList.Get(Index: Integer): IWiimote;
begin
  Result := inherited Get(Index) as IWiimote;
end;

function TWiimoteList.IndexOf(const Item: IWiimote): Integer;
begin
  Result := inherited IndexOf(Item);
end;

function TWiimoteList.IndexOf(AID: Integer): Integer;
var
  idx : Integer;
begin
  Result := -1;

  for idx := 0 to Count - 1 do
  begin
    if Items[idx].GetID = AID then
    begin
      Result := idx;
      break;
    end;
  end;
end;

procedure TWiimoteList.Insert(Index: Integer; const Item: IWiimote);
begin
  inherited Insert(Index, Item);
end;

function TWiimoteList.Last: IWiimote;
begin
  Result := inherited Last as IWiimote;
end;

procedure TWiimoteList.Put(Index: Integer; const Value: IWiimote);
begin
  inherited Put(Index, Value);
end;

function TWiimoteList.Remove(const Item: IWiimote): Integer;
begin
  Result := inherited Remove(Item);
end;

{ TWiimote }

function TWiimote.ButtonIsHeld(AButton: TWiimoteButton): Boolean;
begin
  Result := (FWiimote.btns_held and Word(AButton)) = Word(AButton);
end;

function TWiimote.ButtonIsJustPressed(AButton: TWiimoteButton): Boolean;
begin
  Result := ButtonIsPressed(AButton) and (not ButtonIsHeld(AButton))
end;

function TWiimote.ButtonIsPressed(AButton: TWiimoteButton): Boolean;
begin
  Result := (FWiimote.btns and Word(AButton)) = Word(AButton);
end;

function TWiimote.ButtonIsReleased(AButton: TWiimoteButton): Boolean;
begin
  Result := (FWiimote.btns_released and Word(AButton)) = Word(AButton);
end;

constructor TWiimote.Create(AWiimote : Pwiimote_t);
begin
  FWiimote := AWiimote;
end;

destructor TWiimote.Destroy;
begin

  inherited;
end;

function TWiimote.GetBatteryPercent: Single;
begin
  Result := FWiimote.battery_level * 100;
end;

function TWiimote.GetID: Integer;
begin
  Result := FWiimote.unid;
end;

function TWiimote.GetIRAspectRatio: TWiimoteAspectRatio;
begin
  case FWiimote.ir.aspect of
    WIIUSE_ASPECT_4_3 : Result := wmar4to3;
    WIIUSE_ASPECT_16_9 : Result := wmar16to9;
  end;
end;


function TWiimote.GetIRDot(AIndex: Byte): TWiimoteIRDot;
begin
  FillMemory(@Result, SizeOf(Result), 0);

  if AIndex <= FWiimote.ir.num_dots  then
  begin
    Result.Size := FWiimote.ir.dot[AIndex].size;
    Result.VirtualPosition := Point(FWiimote.ir.dot[AIndex].x, FWiimote.ir.dot[AIndex].y);
    Result.RawPosition := Point(FWiimote.ir.dot[AIndex].rx, FWiimote.ir.dot[AIndex].ry);
  end;
end;

function TWiimote.GetIRDotCount: Byte;
begin
  Result := fWiimote.ir.num_dots;
end;

function TWiimote.GetIREnabled: Boolean;
begin
  Result := FWiimote.state and $080 = $080;
end;

function TWiimote.GetIRPosition: TWiimoteIRPosition;
begin
  case FWiimote.ir.pos of
    WIIUSE_IR_ABOVE : Result := wmirpAbove;
    WIIUSE_IR_BELOW : Result := wmirpBelow;
  end; 
end;

function TWiimote.GetIRSensitivity: Integer;
begin
  if FWiimote.state and $0200 = $0200 then Result := 1
  else
  if FWiimote.state and $0400 = $0400 then Result := 2
  else
  if FWiimote.state and $0800 = $0800 then Result := 3
  else
  if FWiimote.state and $1000 = $1000 then Result := 4
  else
  if FWiimote.state and $2000 = $2000 then Result := 5
  else
    Result := 0;
end;

function TWiimote.GetIRVRes: TPoint;
begin
  Result := Point(FWiimote.ir.vres[0], FWiimote.ir.vres[1]);
end;

function TWiimote.GetLEDOn(ALED: TWiimoteLED): Boolean;
begin
  Result := FWiimote.leds and Byte(ALED) = Byte(ALed);  
end;

function TWiimote.GetPWiimote: Pwiimote_t;
begin
  Result := FWiimote;
end;

procedure TWiimote.SetIRAspectRatio(AValue: TWiimoteAspectRatio);
var
  r : aspect_t;
begin
  case AValue of
    wmar4to3: r := WIIUSE_ASPECT_4_3;
    wmar16to9: r := WIIUSE_ASPECT_16_9;
  end;

  wiiuse_set_aspect_ratio(FWiimote, r);
end;


procedure TWiimote.SetIREnabled(AValue: Boolean);
var
  state : Integer;
begin
  if AValue then
    state := 1
  else
    state := 0;

  wiiuse_set_ir(FWiimote, state);
end;

procedure TWiimote.SetIRPosition(AValue: TWiimoteIRPosition);
var
  p : ir_position_t;
begin
  case AValue of
    wmirpAbove: p := WIIUSE_IR_ABOVE;
    wmirpBelow: p := WIIUSE_IR_BELOW;
  end;

  wiiuse_set_ir_position(FWiimote, p);
end;

procedure TWiimote.SetIRSensitivity(AValue: Integer);
begin
  wiiuse_set_ir_sensitivity(FWiimote, AValue); 
end;

procedure TWiimote.SetIRVRes(AValue: TPoint);
begin
  wiiuse_set_ir_vres(FWiimote, AValue.X, AValue.Y);
end;

procedure TWiimote.SetLEDOn(ALED: TWiimoteLED; AValue: Boolean);
var
  State : Byte;
begin
  State := FWiimote.leds;

  if AValue then
    State := State or Byte(ALED)
  else
    State := State and (not Byte(ALED));

  wiiuse_set_leds(FWiimote, State);
end;

{ TWiimotePoller }

constructor TWiimotePoller.Create(AManager: TWiimoteManager);
begin
  FManager := AManager;

  FreeOnTerminate := false;

  inherited Create(false);
end;

procedure TWiimotePoller.Execute;
begin
  while not Terminated do
  begin
    Sleep(FManager.PollIntervall);

    Synchronize(SyncDoPoll);

    if FEventsOccured then
      Synchronize(SyncDoCheckFoEvents);
  end;
end;

procedure TWiimotePoller.SyncDoCheckFoEvents;
begin
  FManager.CheckForEvents;
end;

procedure TWiimotePoller.SyncDoPoll;
begin
  FEventsOccured := FManager.PollChanges > 0;
end;

end.
