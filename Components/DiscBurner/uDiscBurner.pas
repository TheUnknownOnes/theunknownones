unit uDiscBurner;

interface

uses
  Classes,
  Windows,
  Sysutils,
  ActiveX,
  jwaActiveX,
  jwaIMAPI,
  jwaIMAPIError,
  ComObj,
  Variants,
  uSysTools;

type
  TDiscRecorderState = (rsUnknown,
                        rsIdle,
                        rsOpen,
                        rsBurning);

  TDiscRecorderType = (rtUnknown,
                       rtCDR,
                       rtCDRW);

  TDiscRecorderMediaType = (mtNoMedia,
                            mtCD_Extra,
                            mtCD_I,
                            mtCD_Other,
                            mtCD_ROM_XA,
                            mtCDDA_CDRROM,
                            mtSpecial);
  TDiscRecorderMediaTypes = set of TDiscRecorderMediaType;

  TDiscRecorderMediaFlag = (mfNoMedia,
                            mfBlank,
                            mfRW,
                            mfWriteable);
  TDiscRecorderMediaFlags = set of TDiscRecorderMediaFlag;

  TDiscRecorder = class(TObject)
  private
    FDiscRecorder : IDiscRecorder;
    FFlagDeleteMe : Boolean;

    FVendor,
    FProductID,
    Frevision : String;

    procedure ReadNames;

    function GetProductID: String;
    function GetRevision: String;
    function GetVendor: String;
    function GetState: TDiscRecorderState;
    function GetRekorderType: TDiscRecorderType;
    function GetPath: String;
    function GetMediaFlags: TDiscRecorderMediaFlags;
    function GetMediaType: TDiscRecorderMediaTypes;
    function GetProp(AName: String): Variant;
    procedure SetProp(AName: String; const Value: Variant);
  public
    constructor Create(const ADiskRecorder : IDiscRecorder);
    destructor Destroy; override;

    class function GUIDFromDiscRecorder(const ADiscRecorder : IDiscRecorder) : TGUID;

    function ToString : String;
    procedure Eject;
    procedure Erase(AFull : Boolean);

    procedure ReadPropertyNames(const AProperties : TStrings);

    property Vendor : String read GetVendor;
    property ProductID : String read GetProductID;
    property Revision : String read GetRevision;

    property State : TDiscRecorderState read GetState;
    property RekorderType : TDiscRecorderType read GetRekorderType;
    property Path : String read GetPath;

    property MediaType : TDiscRecorderMediaTypes read GetMediaType;
    property MediaFlags : TDiscRecorderMediaFlags read GetMediaFlags;

    property Prop[AName : String] : Variant read GetProp write SetProp;
  end;

  TDiscRecorderList = class(TList)
  private
    function Get(Index: Integer): TDiscRecorder;
    procedure Put(Index: Integer; const Value: TDiscRecorder);
  public
    procedure ToStrings(const AStrings : TStrings);
    function IndexByGUID(AGUID : TGUID) : Integer;
    function Add(Item: TDiscRecorder): Integer;
    function Extract(Item: TDiscRecorder): TDiscRecorder;
    function First: TDiscRecorder;
    function IndexOf(Item: TDiscRecorder): Integer;
    procedure Insert(Index: Integer; Item: TDiscRecorder);
    function Last: TDiscRecorder;
    function Remove(Item: TDiscRecorder): Integer;
    property Items[Index: Integer]: TDiscRecorder read Get write Put; default;
  end;

  TBlockProgressEvent = procedure(ACompleted, ATotal : Integer) of object;


  TDiscBurner = class(TComponent, IDiscMasterProgressEvents)
  private
    FDiscmaster : IDiscMaster;
    FDiscRecorderList : TDiscRecorderList;
    FOnBlockProgress: TBlockProgressEvent;

    FEventCookie : Cardinal;

    procedure ClearDiscRecorder;

    {$REGION 'IDiscMasterProgressEvents'}
    function QueryCancel(out pbCancel: BOOL): HRESULT; stdcall;
    function NotifyPnPActivity: HRESULT; stdcall;
    function NotifyAddProgress(nCompletedSteps, nTotalSteps: Longint): HRESULT; stdcall;
    function NotifyBlockProgress(nCompleted, nTotal: Longint): HRESULT; stdcall;
    function NotifyTrackProgress(nCurrentTrack, nTotalTracks: Longint): HRESULT; stdcall;
    function NotifyPreparingBurn(nEstimatedSeconds: Longint): HRESULT; stdcall;
    function NotifyClosingDisc(nEstimatedSeconds: Longint): HRESULT; stdcall;
    function NotifyBurnComplete(status: HRESULT): HRESULT; stdcall;
    function NotifyEraseComplete(status: HRESULT): HRESULT; stdcall;
    {$ENDREGION}
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    procedure RefreshDiscRekorderList;

    property DiscRecorderList : TDiscRecorderList read FDiscRecorderList;

  published
    property OnBlockProgress : TBlockProgressEvent read FOnBlockProgress write FOnBlockProgress;
  end;

  EDiscBurnerException = class(Exception)
  public
    class procedure CreateAndRaise(AErrorCode : Integer);
  end;

implementation

function PropVariantToVariant(const APropVariant : TPropVariant) : Variant;
begin
  case APropVariant.vt of
    VT_EMPTY:  VarClear(Result);
    VT_NULL : Result := null;
    VT_I2 : Result := APropVariant.iVal;
    VT_I4 : Result := APropVariant.lVal;
    VT_R4 : Result := APropVariant.fltVal;
    VT_R8 : Result := APropVariant.dblVal;
    VT_CY : Result := APropVariant.cyVal;
    VT_DATE : Result := APropVariant.date;
    VT_BSTR : Result := WideString(APropVariant.bstrVal);
    VT_ERROR : Result := APropVariant.scode;
    VT_BOOL : Result := APropVariant.bool;
    VT_I1 : Result := APropVariant.bVal;
    VT_UI1 : Result := APropVariant.bVal;
    VT_UI2 : Result := APropVariant.uiVal;
    VT_UI4 : Result := APropVariant.ulVal;
    VT_I8 : Result := Int64(APropVariant.hVal);
    VT_UI8 : Result := Int64(APropVariant.uhVal);
    VT_INT : Result := APropVariant.lVal;
    VT_UINT : Result := APropVariant.ulVal;
    VT_LPSTR : Result := StrPas(APropVariant.pszVal);
    VT_LPWSTR : Result := WideString(APropVariant.pwszVal);
  end;
end;

{ TDiscBurner }

procedure TDiscBurner.ClearDiscRecorder;
begin
  while FDiscRecorderList.Count > 0 do
  begin
    FDiscRecorderList.First.Free;
    FDiscRecorderList.Delete(0);
  end;
end;

constructor TDiscBurner.Create(AOwner : TComponent);
var
  Res : HRESULT;
begin
  inherited;
  
  Res := CoCreateInstance(CLSID_MSDiscMasterObj,
                          nil,
                          CLSCTX_LOCAL_SERVER,
                          IID_IDiscMaster,
                          FDiscmaster);

  EDiscBurnerException.CreateAndRaise(Res);

  EDiscBurnerException.CreateAndRaise(FDiscmaster.Open);

  FDiscRecorderList := TDiscRecorderList.Create;
  RefreshDiscRekorderList;

  EDiscBurnerException.CreateAndRaise(FDiscmaster.ProgressAdvise(Self, FEventCookie));
end;

destructor TDiscBurner.Destroy;
begin
  ClearDiscRecorder;
  FDiscRecorderList.Free;


  FDiscmaster.ProgressUnadvise(FEventCookie);
  FDiscmaster.Close;
  FDiscmaster := nil;

  inherited;
end;


function TDiscBurner.NotifyAddProgress(nCompletedSteps,
  nTotalSteps: Integer): HRESULT;
begin
  Result := S_OK;
end;

function TDiscBurner.NotifyBlockProgress(nCompleted, nTotal: Integer): HRESULT;
begin
  if Assigned(FOnBlockProgress) then
    FOnBlockProgress(nCompleted, nTotal);
    
  Result := S_OK;
end;

function TDiscBurner.NotifyBurnComplete(status: HRESULT): HRESULT;
begin
  Result := S_OK;
end;

function TDiscBurner.NotifyClosingDisc(nEstimatedSeconds: Integer): HRESULT;
begin
  Result := S_OK;
end;

function TDiscBurner.NotifyEraseComplete(status: HRESULT): HRESULT;
begin
  Result := S_OK;
end;

function TDiscBurner.NotifyPnPActivity: HRESULT;
begin
  Result := S_OK;
end;

function TDiscBurner.NotifyPreparingBurn(nEstimatedSeconds: Integer): HRESULT;
begin
  Result := S_OK;
end;

function TDiscBurner.NotifyTrackProgress(nCurrentTrack,
  nTotalTracks: Integer): HRESULT;
begin
  Result := S_OK;
end;

function TDiscBurner.QueryCancel(out pbCancel: BOOL): HRESULT;
begin
  Result := E_NOTIMPL;
end;

procedure TDiscBurner.RefreshDiscRekorderList;
var
  idx,
  idy : Integer;
  enum : IEnumDiscRecorders;
  rec : IDiscRecorder;
  fetched : Cardinal;
begin
  for idx := 0 to FDiscRecorderList.Count - 1 do
    FDiscRecorderList[idx].FFlagDeleteMe := true;

  if Succeeded(FDiscmaster.EnumDiscRecorders(enum)) then
  begin
    fetched := 1;
    enum.Next(1, rec, fetched);
    while fetched > 0 do
    begin
      idy := FDiscRecorderList.IndexByGUID(TDiscRecorder.GUIDFromDiscRecorder(rec));

      if idy > -1 then
        FDiscRecorderList[idy].FFlagDeleteMe := false
      else
        FDiscRecorderList.Add(TDiscRecorder.Create(rec));

      enum.Next(1, rec, fetched);
    end;
  end;

  for idx := FDiscRecorderList.Count - 1 downto 0 do
  begin
    if FDiscRecorderList[idx].FFlagDeleteMe then
      FDiscRecorderList.Delete(idx);
  end;
end;

{ TDiscRecorder }

constructor TDiscRecorder.Create(const ADiskRecorder: IDiscRecorder);
begin
  FDiscRecorder := ADiskRecorder;
  FFlagDeleteMe := false;

  FVendor := '';
  FProductID := '';
  Frevision := '';
end;

destructor TDiscRecorder.Destroy;
begin
  inherited;
end;

procedure TDiscRecorder.Eject;
begin
  EDiscBurnerException.CreateAndRaise(FDiscRecorder.OpenExclusive);
  try
    EDiscBurnerException.CreateAndRaise(FDiscRecorder.Eject);
  finally
    FDiscRecorder.Close;
  end;
end;

procedure TDiscRecorder.Erase(AFull: Boolean);
begin
  EDiscBurnerException.CreateAndRaise(FDiscRecorder.OpenExclusive);
  try
    EDiscBurnerException.CreateAndRaise(FDiscRecorder.Erase(AFull));
  finally
    FDiscRecorder.Close;
  end;
end;

function TDiscRecorder.GetMediaFlags: TDiscRecorderMediaFlags;
var
  mt, mf : Integer;
begin
  Result := [];
  
  EDiscBurnerException.CreateAndRaise(FDiscRecorder.OpenExclusive);
  try
    EDiscBurnerException.CreateAndRaise(FDiscRecorder.QueryMediaType(mt, mf));

    if mf = 0 then
      Include(Result, mfNoMedia)
    else
    if MEDIA_BLANK and mf = MEDIA_BLANK then
      Include(Result, mfBlank)
    else
    if MEDIA_RW and mf = MEDIA_RW then
      Include(Result, mfRW)
    else
    if MEDIA_WRITABLE and mf = MEDIA_WRITABLE then
      Include(Result, mfWriteable);
  finally
    FDiscRecorder.Close;
  end;
end;

function TDiscRecorder.GetMediaType: TDiscRecorderMediaTypes;
var
  mt, mf : Integer;
begin
  Result := [];

  EDiscBurnerException.CreateAndRaise(FDiscRecorder.OpenExclusive);
  try
    EDiscBurnerException.CreateAndRaise(FDiscRecorder.QueryMediaType(mt, mf));

    if mt = 0 then
      Include(Result, mtNoMedia)
    else
    if MEDIA_CDDA_CDROM and mf = MEDIA_CDDA_CDROM then
      Include(Result, mtCDDA_CDRROM)
    else
    if MEDIA_CD_ROM_XA and mf = MEDIA_CD_ROM_XA then
      Include(Result, mtCD_ROM_XA)
    else
    if MEDIA_CD_I and mf = MEDIA_CD_I then
      Include(Result, mtCD_I)
    else
    if MEDIA_CD_EXTRA and mf = MEDIA_CD_EXTRA then
      Include(Result, mtCD_Extra)
    else
    if MEDIA_CD_OTHER and mf = MEDIA_CD_OTHER then
      Include(Result, mtCD_Other)
    else
    if MEDIA_SPECIAL and mf = MEDIA_SPECIAL then
      Include(Result, mtSpecial)
  finally
    FDiscRecorder.Close;
  end;
end;

function TDiscRecorder.GetPath: String;
var
  p : PWideChar;
begin
  p := nil;
  EDiscBurnerException.CreateAndRaise(FDiscRecorder.GetPath(p));
  Result := P;  
end;

function TDiscRecorder.GetProductID: String;
begin
  if FProductID = '' then
    ReadNames;
  Result := FProductID;
end;

function TDiscRecorder.GetProp(AName: String): Variant;
var
  Props : IPropertyStorage;
  Spec : PROPSPEC;
  V : TPropVariant;
begin
  EDiscBurnerException.CreateAndRaise(FDiscRecorder.GetRecorderProperties(Props));

  Spec.ulKind := PRSPEC_LPWSTR;
  Spec.lpwstr := PWideChar(AName);

  EDiscBurnerException.CreateAndRaise(Props.ReadMultiple(1,@Spec, @V));

  Result := PropVariantToVariant(V);
end;

function TDiscRecorder.GetRekorderType: TDiscRecorderType;
var
  t : Integer;
begin
  EDiscBurnerException.CreateAndRaise(FDiscRecorder.GetRecorderType(t));

  case t of
    $01: Result := rtCDR;
    $02: Result := rtCDRW;
    else
      Result := rtUnknown;
  end;

end;

function TDiscRecorder.GetRevision: String;
begin
  if Frevision = '' then
    ReadNames;
  Result := Frevision;
end;

function TDiscRecorder.GetState: TDiscRecorderState;
var
  s : Cardinal;
begin
  EDiscBurnerException.CreateAndRaise(FDiscRecorder.GetRecorderState(s));

  case s of
    RECORDER_DOING_NOTHING: Result := rsIdle;
    RECORDER_OPENED: Result := rsOpen;
    RECORDER_BURNING: Result := rsBurning;
    else
      Result := rsUnknown;
  end;
end;

function TDiscRecorder.GetVendor: String;
begin
  if FVendor = '' then
    ReadNames;
  Result := FVendor;
end;

class function TDiscRecorder.GUIDFromDiscRecorder(
  const ADiscRecorder: IDiscRecorder): TGUID;
var
  Buffer : Pointer;
  Fetched : Cardinal;
begin
  Buffer := @Result;
  ADiscRecorder.GetRecorderGUID(Buffer, SizeOf(TGUID), Fetched);
end;


procedure TDiscRecorder.ReadNames;
var
  V, P, R : PWideChar;
begin
  V := nil;
  P := nil;
  R := nil;

  EDiscBurnerException.CreateAndRaise(FDiscRecorder.GetDisplayNames(V, P, R));
  
  FVendor := V;
  FProductID := P;
  Frevision := R;
end;

procedure TDiscRecorder.ReadPropertyNames(const AProperties: TStrings);
var
  Props : IPropertyStorage;
  enum : IEnumSTATPROPSTG;
  s : STATPROPSTG;
  fetched : Cardinal;
begin
  EDiscBurnerException.CreateAndRaise(FDiscRecorder.GetRecorderProperties(Props));

  if Succeeded(Props.Enum(enum)) then
  begin
    enum.Next(1, s, @fetched);
    while fetched > 0 do
    begin
      AProperties.AddObject(s.lpwstrName, TObject(s.propid));
      enum.Next(1, s, @fetched);
    end;
  end;
end;

procedure TDiscRecorder.SetProp(AName: String; const Value: Variant);
begin

end;

function TDiscRecorder.ToString: String;
begin
  Result := Vendor + ' ' + ProductID; 
end;

{ TDiscRecorderList }

function TDiscRecorderList.Add(Item: TDiscRecorder): Integer;
begin
  Result := inherited Add(Item);
end;

function TDiscRecorderList.Extract(Item: TDiscRecorder): TDiscRecorder;
begin
  Result := inherited Extract(Item);
end;

function TDiscRecorderList.First: TDiscRecorder;
begin
  Result := inherited First;
end;

function TDiscRecorderList.Get(Index: Integer): TDiscRecorder;
begin
  Result := inherited Get(Index);
end;

function TDiscRecorderList.IndexByGUID(AGUID: TGUID): Integer;
var
  idx : Integer;
  GUID : TGUID;
begin
  Result := -1;

  for idx := 0 to Count - 1 do
  begin
    GUID := TDiscRecorder.GUIDFromDiscRecorder(Items[idx].FDiscRecorder);

    if SameGUID(AGUID, GUID) then
    begin
      Result := idx;
      break;
    end;
  end;
end;

function TDiscRecorderList.IndexOf(Item: TDiscRecorder): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TDiscRecorderList.Insert(Index: Integer; Item: TDiscRecorder);
begin
  inherited Insert(Index, Item);
end;

function TDiscRecorderList.Last: TDiscRecorder;
begin
  Result := inherited Last;
end;

procedure TDiscRecorderList.Put(Index: Integer; const Value: TDiscRecorder);
begin
  inherited Put(Index, Value);
end;

function TDiscRecorderList.Remove(Item: TDiscRecorder): Integer;
begin
  Result := inherited Remove(Item);
end;

procedure TDiscRecorderList.ToStrings(const AStrings: TStrings);
var
  idx : Integer;
begin
  for idx := 0 to Count - 1 do
    AStrings.AddObject(Items[idx].ToString, Items[idx]);
end;

{ EDiscBurnerException }

class procedure EDiscBurnerException.CreateAndRaise(AErrorCode: Integer);
var
  msg : String;
begin
  msg := '';

  case AErrorCode of
    IMAPI_S_PROPERTIESIGNORED: msg := 'An unknown property was passed in a property set and it was ignored.';
    IMAPI_S_BUFFER_TO_SMALL: msg := 'The output buffer is too small.';
    IMAPI_E_NOTOPENED: msg := 'A call to IDiscMaster::Open has not been made.';
    IMAPI_E_NOTINITIALIZED: msg := 'A recorder object has not been initialized.';
    IMAPI_E_USERABORT: msg := 'The user canceled the operation.';
    IMAPI_E_GENERIC: msg := 'A generic error occurred.';
    IMAPI_E_MEDIUM_NOTPRESENT: msg := 'There is no disc in the device.';
    IMAPI_E_MEDIUM_INVALIDTYPE: msg := 'The media is not a type that can be used.';
    IMAPI_E_DEVICE_NOPROPERTIES: msg := 'The recorder does not support any properties.';
    IMAPI_E_DEVICE_NOTACCESSIBLE: msg := 'The device cannot be used or is already in use.';
    IMAPI_E_DEVICE_NOTPRESENT: msg := 'The device is not present or has been removed.';
    IMAPI_E_DEVICE_INVALIDTYPE: msg := 'The recorder does not support an operation.';
    IMAPI_E_INITIALIZE_WRITE: msg := 'The drive interface could not be initialized for writing.';
    IMAPI_E_INITIALIZE_ENDWRITE: msg := 'The drive interface could not be initialized for closing.';
    IMAPI_E_FILESYSTEM: msg := 'An error occurred while enabling/disabling file system access or during auto-insertion detection.';
    IMAPI_E_FILEACCESS: msg := 'An error occurred while writing the image file.';
    IMAPI_E_DISCINFO: msg := 'An error occurred while trying to read disc data from the device.';
    IMAPI_E_TRACKNOTOPEN: msg := 'An audio track is not open for writing.';
    IMAPI_E_TRACKOPEN: msg := 'An open audio track is already being staged.';
    IMAPI_E_DISCFULL: msg := 'The disc cannot hold any more data.';
    IMAPI_E_BADJOLIETNAME: msg := 'The application tried to add a badly named element to a disc.';
    IMAPI_E_INVALIDIMAGE: msg := 'The staged image is not suitable for a burn. It has been corrupted or cleared and has no usable content.';
    IMAPI_E_NOACTIVEFORMAT: msg := 'An active format master has not been selected using  IDiscMaster::SetActiveDiscMasterFormat.';
    IMAPI_E_NOACTIVERECORDER: msg := 'An active disc recorder has not been selected using  IDiscMaster::SetActiveDiscRecorder.';
    IMAPI_E_WRONGFORMAT: msg := 'A call to IJolietDiscMaster has been made when  IRedbookDiscMaster is the active format, or vice versa. To use a different format, change the format and clear the image file contents.';
    IMAPI_E_ALREADYOPEN: msg := 'A call to IDiscMaster::Open has already been made against this object by your application.';
    IMAPI_E_WRONGDISC: msg := 'The IMAPI multi-session disc has been removed from the active recorder.';
    IMAPI_E_FILEEXISTS: msg := 'The file to add is already in the image file and the overwrite flag was not set.';
    IMAPI_E_STASHINUSE: msg := 'Another application is already using the IMAPI stash file required to stage a disc image. Try again later.';
    IMAPI_E_DEVICE_STILL_IN_USE: msg := 'Another application is already using this device, so IMAPI cannot access the device.';
    IMAPI_E_LOSS_OF_STREAMING: msg := 'Content streaming was lost; a buffer under-run may have occurred.';
    IMAPI_E_COMPRESSEDSTASH: msg := 'The stash is located on a compressed volume and cannot be read.';
    IMAPI_E_ENCRYPTEDSTASH: msg := 'The stash is located on an encrypted volume and cannot be read.';
    IMAPI_E_NOTENOUGHDISKFORSTASH: msg := 'There is not enough free space to create the stash file on the specified volume.';
    IMAPI_E_REMOVABLESTASH: msg := 'The selected stash location is on a removable media.';
    IMAPI_E_CANNOT_WRITE_TO_MEDIA: msg := 'The media cannot be written to.';
    IMAPI_E_TRACK_NOT_BIG_ENOUGH: msg := 'The track is not big enough.';
    IMAPI_E_BOOTIMAGE_AND_NONBLANK_DISC: msg := 'Attempt to create a bootable image on a non-blank disc.';
    else
      OleCheck(AErrorCode);
  end;

  if msg <> '' then
    raise EDiscBurnerException.Create(msg);
end;

initialization
  CoInitFlags := COINIT_APARTMENTTHREADED;

end.
