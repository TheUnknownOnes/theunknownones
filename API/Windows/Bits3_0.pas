unit Bits3_0;

interface

uses
  JwaWinBase,
  JwaWinType,
  JwaBits,
  JwaBits1_5,
  Bits2_0,
  Bits2_5;

const
  IID_IBitsPeerCacheRecord : TGUID = '{659cdeaf-489e-11d9-a9cd-000d56965251}';
  {$EXTERNALSYM IID_IBitsPeerCacheRecord}

type
  IBitsPeerCacheRecord = interface(IUnknown)
  ['{659cdeaf-489e-11d9-a9cd-000d56965251}']
    function GetId(out ID : GUID) : HRESULT; stdcall;
    function GetOriginUrl(out OriginUrl : LPWSTR) : HRESULT; stdcall;
    function GetFileSize(out FileSize : UINT64) : HRESULT; stdcall;
    function GetFileModificationTime(out ModificationTime : FILETIME) : HRESULT; stdcall;
    function GetLastAccessTime(out AccessTime : FILETIME) : HRESULT; stdcall;
    function IsFileValidated() : HRESULT; stdcall;
    function GetFileRanges(out RangeCount : DWORD; out Ranges : TBgFileRanges) : HRESULT; stdcall;
  end;
  {$EXTERNALSYM IBitsPeerCacheRecord}

const
  IID_IEnumBitsPeerCacheRecords : TGUID = '{659cdea4-489e-11d9-a9cd-000d56965251}';
  {$EXTERNALSYM IID_IEnumBitsPeerCacheRecords}

type
  IEnumBitsPeerCacheRecords = interface(IUnknown)
  ['{659cdea4-489e-11d9-a9cd-000d56965251}']
    function Next(celt: ULONG; out rgelt: IBitsPeerCacheRecord;  pceltFetched: PULONG): HRESULT; stdcall;
    function Skip(celt: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppenum: IEnumBitsPeerCacheRecords): HRESULT; stdcall;
    function GetCount(out puCount: ULONG): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IEnumBitsPeerCacheRecords}

const
  IID_IBitsPeer : TGUID = '{659cdea2-489e-11d9-a9cd-000d56965251}';

type
  IBitsPeer = interface(IUnknown)
  ['{659cdea2-489e-11d9-a9cd-000d56965251}']
    function GetPeerName(out PeerName : LPWSTR) : HRESULT; stdcall;
    function IsAuthenticated(out Authenticated : BOOL) : HRESULT; stdcall;
    function IsAvailable(out Available : BOOL) : HRESULT; stdcall; 
  end;
  {$EXTERNALSYM IBitsPeer}

const
  IID_IEnumBitsPeers : TGUID = '{659cdea5-489e-11d9-a9cd-000d56965251}';

type
  IEnumBitsPeers = interface(IUnknown)
  ['{659cdea5-489e-11d9-a9cd-000d56965251}']
    function Next(celt: ULONG; out rgelt: IBitsPeer;  pceltFetched: PULONG): HRESULT; stdcall;
    function Skip(celt: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppenum: IEnumBitsPeers): HRESULT; stdcall;
    function GetCount(out puCount: ULONG): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IEnumBitsPeers}

const
  BG_ENABLE_PEERCACHING_CLIENT = $0001;
  {$EXTERNALSYM BG_ENABLE_PEERCACHING_CLIENT}
  BG_ENABLE_PEERCACHING_SERVER = $0002;
  {$EXTERNALSYM BG_ENABLE_PEERCACHING_SERVER}

  IID_IBitsPeerCacheAdministration : TGUID = '{659cdead-489e-11d9-a9cd-000d56965251}';
  {$EXTERNALSYM IID_IBitsPeerCacheAdministration}

type
  IBitsPeerCacheAdministration = interface(IUnknown)
  ['{659cdead-489e-11d9-a9cd-000d56965251}']
    function GetMaximumCacheSize(out Bytes : DWORD) : HRESULT; stdcall;
    function SetMaximumCacheSize(Bytes : DWORD) : HRESULT; stdcall;
    function GetMaximumContentAge(out Seconds : ULONG) : HRESULT; stdcall;
    function SetMaximumContentAge(Seconds : ULONG) : HRESULT; stdcall;
    function GetConfigurationFlags(out Flags : DWORD) : HRESULT; stdcall;
    function SetConfigurationFlags(Flags : DWORD) : HRESULT; stdcall;
    function EnumRecords(out Enum : IEnumBitsPeerCacheRecords) : HRESULT; stdcall;
    function GetRecord(var Id : TGUID; out _Record : IBitsPeerCacheRecord) : HRESULT; stdcall;
    function ClearRecords() : HRESULT; stdcall;
    function DeleteRecord(var Id : TGUID) : HRESULT; stdcall;
    function DeleteUrl(Url : LPCWSTR) : HRESULT; stdcall;
    function EnumPeers(out Enum : IEnumBitsPeers) : HRESULT; stdcall;
    function ClearPeers() : HRESULT; stdcall;
    function DiscoverPeers : HRESULT; stdcall;
  end;
  {$EXTERNALSYM IBitsPeerCacheAdministration}

const
  BG_JOB_ENABLE_PEERCACHING_CLIENT = $0001;
  {$EXTERNALSYM BG_JOB_ENABLE_PEERCACHING_CLIENT}
  BG_JOB_ENABLE_PEERCACHING_SERVER = $0002;
  {$EXTERNALSYM BG_JOB_ENABLE_PEERCACHING_SERVER}

  IID_IBackgroundCopyJob4 : TGUID = '{659cdeae-489e-11d9-a9cd-000d56965251}';
  {$EXTERNALSYM IID_IBackgroundCopyJob4}

type
  IBackgroundCopyJob4 = interface(IBackgroundCopyJob3)
  ['{659cdeae-489e-11d9-a9cd-000d56965251}']
    function SetPeerCachingFlags(Flags : DWORD) : HRESULT; stdcall;
    function GetPeerCachingFlags(out Flags : DWORD) : HRESULT; stdcall;
    function GetOwnerIntegrityLevel(out Level : ULONG) : HRESULT; stdcall;
    function GetOwnerElevationState(out Elevated : BOOL) : HRESULT; stdcall;
    function SetMaximumDownloadTime(Timeout : ULONG) : HRESULT; stdcall;
    function GetMaximumDownloadTime(out Timeout : ULONG) : HRESULT; stdcall;
  end;
  {$EXTERNALSYM IBackgroundCopyJob4}

const
  IID_IBackgroundCopyFile3 : TGUID = '{659cdeaa-489e-11d9-a9cd-000d56965251}';
  {$EXTERNALSYM IID_IBackgroundCopyFile3}

type
  IBackgroundCopyFile3 = interface(IBackgroundCopyFile2)
  ['{659cdeaa-489e-11d9-a9cd-000d56965251}']
    function GetTemporaryName(out Filename : LPWSTR) : HRESULT; stdcall;
    function SetValidationState(State : BOOL) : HRESULT; stdcall;
    function GetValidationState(out State : BOOL) : HRESULT; stdcall;
    function IsDownloadedFromPeer(out FromPeer : BOOL) : HRESULT; stdcall;
  end;  
  {$EXTERNALSYM IBackgroundCopyFile3}

const
  IID_IBackgroundCopyCallback2 : TGUID = '{659cdeac-489e-11d9-a9cd-000d56965251}';
  {$EXTERNALSYM IID_IBackgroundCopyCallback2}

type
  IBackgroundCopyCallback2 = interface(IBackgroundCopyCallback)
  ['{659cdeac-489e-11d9-a9cd-000d56965251}']
    function FileTransferred(Job : IBackgroundCopyJob; _File : IBackgroundCopyFile) : HRESULT; stdcall;  
  end;

//---------------------------------------------------------------------------

const
  LIBID_BackgroundCopyManager3_0: GUID = '{659cdea7-489e-11d9-a9cd-000d56965251}';
  {$EXTERNALSYM LIBID_BackgroundCopyManager3_0}
  CLSID_BackgroundCopyManager3_0: GUID = '{659cdea7-489e-11d9-a9cd-000d56965251}';
  {$EXTERNALSYM CLSID_BackgroundCopyManager3_0}

implementation

end.
