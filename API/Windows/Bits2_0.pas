unit Bits2_0;

interface

uses
  JwaWinBase,
  JwaWinType,
  JwaBits,
  JwaBits1_5;

type
  _BG_FILE_RANGE = record
    InitialOffset : UINT64;
    Length : UINT64;
  end;
  {$EXTERNALSYM _BG_FILE_RANGE}
  BG_FILE_RANGE = _BG_FILE_RANGE;
  {$EXTERNALSYM BG_FILE_RANGE}
  TBgFileRange = BG_FILE_RANGE;
  PBgFileRange = ^BG_FILE_RANGE;
  TBgFileRanges = array of TBgFileRange;

const
  BG_COPY_FILE_OWNER = 1;
  {$EXTERNALSYM BG_COPY_FILE_OWNER}
  BG_COPY_FILE_GROUP = 2;
  {$EXTERNALSYM BG_COPY_FILE_GROUP}
  BG_COPY_FILE_DACL = 4;
  {$EXTERNALSYM BG_COPY_FILE_DACL}
  BG_COPY_FILE_SACL = 8;
  {$EXTERNALSYM BG_COPY_FILE_SACL}
  BG_COPY_FILE_ALL = 15;
  {$EXTERNALSYM BG_COPY_FILE_ALL}

  IID_IBackgroundCopyJob3 : TGUID = '{443c8934-90ff-48ed-bcde-26f5c7450042}';
  {$EXTERNALSYM IID_IBackgroundCopyJob3}

type
  IBackgroundCopyJob3 = interface(IBackgroundCopyJob2)
  ['{443c8934-90ff-48ed-bcde-26f5c7450042}']
    function ReplaceRemotePrefix(OldPrefix : LPCWSTR; NewPrefix : LPCWSTR) : HRESULT; stdcall;
    function AddFileWithRanges(RemoteUrl : LPCWSTR; LocalName : LPCWSTR; RangeCount : DWORD; Ranges : TBgFileRanges) : HRESULT; stdcall;
    function SetFileACLFlags(Flags : DWORD) : HRESULT; stdcall;
    function GetFileACLFlags(out Flags : DWORD) : HRESULT; stdcall;
  end;
  {$EXTERNALSYM IBackgroundCopyJob3}

const
  IID_IBackgroundCopyFile2 : TGUID = '{83e81b93-0873-474d-8a8c-f2018b1a939c}';
  {$EXTERNALSYM IID_IBackgroundCopyFile2}

type
  IBackgroundCopyFile2 = interface(IBackgroundCopyFile)
  ['{83e81b93-0873-474d-8a8c-f2018b1a939c}']
    function GetFileRanges(var RangeCount : DWORD; var Ranges : TBgFileRanges) : HRESULT; stdcall;
    function SetRemoteName(RemoteName : LPCWSTR) : HRESULT; stdcall;
  end;

//---------------------------------------------------------------------------

const
  LIBID_BackgroundCopyManager2_0: GUID = '{6d18ad12-bde3-4393-b311-099c346e6df9}';
  {$EXTERNALSYM LIBID_BackgroundCopyManager2_0}
  CLSID_BackgroundCopyManager2_0: GUID = '{6d18ad12-bde3-4393-b311-099c346e6df9}';
  {$EXTERNALSYM LIBID_BackgroundCopyManager2_0}


implementation

end.
