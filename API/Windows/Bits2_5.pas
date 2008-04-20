unit Bits2_5;

interface

uses
  JwaWinBase,
  JwaWinType,
  JwaBits,
  JwaBits1_5,
  Bits2_0;

type
  BG_CERT_STORE_LOCATION = (BG_CERT_STORE_LOCATION_CURRENT_USER,
                            BG_CERT_STORE_LOCATION_LOCAL_MACHINE,
                            BG_CERT_STORE_LOCATION_CURRENT_SERVICE,
                            BG_CERT_STORE_LOCATION_SERVICES,
                            BG_CERT_STORE_LOCATION_USERS,
                            BG_CERT_STORE_LOCATION_CURRENT_USER_GROUP_POLICY,
                            BG_CERT_STORE_LOCATION_LOCAL_MACHINE_GROUP_POLICY,
                            BG_CERT_STORE_LOCATION_LOCAL_MACHINE_ENTERPRISE);
  {$EXTERNALSYM BG_CERT_STORE_LOCATION}
  TBgCertStoreLocation = BG_CERT_STORE_LOCATION;
  PBgCertStoreLocation = ^BG_CERT_STORE_LOCATION;

const
  IID_IBackgroundCopyJobHttpOptions : TGUID = '{f1bd1079-9f01-4bdc-8036-f09b70095066}';
  {$EXTERNALSYM IID_IBackgroundCopyJobHttpOptions}

type
  IBackgroundCopyJobHttpOptions = interface(IUnknown)
  ['{f1bd1079-9f01-4bdc-8036-f09b70095066}']
    function SetClientCertificateByID(StoreLocation : TBgCertStoreLocation; StoreName : LPCWSTR; CertHashBlob : PBYTE) : HRESULT; stdcall;
    function SetClientCertificateByName(StoreName : LPCWSTR; SubjectName : LPWSTR) : HRESULT; stdcall;
    function RemoveClientCertificate() : HRESULT; stdcall;
    function GetClientCertificate(out StoreLocation : TBgCertStoreLocation; out StoreName : LPWSTR; out CertHashBlob : PByte; out SubjectName : LPWSTR) : HRESULT; stdcall;
    function SetCustomHeaders(RequestHeaders : LPCWSTR) : HRESULT; stdcall;
    function GetCustomHeaders(out RequestHeaders : LPCWSTR) : HRESULT; stdcall;
    function SetSecurityFlags(Flags : ULONG) : HRESULT; stdcall;
    function GetSecurityFlags(out Flags : ULONG) : HRESULT; stdcall;
  end;
  {$EXTERNALSYM IBackgroundCopyJobHttpOptions}

//---------------------------------------------------------------------------

const
  LIBID_BackgroundCopyManager2_5: GUID = '{03ca98d6-ff5d-49b8-abc6-03dd84127020}';
  {$EXTERNALSYM LIBID_BackgroundCopyManager2_5}
  CLSID_BackgroundCopyManager2_5: GUID = '{03ca98d6-ff5d-49b8-abc6-03dd84127020}';
  {$EXTERNALSYM CLSID_BackgroundCopyManager2_5}

  BG_SSL_ENABLE_CRL_CHECK                     = $0001;
  {$EXTERNALSYM BG_SSL_ENABLE_CRL_CHECK}
  BG_SSL_IGNORE_CERT_CN_INVALID               = $0002;
  {$EXTERNALSYM BG_SSL_IGNORE_CERT_CN_INVALID}
  BG_SSL_IGNORE_CERT_DATE_INVALID             = $0004;
  {$EXTERNALSYM BG_SSL_IGNORE_CERT_DATE_INVALID}
  BG_SSL_IGNORE_UNKNOWN_CA                    = $0008;
  {$EXTERNALSYM BG_SSL_IGNORE_UNKNOWN_CA}
  BG_SSL_IGNORE_CERT_WRONG_USAGE              = $0010;
  {$EXTERNALSYM BG_SSL_IGNORE_CERT_WRONG_USAGE}
  BG_HTTP_REDIRECT_POLICY_MASK                = $0700;
  {$EXTERNALSYM BG_HTTP_REDIRECT_POLICY_MASK}
  BG_HTTP_REDIRECT_POLICY_ALLOW_SILENT        = $0000;
  {$EXTERNALSYM BG_HTTP_REDIRECT_POLICY_ALLOW_SILENT}
  BG_HTTP_REDIRECT_POLICY_ALLOW_REPORT        = $0100;
  {$EXTERNALSYM BG_HTTP_REDIRECT_POLICY_ALLOW_REPORT}
  BG_HTTP_REDIRECT_POLICY_DISALLOW            = $0200;
  {$EXTERNALSYM BG_HTTP_REDIRECT_POLICY_DISALLOW}
  BG_HTTP_REDIRECT_POLICY_ALLOW_HTTPS_TO_HTTP = $0800;
  {$EXTERNALSYM BG_HTTP_REDIRECT_POLICY_ALLOW_HTTPS_TO_HTTP}

implementation

end.
