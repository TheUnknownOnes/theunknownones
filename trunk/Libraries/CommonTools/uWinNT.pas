//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uWinNT;

interface

uses
  Windows;

//function ConvertSidToStringSid(Sid: PSID; var StringSid: PChar): Boolean; stdcall; external 'advapi32.dll' name 'ConvertSidToStringSidA';

function GetSIDStringFromUser(AUsername : AnsiString; AServerName : AnsiString = '') : String;

implementation

type
  TConvertSidToStringSid = function(Sid: PSID; var StringSid: PAnsiChar): Boolean; stdcall;
  TLookupAccountName = function(lpSystemName, lpAccountName: PAnsiChar;
                                Sid: PSID; var cbSid: DWORD; ReferencedDomainName: PAnsiChar;
                                var cbReferencedDomainName: DWORD; var peUse: SID_NAME_USE): BOOL; stdcall;

function ConvertSidToStringSid(Sid: PSID; var StringSid: PAnsiChar): Boolean;
var
  DLL : Cardinal;
  Func : TConvertSidToStringSid;
begin
  Result:=false;
  StringSid:='';

  DLL:=LoadLibrary('advapi32.dll');
  try
    if DLL>0 then
    begin
      @Func:=GetProcAddress(DLL, 'ConvertSidToStringSidA');
      Result:=Func(Sid, StringSid);
      exit;
    end;
  finally
    FreeLibrary(DLL)
  end;
end;

function MyLookupAccountName(lpSystemName, lpAccountName: PAnsiChar;
                             Sid: PSID; var cbSid: DWORD; ReferencedDomainName: PAnsiChar;
                             var cbReferencedDomainName: DWORD; var peUse: SID_NAME_USE): Boolean;
var
  DLL : Cardinal;
  Func : TLookupAccountName;
begin
  Result:=false;

  DLL:=LoadLibrary('advapi32.dll');
  try
    if DLL>0 then
    begin
      @Func:=GetProcAddress(DLL, 'LookupAccountNameA');
      Result:=Func(lpSystemName, lpAccountName, Sid, cbSid, ReferencedDomainName, cbReferencedDomainName, peUse);
      exit;
    end;
  finally
    FreeLibrary(DLL)
  end;
end;


function GetSIDStringFromUser(AUsername : AnsiString; AServerName : AnsiString = '') : String;
var
  SID : PSID;
  DomainLen,
  SIDSize : DWORD;
  SIDType : SID_NAME_USE;
  Buffer : PAnsiChar;
  Domain : AnsiString;
begin
  SIDSize:=0;
  DomainLen:=0;
  
  if not LookupAccountNameA(PAnsiChar(AServerName), PAnsiChar(AUsername), nil, SIDSize, nil, DomainLen, sidtype) then
  begin
    SetLength(domain, domainlen);
    GetMem(SID, SIDSize);
    if MyLookupAccountName(PAnsiChar(AServerName), PAnsiChar(AUsername), SID, SIDSize, PAnsiChar(domain), DomainLen, sidtype) then
    begin
      ConvertSidToStringSid(sid, buffer);
      Result:=buffer;
      LocalFree(Cardinal(Buffer));
    end;
    FreeMem(SID, SIDSize);
  end;

end;

end.
