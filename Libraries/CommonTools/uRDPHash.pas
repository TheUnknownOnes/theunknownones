{******************************************************************}
{ Author: Remko Weijnen (r dot weijnen at gmail dot com)           }
{ Version: 0.1                                                     }
{ Date: 21-03-2007                                                 }
{                                                                  }
{ The contents of this file are subject to                         }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/MPL/MPL-1.1.html                          }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{******************************************************************}

unit uRDPHash;

interface

uses Windows, Sysutils {$ifndef FPC}, JwaWinCrypt{$endif};

{$ifdef FPC}
type
  TBlobData = record
    cbData : DWORD;
    pbData : LPBYTE;
  end;
  DATA_BLOB = TBlobData;
  PBlobData = ^TBlobData;
{$endif}


function CryptRDPPassword(sPassword: string): string;
{$ifndef FPC}function DecryptRDPPassword(sPasswordHash: string): string;{$endif}
function BlobDataToHexStr(P: PByte; I: Integer): string;
function PasswordHashToBlobData(sPasswordHash: string): DATA_BLOB;

implementation

{$ifdef FPC}

const
 CRYPTPROTECT_UI_FORBIDDEN = 1;

type
  LPLPWSTR = ^LPWSTR;

function CryptProtectData(pDataIn: PBlobData; szDataDescr: LPCWSTR;
  pOptionalEntropy: PBlobData; pvReserved: Pointer;
  pPromptStruct: Pointer; dwFlags: DWORD; pDataOut: PBlobData): BOOL; stdcall; external 'coredll' name 'CryptProtectData';

function CryptUnprotectData(pDataIn: PBlobData; ppszDataDescr: LPLPWSTR;
  pOptionalEntropy: PBlobData; pvReserved: Pointer;
  pPromptStruct: Pointer; dwFlags: DWORD; pDataOut: PBlobData): BOOL; stdcall; external 'coredll' name 'CryptUnProtectData';

{$endif}

{***********************************************************}
{ HexToByte: Converts Hex value to Byte                     }
{ Found this somewhere on the internet                      }
{***********************************************************}
function HexToByte(s : String) : Byte;
const
  cs = '0123456789ABCDEF';
begin
  result := 0;
  if (length(s) = 2) and
     (s[1] in ['0'..'9','A'..'F']) and
     (s[2] in ['0'..'9','A'..'F']) then
    result := ((pos(s[1],cs)-1) *16) + (pos(s[2],cs)-1)
  else raise EConvertError.CreateFmt('%s is not a Hexformatstring',[s]);
end;

{***********************************************************}
{ PasswordHashToBlobData: Converts a RDP password Hash to   }
{                         a DATA_BLOB structure             }
{ sPasswordHash : RDP Password Hash (HEX String             }
{***********************************************************}
function PasswordHashToBlobData(sPasswordHash: string): DATA_BLOB;
var Buf: array of Byte;
  dwBufSize: Cardinal;
  i: Cardinal;
  j: Cardinal;
  dwHashSize: Cardinal;
begin
  dwBufSize := Length(sPassWordHash) DIV 2;
  dwHashSize := Length(sPasswordHash);
  SetLength(Buf, dwBufSize);

  i := 1;
  j := 0;
  while i < dwHashSize do begin
    Buf[j] := HexToByte(sPassWordHash[i] + sPassWordHash[i+1]);
    Inc(i, 2);
    Inc(j);
  end;

  GetMem(Result.pbData, dwBufSize);
  Result.cbData := dwBufSize;
  Result.pbData := PByte(Buf);
end;

{***********************************************************}
{ BlobDataToHexStr: Converts a PByte from a DATA_BLOB       }
{                   to a Hex String so it can be saved in   }
{                   an RDP file                             }
{ P : PByte (pbData) from DATA_BLOB                         }
{ I : Integer (cbData) from DATA_BLOB                       }
{***********************************************************}
function BlobDataToHexStr(P: PByte; I: Integer): string;
var HexStr: string;
begin
  HexStr := '';
  while (I > 0) do begin
    Dec(I);
    HexStr := HexStr + IntToHex(P^, 2);
    Inc(P);
  end;
  Result := HexStr;
end;

{***********************************************************}
{ CryptRDPPassword: Converts a plaintext password to        }
{                   encrypted password hash                 }
{                   an RDP file                             }
{ sPassword: plaintext password                             }
{***********************************************************}
function CryptRDPPassword(sPassword: string): string;
var DataIn: DATA_BLOB;
    DataOut: DATA_BLOB;
    pwDescription: PWideChar;
    PwdHash: string;
begin
  PwdHash := '';

  DataOut.cbData := 0;
  DataOut.pbData := nil;

  // RDP uses UniCode
  DataIn.pbData := Pointer(WideString(sPassword));
  DataIn.cbData := Length(sPassword) * SizeOf(WChar);

  // RDP always sets description to psw
  pwDescription := WideString('psw');

  if CryptProtectData(@DataIn,
                      pwDescription,
                      nil,
                      nil,
                      nil,
                      CRYPTPROTECT_UI_FORBIDDEN,  // Never show interface
                      @DataOut) then
  begin
    PwdHash := BlobDataToHexStr(DataOut.pbData, DataOut.cbData);
  end;
  Result := PwdHash;

  // Cleanup
  LocalFree(Cardinal(DataOut.pbData));
  LocalFree(Cardinal(DataIn.pbData));

end;

{***********************************************************}
{ DecryptRDPPassword: Converts an RDP Password Hash back    }
{                     to it's original password.            }
{                     Note that this only works for the user}
{                     who encrypted the password (or on the }
{                     same computer in case it was encrypted}
{                     with the computerkey                  }
{ sPasswordHash: Password hash (string)                     }
{***********************************************************}

{$ifndef FPC}
function DecryptRDPPassword(sPasswordHash: string): string;
var DataIn: DATA_BLOB;
    DataOut: DATA_BLOB;
    sPassword: string;
    pwDecrypted: PWideChar;
    pwDescription: PWideChar;
begin

  DataIn := PasswordHashToBlobData(sPasswordHash);

  DataOut.cbData := 0;
  DataOut.pbData := nil;

  if CryptUnprotectData(@DataIn,
                        @pwDescription,
                        nil,
                        nil,
                        nil,
                        CRYPTPROTECT_UI_FORBIDDEN,  // Never show interface
                        @DataOut) then
  begin
    Getmem(pwDecrypted, DataOut.cbData);
    lstrcpynW(pwDecrypted, PWideChar(DataOut.pbData), (DataOut.cbData DIV 2) + 1);
    sPassword := pwDecrypted;
    FreeMem(pwDecrypted);
  end
  else
  begin
    raise EConvertError.CreateFmt('Error decrypting: %s',[SysErrorMessage(GetLastError)]);
  end;

  Result := sPassword;

  // Cleanup
  if DataOut.cbData > 0 then
  begin
    LocalFree(Cardinal(DataOut.pbData));
  end;
end;

{$endif}


end.
