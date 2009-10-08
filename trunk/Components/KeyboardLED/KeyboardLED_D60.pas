//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit KeyboardLED_D60;

{$R 'hwint.res'}

interface

type
  TKeyboardLEDState = (klsScrollLock, klsNUMLock, klsCAPSLock);
  TKeyboardLEDStates= set of TKeyboardLEDState;

function SetKeyboardLED(aKeyLEDState : TKeyboardLEDStates): Byte;
function ResetKeyboardLED: Byte;

implementation

uses Windows, WinSVC, SysUtils, Dialogs;

//function Inp32(wAddr:word):byte; stdcall; //external 'inpout32.dll';
//function Out32(wAddr:word;bOut:byte):byte; stdcall; // external 'inpout32.dll';

const
  KEYSTATUS = $64;
  KEYDATA   = $60;
  LEDUPDATE = $ed;
  OB_FULL   = 1;
  IB_FULL   = 2;
  KEY_ACK   = $fa;

// bit masks to be sent */
  SCROLLOCK = 1;
  NUMLOCK   = 2;
  CAPLOCK   = 4;

  METHOD_BUFFERED = 0;
  FILE_ANY_ACCESS = 0;

var
  sa      : TSecurityAttributes;
  SysVer  : Integer;
  hdriver : THandle;  

function SystemVersion():Byte;
var
  osvi : TOSVersionInfo;
  bOsVersionInfoEx : Boolean;
begin
  ZeroMemory(@osvi, sizeOf(TOSVersionInfo));
  osvi.dwOSVersionInfoSize:=sizeOf(TOSVersionInfo);

  bOsVersionInfoEx:=GetVersionEx(osvi);

  if not bOsVersionInfoEx then
  begin
    result:=0;
  end;

  case osvi.dwPlatformId of
    VER_PLATFORM_WIN32_NT : result:=2;
    VER_PLATFORM_WIN32_WINDOWS : result:=1;
  else
    result:=0;
  end;
end;

function LastErrorMsgStr: String; 
var 
  szerror: array [0..255] of Char; 
begin 
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, GetLastError(), 0, szError, 
    sizeof(szError), nil); 
  writeln(String(szError)); 
end;


function PortIn(IOport:word):byte; assembler;
asm
  mov dx,ax
  in al,dx
end;


function PortInW(IOport:word):word; assembler;
asm
  mov dx,ax
  in ax,dx
end;

procedure PortOut(IOport:word; Value:byte); assembler;
asm
  xchg ax,dx
  out dx,al
end;

procedure PortOutW(IOport:word; Value:word); assembler;
asm
  xchg ax,dx
  out dx,ax
end;


function CTL_CODE(DeviceType: integer; func: integer; meth: integer; access: integer): DWORD;
Begin
  result := (DeviceType shl 16) or (Access shl 14) or (func shl 2) or (meth);
end;

function Inp32(wAddr:word):byte; stdcall;
var
  error:boolean;
  BytesReturned:DWORD;
  Buffer:array[0..2]of byte;
  pBuffer:pword;
  retval : byte;
begin
  result:=0;
  case sysver of
    1 : begin
          retval:=PortIn(wAddr);
          result:=retval;
        end;
    2 : begin
          pBuffer := pword(@Buffer[0]);
          pBuffer^ := wAddr;
          Buffer[2]:=0;

          error := DeviceIoControl(hdriver,
                            cardinal(CTL_CODE(40000, $801, METHOD_BUFFERED, FILE_ANY_ACCESS)),
                            @Buffer,
                            2,
                            @Buffer,
                            1,
                            BytesReturned,
                            nil);
          result:=Buffer[0];
        end;
  end;
end;

procedure Out32(wAddr:word;bOut:byte);
var
  Error : LongBool;
  BytesReturned : Cardinal;
  Buffer : Array [0..2] of byte;
  pBuffer: pWord;
  overlapped: POVERLAPPED;
begin
  case sysver of
    1 : PortOut(wAddr, bOut);
    2 : begin
          overlapped:=Nil;
          pBuffer:=pWord(@Buffer[0]);
          pBuffer^:=wAddr;
          Buffer[2]:=bout;

        	error := DeviceIoControl(hdriver,
                            Cardinal(CTL_CODE(40000, $802, METHOD_BUFFERED, FILE_ANY_ACCESS)),
                            @Buffer,
                            3,
                            NIL,
                            0,
                            BytesReturned,
							              overlapped);
        end;
	end;
end;



function start():Integer;
type
  TNewStartService=function (hService: SC_HANDLE; dwNumServiceArgs: DWORD;
    lpServiceArgVectors: PPChar): BOOL; stdcall;
var
  Mgr : SC_HANDLE;
  Ser : SC_HANDLE;
begin
  Mgr := OpenSCManager (NIL, NIL,SC_MANAGER_ALL_ACCESS);
  result:=1;
  if (Mgr = 0) then
  begin
    //No permission to create service
    if (GetLastError() = ERROR_ACCESS_DENIED) then
    begin
			Mgr := OpenSCManager (NIL, NIL,GENERIC_READ);
			Ser := OpenService(Mgr,'hwinterface',GENERIC_EXECUTE);
			if (Ser <> 0) then
      begin
        // we have permission to start the service
				if (not TNewStartService(@StartService) (Ser,0,nil)) then
				begin
						CloseServiceHandle (Ser);
						result:=4; // we could open the service but unable to start
        end;
      end;
    end;
  end
  else
	begin
  	// Successfuly opened Service Manager with full access

		Ser := OpenService(Mgr,'hwinterface',GENERIC_EXECUTE);
    if (Ser<>0) then
    begin
      if(not TNewStartService(@StartService) (Ser,0,nil)) then
      begin
        LastErrorMsgStr;

        CloseServiceHandle (Ser);
				result:=3; // opened the Service handle with full access permission, but unable to start
      end
      else
			begin
      	CloseServiceHandle (Ser);
				result:=0;
      end;
    end;
  end;    
end;

function inst():Integer;
var
  Mgr: THandle;
  Ser: THandle;

  hResource : THandle;
  binGlob   : THandle;

  BinData   : Pointer;
  myFile    : THandle;

  Size : DWord;
   Written : Cardinal;
   overlapped : POverlapped;

  path : array[0..255] of char;
  fNam : String;
begin
	GetSystemDirectory(path , sizeof(path));
	hResource := FindResource(HInstance, 'HWINTERFACE', 'BIN');
	if(hResource<>0) then
  begin
		binGlob := LoadResource(HInstance, hResource);

		if(binGlob<>0) then
    begin
			binData := LockResource(binGlob);

			if(binData<>nil) then
      begin
        fnam:=String(path)+'\Drivers\hwinterface.sys';

				myfile := CreateFile(PChar(fNam),
								  GENERIC_WRITE,
								  0,
								  NIL,
								  CREATE_ALWAYS,
								  0,
								  0);

				if(myfile<>0) then
				begin
          overlapped:=NIL;
					size := SizeofResource(HInstance, hResource);
					WriteFile(myfile, binData, size, written, overlapped);
					CloseHandle(myfile);
        end;
      end;
    end;
  end;

	Mgr := OpenSCManager (NIL, NIL,SC_MANAGER_ALL_ACCESS);
  if (Mgr = 0) then
	begin
    //No permission to create service
    if (GetLastError() = ERROR_ACCESS_DENIED) then
      result:=5;  // error access denied
  end
  else
	begin
    Ser := CreateService (Mgr,
                         'hwinterface',
                         'hwinterface',
                         SERVICE_ALL_ACCESS,
                         SERVICE_KERNEL_DRIVER,
                         SERVICE_SYSTEM_START,
                         SERVICE_ERROR_NORMAL,
                         '\System32\Drivers\hwinterface.sys',
                         NIL,
                         NIL,
                         NIL,
                         NIL,
                         NIL
                         );
  end;

  LastErrorMsgStr;

  CloseServiceHandle(Ser);
  CloseServiceHandle(Mgr);

	result:=0;
end;

function Opendriver():Integer;
begin
  hdriver := CreateFile('\\.\hwinterface',
                       GENERIC_READ or GENERIC_WRITE,
                       0,
                       NIL,
                       OPEN_EXISTING,
                       FILE_ATTRIBUTE_NORMAL,
                       0);

	if(hdriver = INVALID_HANDLE_VALUE) then
  begin
		if(start()<>0) then
    begin
		 	inst();
			start();

		  hdriver := CreateFile('\\.\hwinterface',
                            GENERIC_READ or GENERIC_WRITE,
                            0,
                            NIL,
                            OPEN_EXISTING,
                            FILE_ATTRIBUTE_NORMAL,
                            0); 
		end;

    result:=1;
  end;
  result:=0;
end;

function sendKeyControl(aKeyToSend : Byte): smallint;
var
  count : smallint;
  err   : smallint;
  c     : Byte;
begin
  err:=1;

  for count:=0 to 2 do
  begin
    repeat
      c:=Inp32(KEYSTATUS);
    until (c and IB_FULL <> IB_FULL);

    Out32(KEYDATA, aKeyToSend);

    repeat
      c:=Inp32(KEYSTATUS);
    until (c and OB_FULL <> OB_FULL);

    c:=Inp32(KEYDATA);
    if c=KEY_ACK then
    begin
      err:=0;
      break;
    end;
  end;
  result:=err;
end;

function setKeyboardLED(aKeyLEDState : TKeyboardLEDStates): Byte;
var
  myBits: Byte;
begin
  result:=1;
  myBits:=0;

  if klsScrollLock in aKeyLEDState then
    myBits:=myBits or SCROLLOCK;

  if klsNUMLock in aKeyLEDState then
    myBits:=myBits or NumLock;

  if klsCAPSLock in aKeyLEDState then
    myBits:=myBits or CapLock;

  if sendKeyControl(LEDUPDATE)=0 then
    Result:=sendKeyControl(myBits);
end;

function ResetKeyboardLED: Byte;
var
  KS :  TKeyboardState;
  LS :  TKeyboardLEDStates;
begin
  GetKeyboardState(KS);
  if  (KS[VK_NUMLOCK] and 1 = 1) then
    LS:=LS+[klsNUMLock];
  if  (KS[VK_SCROLL] and 1 = 1) then
  begin
    LS:=LS+[klsScrollLock];
  end;
  if  (KS[VK_CAPITAL] and 1 = 1) then
  begin
    LS:=LS+[klsCAPSLock];
  end;
  SetKeyboardLED(LS);
end;

initialization
  sysVer:=SystemVersion;
  if SysVer=2 then
    Opendriver;

finalization
  if SysVer=2 then
    CloseHandle(hdriver);

end.
