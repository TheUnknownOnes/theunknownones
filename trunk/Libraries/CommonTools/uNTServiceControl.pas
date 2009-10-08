{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 * Unit Name : uNTServiceControl
 * Autor     : Daniel Wischnewski
 * Copyright : Copyright © 2002 by gate(n)etwork. All Right Reserved.
 * Urheber   : Daniel Wischnewski
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit uNTServiceControl;

interface

uses
	Windows, Classes;

function ServiceGetStatus(sMachine, sService: AnsiString): DWord;
// starts a service
function ServiceStart(aMachine, aServiceName: AnsiString; ProcessAppMsg: Boolean = False): Boolean;
// stops a service
function ServiceStop(aMachine, aServiceName: AnsiString; ProcessAppMsg: Boolean = False): Boolean;
// enumartes all services a secific service depends on
procedure ServiceEnumDependent(aMachine, aServiceName: AnsiString; Dependend: TStrings);
// restarts a service with respect to all services that depend on it
function ServiceRestart(aMachine, aServiceName: AnsiString): Boolean;

function WinExit(iFlags: Integer): Boolean;

implementation

uses
	SysUtils, WinSvc, Forms;

function ServiceGetStatus(sMachine, sService: AnsiString): DWord;
var
	srvManager,srvHandle: SC_Handle;
	srvStatus: TServiceStatus;
	hStat: DWord;
begin
	hStat := 1;
	srvManager := OpenSCManager(PChar(sMachine), nil, SC_MANAGER_CONNECT);
	if srvManager > 0 then
	begin
		srvHandle := OpenService(srvManager, PChar(sService), SERVICE_QUERY_STATUS);
		if srvHandle > 0 then
		begin
			if QueryServiceStatus(srvHandle, srvStatus) then
				hStat := srvStatus.dwCurrentState;
			CloseServiceHandle(srvHandle);
		end;
		CloseServiceHandle(srvManager);
	end;
	Result := hStat;
end;

function ServiceStart(
  aMachine, aServiceName: AnsiString; ProcessAppMsg: Boolean = False
): Boolean;
var
	srvManager, srvHandle: SC_Handle;
	srvStatus: TServiceStatus;
	Temp: PChar;
	Start: TDateTime;
begin
	srvStatus.dwCurrentState := 1;
	srvManager := OpenSCManager(PChar(aMachine), nil, SC_MANAGER_CONNECT);
	if srvManager > 0 then
	begin
		srvHandle := OpenService(
      srvManager, PChar(aServiceName), SERVICE_START or SERVICE_QUERY_STATUS
    );
		if srvHandle > 0 then
		begin
			Temp := nil;
			if StartService(srvHandle, 0, Temp) then
				if QueryServiceStatus(srvHandle, srvStatus) then
				begin
					Start := Now;
					while SERVICE_RUNNING <> srvStatus.dwCurrentState do
					begin
						Sleep(125);
            if ProcessAppMsg then
              Application.ProcessMessages;
						if Start + (1 / 24 / 60 * 2) < Now then
							Break;
						QueryServiceStatus(srvHandle, srvStatus);
					end;
				end;
			CloseServiceHandle(srvHandle);
		end;
		CloseServiceHandle(srvManager);
	end;
	Result := SERVICE_RUNNING = srvStatus.dwCurrentState;
end;

function ServiceStop(
  aMachine, aServiceName: AnsiString; ProcessAppMsg: Boolean = False
): Boolean;
var
	srvManager, srvHandle: SC_Handle;
	srvStatus: TServiceStatus;
	Start: TDateTime;
  Err : array[0..1024] of char;
begin
	srvManager := OpenSCManager(PChar(aMachine), nil, SC_MANAGER_CONNECT);
	if srvManager > 0 then
	begin
		srvHandle := OpenService(
      srvManager, PChar(aServiceName), SERVICE_STOP or SERVICE_QUERY_STATUS
    );
		if srvHandle > 0 then
		begin
			if ControlService(srvHandle, SERVICE_CONTROL_STOP, srvStatus) then
			begin
				if QueryServiceStatus(srvHandle, srvStatus) then
				begin
					Start := Now;
					while SERVICE_STOPPED <> srvStatus.dwCurrentState do
					begin
						Sleep(125);
            if ProcessAppMsg then
              Application.ProcessMessages;
						if Start + (1 / 24 / 60 * 2) < Now then
							Break;
						QueryServiceStatus(srvHandle, srvStatus);
					end;
				end;
			end;
			CloseServiceHandle(srvHandle);
		end;
		CloseServiceHandle(srvManager);
	end;
	Result := SERVICE_STOPPED = srvStatus.dwCurrentState;
end;

procedure ServiceEnumDependent(
  aMachine, aServiceName: AnsiString; Dependend: TStrings
);
var
	srvManager, srvHandle: SC_Handle;
	srvStatus: TServiceStatus;
	I, dwCheckPoint, Running: DWord;
	Chars: PChar;
	Services: array[0..255] of ENUM_SERVICE_STATUSA;
begin
	srvStatus.dwCurrentState := 1;
	srvManager := OpenSCManager(PChar(aMachine), nil, SC_MANAGER_CONNECT);
	if srvManager > 0 then
	begin
		srvHandle := OpenService(
      srvManager, PChar(aServiceName), SERVICE_ALL_ACCESS or
      SERVICE_ENUMERATE_DEPENDENTS or SERVICE_QUERY_STATUS
    );
		if srvHandle > 0 then
		begin
			FillChar(Chars, SizeOf(Chars), 0);
			EnumDependentServices(
        srvHandle, SERVICE_ACTIVE, Services[0],
        256 * SizeOf(ENUM_SERVICE_STATUSA), dwCheckPoint, Running
      );
			if Running > 0 then
				for I := 0 to Running - 1 do
					Dependend.Add(Services[I].lpServiceName);
		end;
	end;
end;

function ServiceRestart(aMachine, aServiceName: AnsiString): Boolean;
var
	I: Integer;
	List: TStringList;
begin
	Result := False;
	try
		List := TStringList.Create;
		try
			ServiceEnumDependent(aMachine, aServiceName, List);
			for I := 0 to List.Count - 1 do
				if not ServiceStop(aMachine, List.Strings[I]) then
					Exit;
			ServiceStop(aMachine, aServiceName);
			ServiceStart(aMachine, aServiceName);
			for I := List.Count - 1 downto 0 do
				if not ServiceStart(aMachine, List.Strings[I]) then
					Exit;
			Result := True;
		finally
			List.Free;
		end;
	except
	end;
end;

function SetPrivilege(aPrivilegeName : string; aEnabled : boolean ): boolean;
var
	TPPrev, TP: TTokenPrivileges;
	Token: Cardinal;
	dwRetLen: DWord;
begin
	Result := False;
	OpenProcessToken(
    GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, Token
  );
	TP.PrivilegeCount := 1;
	if (LookupPrivilegeValue(nil, PChar(aPrivilegeName), TP.Privileges[0].LUID))
	then
  begin
		if aEnabled then
			TP.Privileges[0].Attributes:= SE_PRIVILEGE_ENABLED
		else
			TP.Privileges[0].Attributes:= 0;
		dwRetLen := 0;
		Result :=
      AdjustTokenPrivileges(Token, False,TP, SizeOf(TPPrev), TPPrev, dwRetLen);
	end;
	CloseHandle(Token);
end;

function WinExit(iFlags: Integer): Boolean;
begin
	Result := True;
	if( SetPrivilege('SeShutdownPrivilege', True)) then
	begin
		if not ExitWindowsEx(iFlags, 0) then
			Result := False;
		SetPrivilege('SeShutdownPrivilege', False)
	end else begin
		Result := False;
	end;
end;

end.
