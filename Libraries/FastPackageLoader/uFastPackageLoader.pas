unit uFastPackageLoader;

interface

uses
 Windows, SysUtils, SysConst;

implementation

type
 TRedirectCode = packed record
   Code: packed record
     PushEBP: Byte; // $55
     PopEBP: Byte; // $5D
     Jump: Byte;
     Offset: Integer;
   end;
   // additional data
   RealProc: Pointer;
   Count: Integer;
 end;

function WriteProtectedMemory(BaseAddress, Buffer: Pointer; Size:
Cardinal; out WrittenBytes: Cardinal): Boolean;
var
 OldProt: Cardinal;
begin
 VirtualProtect(BaseAddress, Size, PAGE_EXECUTE_READWRITE, OldProt);
 Result := WriteProcessMemory(GetCurrentProcess, BaseAddress, Buffer,
Size, WrittenBytes);
 VirtualProtect(BaseAddress, Size, OldProt, nil);
 FlushInstructionCache(GetCurrentProcess, BaseAddress, WrittenBytes);
end;


function ReadProtectedMemory(BaseAddress, Buffer: Pointer; Size:
Cardinal;  out ReadBytes: Cardinal): Boolean;
begin
 Result := ReadProcessMemory(GetCurrentProcess, BaseAddress, Buffer,
Size, ReadBytes);
end;

procedure CodeRedirectEx(Proc: Pointer; NewProc: Pointer; out Data:
TRedirectCode);
type
 PPointer = ^Pointer;
 TRelocationRec = packed record
   Jump: Word;
   Address: PPointer;
 end;

var
 Code: TRedirectCode;
 Relocation: TRelocationRec;
 n: Cardinal;
begin
 if Proc = nil then
 begin
   Data.RealProc := nil;
   Exit;
 end;

 if Data.Count = 0 then // do not overwrite an already backuped code
 begin
   ReadProtectedMemory(Proc, @Data.Code, SizeOf(Data.Code), n);
   if (Data.Code.PushEBP = $FF) and (Data.Code.PopEBP = $25) then //Proc is in a dll/so or package
   begin
     ReadProtectedMemory(Proc, @Relocation, SizeOf(Relocation), n);
     Data.RealProc := Relocation.Address^;
     Proc := Data.RealProc;
     ReadProtectedMemory(Proc, @Data.Code, SizeOf(Data.Code), n);
   end
   else
     Data.RealProc := Proc;

   Code.Code.PushEBP := $55;
   Code.Code.PopEBP := $5D;
   Code.Code.Jump := $E9;
   Code.Code.Offset := Integer(NewProc) - Integer(Proc) - SizeOf(Data.Code);
   WriteProtectedMemory(Proc, @Code.Code, SizeOf(Data.Code), n);
 end;
 Inc(Data.Count);
end;

function CodeRedirect(Proc: Pointer; NewProc: Pointer): TRedirectCode;
begin
 Result.Count := 0;
 Result.RealProc := nil;
 CodeRedirectEx(Proc, NewProc, Result);
end;

procedure CodeRestore(var Data: TRedirectCode);
var
 n: Cardinal;
begin
 if (Data.RealProc <> nil) and (Data.Count = 1) then
   WriteProtectedMemory(Data.RealProc, @Data.Code, SizeOf(Data.Code), n);
 Dec(Data.Count);
end;

type
 pFastInitializePackage = ^TFastInitializePackage;
 TFastInitializePackage = procedure (Module: HMODULE; AValidatePackage: TValidatePackageProc);
var
 FFastInitializePackage: TRedirectCode;

procedure FastInitializePackage(Module: HMODULE; AValidatePackage: TValidatePackageProc);
type
  TPackageLoad = procedure;
var
  PackageLoad: TPackageLoad;
begin
  //CheckForDuplicateUnits(Module, AValidatePackage); <<--- this makes it slow
  @PackageLoad := GetProcAddress(Module, 'Initialize'); //Do not localize
  if Assigned(PackageLoad) then
    PackageLoad
  else
    raise EPackageError.CreateFmt(sInvalidPackageFile, [GetModuleName(Module)]);
end;

const
  oldProc : TFastInitializePackage = SysUtils.InitializePackage;

initialization
 FFastInitializePackage :=CodeRedirect(@oldProc, @FastInitializePackage);

finalization
 CodeRestore(FFastInitializePackage);

end.
