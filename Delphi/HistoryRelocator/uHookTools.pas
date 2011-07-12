unit uHookTools;

interface

function HookImport(ModuleHandle : Cardinal; ModuleName, ImportName: PChar; HookProc: Pointer; var DLLProc: Pointer): Boolean;

function UnhookImport(ModuleHandle : Cardinal; ModuleName, ImportName: PChar; HookProc: Pointer; var DLLProc: Pointer): Boolean;

implementation

uses
  Classes,
  Windows,
  Sysutils;

type
LONG = DWORD;
PImageDOSHeader = ^TImageDosHeader;
TImageDOSHeader = packed record // DOS .EXE header
e_magic: WORD; // Magic number
e_cblp: WORD; // Bytes on last page of file
e_cp: WORD; // Pages in file
e_crlc: WORD; // Relocations
e_cparhdr: WORD; // Size of header in paragraphs
e_minalloc: WORD; // Minimum extra paragraphs needed
e_maxalloc: WORD; // Maximum extra paragraphs needed
e_ss: WORD; // Initial (relative) SS value
e_sp: WORD; // Initial SP value
e_csum: WORD; // Checksum
e_ip: WORD; // Initial IP value
e_cs: WORD; // Initial (relative) CS value
e_lfarlc: WORD; // File address of relocation table
e_ovno: WORD; // Overlay number
e_res: array[0..4-1] of WORD; // Reserved words
e_oemid: WORD; // OEM identifier (for e_oeminfo)
e_oeminfo: WORD; // OEM information: e_oemid specific
e_res2: array[0..10-1] of WORD; // Reserved words
e_lfanew: LONG; // File address of new exe header
end;

type
PImageImportByName = ^TImageImportByName;
TImageImportByName = packed record
Hint : WORD;
Name : array[0..255] of Char;
end;

PImageThunkData = ^TImageThunkData;
TImageThunkData = packed record
case integer of
0 : (ForwarderString: PBYTE);
1 : (FunctionPtr : PDWORD);
2 : (Ordinal : DWORD);
3 : (AddressOfData : PImageImportByName);
end;

type
PImageImportDescriptor = ^TImageImportDescriptor;
TImageImportDescriptor = packed record
HintNameTableOffset: DWORD; // RVA to original unbound IAT, aka Characteristics
// 0 for terminating null import descriptor
// PImageThunkData
TimeDateStamp : DWORD; // 0 if not bound,
// -1 if bound, and real date\time stamp
// in IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT (new BIND)
// O.W. date/time stamp of DLL bound to (Old BIND)
ForwarderChain : DWORD; // -1 if no forwarders
NameOffset : DWORD; // RVA to name of the module we're importing from
IATOffset : DWORD; // RVA to IAT (if bound this IAT has actual addresses)
// PImageThunkData
end;

type
PWin95CallThunk = ^TWin95CallThunk;
TWin95CallThunk = packed record
PUSH: byte; // PUSH instruction opcode (=$68)
Addr: pointer; // The actual address of the DLL routine
JMP : byte; // JMP instruction opcode (=$E9)
Rel : Integer; // Relative displacement (a Kernel32 address)
end;

function IsWin95CallThunk(Thunk: PWin95CallThunk): boolean;
//------------------------------------------------------------------------------
begin
Result := (Thunk^.PUSH = $68) and (Thunk^.JMP = $E9);
end;

function GetImageNtHeader(Base: Pointer): PImageNtHeaders;
//------------------------------------------------------------------------------
var
DOSHeader: PImageDOSHeader;
begin
DOSHeader := PImageDOSHeader(Base);
if DOSHeader.e_magic <> IMAGE_DOS_SIGNATURE then
raise Exception.Create('Hook Error: Not a valid MZ-file!');
Result := PImageNtHeaders(DWORD(Base) + DOSHeader.e_lfanew);
if Result.Signature <> IMAGE_NT_SIGNATURE then
raise Exception.Create('Hook Error: Not a valid PE-file!');
end;

function ReplaceImport(Base: Pointer; ModuleName: PChar; FromProc, ToProc: Pointer): Boolean;
//------------------------------------------------------------------------------
var
NtHeader : PImageNtHeaders;
ImportDescriptor : PImageImportDescriptor;
ImportEntry : PImageThunkData;
CurrModuleName : PChar;
IsThunked : Boolean;
FromProcThunk : PWin95CallThunk;
ImportThunk : PWin95CallThunk;
FoundProc : Boolean;
begin
// Assume failure
Result := False;

// Cache some Win95-specific knowledge about the FromProc
// On Win95/98, the import entries and GetProcAddress of some system
// module routines point to PUSH [ActualAdress]/JMP [Rel] thunks,
// so we have to look into the code to see if the final target matches
// Convert the FromProc into the thunk code it /might/ point to
FromProcThunk := PWin95CallThunk(FromProc);
// Is it a valid Win95 thunk?
IsThunked := (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
IsWin95CallThunk(FromProcThunk);

// Get a pointer to the PE-header
NtHeader := GetImageNtHeader(Base);
// Get a pointer to the import descriptor table
ImportDescriptor := PImageImportDescriptor(DWORD(Base)+
NtHeader.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress);

// The table of import descriptors is marked with a null name offset
while ImportDescriptor^.NameOffset <> 0 do begin
// Calculate a pointer to this module name
CurrModuleName := PChar(Base) + ImportDescriptor^.NameOffset;
// Only search matching modules
//if (StrIComp(CurrModuleName, ModuleName) = 0) then begin
begin
// Calculate a pointer to the first import entry for this module
ImportEntry := PImageThunkData(DWORD(Base) + ImportDescriptor^.IATOffset);

// Loop until we have reached the end of the list
while ImportEntry^.FunctionPtr <> nil do begin
// Now we must determine if this import entry pointer
// is equivalent to the FromProc address

if IsThunked then begin
// Convert the ImportEntry into the thunk code it /might/ point to
ImportThunk := PWin95CallThunk(ImportEntry^.FunctionPtr);

// If the routine we're hooking points to a Win95 thunk,
// see if the final target matches
FoundProc := IsWin95CallThunk(ImportThunk) and
(ImportThunk^.Addr = FromProcThunk^.Addr)
end
else
// otherwise, only check for clean matches
FoundProc := (ImportEntry^.FunctionPtr = FromProc);

// If we found the correct import entry, patch it!
if FoundProc then begin
// Note that all import sections have Read/Write access by default,
// so there is no need to play around with WriteProcessMemory or
// VirtualProtect
ImportEntry^.FunctionPtr := ToProc;

// There could be more imports of the same routine, so just flag
// success and keep looking for more matches
Result := true;
end;

// Look at the next Import Entry for this module
Inc(ImportEntry);
end;
end;
// Look at the next Import Descriptor
Inc(ImportDescriptor);
end;
end;

function HookImport(ModuleHandle : Cardinal; ModuleName, ImportName: PChar; HookProc: Pointer; var DLLProc: Pointer): Boolean;
//------------------------------------------------------------------------------
begin
  Result := not Assigned(DLLProc);

  if Result then
  begin
    DllProc := Windows.GetProcAddress(Windows.GetModuleHandle(ModuleName), ImportName);
    Result := Assigned(DllProc) and
              ReplaceImport(Pointer(ModuleHandle), ModuleName, DllProc, HookProc);
    if not Result then
      DLLProc := nil;
  end;
end;

function UnhookImport(ModuleHandle : Cardinal; ModuleName, ImportName: PChar; HookProc: Pointer; var DLLProc: Pointer): Boolean;
//------------------------------------------------------------------------------
begin
  Result := Assigned(DllProc) and
  ReplaceImport(Pointer(ModuleHandle), ModuleName, HookProc, DllProc);
  if Result then
    DLLProc := nil;
end;


end.


