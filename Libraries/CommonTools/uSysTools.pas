//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uSysTools;

interface

uses
  Windows, Messages, Classes, SysUtils, StrUtils, RegExpr, DB, Graphics, Forms,
  Dialogs, ActiveX, ShlObj, ComObj, Variants, WideStrings, WideStrUtils;

{$REGION 'FileSearching'}

function GetRegExFileList(APath           : String;           //The path where we should search the files
                          APattern        : String = '.*';    //The RegEx pattern
                          ASubDirs        : Boolean = false;  //if true, the funtion will search inside the subdirs
                          AResultList     : TStrings = nil;   //if nil, only the function result is interesting
                          AAttributes     : Integer = 0;      //The required attributes for the files to find
                          AWithPath       : Boolean = false;  //Path\File or only File ?
                          APathPattern    : String = '.*'     //the pattern the path must match
                         )                : Integer;          //count of files found

procedure FindFileInPaths(const AFileName: String;            //Filename to be searched for
                          const ASearchPaths: String;         //semikolon separated list of search paths. Environment variables may be used
                          AMinimumFileSize: Int64;            //Minimum filesize to match.
                          const AResultList: TStrings);       //List that receives found files


{$ENDREGION}

{$REGION 'Window-Printing'}
procedure PrintWindow(Wnd: HWND; ATo: TBitmap);
{$ENDREGION}

{$REGION 'Fileoperations'}
  function GetTempFile(AExtension : String = '') : String;
  function DeleteFileToWasteBin(AFileName:string): boolean;
  function ExpandEnvVars(AInputString : String) : String;
  function FileSize(AFileName: String): Int64;
{$ENDREGION}

{$REGION 'Information'}
  function IsMultiprocessor : Boolean;
  function ProcessorCount : Cardinal;
  function IsAdmin : Boolean;
{$ENDREGION}

{$REGION 'StringOperations'}

function MultipleStringReplace(AString: String;
                               AOldPatterns,
                               ANewPatterns: array of String;
                               AFlags: TReplaceFlags
                              ): String;

function MakeFileName(ADesiredFName : String): String;

{$ENDREGION}

{$REGION 'Variant operations'}
function VarRecToVariant (AValue : TVarRec) : Variant;
function VariantToTypedVarRec(const Item: Variant; VarType: TVarType): TVarRec;
function VariantToVarRec(const Item: Variant): TVarRec;
procedure FinalizeVarRec(var Item: TVarRec);
{$ENDREGION}

{$REGION 'WideString operations'}
function GetWindowText(wnd: HWND): WideString;
procedure SetWindowText(wnd: HWND; txt: WideString);

function MultipleWideStringReplace(AString: WideString;
                               AOldPatterns,
                               ANewPatterns: array of WideString;
                               AFlags: TReplaceFlags
                              ): WideString;
{$ENDREGION}

{$REGION 'Key Checking'}
procedure AcceptNumericOnly(var Key: Char; Comma : Boolean);      //nur numerische Werte zulassen
{$ENDREGION}

{$REGION 'GUID Helper'}
function SameGUID(AGUID1, AGUID2 : TGUID) : Boolean;
{$ENDREGION}

{$REGION 'BIT-Tools'}
function IsBitSet(AValue : Integer; ABitIndex : Byte) : Boolean;
{$ENDREGION}

{$REGION 'Conversion'}
function ClientToScreen(AWindow: HWND; var APoint: TPoint) : Boolean;
{$ENDREGION}

{$REGION 'IDataObject'}
function GetDataObjectFromFileList(const Directory: string;
      Files: TStrings): IDataObject;
function GetDataObjectFromFile(const Directory: string;
      AFile: String): IDataObject;
function GetFileListFromDataObject(const DataObject : IDataObject; Files : TStrings): Integer;
{$ENDREGION}

{$REGION 'Keyboard'}
function KeyPressed(AKey : Smallint) : Boolean; overload;
function KeyPressed(AKey : Byte) : Boolean; overload;
function KeyToogled(AKey : Smallint) : Boolean; overload;
function KeyToogled(AKey : Byte) : Boolean; overload;
{$ENDREGION}

{$REGION 'SpecialFolder'}
function GetShellFolder(CSIDL: integer): string;
{$ENDREGION}

implementation

uses
  ShellAPI;

{$REGION 'FileSearching'}

function GetRegExFileList(APath           : String;
                          APattern        : String = '.*';
                          ASubDirs        : Boolean = false;
                          AResultList     : TStrings = nil;
                          AAttributes     : Integer = 0;
                          AWithPath       : Boolean = false;
                          APathPattern    : String = '.*'
                         )                : Integer;
var
  SR : TSearchRec;
  TempResult : TStrings;
  Reg : TRegExpr;
  LastError : Integer;

  function IsPath : Boolean;
  begin
    Result:=(SR.Attr and faDirectory)=faDirectory;
  end;

  procedure AddEntry;
  begin
    TempResult.AddObject(IfThen(AWithPath, APath)+sr.Name+IfThen(IsPath,'\'),
                         TObject(sr.Attr));
  end;
  
begin
  Result:=0; //by default, we found no file

  //If we have to parse subdirs, we have to find them
  if (AAttributes and faDirectory)=faDirectory then
    AAttributes:=AAttributes or faDirectory;

  //Init the temproray List for the result
  TempResult:=TStringList.Create;

  //We need also a RegExpr-Object
  Reg:=TRegExpr.Create;

  //In windows, we work caseinsensitive
  Reg.ModifierI:=true;

  //Mostly we are not greedy :)
  //Reg.ModifierG:=true;

  try
    //better to have a path delimiter
    APath:=IncludeTrailingPathDelimiter(APath);

    //Lets start searching
    LastError:=FindFirst(APath+'*.*',AAttributes,SR);

    if LastError=0 then
      //raise Exception.Create(SysErrorMessage(LastError))
   // else
    begin
      repeat
        //if the name is '.' or '..' we skip this
        if (sr.Name='.') or (sr.Name='..') then
          Continue;

        //Set the pattern
        Reg.Expression:=IfThen(IsPath, APathPattern, APattern);

        //if the name matches our pattern, we take it
        if Reg.Exec(sr.Name) then
        begin
          AddEntry;  //Add the item to the list

          if (IsPath) and (ASubDirs) then
          begin
            GetRegExFileList(APath+sr.Name+'\',
                             APattern,
                             ASubDirs,
                             TempResult,
                             AAttributes,
                             AWithPath,
                             APathPattern);
          end;
        end;

      until (FindNext(SR)<>0);
    end;

  //write the result to the supplied stringlist
  if Assigned(AResultList) then
    AResultList.AddStrings(TempResult);

  Result:=TempResult.Count;

  finally //Free all mem we used
    FindClose(SR);
    TempResult.Free;
    Reg.Free;
  end;
end;

procedure FindFileInPaths(const AFileName: String; const ASearchPaths: String; AMinimumFileSize: Int64; const AResultList: TStrings);
var
  sl : TStringList;
  FileName : String;
  idx: Integer;
begin
  Assert(Assigned(AResultList), 'Result list is not assigned!');
  Assert(AMinimumFileSize>=0, 'Minimum filesize must be larger or equal 0!');

  sl:=TStringList.Create;
  try
    sl.Delimiter:=';';
    sl.StrictDelimiter:=True;
    sl.DelimitedText:=ExpandEnvVars(ASearchPaths);

    for idx := 0 to sl.Count - 1 do
    begin
      FileName:=IncludeTrailingPathDelimiter(sl[idx])+AFileName;
      if (FileSize(FileName)>=AMinimumFileSize) then
      begin
        AResultList.Add(FileName);
        break;
      end;
    end;
  finally
    sl.Free;
  end;
end;

{$ENDREGION}

{$REGION 'Window-Printing'}
procedure PrintWindow(Wnd: HWND; ATo: TBitmap);
var
  hDCMem : HDC;
  DC     : HDC;
  rect   : TRect;
  bmp    : HBITMAP;
  hOld   : HGDIOBJ;
begin
  hDCMem:=CreateCompatibleDC(0);

  GetWindowRect(Wnd, rect);

  bmp:=0;

  DC:= GetDC(Wnd);
  bmp:= CreateCompatibleBitmap(DC, rect.Right-rect.Left, rect.Bottom-rect.Top);
  ReleaseDC(Wnd, DC);

  hOld:=SelectObject(hDCMem, bmp);
  SendMessage(wnd, wm_Print, hDCMem, PRF_CHILDREN or PRF_CLIENT or PRF_ERASEBKGND or PRF_NONCLIENT or PRF_OWNED);

  selectObject(hDCMem, hold);
  DeleteObject(hDCMem);

  ATo.FreeImage;
  ATo.Handle:=bmp;
end;
{$ENDREGION}

{$REGION 'Fileoperations'}
function GetTempFile(AExtension : String = '') : String;
var
  TempPath,
  TempFile :  array[0..MAX_PATH-1] of char;
  TempPathString,
  TempFileString : String;
begin
  if GetTempPath(MAX_PATH,TempPath)>0 then
    TempPathString:=StrPas(TempPath)
  else
    TempPathString:=GetEnvironmentVariable('TEMP');

  TempPathString:=IncludeTrailingPathDelimiter(TempPathString);

  if Windows.GetTempFileName(PChar(TempPathString),nil,0,TempFile)>0 then
    TempFileString:=TempFile
  else
    TempFileString:=TempFileString+IntToStr(Random(MaxInt-1))+'.tmp';
                                                                                               
  if AExtension[1]<>'.' then
    AExtension:='.'+AExtension;

  if AExtension<>'' then
    Result:=ChangeFileExt(TempFileString,AExtension)
  else
    Result:=TempFileString;

  if FileExists(Result) then
    DeleteFile(Result);
end;


function DeleteFileToWasteBin(AFileName:string): boolean;
var Struct: TSHFileOpStruct;
    pFromc: array[0..255] of char;
    Resultval: integer;
begin
   if not FileExists(AFileName) then begin
      Result := False;
      exit;
   end
   else begin
      fillchar(pfromc,sizeof(pfromc),0) ;
      StrPcopy(pfromc,expandfilename(AFileName)+#0#0) ;
      Struct.wnd := 0;
      Struct.wFunc := FO_DELETE;
      Struct.pFrom := pFromC;
      Struct.pTo := nil;
      Struct.fFlags:= FOF_ALLOWUNDO or FOF_NOCONFIRMATION
         or FOF_SILENT;
      Struct.fAnyOperationsAborted := false;
      Struct.hNameMappings := nil;
      Resultval := ShFileOperation(Struct) ;
      Result := (Resultval = 0) ;
   end;
end;

function ExpandEnvVars(AInputString : String) : String;
var
  idx : Integer;
  Reg : TRegExpr;
begin
  Result:=AInputString;

  Reg:=TRegExpr.Create;
  Reg.InputString:=AInputString;
  Reg.ModifierI:=true;
  Reg.Expression:='%(.+)%';

  if Reg.Exec then
  begin
    for idx:=1 to Reg.SubExprMatchCount do
    begin
      Result:=StringReplace(Result,'%'+Reg.Match[idx]+'%',
                            GetEnvironmentVariable(Reg.Match[idx]),
                            [rfReplaceAll, rfIgnoreCase]);
    end;
  end;

  Reg.Free;
end;

function FileSize(AFileName : String) : Int64;
var
  sr : TSearchRec;
begin
  if FindFirst(AFileName, faAnyFile, sr ) = 0 then
     result := Int64(sr.FindData.nFileSizeHigh) shl Int64(32) +  Int64(sr.FindData.nFileSizeLow)
  else
     result := -1;

  FindClose(sr) ;
end;

{$ENDREGION}

{$REGION 'Information'}

function IsMultiprocessor : Boolean;
begin
  Result:=ProcessorCount>1;
end;

function ProcessorCount : Cardinal;
var
  SysInfo : Tsysteminfo;
begin
  GetSystemInfo(SysInfo);
  Result:=SysInfo.dwNumberOfProcessors;
end;


// Thanks to Thomas Stutz
// http://www.swissdelphicenter.ch/de/showcode.php?id=189
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority =
    (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;

function IsAdmin: Boolean;
var
  hAccessToken: THandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: DWORD;
  psidAdministrators: PSID;
  x: Integer;
  bSuccess: BOOL;
begin
  Result   := False;
  bSuccess := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True,
    hAccessToken);
  if not bSuccess then
  begin
    if GetLastError = ERROR_NO_TOKEN then
      bSuccess := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY,
        hAccessToken);
  end;
  if bSuccess then
  begin
    GetMem(ptgGroups, 1024);
    bSuccess := GetTokenInformation(hAccessToken, TokenGroups,
      ptgGroups, 1024, dwInfoBufferSize);
    CloseHandle(hAccessToken);
    if bSuccess then
    begin
      AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
        SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS,
        0, 0, 0, 0, 0, 0, psidAdministrators);
      {$R-}
      for x := 0 to ptgGroups.GroupCount - 1 do
        if EqualSid(psidAdministrators, ptgGroups.Groups[x].Sid) then
        begin
          Result := True;
          Break;
        end;
      {$R+}
      FreeSid(psidAdministrators);
    end;
    FreeMem(ptgGroups);
  end;
end;

{$ENDREGION}

{$REGION 'StringOperations'}

function MultipleStringReplace(AString: String;
                               AOldPatterns,
                               ANewPatterns: array of String;
                               AFlags: TReplaceFlags
                              ): String;
var
  idx : Integer;
begin
  Result:=AString;

  if Length(AOldPatterns)<>Length(ANewPatterns) then
    raise Exception.Create('Patternlengths not equal');

  for idx:=Low(AOldPatterns) to High(AOldPatterns) do
    Result:=StringReplace(Result,AOldPatterns[idx],ANewPatterns[idx],AFlags);
end;

function MakeFileName(ADesiredFName : String): String;
const
  NotAllowed   : Array [0..8] of String = ('\','/','*','?','"','<','>','|',':');
  ToBeReplacedWith : Array [0..8] of String = ('_','_',' ',' ','''',' ',' ','I','.');
begin
  Result:=MultipleStringReplace(ADesiredFName,NotAllowed,ToBeReplacedWith,[rfReplaceAll]);
end;

{$ENDREGION}

{$REGION 'Variant operations'}

function VarRecToVariant (AValue : TVarRec) : Variant;
begin
  case AValue.VType of
    vtInteger:
      Result:=AValue.VInteger;
    vtBoolean:
      Result:=AValue.VBoolean;
    vtChar:
      Result:=AValue.VChar;
    vtExtended:
      Result:=AValue.VExtended^;
    vtString:
      Result:=AValue.VString^;
    vtPointer:
      Result:=Integer(AValue.VPointer);
    vtPChar:
      Result:=StrPas(AValue.VPChar);
    vtAnsiString:
      Result:=String(AValue.VAnsiString);
    vtCurrency:
      Result:=AValue.VCurrency^;
    vtVariant:
      Result:=AValue.VVariant^;
    vtInt64:
      Result :=  AValue.VInt64^;
    {$IFDEF UNICODE}
    vtUnicodeString:
      Result := String(PChar(AValue.VUnicodeString));
    {$ENDIF}
    else
    raise Exception.Create ('invalid data type ' + IntToStr(AValue.VType));
  end;
end;

function VariantToTypedVarRec(const Item: Variant; VarType: TVarType): TVarRec;
var
  W: WideString;
begin
  case VarType of
    varInteger, varSmallint, varShortInt, varByte, varWord, varLongWord:
      begin
        Result.VType:=vtInteger;
        Result.VInteger:=Item;
      end;
    varNull, varUnknown, varEmpty:
      begin
        Result.VType:=vtInteger;
        Result.VInteger:=0;
      end;
    varBoolean:
      begin
        Result.VType:=vtBoolean;
        Result.VBoolean:=Item;
      end;
    varDouble, varSingle:
      begin
        Result.VType:=vtExtended;
        New(Result.VExtended);
        Result.VExtended^ := Item;
      end;
    varString:
      begin
        Result.VType:=vtString;
        New(Result.VString);
        Result.VString^ := ShortString(Item);
      end;
    varCurrency:
      begin
        Result.VType:=vtCurrency;
        New(Result.VCurrency);
        Result.VCurrency^ := Item;
      end;
    varVariant:
      begin
        Result.VType:=vtVariant;
        New(Result.VVariant);
        Result.VVariant^ := Item;
      end;
    varOleStr:
      begin
        Result.VType:=vtWideString;
        Result.VWideString := nil;
        WideString(Result.VWideString) := WideString(Item);
      end;
    varInt64:
      begin
        Result.VType:=vtInt64;
        New(Result.VInt64);
        Result.VInt64^ := Item;
      end;
    {$IFDEF UNICODE}
    varUString:
      begin
        Result.VType:=vtUnicodeString;
        Result.VUnicodeString:=nil;
        UnicodeString(Result.VUnicodeString) := UnicodeString(Item);
      end;
    {$ENDIF}
  end;
end;

function VariantToVarRec(const Item: Variant): TVarRec;
begin
  Result:=VariantToTypedVarRec(Item, TVarData(Item).VType);
end;

procedure FinalizeVarRec(var Item: TVarRec);
begin
  case Item.VType of
    vtExtended: Dispose(Item.VExtended);
    vtString: Dispose(Item.VString);
    vtPChar: StrDispose(Item.VPChar);
    vtPWideChar: FreeMem(Item.VPWideChar);
    vtAnsiString: AnsiString(Item.VAnsiString) := '';
    vtCurrency: Dispose(Item.VCurrency);
    vtVariant: Dispose(Item.VVariant);
    vtInterface: IInterface(Item.VInterface) := nil;
    vtWideString: WideString(Item.VWideString) := '';
    vtInt64: Dispose(Item.VInt64);
    {$IFDEF UNICODE}
    vtUnicodeString: UnicodeString(Item.VUnicodeString) := '';
    {$ENDIF}
  end;
  Item.VInteger := 0;
end;

{$ENDREGION}

{$REGION 'WideString operations'}
function GetWindowText(wnd: HWND): WideString;
var
  TextLength: Integer;
  Text: PWideChar;
begin 
  Result := '';
  if wnd = 0 then 
    Exit; 
  TextLength := SendMessageW(wnd, WM_GETTEXTLENGTH, 0, 0);
  if TextLength <> 0 then
  begin 
    GetMem(Text, TextLength * 2  + 1); 
    SendMessageW(wnd, WM_GETTEXT, TextLength + 1, Integer(Text)); 
    Result := Text; 
    FreeMem(Text); 
  end; 
end;

procedure SetWindowText(wnd: HWND; txt: WideString);
begin
  SendMessage(wnd, WM_SETTEXT, 0, lParam(PWideChar(WideString(txt))));
end;

function MultipleWideStringReplace(AString: WideString;
                               AOldPatterns,
                               ANewPatterns: array of WideString;
                               AFlags: TReplaceFlags
                              ): WideString;
var
  idx : Integer;
begin
  Result:=AString;

  if Length(AOldPatterns)<>Length(ANewPatterns) then
    raise Exception.Create('Patternlengths not equal');

  for idx:=Low(AOldPatterns) to High(AOldPatterns) do
    Result:=WideStringReplace(Result,AOldPatterns[idx],ANewPatterns[idx],AFlags);
end;

{$ENDREGION}

{$REGION 'Key Checking'}
// Prozedur "AcceptNumericOnly" kann bei dem Ereignis OnKeyPress eines Editfeldes aufgerufen werden
//      Comma - wenn dieser Wert True ist kann ein Komma eingegeben werden '.' wird in ',' umgewandelt
procedure AcceptNumericOnly(var Key: Char; Comma : Boolean);
var
  myValue : integer;
begin
  if (not TryStrToInt(Char(Key),myValue)) and (key>=#$21) and (key<=#$FF) then
    if (Comma) and ((Key<>',') or (Key<>'.')) then
      key:=','
    else
      key:=#0;
end;
{$ENDREGION}

{$REGION 'GUID Helper'}
function SameGUID(AGUID1, AGUID2 : TGUID) : Boolean;
begin
  Result:=(AGUID1.D1=AGUID2.D1) and
          (AGUID1.D2=AGUID2.D2) and
          (AGUID1.D3=AGUID2.D3) and
          (AGUID1.D4[0]=AGUID2.D4[0]) and
          (AGUID1.D4[1]=AGUID2.D4[1]) and
          (AGUID1.D4[2]=AGUID2.D4[2]) and
          (AGUID1.D4[3]=AGUID2.D4[3]) and
          (AGUID1.D4[4]=AGUID2.D4[4]) and
          (AGUID1.D4[5]=AGUID2.D4[5]) and
          (AGUID1.D4[6]=AGUID2.D4[6]) and
          (AGUID1.D4[7]=AGUID2.D4[7]);
end;
{$ENDREGION}

{$REGION 'BIT-Tools'}
function IsBitSet(AValue : Integer; ABitIndex : Byte) : Boolean;
begin
  Result:=(AValue and (1 shl ABitIndex))=(1 shl ABitIndex);
end;
{$ENDREGION}

{$REGION 'Conversion'}
function ClientToScreen(AWindow: HWND; var APoint: TPoint) : Boolean;
var
  ParentWindow : HWND;
  ParentRect : TRect;
begin
  GetWindowRect(AWindow, ParentRect);
  APoint.X:=APoint.X+ParentRect.Left;
  APoint.Y:=APoint.Y+ParentRect.Top;
end;
{$ENDREGION}

{$REGION 'IDataObject'}
function GetDataObjectFromFileList(const Directory: string; Files: TStrings): IDataObject;
type
  PArrayOfPItemIDList = ^TArrayOfPItemIDList;
  TArrayOfPItemIDList = array[0..0] of PItemIDList;
var
  Malloc: IMalloc;
  Root: IShellFolder;
  FolderPidl: PItemIDList;
  Folder: IShellFolder;
  p: PArrayOfPItemIDList;
  chEaten: ULONG;
  dwAttributes: ULONG;
  FileCount: Integer;
  i: Integer;
begin
  {$R-}
  dwAttributes := 0;
  Result := nil;
  if Files.Count = 0 then
    Exit;
  OleCheck(SHGetMalloc(Malloc));
  OleCheck(SHGetDesktopFolder(Root));
  OleCheck(Root.ParseDisplayName(0, nil,
    PWideChar(WideString(Directory)),
    chEaten, FolderPidl, dwAttributes));
  try
    OleCheck(Root.BindToObject(FolderPidl, nil, IShellFolder,
      Pointer(Folder)));
    FileCount := Files.Count;
    p := AllocMem(SizeOf(PItemIDList) * FileCount);
    try
      for i := 0 to FileCount - 1 do
      begin
        OleCheck(Folder.ParseDisplayName(0, nil,
          PWideChar(WideString(Files[i])), chEaten, p^[i],
          dwAttributes));
      end;
      OleCheck(Folder.GetUIObjectOf(0, FileCount, p^[0], IDataObject,
        nil,
        Pointer(Result)));
    finally
      for i := 0 to FileCount - 1 do begin
        if p^[i] <> nil then Malloc.Free(p^[i]);
      end;
      FreeMem(p);
    end;
  finally
    Malloc.Free(FolderPidl);
  end;
end;


function GetDataObjectFromFile(const Directory: string; AFile: String): IDataObject;
var
  sl : TStrings;
begin
  sl:=TStringList.Create;
  sl.Text:=AFile;
  Result:=GetDataObjectFromFileList(Directory, sl);
  sl.Free;
end;

function GetFileListFromDataObject(const DataObject : IDataObject; Files : TStrings): Integer;
var
  DragH: Integer;
  Medium: TStgMedium;
  Name: string;
  I, Count, Len: Integer;
  FileDropFormatEtc : TFormatEtc;
begin
  Result := 0;

  with FileDropFormatEtc do
  begin
    cfFormat := CF_HDROP;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := 0;
    tymed := TYMED_HGLOBAL;
  end;

  if DataObject.GetData(FileDropFormatEtc, Medium) = S_OK then
    try
      try
        DragH := Integer(GlobalLock(Medium.hGlobal));
        try
          Count := DragQueryFile(DragH, Cardinal(-1), nil, 0);
          if Files <> nil then
            for I := 0 to Count - 1 do
            begin
              Len := DragQueryFile(DragH, I, nil, 0);
              if Len > 0 then
              begin
                SetLength(Name, Len + 1);
                DragQueryFile(DragH, I, PChar(Name), Len + 1);
                SetLength(Name, Len);
                Files.Add(Name);
              end;
            end;
          Result := Count;
        finally
          GlobalUnlock(Medium.hGlobal);
        end;
      finally
        ReleaseStgMedium(Medium);
      end;
    except
      Result := 0;
    end;
end;

{$ENDREGION}

{$REGION 'Keyboard'}
function KeyPressed(AKey : Smallint) : Boolean; overload;
begin
  Result:=(AKey shr ((SizeOf(AKey)*8)-1))=1;
end;

function KeyPressed(AKey : Byte) : Boolean; overload;
begin
  Result:=(AKey shr ((SizeOf(AKey)*8)-1))=1;
end;

function KeyToogled(AKey : Smallint) : Boolean; overload;
begin
  Result:=(AKey shl ((SizeOf(AKey)*8)-1))=1;
end;

function KeyToogled(AKey : Byte) : Boolean; overload;
begin
  Result:=(AKey shl ((SizeOf(AKey)*8)-1))=1;
end;
{$ENDREGION}

{$REGION 'SpecialFolder'}
function GetShellFolder(CSIDL: integer): string;
//thx to Michael (http://www.michael-puff.de/Artikel/HOMEDIR.php)
var
  pidl                   : PItemIdList;
  FolderPath             : string;
  SystemFolder           : Integer;
  Malloc                 : IMalloc;
begin
  Malloc := nil;
  FolderPath := '';
  SHGetMalloc(Malloc);
  if Malloc = nil then
  begin
    Result := FolderPath;
    Exit;
  end;
  try
    SystemFolder := CSIDL;
    if SUCCEEDED(SHGetSpecialFolderLocation(0, SystemFolder, pidl)) then
    begin
      SetLength(FolderPath, max_path);
      if SHGetPathFromIDList(pidl, PChar(FolderPath)) then
      begin
        SetLength(FolderPath, length(PChar(FolderPath)));
      end;
    end;
    Result := FolderPath;
  finally
    Malloc.Free(pidl);
  end;
end;
{$ENDREGION}

end.
