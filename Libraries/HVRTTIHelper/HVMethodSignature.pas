unit HVMethodSignature;

interface

uses Classes, SysUtils, TypInfo, HVVMT;

type  
  TParamLocation = (plUnknown=-1, plEAX=0, plEDX=1, plECX=2, plStack1=3, plStackN=$FFFF);
  TCallConv = (ccReg, ccCdecl, ccPascal, ccStdCall, ccSafeCall);
  TParamFlag =  (pfVar, pfConst, pfArray, pfAddress, pfReference, pfOut, pfResult);
  TParamFlags = set of TParamFlag;
  PMethodParam = ^TMethodParam;
  TMethodParam = record
    Flags: TParamFlags;
    ParamName: string;
    TypeName: string;
    TypeInfo: PTypeInfo;
    Location: TParamLocation; 
  end;
  TMethodParamList = array of TMethodParam;
  PMethodSignature = ^TMethodSignature;
  TMethodSignature = record
    Name: string;
    MethodKind: TMethodKind;
    CallConv: TCallConv;
    HasSignatureRTTI: boolean;
    Address: Pointer;
    ParamCount: Byte;
    Parameters: TMethodParamList;
    ResultTypeName: string;
    ResultTypeInfo: PTypeInfo;
  end;  
  PPackedShortString = ^TPackedShortString;
  TPackedShortString = string[1];
  
function Skip(Value: PShortstring): pointer; overload;
function Skip(Value: PPackedShortString; var NextField{: Pointer}): PShortString; overload;
function Skip(CurrField: pointer; FieldSize: integer): pointer; overload;

function Dereference(P: PPTypeInfo): PTypeInfo;

function MethodKindString(MethodKind: TMethodKind): string;

function MethodParamString(const MethodParam: TMethodParam; ExcoticFlags: boolean = False): string;

function MethodParametesString(const MethodSignature: TMethodSignature; SkipSelf: boolean = True): string;

function MethodSignatureToString(const Name: string; const MethodSignature: TMethodSignature): string; overload;

function MethodSignatureToString(const MethodSignature: TMethodSignature): string; overload;

implementation

function Skip(Value: PShortstring): pointer; overload;
begin
  Result := Value;
  Inc(PChar(Result), SizeOf(Value^[0]) + Length(Value^));
end;  

function Skip(Value: PPackedShortString; var NextField{: Pointer}): PShortString; overload;
begin
  Result := PShortString(Value);
  Inc(PChar(NextField), SizeOf(Char) + Length(Result^) - SizeOf(TPackedShortString));
end;  

function Skip(CurrField: pointer; FieldSize: integer): pointer; overload;
begin
  Result := PChar(Currfield) + FieldSize;
end;

function Dereference(P: PPTypeInfo): PTypeInfo;
begin
  if Assigned(P) 
  then Result := P^
  else Result := nil;
end;  

function MethodKindString(MethodKind: TMethodKind): string;
begin
  case MethodKind of
    mkSafeProcedure, 
    mkProcedure     : Result := 'procedure';
    mkSafeFunction,
    mkFunction      : Result := 'function';
    mkConstructor   : Result := 'constructor';
    mkDestructor    : Result := 'destructor';
    mkClassProcedure: Result := 'class procedure'; 
    mkClassFunction : Result := 'class function';
  end;  
end;  

function MethodParamString(const MethodParam: TMethodParam; ExcoticFlags: boolean = False): string;
begin
       if pfVar       in MethodParam.Flags then Result := 'var '
  else if pfConst     in MethodParam.Flags then Result := 'const '
  else if pfOut       in MethodParam.Flags then Result := 'out '
  else                                          Result := '';
  if ExcoticFlags then
  begin
    if pfAddress   in MethodParam.Flags then Result := '{addr} ' + Result;
    if pfReference in MethodParam.Flags then Result := '{ref} ' + Result;
    if pfResult    in MethodParam.Flags then Result := '{result} ' + Result;
  end;  
  
  Result := Result + MethodParam.ParamName + ': ';
  if pfArray in MethodParam.Flags then 
    Result := Result + 'array of ';
  Result := Result + MethodParam.TypeName;
end;  

function MethodParametesString(const MethodSignature: TMethodSignature; SkipSelf: boolean = True): string;
var
  i: integer;
  MethodParam: PMethodParam;
  ParamIndex: integer;
begin
  Result := '';
  ParamIndex := 0;
  if MethodSignature.HasSignatureRTTI then
    for i := 0 to MethodSignature.ParamCount-1 do
    begin
      MethodParam := @MethodSignature.Parameters[i];
      // Skip the implicit Self parameter for class and interface methods
      // Note that Self is not included in event types 
      if SkipSelf and 
         (i = 0) and 
         (MethodParam.ParamName = 'Self') and 
         (MethodParam.TypeInfo.Kind in [tkInterface, tkClass]) then
        Continue;
      if pfResult in MethodParam.Flags then
        Continue;
      if ParamIndex > 0 then
        Result := Result + '; ';
      Result := Result + MethodParamString(MethodParam^);
      Inc(ParamIndex);
    end
  else
    Result := '{??}';  
end;  

function CallingConventionToString(CallConv: TCallConv): string;
begin
  case CallConv of
    ccReg     : Result := 'register';
    ccCdecl   : Result := 'cdecl';
    ccPascal  : Result := 'pascal';
    ccStdCall : Result := 'stdcall';
    ccSafeCall: Result := 'safecall';
    else        Result := 'TCallConv('+IntToStr(Ord(CallConv))+')';
  end;  
end;

function MethodSignatureToString(const Name: string; const MethodSignature: TMethodSignature): string; overload;
begin
  Result := Format('%s %s(%s)', 
    [MethodKindString(MethodSignature.MethodKind), 
     Name, 
     MethodParametesString(MethodSignature)]); 
  if MethodSignature.HasSignatureRTTI and (MethodSignature.MethodKind = mkFunction) then
    Result := Result + ': ' + MethodSignature.ResultTypeName;
  Result := Result + ';' ;    
  if MethodSignature.CallConv <> ccReg then
    Result := Result + ' ' + CallingConventionToString(MethodSignature.CallConv) + ';';
end;  

function MethodSignatureToString(const MethodSignature: TMethodSignature): string; overload;
begin
  Result := MethodSignatureToString(MethodSignature.Name, MethodSignature);
end;  

end.
