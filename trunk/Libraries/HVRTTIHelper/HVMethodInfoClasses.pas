unit HVMethodInfoClasses;

interface

uses 
  TypInfo, 
  HVMethodSignature, 
  HVVMT;

type
  // Easy-to-use fixed size structure
  PClassInfo = ^TClassInfo;
  TClassInfo = record
    UnitName: string; 
    Name: string;
    ClassType: TClass;
    ParentClass: TClass;
    MethodCount: Word;
    Methods: array of TMethodSignature;  
  end;

procedure GetClassInfo(ClassTypeInfo: PTypeInfo; var ClassInfo: TClassInfo);
  
implementation

type
  // compiler implementation-specific structures, subject to change in future Delphi versions
  // Derived from declarations in ObjAuto.pas
  PReturnInfo = ^TReturnInfo;
  TReturnInfo = packed record
    Version: Byte; 
    CallingConvention: TCallConv;
    ReturnType: PPTypeInfo;
    ParamSize: Word;
  end;
  PParamInfo = ^TParamInfo;
  TParamInfo = packed record
    Flags: TParamFlags;
    ParamType: PPTypeInfo;
    Access: Word;
    Name: ShortString;
  end;
  
function ClassOfTypeInfo(P: PPTypeInfo): TClass;
begin
  Result := nil;
  if Assigned(P) and (P^.Kind = tkClass) then
    Result := GetTypeData(P^).ClassType;
end;  
  
procedure GetClassInfo(ClassTypeInfo: PTypeInfo; 
  var ClassInfo: TClassInfo);
// Converts from raw RTTI structures to user-friendly Info structures
var
  TypeData: PTypeData;
  i, j: integer;
  MethodInfo: PMethodSignature;
  PublishedMethod: PPublishedMethod;
  MethodParam: PMethodParam;
  ReturnRTTI: PReturnInfo;
  ParameterRTTI: PParamInfo;
  SignatureEnd: Pointer;
begin
  Assert(Assigned(ClassTypeInfo));
  Assert(ClassTypeInfo.Kind = tkClass);
  // Class
  TypeData  := GetTypeData(ClassTypeInfo);
  ClassInfo.UnitName        := TypeData.UnitName;
  ClassInfo.ClassType       := TypeData.ClassType;
  ClassInfo.Name            := TypeData.ClassType.ClassName;
  ClassInfo.ParentClass     := ClassOfTypeInfo(TypeData.ParentInfo);  
  ClassInfo.MethodCount     := GetPublishedMethodCount(ClassInfo.ClassType);
  SetLength(ClassInfo.Methods, ClassInfo.MethodCount);
  // Methods
  PublishedMethod := GetFirstPublishedMethod(ClassInfo.ClassType);
  for i := Low(ClassInfo.Methods) to High(ClassInfo.Methods) do
  begin
    // Method
    MethodInfo := @ClassInfo.Methods[i];
    MethodInfo.Name       := PublishedMethod.Name;
    MethodInfo.Address    := PublishedMethod.Address;
    MethodInfo.MethodKind := mkProcedure; // Assume procedure by default
    
    // Return info and calling convention
    ReturnRTTI := Skip(@PublishedMethod.Name);
    SignatureEnd := Pointer(Cardinal(PublishedMethod) 
      + PublishedMethod.Size);
    if Cardinal(ReturnRTTI) >= Cardinal(SignatureEnd) then
    begin
      MethodInfo.CallConv := ccReg; // Assume register calling convention 
      MethodInfo.HasSignatureRTTI := False;
    end
    else  
    begin
      MethodInfo.ResultTypeInfo := Dereference(ReturnRTTI.ReturnType);
      if Assigned(MethodInfo.ResultTypeInfo) then 
      begin
        MethodInfo.MethodKind := mkFunction;
        MethodInfo.ResultTypeName := MethodInfo.ResultTypeInfo.Name;
      end  
      else 
        MethodInfo.MethodKind := mkProcedure;
      MethodInfo.CallConv := ReturnRTTI.CallingConvention;
      MethodInfo.HasSignatureRTTI := True;
      // Count parameters
      ParameterRTTI := Pointer(Cardinal(ReturnRTTI) + SizeOf(ReturnRTTI^));
      MethodInfo.ParamCount := 0;
      while Cardinal(ParameterRTTI) < Cardinal(SignatureEnd) do
      begin
        Inc(MethodInfo.ParamCount); // Assume less than 255 parameters ;)!
        ParameterRTTI := Skip(@ParameterRTTI.Name);
      end;  
      // Read parameter info
      ParameterRTTI := Pointer(Cardinal(ReturnRTTI) + SizeOf(ReturnRTTI^));
      SetLength(MethodInfo.Parameters, MethodInfo.ParamCount);
      for j := Low(MethodInfo.Parameters) to High(MethodInfo.Parameters) do
      begin
        MethodParam := @MethodInfo.Parameters[j];
        MethodParam.Flags      := ParameterRTTI.Flags;
        if pfResult in MethodParam.Flags 
        then MethodParam.ParamName  := 'Result'
        else MethodParam.ParamName  := ParameterRTTI.Name;
        MethodParam.TypeInfo   := Dereference(ParameterRTTI.ParamType);
        if Assigned(MethodParam.TypeInfo) then
          MethodParam.TypeName := MethodParam.TypeInfo.Name;
        MethodParam.Location   := TParamLocation(ParameterRTTI.Access);
        ParameterRTTI := Skip(@ParameterRTTI.Name);
      end;  
    end;
    PublishedMethod := GetNextPublishedMethod(ClassInfo.ClassType, 
      PublishedMethod);
  end;  
end;  

end.


