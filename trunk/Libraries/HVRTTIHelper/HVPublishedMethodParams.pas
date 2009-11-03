unit HVPublishedMethodParams;
 
interface

uses Classes, SysUtils, TypInfo, HVVMT, HVMethodSignature;

function SkipPackedShortString(Value: PShortstring): pointer;

function GetMethodSignature(Event: PPropInfo): TMethodSignature;        

function FindEventProperty(Instance: TObject; Code: Pointer): PPropInfo;

function FindEventFor(Instance: TObject; Code: Pointer): PPropInfo;

function FindPublishedMethodSignature(Instance: TObject; Code: Pointer; var MethodSignature: TMethodSignature): boolean; 

function PublishedMethodToString(Instance: TObject; Method: PPublishedMethod): string;

procedure GetPublishedMethodsWithParameters(Instance: TObject; List: TStrings);

implementation

function SkipPackedShortString(Value: PShortstring): pointer;
begin
  Result := Value;
  Inc(PChar(Result), SizeOf(Value^[0]) + Length(Value^));
end;  

function PackedShortString(Value: PShortstring; var NextField{: Pointer}): PShortString; overload;
begin
  Result := Value;
  PShortString(NextField) := Value;
  Inc(PChar(NextField), SizeOf(Result^[0]) + Length(Result^));
end;  

function PackedShortString(var NextField{: Pointer}): PShortString; overload;
begin
  Result := PShortString(NextField);
  Inc(PChar(NextField), SizeOf(Result^[0]) + Length(Result^));
end;  

function GetMethodSignature(Event: PPropInfo): TMethodSignature;        
(* From TypInfo
  TTypeData = packed record
    case TTypeKind of
     ...
     tkMethod: (
        MethodKind: TMethodKind;
        ParamCount: Byte;
        Parameters: array[0..1023] of Char
       {Parameters: array[1..ParamCount] of
          record
            Flags: TParamFlags;
            ParamName: ShortString;
            TypeName: ShortString;
          end;
        ResultTypeName: ShortString);*)
type
  PParamListRecord = ^TParamListRecord;
  TParamListRecord = packed record 
    Flags: TParamFlags;
    ParamName: {packed} ShortString; // Really string[Length(ParamName)]
    TypeName:  {packed} ShortString; // Really string[Length(TypeName)]
  end;
var
  EventData: PTypeData;
  i: integer;
  MethodParam: PMethodParam;
  ParamListRecord: PParamListRecord;
begin
  Assert(Assigned(Event) and Assigned(Event.PropType));
  Assert(Event.PropType^.Kind = tkMethod);
  EventData := GetTypeData(Event.PropType^);
  Result.MethodKind := EventData.MethodKind;
  Result.ParamCount := EventData.ParamCount;
  SetLength(Result.Parameters, Result.ParamCount);
  ParamListRecord := @EventData.ParamList;
  for i := 0 to Result.ParamCount-1 do
  begin
    MethodParam := @Result.Parameters[i];
    MethodParam.Flags     := ParamListRecord.Flags;
    MethodParam.ParamName := PackedShortString(@ParamListRecord.ParamName, ParamListRecord)^;
    MethodParam.TypeName  := PackedShortString(ParamListRecord)^;
  end;  
  Result.ResultTypeName := PackedShortString(ParamListRecord)^;
end;  

function FindEventProperty(Instance: TObject; Code: Pointer): PPropInfo;
// Tries to find an event property that is assigned to a specific code address
var
  Count: integer;
  PropList: PPropList;
  i: integer;
  Method: TMethod;
begin
  Assert(Assigned(Instance));
  Count := GetPropList(Instance, PropList);
  if Count > 0 then
    try
      for i := 0 to Count-1 do
      begin
        Result := PropList^[i];
        if Result.PropType^.Kind = tkMethod then
        begin
          Method := GetMethodProp(Instance, Result);
          if Method.Code = Code then
            Exit;
        end;  
      end;  
    finally
      FreeMem(PropList);
    end;
  Result := nil;
end;  

function FindEventFor(Instance: TObject; Code: Pointer): PPropInfo;
// Tries to find an event property that is assigned to a specific code address
// In this instance or in one if its owned components (if the instance is a component)
var
  i: integer;
  Component: TComponent;
begin
  Result := FindEventProperty(Instance, Code);
  if Assigned(Result) then Exit;
  if Instance is TComponent then
  begin
    Component := TComponent(Instance);
    for i:= 0 to Component.ComponentCount-1 do
    begin
      Result := FindEventFor(Component.Components[i], Code);
      if Assigned(Result) then Exit;
    end;  
  end;
  Result := nil;
  // TODO: Check published fields system
end;  

function FindPublishedMethodSignature(Instance: TObject; Code: Pointer; var MethodSignature: TMethodSignature): boolean; 
var
  Event: PPropInfo;
begin
  Assert(Assigned(Code));
  Event := FindEventFor(Instance, Code);
  Result := Assigned(Event);
  if Result then
    MethodSignature := GetMethodSignature(Event);
end;  

function PublishedMethodToString(Instance: TObject; Method: PPublishedMethod): string;
var
  MethodSignature: TMethodSignature;
begin
  if FindPublishedMethodSignature(Instance, Method.Address, MethodSignature) then
    Result := MethodSignatureToString(Method.Name, MethodSignature)
  else 
    Result := Format('procedure %s(???);', [Method.Name]);  
end;  

procedure GetPublishedMethodsWithParameters(Instance: TObject; List: TStrings);
var
  i : integer;
  Method: PPublishedMethod;
  AClass: TClass;
  Count: integer;
begin
  List.BeginUpdate;
  try
    List.Clear;
    AClass := Instance.ClassType;
    while Assigned(AClass) do
    begin
      Count := GetPublishedMethodCount(AClass);
      if Count > 0 then
      begin
        List.Add(Format('Published methods in %s', [AClass.ClassName]));
        Method := GetFirstPublishedMethod(AClass);
        for i := 0 to Count-1 do
        begin
          List.Add(PublishedMethodToString(Instance, Method));
          Method := GetNextPublishedMethod(AClass, Method);
        end;
      end;  
      AClass := AClass.ClassParent;
    end;
  finally
    List.EndUpdate;
  end;
end;

end.

