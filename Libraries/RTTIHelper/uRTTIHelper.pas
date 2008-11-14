{-----------------------------------------------------------------------------
 Project: RTTIHelper_D100R
 Purpose: Helper functions for working with RTTI 
 Created: 06.11.2008 09:02:38
 
 (c) by TheUnknownOnes
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uRTTIHelper;

interface

uses
  TypInfo,
  Classes,
  SysUtils,
  Variants,
  WideStrings,
  StrUtils,
  Math;


function rttihGetPropertiesList(AInstance : TObject;
                                const AList : TWideStrings;
                                ARecursive : Boolean = false;
                                AIncludeTypeKinds : TTypeKinds = [];
                                AExcludeTypeKinds : TTypeKinds = [];
                                ASkipExceptions : Boolean = true;
                                APrefix : WideString = '';
                                ADelimiter : WideChar = '.';
                                AIgnoreClasses : TList = nil) : Integer; overload;
function rttihGetPropertiesList(AClassInfo : PTypeInfo;
                                const AList : TWideStrings;
                                ARecursive : Boolean = false;
                                AIncludeTypeKinds : TTypeKinds = [];
                                AExcludeTypeKinds : TTypeKinds = [];
                                ASkipExceptions : Boolean = true;
                                APrefix : WideString = '';
                                ADelimiter : WideChar = '.';
                                AIgnoreClasses : TList = nil) : Integer; overload;
function rttihGetPropertiesList(AClass : TClass;
                                const AList : TWideStrings;
                                ARecursive : Boolean = false;
                                AIncludeTypeKinds : TTypeKinds = [];
                                AExcludeTypeKinds : TTypeKinds = [];
                                ASkipExceptions : Boolean = true;
                                APrefix : WideString = '';
                                ADelimiter : WideChar = '.';
                                AIgnoreClasses : TList = nil) : Integer; overload;
//Lists all properties in the format "PropertyName[.SubPropertyName]"
// if ATypeKinds = [] then all properties a returned
// AIncludeTypeKinds has the priority before AExcludeTypeKinds


//==============================================================================


function rttihGetPropertyByName(AInstance : TObject;
                                APropertyName : WideString;
                                ADelimiter : WideChar = '.') : PPropInfo; overload;
function rttihGetPropertyByName(AClass : TClass;
                                APropertyName : WideString;
                                ADelimiter : WideChar = '.') : PPropInfo; overload;
function rttihGetPropertyByName(AClassInfo : PTypeInfo;
                                APropertyName : WideString;
                                ADelimiter : WideChar = '.') : PPropInfo; overload;
//Returns the property if existing, otherwise nil
//Example: rttihGetPropertyByName(Memo1, 'Font.Name');


//==============================================================================


function rttihGetPropertyValue(AInstance : TObject;
                               APropertyName : WideString;
                               ADelimiter : WideChar = '.') : Variant;
//returns the current value of the property or null if something went wrong
//Example: v := rttihGetPropertyValue(Button1, 'Font.Name');


//==============================================================================


procedure rttihSetPropertyValue(AInstance : TObject;
                                APropertyName : WideString;
                                AValue : Variant;
                                ADelimiter : WideChar = '.');
//sets the value to the specified property
//Example: rttihSetPropertyValue(Button1, 'Font.Name', 'Webdings');


//==============================================================================


function rttihGetInheritancePath(AClassInfo : PTypeInfo;
                                 ADelimiter : WideChar = '.') : WideString; overload;
function rttihGetInheritancePath(AClass : TClass;
                                 ADelimiter : WideChar = '.') : WideString; overload;
function rttihGetInheritancePath(AInstance : TObject;
                                 ADelimiter : WideChar = '.') : WideString; overload;
//returns the inheritance path of a class
//Example: rttihGetInheritancePath(Button1) returns 'TObject.TPersistent.TComponent.TControl.TWinControl.TButtonControl.TButton'


//==============================================================================


function rttihGetUnit(AClassInfo : PTypeInfo) : WideString; overload;
function rttihGetUnit(AClass : TClass) : WideString; overload;
function rttihGetUnit(AInstance : TObject) : WideString; overload;
//returns the name of the unit where the class is defined


//==============================================================================


function rttihOrdinalToString(ATypeInfo : PTypeInfo; Value : Integer) : WideString;
function rttihSetToList(ATypeInfo : PTypeInfo; const AList : TWideStrings) : Integer;
function rttihEnumToList(ATypeInfo : PTypeInfo; const AList : TWideStrings) : Integer;



implementation

function rttihGetPropertiesList(AInstance : TObject;
                                const AList : TWideStrings;
                                ARecursive : Boolean = false;
                                AIncludeTypeKinds : TTypeKinds = [];
                                AExcludeTypeKinds : TTypeKinds = [];
                                ASkipExceptions : Boolean = true;
                                APrefix : WideString = '';
                                ADelimiter : WideChar = '.';
                                AIgnoreClasses : TList = nil) : Integer;
begin
  Assert(Assigned(AInstance), 'Invalid object');
  Result := rttihGetPropertiesList(AInstance.ClassInfo,
                                   AList,
                                   ARecursive,
                                   AIncludeTypeKinds,
                                   AExcludeTypeKinds,
                                   ASkipExceptions,
                                   APrefix,
                                   ADelimiter,
                                   AIgnoreClasses);
end;

function rttihGetPropertiesList(AClass : TClass;
                                const AList : TWideStrings;
                                ARecursive : Boolean = false;
                                AIncludeTypeKinds : TTypeKinds = [];
                                AExcludeTypeKinds : TTypeKinds = [];
                                ASkipExceptions : Boolean = true;
                                APrefix : WideString = '';
                                ADelimiter : WideChar = '.';
                                AIgnoreClasses : TList = nil) : Integer; overload;
begin
  Assert(Assigned(AClass), 'Invalid class');
  Result := rttihGetPropertiesList(AClass.ClassInfo,
                                   AList,
                                   ARecursive,
                                   AIncludeTypeKinds,
                                   AExcludeTypeKinds,
                                   ASkipExceptions,
                                   APrefix,
                                   ADelimiter,
                                   AIgnoreClasses);
end;

function rttihGetPropertiesList(AClassInfo : PTypeInfo;
                                const AList : TWideStrings;
                                ARecursive : Boolean = false;  
                                AIncludeTypeKinds : TTypeKinds = [];
                                AExcludeTypeKinds : TTypeKinds = [];
                                ASkipExceptions : Boolean = true;
                                APrefix : WideString = '';
                                ADelimiter : WideChar = '.';
                                AIgnoreClasses : TList = nil) : Integer;
var
  TypeData : PTypeData;
  PropInfoList : PPropList;
  idx : Integer;
  FreeIgnoreClasses : Boolean;
begin
  Assert(Assigned(AClassInfo) and (AClassinfo.Kind = tkClass), 'Invalid classinfo');

  Result := 0;

  TypeData := GetTypeData(AClassInfo);

  GetMem(PropInfoList, TypeData.PropCount * SizeOf(PPropInfo));
  try
    GetPropInfos(AClassInfo, PropInfoList);

    for idx := 0 to TypeData.PropCount - 1 do
    begin
      try
        if (AIncludeTypeKinds = []) and (AExcludeTypeKinds = []) or
           ((PropInfoList[idx].PropType^.Kind in AIncludeTypeKinds) and (AIncludeTypeKinds <> [])) or
           ((not (PropInfoList[idx].PropType^.Kind in AExcludeTypeKinds)) and (AExcludeTypeKinds <> [])) then
        begin
          AList.Add(APrefix + PropInfoList[idx].Name);

          Inc(Result);
        end;

        if ARecursive and (PropInfoList[idx].PropType^.Kind = tkClass) then
        begin
          if not Assigned(AIgnoreClasses) then
          begin
            AIgnoreClasses := TList.Create;
            FreeIgnoreClasses := true;
          end
          else
            FreeIgnoreClasses := false;

          try

            if AIgnoreClasses.IndexOf(AIgnoreClasses) = -1 then
            begin
              AIgnoreClasses.Add(AIgnoreClasses);

              rttihGetPropertiesList(PropInfoList[idx].PropType^,
                                     AList,
                                     ARecursive,
                                     AIncludeTypeKinds,
                                     AExcludeTypeKinds,
                                     ASkipExceptions,
                                     APrefix + PropInfoList[idx].Name + ADelimiter,
                                     ADelimiter,
                                     AIgnoreClasses);
            end;

          finally
            if FreeIgnoreClasses then
            begin
              AIgnoreClasses.Free;
              AIgnoreClasses := nil;
            end;
          end;
        end;
        
      except
        on E : Exception do
          if not ASkipExceptions then
            raise;
      end;    
    end;

  finally
    FreeMem(PropInfoList, TypeData.PropCount * SizeOf(PPropInfo));
  end;
end;


//==============================================================================


function rttihGetPropertyByName(AInstance : TObject;
                                APropertyName : WideString;
                                ADelimiter : WideChar = '.') : PPropInfo;
begin
  Assert(Assigned(AInstance), 'Invalid object');
  Result := rttihGetPropertyByName(AInstance.ClassInfo, APropertyName, ADelimiter);
end;

function rttihGetPropertyByName(AClass : TClass;
                                APropertyName : WideString;
                                ADelimiter : WideChar = '.') : PPropInfo; overload;
begin
  Assert(Assigned(AClass), 'Invalid class');
  Result := rttihGetPropertyByName(AClass.ClassInfo, APropertyName, ADelimiter);
end;

function rttihGetPropertyByName(AClassInfo : PTypeInfo;
                                APropertyName : WideString;
                                ADelimiter : WideChar = '.') : PPropInfo;
var
  PosOfDelim : Integer;
  PropName : WideString;
begin
  Assert(Assigned(AClassInfo) and (AClassinfo.Kind = tkClass), 'Invalid classinfo');

  Result := nil;

  PosOfDelim := Pos(ADelimiter, APropertyName);
  if PosOfDelim = 0 then
    PosOfDelim := Length(APropertyName) + 1;


  if PosOfDelim > 0 then
  begin
    PropName := Copy(APropertyName, 1, PosOfDelim - 1);
    Result := GetPropInfo(AClassInfo, PropName);

    if Assigned(Result) and
       (Result.PropType^.Kind = tkClass) and
       (PosOfDelim < Length(APropertyName)) then
    begin
      Result := rttihGetPropertyByName(Result.PropType^, Copy(APropertyName, PosOfDelim + 1, Length(APropertyName)));
    end;

  end;
end;


//==============================================================================


function rttihGetPropertyValue(AInstance : TObject;
                               APropertyName : WideString;
                               ADelimiter : WideChar = '.') : Variant;
var
  PosOfDelim : Integer;
  PropName : WideString;
  PropInfo : PPropInfo;
begin
  Result := null;

  PosOfDelim := Pos(ADelimiter, APropertyName);
  if PosOfDelim = 0 then
    PosOfDelim := Length(APropertyName) + 1;


  if PosOfDelim > 0 then
  begin
    PropName := Copy(APropertyName, 1, PosOfDelim - 1);
    Result := GetPropValue(AInstance, PropName, false);
    PropInfo := GetPropInfo(AInstance, PropName);

    if (not VarIsNull(Result)) and
       (PropInfo.PropType^.Kind = tkClass) and
       (PosOfDelim < Length(APropertyName)) then
    begin
      Result := rttihGetPropertyValue(TObject(Integer(Result)),
                                      Copy(APropertyName, PosOfDelim + 1, Length(APropertyName)));
    end

  end;
end;


//==============================================================================


procedure rttihSetPropertyValue(AInstance : TObject;
                                APropertyName : WideString;
                                AValue : Variant;
                                ADelimiter : WideChar = '.');
var
  PosOfDelim : Integer;
  PropName : WideString;
  PropInfo : PPropInfo;
  PropValue : Variant;
begin
  PosOfDelim := Pos(ADelimiter, APropertyName);
  if PosOfDelim = 0 then
    PosOfDelim := Length(APropertyName) + 1;


  if PosOfDelim > 0 then
  begin
    PropName := Copy(APropertyName, 1, PosOfDelim - 1);
    PropValue := GetPropValue(AInstance, PropName, false);
    PropInfo := GetPropInfo(AInstance, PropName);

    if (PropInfo.PropType^.Kind = tkClass) and
       (Integer(PropValue) <> 0) and
       (PosOfDelim < Length(APropertyName)) then
    begin
      rttihSetPropertyValue(TObject(Integer(PropValue)),
                            Copy(APropertyName,PosOfDelim + 1, Length(APropertyName)),
                            AValue,
                            ADelimiter);
    end
    else
    if PosOfDelim > Length(APropertyName) then
    begin
      case PropInfo.PropType^.Kind of
        tkClass: SetOrdProp(AInstance, PropInfo, AValue);
        else
          SetPropValue(AInstance, PropInfo, AValue);
      end;
    end;

  end;
end;


//==============================================================================


function rttihGetInheritancePath(AClass : TClass;
                                 ADelimiter : WideChar = '.') : WideString;
begin
  Result := rttihGetInheritancePath(AClass.ClassInfo,
                                    ADelimiter);
end;

function rttihGetInheritancePath(AInstance : TObject;
                                 ADelimiter : WideChar = '.') : WideString;
begin
  Result := rttihGetInheritancePath(AInstance.ClassInfo,
                                    ADelimiter);
end;

function rttihGetInheritancePath(AClassInfo : PTypeInfo;
                                 ADelimiter : WideChar = '.') : WideString;
var
  TypeData : PTypeData;
begin
  Assert(Assigned(AClassInfo) and (AClassinfo.Kind = tkClass), 'Invalid classinfo');

  repeat
    Result := PTypeInfo(AClassInfo).Name + ADelimiter + Result;

    TypeData := GetTypeData(AClassInfo);
    if Assigned(TypeData.ParentInfo) then
      AClassInfo := TypeData.ParentInfo^
    else
      AClassInfo := nil;
  until not Assigned(AClassInfo);

  Result := LeftStr(Result, Length(Result) - 1); //cut the last delimiter

end;


//==============================================================================


function rttihGetUnit(AClassInfo : PTypeInfo) : WideString;
var
  TypeData : PTypeData;
begin
  Assert(Assigned(AClassInfo) and (AClassinfo.Kind = tkClass), 'Invalid classinfo');

  TypeData := GetTypeData(AClassInfo);

  if Assigned(TypeData) then
    Result := TypeData.UnitName
  else
    Result := EmptyWideStr;
end;

function rttihGetUnit(AClass : TClass) : WideString;
begin
  Assert(Assigned(AClass), 'Invalid class');
  Result := rttihGetUnit(AClass.ClassInfo);
end;

function rttihGetUnit(AInstance : TObject) : WideString;
begin
  Assert(Assigned(AInstance), 'Invalid object');
  Result := rttihGetUnit(AInstance.ClassInfo);
end;


//==============================================================================


function rttihOrdinalToString(ATypeInfo : PTypeInfo; Value : Integer) : WideString;
const
  AsciiChars = [32..127];
begin
  Assert(Assigned(ATypeInfo) and
        (ATypeInfo.Kind in [tkInteger, tkInt64, tkChar, tkWChar, tkEnumeration]), 'Invalid TypeInfo');

  case ATypeInfo.Kind of
    tkInteger, tkInt64: Result := IntToStr(Value);
    tkChar, tkWChar:
    begin
      if Value in AsciiChars then
        Result := '''' + Chr(Value) + ''''
      else
        Result := WideFormat('#%d', [Value]);
    end;
    tkEnumeration:
    begin
      Result := GetEnumName(ATypeInfo, Value);
    end;

  end;
end;

function rttihSetToList(ATypeInfo : PTypeInfo; const AList : TWideStrings) : Integer;
var
  TypeInfoComp : PTypeInfo;
  TypeData,
  TypeDataComp : PTypeData;
  Element : 0..255; //May be adapted to future changes
begin
  Assert(Assigned(ATypeInfo) and (ATypeInfo.Kind = tkSet), 'Invalid TypeInfo');

  Result := 0;

  TypeData := GetTypeData(ATypeInfo);
  TypeInfoComp := TypeData.CompType^;
  TypeDataComp := GetTypeData(TypeInfoComp);

  for Element := TypeDataComp.MinValue to TypeDataComp.MaxValue do
  begin
    AList.AddObject(WideFormat('%s%s%d', [rttihOrdinalToString(TypeInfoComp, Element),
                                          AList.NameValueSeparator,
                                          Trunc(Power(2, Element))]),
                    TObject(Trunc(Power(2, Element))));
    Inc(Result);
  end;
end;

function rttihEnumToList(ATypeInfo : PTypeInfo; const AList : TWideStrings) : Integer;
var
  TypeData : PTypeData;
  Element : 0..255; //May be adapted to future changes
begin
  Assert(Assigned(ATypeInfo) and (ATypeInfo.Kind = tkEnumeration), 'Invalid TypeInfo');

  TypeData := GetTypeData(ATypeInfo);

  Result := 0;

  for Element := TypeData.MinValue to TypeData.MaxValue do
  begin
    AList.AddObject(WideFormat('%s%s%d', [rttihOrdinalToString(ATypeInfo, Element),
                                          AList.NameValueSeparator,
                                          Element]),
                   TObject(Element));
    Inc(Result);
  end;
    
end;

end.
