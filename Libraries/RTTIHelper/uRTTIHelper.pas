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
  StrUtils;


function rttihGetPropertiesList(AInstance : TObject;
                                const AList : TWideStrings;
                                ARecursive : Boolean = false;
                                AIncludeTypeKinds : TTypeKinds = [];
                                AExcludeTypeKinds : TTypeKinds = [];
                                ASkipExceptions : Boolean = true;
                                APrefix : WideString = '';
                                ADelimiter : WideChar = '.';
                                AIgnoreClasses : TList = nil) : Integer; overload;
function rttihGetPropertiesList(AClassInfo : Pointer;
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
function rttihGetPropertyByName(AClassInfo : Pointer;
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


function rttihGetInheritancePath(AClassInfo : Pointer;
                                 ADelimiter : WideChar = '.') : WideString; overload;
function rttihGetInheritancePath(AClass : TClass;
                                 ADelimiter : WideChar = '.') : WideString; overload;
function rttihGetInheritancePath(AInstance : TObject;
                                 ADelimiter : WideChar = '.') : WideString; overload;


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

function rttihGetPropertiesList(AClassInfo : Pointer;
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
  Assert(Assigned(AClassInfo), 'Invalid classinfo');

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
          AList.Add(APrefix + PropInfoList[idx].Name);

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

  Result := AList.Count;
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

function rttihGetPropertyByName(AClassInfo : Pointer;
                                APropertyName : WideString;
                                ADelimiter : WideChar = '.') : PPropInfo;
var
  PosOfDelim : Integer;
  PropName : WideString;
begin
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

    if (not VarIsNull(PropValue)) and
       (PropInfo.PropType^.Kind = tkClass) and
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
      SetPropValue(AInstance, PropInfo, AValue);
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

function rttihGetInheritancePath(AClassInfo : Pointer;
                                 ADelimiter : WideChar = '.') : WideString;
var
  TypeData : PTypeData;
begin
  Assert(Assigned(AClassInfo), 'Invalid classinfo');

  repeat
    Result := PTypeInfo(AClassInfo).Name + ADelimiter + Result;

    TypeData := GetTypeData(AClassInfo);
    AClassInfo := Typedata.ParentInfo;
    if Assigned(AClassInfo) then
      AClassInfo := PPTypeInfo(AClassInfo)^;
  until not Assigned(AClassInfo);

  Result := LeftStr(Result, Length(Result) - 1); //cut the last delimiter

end;

end.
