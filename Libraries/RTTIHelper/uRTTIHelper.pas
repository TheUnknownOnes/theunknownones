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
  Variants;
  

function rttihGetPropertiesList(AInstance : TObject;
                                const AList : TStrings;
                                ARecursive : Boolean = false;
                                ATypeKinds : TTypeKinds = [];
                                ASkipExceptions : Boolean = true;
                                APrefix : String = '') : Integer; overload;
function rttihGetPropertiesList(AClassInfo : Pointer;
                                const AList : TStrings;
                                ARecursive : Boolean = false;
                                ATypeKinds : TTypeKinds = [];
                                ASkipExceptions : Boolean = true;
                                APrefix : String = '') : Integer; overload;
function rttihGetPropertiesList(AClass : TClass;
                                const AList : TStrings;
                                ARecursive : Boolean = false;
                                ATypeKinds : TTypeKinds = [];
                                ASkipExceptions : Boolean = true;
                                APrefix : String = '') : Integer; overload;
//Lists all properties in the format "PropertyName[.SubPropertyName]"
// if ATypeKinds = [] then all properties a returned


//==============================================================================


function rttihGetPropertyByName(AInstance : TObject;
                                APropertyName : String) : PPropInfo; overload;
function rttihGetPropertyByName(AClass : TClass;
                                APropertyName : String) : PPropInfo; overload;
function rttihGetPropertyByName(AClassInfo : Pointer;
                                APropertyName : String) : PPropInfo; overload;
//Returns the property if existing, otherwise nil
//Example: rttihGetPropertyByName(Memo1, 'Font.Name');


//==============================================================================


function rttihGetPropertyValue(AInstance : TObject;
                               APropertyName : String) : Variant;
//returns the current value of the property or null if something went wrong


implementation

function rttihGetPropertiesList(AInstance : TObject;
                                const AList : TStrings;
                                ARecursive : Boolean = false;
                                ATypeKinds : TTypeKinds = [];
                                ASkipExceptions : Boolean = true;
                                APrefix : String = '') : Integer;
begin
  Assert(Assigned(AInstance), 'Invalid object');
  Result := rttihGetPropertiesList(AInstance.ClassInfo, AList, ARecursive, ATypeKinds, ASkipExceptions);
end;

function rttihGetPropertiesList(AClass : TClass;
                                const AList : TStrings;
                                ARecursive : Boolean = false;
                                ATypeKinds : TTypeKinds = [];
                                ASkipExceptions : Boolean = true;
                                APrefix : String = '') : Integer; overload;
begin
  Assert(Assigned(AClass), 'Invalid class');
  Result := rttihGetPropertiesList(AClass.ClassInfo, AList, ARecursive, ATypeKinds, ASkipExceptions);
end;

function rttihGetPropertiesList(AClassInfo : Pointer;
                                const AList : TStrings;
                                ARecursive : Boolean = false;  
                                ATypeKinds : TTypeKinds = [];
                                ASkipExceptions : Boolean = true;
                                APrefix : String = '') : Integer;
var
  TypeData : PTypeData;
  PropInfoList : PPropList;
  idx : Integer;
begin
  Assert(Assigned(AClassInfo), 'Invalid classinfo');

  TypeData := GetTypeData(AClassInfo);

  GetMem(PropInfoList, TypeData.PropCount * SizeOf(PPropInfo));
  try
    GetPropInfos(AClassInfo, PropInfoList);

    for idx := 0 to TypeData.PropCount - 1 do
    begin
      try
        if (ATypeKinds = []) or (PropInfoList[idx].PropType^.Kind in ATypeKinds) then
          AList.Add(APrefix + PropInfoList[idx].Name);

        if ARecursive and (PropInfoList[idx].PropType^.Kind = tkClass) then
        begin
          rttihGetPropertiesList(PropInfoList[idx].PropType^,
                                 AList,
                                 ARecursive,
                                 ATypeKinds,
                                 ASkipExceptions,
                                 APrefix + PropInfoList[idx].Name + '.');
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
                                APropertyName : String) : PPropInfo;
begin
  Assert(Assigned(AInstance), 'Invalid object');
  Result := rttihGetPropertyByName(AInstance.ClassInfo, APropertyName);
end;

function rttihGetPropertyByName(AClass : TClass;
                                APropertyName : String) : PPropInfo; overload;
begin
  Assert(Assigned(AClass), 'Invalid class');
  Result := rttihGetPropertyByName(AClass.ClassInfo, APropertyName);
end;

function rttihGetPropertyByName(AClassInfo : Pointer;
                                APropertyName : String) : PPropInfo;
var
  PosOfPoint : Integer;
  PropName : String;
begin
  Result := nil;

  PosOfPoint := Pos('.', APropertyName);
  if PosOfPoint = 0 then
    PosOfPoint := Length(APropertyName) + 1;
  

  if PosOfPoint > 0 then
  begin
    PropName := Copy(APropertyName, 1, PosOfPoint - 1);
    Result := GetPropInfo(AClassInfo, PropName);

    if Assigned(Result) and
       (Result.PropType^.Kind = tkClass) and
       (PosOfPoint < Length(APropertyName)) then
    begin
      Result := rttihGetPropertyByName(AClassInfo, Copy(APropertyName, PosOfPoint + 1, Length(APropertyName)));
    end;

  end;
end;


//==============================================================================


function rttihGetPropertyValue(AInstance : TObject;
                               APropertyName : String) : Variant;
var
  PosOfPoint : Integer;
  PropName : String;
  PropInfo : PPropInfo;
begin
  Result := null;

  PosOfPoint := Pos('.', APropertyName);
  if PosOfPoint = 0 then
    PosOfPoint := Length(APropertyName) + 1;


  if PosOfPoint > 0 then
  begin
    PropName := Copy(APropertyName, 1, PosOfPoint - 1);
    Result := GetPropValue(AInstance, PropName, false);
    PropInfo := GetPropInfo(AInstance, PropName);

    if (not VarIsNull(Result)) and
       (PropInfo.PropType^.Kind = tkClass) and
       (PosOfPoint < Length(APropertyName)) then
    begin
      Result := rttihGetPropertyValue(TObject(Integer(Result)), Copy(APropertyName, PosOfPoint + 1, Length(APropertyName)));
    end

  end;
end;

end.
