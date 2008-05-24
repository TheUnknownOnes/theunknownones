unit uSettingsRTTI;

interface

uses
  Classes,
  SysUtils,
  TypInfo,
  Variants,
  WideStrings;

type
  TsrPropertyName = WideString;

  TsrPropertyList = class(TWideStringList)
  protected
    FOwner : TObject;

    function GetPropertyValue(const AObject : TObject;
                              const AProperty : PPropInfo) : Variant;

    procedure AddPropertiesFromObject(const AObject : TObject;
              AParentPropertyName : TsrPropertyName); overload;
  public
    constructor Create(AOwner : TObject); virtual;

    procedure ReadPropertiesFromObject(const AObject : TObject;
                                      ADontAddNewOnes : Boolean = false); overload;
    function GetPropertyFromObject(AIndex : Integer; AObject : TObject) : PPropInfo;

    property Owner : TObject read FOwner;
  end;

implementation

uses uSettingsBase;

{ TsrPropertyList }

procedure TsrPropertyList.ReadPropertiesFromObject(const AObject: TObject;
                                                  ADontAddNewOnes : Boolean);
var
  idx,
  OldCount : Integer;
begin
  OldCount := Count;

  if not ADontAddNewOnes then  
    AddPropertiesFromObject(AObject, '');

  //Delete all, which doesnt match the object
  for idx := OldCount - 1 downto 0 do
  begin
    if not Assigned(GetPropertyFromObject(idx, AObject)) then
      Delete(idx);
  end;
end;

procedure TsrPropertyList.AddPropertiesFromObject(const AObject: TObject;
  AParentPropertyName: TsrPropertyName);
var
  ObjectClassInfo : PTypeInfo;
  TypeData : PTypeData;
  PropertyCount : Smallint;
  PropertyList : PPropList;
  PropertyValue : Variant;
  idxProperty : Integer;
  PropertyName : TsrPropertyName;
begin
  if not Assigned(AObject) then
    exit;

  ObjectClassInfo := AObject.ClassInfo;

  if not Assigned(ObjectClassInfo)  then
    exit;

  TypeData := GetTypeData(ObjectClassInfo);
  PropertyCount := TypeData^.PropCount;

  if PropertyCount > 0 then
  begin
    GetMem(PropertyList, SizeOf(PPropInfo) * PropertyCount);
    try
      GetPropInfos(ObjectClassInfo, PropertyList);

      for idxProperty := 0 to PropertyCount - 1 do
      begin

        PropertyValue := GetPropertyValue(AObject, PropertyList[idxProperty]);

        if VarIsEmpty(PropertyValue) or
           ((PropertyList^[idxProperty].PropType^.Kind = tkClass) and
            (VarIsNull(PropertyValue))) or
           (PropertyList^[idxProperty].PropType^.Kind = tkMethod) then
           //Todo: Check for readonly properties
        begin
          Continue;
        end;

        
        PropertyName := AParentPropertyName + PropertyList^[idxProperty].Name;

        if PropertyList[idxProperty]^.PropType^.Kind = tkClass then
          AddPropertiesFromObject(TObject(Integer(PropertyValue)),
                                  PropertyName + SettingsPathDelimiter)
        else
        if SettingsCheckValueType(PropertyValue, false) then
          Add(PropertyName);
      end;

    finally
      FreeMem(PropertyList, SizeOf(PPropInfo) * PropertyCount);
    end;
  end;
end;

constructor TsrPropertyList.Create(AOwner : TObject);
begin
  FOwner := AOwner;
  
  Sorted := true;
  Duplicates := dupIgnore;
end;

function TsrPropertyList.GetPropertyFromObject(AIndex: Integer;
  AObject: TObject): PPropInfo;
var
  Path : TWideStringList;
  idx : Integer;
  tempInfo : PPropInfo;
  PropValue : Variant;
begin
  Result := nil;

  Path := TWideStringList.Create;
  try
    SettingsSplitPath(Strings[AIndex], Path);
    if Path.Count > 0 then
    begin
      for idx := 0 to Path.Count - 1 do
      begin
        tempInfo := GetPropInfo(AObject, Path[idx]);

        PropValue := GetPropertyValue(AObject, tempInfo);

        if Assigned(tempInfo) then
        begin
          if (tempInfo^.PropType^.Kind = tkClass) and
             (PropValue <> 0) then
          begin
            AObject := TObject(Integer(PropValue));
          end
          else
            Result := tempInfo; //last property found
        end
        else
          break;
      end;
    end;
  finally
    Path.Free;
  end;
end;

function TsrPropertyList.GetPropertyValue(const AObject: TObject;
  const AProperty: PPropInfo): Variant;
begin
  VarClear(Result);
  try
    Result := GetPropValue(AObject, AProperty);
  except
    //not the fine way ... but who knows what the components are doing? :)
    //if somethings goes wrong, Result should be empty (VarIsEmpty)
  end;
end;

end.
