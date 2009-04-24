unit uSettingsRTTI;

interface

uses
  Classes,
  SysUtils,
  TypInfo,
  Variants,
  WideStrings,
  uRTTIHelper;

type
  TsrPropertyName = WideString;

  TsrPropertyList = class(TStringList)
  protected
    FOwner : TObject;

  public
    constructor Create(AOwner : TObject); virtual;

    procedure ReadPropertiesFromObject(const AObject : TObject;
                                      ADontAddNewOnes : Boolean = false); overload;

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
    rttihGetPropertiesList(AObject, Self, true, [], [tkUnknown, tkMethod], true, '', SettingsPathDelimiter);

  //Delete all, which doesnt match the object
  for idx := OldCount - 1 downto 0 do
  begin
    if not Assigned(rttihGetPropertyByName(AObject, Strings[idx], SettingsPathDelimiter)) then
      Delete(idx);
  end;
end;

constructor TsrPropertyList.Create(AOwner : TObject);
begin
  FOwner := AOwner;
  
  Sorted := true;
  Duplicates := dupIgnore;
end;


end.
