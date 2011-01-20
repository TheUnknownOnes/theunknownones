unit uJSHelper;

interface

uses
  Classes,
  TypInfo,
  SysUtils,
  StrUtils,
  Variants,
  uRTTIHelper;

type
  {$M+}
  TjsonObject = class
  public
    function ToJSON : String; virtual;
  end;
  {$M-}

  TjsonObjectList = class;

  TjsonObjectListEnumerator = class
  private
    FIndex: Integer;
    FList: TjsonObjectList;
  public
    constructor Create(AList: TjsonObjectList);
    function GetCurrent: TjsonObject;
    function MoveNext: Boolean;
    property Current: TjsonObject read GetCurrent;
  end;

  TjsonObjectList = class(TjsonObject)
  private
    function GetItem(AIndex: Integer): TjsonObject;
    procedure SetItem(AIndex: Integer; const Value: TjsonObject);
    function GetCount: Integer;
  protected
    FItems : TList;
  public
    constructor Create();
    destructor Destroy(); override;

    function ToJSON : String; override;

    function Add(AObject : TjsonObject) : Integer;
    procedure Clear;
    procedure Delete(AIndex : Integer); overload;
    procedure Delete(AObject : TjsonObject); overload;
    function Extract(AIndex : Integer) : TjsonObject; overload;
    function Extract(AObject : TjsonObject) : TjsonObject; overload;
    procedure Insert(AIndex: Integer; AObject : TjsonObject);
    function First : TjsonObject;
    function Last : TjsonObject;
    procedure Move(ACurIndex, ANewIndex: Integer);
    function Remove(AObject : TjsonObject): Integer;
    procedure Assign(AListA: TjsonObjectList; AOperator: TListAssignOp = laCopy; AListB: TjsonObjectList = nil);

    function GetEnumerator: TjsonObjectListEnumerator;

    property Count: Integer read GetCount;
    property Items[AIndex : Integer] : TjsonObject read GetItem write SetItem; default;
  end;

function ToJSString(AString : String) : String;
function ToJSCode(AValue : Variant) : String;

implementation

uses uJSDirect;

function ToJSString(AString : String) : String;
begin
  AString := StringReplace(AString, #13, '\r', [rfReplaceAll]);
  AString := StringReplace(AString, #10, '\n', [rfReplaceAll]);
  AString := StringReplace(AString, '"', '\"', [rfReplaceAll]);

  Result := '"' + AString + '"';
end;

function ToJSCode(AValue : Variant) : String;
begin
  case VarType(AValue) of
    varInteger, varSmallint, varShortInt, varByte, varWord, varLongWord, varInt64:
      Result := IntToStr(AValue);
    varNull, varUnknown, varEmpty:
      Result := 'null';
    varBoolean:
      Result := IfThen(AValue, 'true', 'false');
    varDouble, varSingle, varCurrency:
      Result := StringReplace(FloatToStr(AValue), ',', '.', [rfReplaceAll]);
    varString, varOleStr:
      Result := ToJSString(AValue);
    {$IFDEF UNICODE}
    varUString:
      Result := ToJSString(AValue);
    {$ENDIF}
    else
      Result := ToJSString(VarToStrDef(AValue, ''));
  end;
end;


{ TjsonObject }

function TjsonObject.ToJSON: String;
var
  Props : TStringList;
  idx : Integer;
  o : TObject;
begin
  Props := TStringList.Create;
  Result := '{';

  rttihGetPropertiesList(Self, Props, false);

  for idx := 0 to Props.Count - 1 do
  begin
    if idx > 0 then
      Result := Result + ',';

    case PPropInfo(Props.Objects[idx])^.PropType^.Kind of
      tkClass:
      begin
        o := TObject(Integer(rttihGetPropertyValue(Self, Props[idx])));

        if Assigned(o) then
        begin
          if o is TJSONObject then
            Result := Result + Props[idx] + ':' + TJSONObject(o).ToJSON
          else
          if o is TjsdElement then
            Result := Result + Props[idx] + ':' + TjsdElement(o)._JSVar;
        end;
      end;
      tkString, tkChar, tkWChar, tkLString, tkWString, tkVariant, tkUString:
        Result := Result + Props[idx] + ':' + ToJSString(GetPropValue(Self, Props[idx]));
      tkInteger, tkInt64:
        Result := Result + Props[idx] + ':' + String(GetPropValue(Self, Props[idx]));
      tkFloat:
        Result := Result + Props[idx] + ':' + StringReplace(String(GetPropValue(Self, Props[idx])), ',', '.', [rfReplaceAll]);
      tkEnumeration:
        Result := Result + Props[idx] + ':' + LowerCase(String(GetPropValue(Self, Props[idx])));
    end;

  end;

  Result := Result + '}';
end;

{ TjsonObjectList }

function TjsonObjectList.Add(AObject: TjsonObject): Integer;
begin
  Result := FItems.Add(AObject);
end;

procedure TjsonObjectList.Assign(AListA: TjsonObjectList;
  AOperator: TListAssignOp; AListB: TjsonObjectList);
begin
  if Assigned(AListB) then
    FItems.Assign(AListA.FItems, AOperator, AListB.FItems)
  else
    FItems.Assign(FItems, AOperator, nil);
end;

procedure TjsonObjectList.Clear;
begin
  FItems.Clear;
end;

constructor TjsonObjectList.Create;
begin
  FItems := TList.Create;
end;

procedure TjsonObjectList.Delete(AIndex: Integer);
begin
  FItems.Delete(AIndex);
end;

procedure TjsonObjectList.Delete(AObject: TjsonObject);
begin
  FItems.Extract(AObject);
end;

destructor TjsonObjectList.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TjsonObjectList.Extract(AIndex: Integer): TjsonObject;
begin
  Result := Items[AIndex];
  Delete(AIndex);
end;

function TjsonObjectList.Extract(AObject: TjsonObject): TjsonObject;
begin
  Result := TjsonObject(FItems.Extract(AObject));
end;

function TjsonObjectList.First: TjsonObject;
begin
  Result := TjsonObject(FItems.First);
end;

function TjsonObjectList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TjsonObjectList.GetEnumerator: TjsonObjectListEnumerator;
begin
  Result := TjsonObjectListEnumerator.Create(Self);
end;

function TjsonObjectList.GetItem(AIndex: Integer): TjsonObject;
begin
  Result := TjsonObject(FItems[AIndex]);
end;

procedure TjsonObjectList.Insert(AIndex: Integer; AObject: TjsonObject);
begin
  FItems.Insert(AIndex, AObject);
end;

function TjsonObjectList.Last: TjsonObject;
begin
  Result := TjsonObject(FItems.Last);
end;

procedure TjsonObjectList.Move(ACurIndex, ANewIndex: Integer);
begin
  FItems.Move(ACurIndex, ANewIndex);
end;

function TjsonObjectList.Remove(AObject: TjsonObject): Integer;
begin
  Result := FItems.Remove(AObject);
end;

procedure TjsonObjectList.SetItem(AIndex: Integer; const Value: TjsonObject);
begin
  FItems[AIndex] := Value;
end;

function TjsonObjectList.ToJSON: String;
var
  idx : Integer;
begin
  Result := '[';

  for idx := 0 to Count - 1 do
  begin
    if idx > 0 then
      Result := Result + ',';

    Result := Result + TJSONObject(Items[idx]).ToJSON;
  end;

  Result := Result + ']';
end;

{ TjsonObjectListEnumerator }

constructor TjsonObjectListEnumerator.Create(AList: TjsonObjectList);
begin
  inherited Create;
  FIndex := -1;
  FList := AList;
end;

function TjsonObjectListEnumerator.GetCurrent: TjsonObject;
begin
  Result := FList[FIndex];
end;

function TjsonObjectListEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex);
end;

end.
