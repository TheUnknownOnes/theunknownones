//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************

unit uOptionParser;

interface

uses
  Classes, SysUtils;

const
  {$IFNDEF Unix}
  DefaultIgnoreCase = true;
  {$ELSE}
  DefaultIgnoreCase = false;
  {$ENDIF}
  DefaultOptionChars = '-';
  DefaultValueChars = '=';

resourcestring
  SErrUnknownOption = 'Unknown option "%s"';
  SErrDuplicateOption = 'Duplicate option "%s"';
  SErrRequiredValue = 'Option "%s" requires a value';
  SErrRequiredOption = 'Missing required option "%s"';
  SErrNoValueNeeded = 'Option "%s" need no value';

type
  TOptionParser = class;
  TOptionDefs = class;

  TOptionFlag = (ofRequired, ofValuePossible, ofValueRequired, ofAllowMultiple);
  TOptionFlags = set of TOptionFlag;

  TOptionDef = class(TCollectionItem)
  protected
    FLongName: String;
    FShortName: Char;
    FFlags: TOptionFlags;
    FDescription: String;

    procedure SetDescription(const Value: String);
    procedure SetFlags(const Value: TOptionFlags);
    procedure SetLongName(const Value: String);
    procedure SetShortName(const Value: Char);
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;

    function FlagSet(AFlag : TOptionFlag) : Boolean;
  published
    property Flags : TOptionFlags read FFlags write SetFlags default [];
    property LongName : String read FLongName write SetLongName;
    property ShortName : Char read FShortName write SetShortName;
    property Description : String read FDescription write SetDescription;
  end;

  TOptionDefs = class(TCollection)
  protected
    FOptionParser : TOptionParser;

    function GetItem(AIndex: Integer): TOptionDef;
    procedure SetItem(AIndex: Integer; const AValue: TOptionDef);

    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    constructor Create(AParamParser : TOptionParser);

    function Add() : TOptionDef; overload;
    function Add(ALongParam : String; ADescription : String = '') : TOptionDef; overload;
    function Add(ALongParam : String; AFlags : TOptionFlags; ADescription : String = '') : TOptionDef; overload;
    function Add(AShortParam : Char; ADescription : String = '') : TOptionDef; overload;
    function Add(AShortName : Char; AFlags : TOptionFlags; ADescription : String = '') : TOptionDef; overload;
    function Add(ALongParam : String; AShortParam : Char; ADescription : String = '') : TOptionDef; overload;
    function Add(ALongName : String; AShortParam : Char; AFlags : TOptionFlags; ADescription : String = '') : TOptionDef; overload;

    property Items[AIndex: Integer]: TOptionDef read GetItem write SetItem; default;
  end;

  TExceptionMode = (emIgnore, emCollect, emRaise);
  TParsingFlag = (pfIgnoreCase);
  TParsingFlags = set of TParsingFlag;

  TParserEnvironment = record
    ParamIndex : Integer;
    ParamValue : String;
    LongOption : String;
    ShortOption : Char;
    OptionValue : String;
  end;

  EOptionParserException = type Exception;

  { TOptionParser }

  TOptionParser = class(TComponent)
  protected
    FOptions,
    FParams : TStringList;
    FOptionDefs : TOptionDefs;
    FParsed : Boolean;
    FParsing : Boolean;
    FOptionChars: String;
    FExceptionMode: TExceptionMode;
    FParsingFlags: TParsingFlags;
    FExceptions : TStringList;
    FValueChars: String;

    procedure SetValueChars(const Value: String);
    procedure SetParsingFlags(const Value: TParsingFlags);
    procedure SetOptionChars(const Value: String);
    procedure SetDefs(const Value: TOptionDefs);
    function GetParams: TStringList;
    function GetOptions: TStringList;

    function FindOptionDef(ALongName : String; out AOptionDef : TOptionDef) : Boolean; overload;
    function FindOptionDef(AShortName : Char; out AOptionDef : TOptionDef) : Boolean; overload;

    function ParserSameStr(AStr1, AStr2 : String) : Boolean;
    procedure ParseParam(var AEnv : TParserEnvironment);
    procedure ParseLongOption(var AEnv : TParserEnvironment);
    procedure ParseShortOption(var AEnv : TParserEnvironment);

    procedure AddOption(AName, AValue : String; AOptionDef : TOptionDef); overload;
    procedure AddOption(AName : String; AOptionDef : TOptionDef); overload;
    procedure AddParam(AValue : String);

    procedure NeedParsed;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    //has not to be called explictly ... its done automatically
    function Parse : Boolean;

    function HasOption(AOptionDef : TOptionDef) : Boolean; overload;
    function HasOption(AShortName : Char) : Boolean; overload;
    function HasOption(ALongName : String) : Boolean; overload;

    function GetOptionValue(AOptionDef : TOptionDef; ADefault : String = '') : String; overload;
    function GetOptionValue(AShortName : Char; ADefault : String = '') : String; overload;
    function GetOptionValue(ALongName : String; ADefault : String = '') : String; overload;

    function GetHelpText(AIndent : String) : String;

    property Exceptions : TStringList read FExceptions;
    property Options : TStringList read GetOptions;
    property Params : TStringList read GetParams;
  published
    //defines what to do if an exception occurs while parsing
    property ExcpetionMode : TExceptionMode read FExceptionMode write FExceptionMode default emRaise;
    //defines which chars starts an option
    property OptionsChars : String read FOptionChars write SetOptionChars;
    //defines the known options
    property OptionDefs : TOptionDefs read FOptionDefs write SetDefs;
    {- defines the chars which may delimit the option from the value
     - this applies only to long options: the value of a short option is the next param}
    property ValueChars : String read FValueChars write SetValueChars;

    property ParsingFlags : TParsingFlags read FParsingFlags write SetParsingFlags default [];
  end;

implementation

{ TOptionDefs }

function TOptionDefs.Add(ALongParam: String;
  AFlags: TOptionFlags; ADescription : String): TOptionDef;
begin
  Result := Add();
  Result.LongName := ALongParam;
  Result.Flags := AFlags;
  Result.Description := ADescription;
end;

function TOptionDefs.Add(ALongParam: String; ADescription : String): TOptionDef;
begin
  Result := Add();
  Result.LongName := ALongParam;
  Result.Description := ADescription;
end;

function TOptionDefs.Add(): TOptionDef;
begin
  Result := TOptionDef(inherited Add());
end;

function TOptionDefs.Add(AShortParam: Char; ADescription : String): TOptionDef;
begin
  Result := Add();
  Result.ShortName := AShortParam;
  Result.Description := ADescription;
end;

function TOptionDefs.Add(ALongName: String;
  AShortParam: Char; AFlags: TOptionFlags; ADescription : String): TOptionDef;
begin
  Result := Add();
  Result.LongName := ALongName;
  Result.ShortName := AShortParam;
  Result.Flags := AFlags;
  Result.Description := ADescription;
end;

function TOptionDefs.Add(ALongParam: String;
  AShortParam: Char; ADescription : String): TOptionDef;
begin
  Result := Add();
  Result.LongName := ALongParam;
  Result.ShortName := AShortParam;
  Result.Description := ADescription;
end;

function TOptionDefs.Add(AShortName: Char;
  AFlags: TOptionFlags; ADescription : String): TOptionDef;
begin
  Result := Add();
  Result.ShortName := AShortName;
  Result.Flags := AFlags;
  Result.Description := ADescription;
end;

constructor TOptionDefs.Create(AParamParser: TOptionParser);
begin
  inherited Create(TOptionDef);
  FOptionParser := AParamParser;
end;

function TOptionDefs.GetItem(AIndex: Integer): TOptionDef;
begin
  Result := TOptionDef(inherited GetItem(AIndex));
end;

function TOptionDefs.GetOwner: TPersistent;
begin
  Result := FOptionParser;
end;

procedure TOptionDefs.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited;
  FOptionParser.FParsed := false;
end;

procedure TOptionDefs.SetItem(AIndex: Integer; const AValue: TOptionDef);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TOptionDefs.Update(Item: TCollectionItem);
begin
  FOptionParser.FParsed := false;
end;

{ TOptionParser }

procedure TOptionParser.AddParam(AValue: String);
begin
  FParams.Add(AValue);
end;

procedure TOptionParser.AddOption(AName, AValue: String; AOptionDef: TOptionDef);
begin
  FOptions.AddObject(AName + FOptions.NameValueSeparator + AValue, AOptionDef);
end;

procedure TOptionParser.AddOption(AName: String; AOptionDef: TOptionDef);
begin
  FOptions.AddObject(AName, AOptionDef);
end;

procedure TOptionParser.Assign(Source: TPersistent);
begin
  if Source is TOptionParser then
  begin
    OptionsChars := TOptionParser(Source).OptionsChars;
    OptionDefs := TOptionParser(Source).OptionDefs;
    ExcpetionMode := TOptionParser(Source).ExcpetionMode;
    ParsingFlags := TOptionParser(Source).ParsingFlags;
    ValueChars := TOptionParser(Source).ValueChars;
  end
  else
    inherited Assign(Source);
end;

constructor TOptionParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOptionDefs := TOptionDefs.Create(Self);
  FParsed := false;
  FParsing := false;
  FOptionChars := DefaultOptionChars;
  FExceptionMode := emRaise;
  FParsingFlags := [];
  if DefaultIgnoreCase then
    FParsingFlags := FParsingFlags + [pfIgnoreCase];

  FValueChars := DefaultValueChars;

  FOptions := TStringList.Create;
  FOptions.NameValueSeparator := '=';
  FParams := TStringList.Create;
  FExceptions := TStringList.Create;
end;

destructor TOptionParser.Destroy;
begin
  FOptionDefs.Free;
  FExceptions.Free;
  FOptions.Free;
  FParams.Free;

  inherited;
end;

function TOptionParser.FindOptionDef(ALongName: String;
  out AOptionDef: TOptionDef): Boolean;
var
  idx : Integer;
begin
  Result := false;

  for idx := 0 to FOptionDefs.Count - 1 do
  begin
    if ParserSameStr(ALongName, FOptionDefs[idx].LongName) then
    begin
      Result := true;
      AOptionDef := FOptionDefs[idx];
    end;
  end;
end;

function TOptionParser.FindOptionDef(AShortName: Char;
  out AOptionDef: TOptionDef): Boolean;
var
  idx : Integer;
begin
  Result := false;

  for idx := 0 to FOptionDefs.Count - 1 do
  begin
    if ParserSameStr(AShortName, FOptionDefs[idx].ShortName) then
    begin
      Result := true;
      AOptionDef := FOptionDefs[idx];
    end;
  end;
end;

function TOptionParser.GetHelpText(AIndent: String): String;
var
  idx : Integer;
  LongOptStart,
  ShortOptStart,
  ValueDelim: String;
  HasLongName : Boolean;
begin
  Result := EmptyStr;

  if Length(OptionsChars) > 0 then
  begin
    LongOptStart := OptionsChars[1] + OptionsChars[1];
    ShortOptStart := OptionsChars[1];
  end;
  if Length(ValueChars) > 0 then
    ValueDelim := ValueChars[1];

  for idx := 0 to OptionDefs.Count - 1 do
  begin
    if idx > 0 then Result := Result + sLineBreak;
    Result := Result + AIndent;

    if not OptionDefs[idx].FlagSet(ofRequired) then Result := Result + '[';

    if OptionDefs[idx].LongName <> EmptyStr then
    begin
      HasLongName := true;
      Result := Result + LongOptStart + OptionDefs[idx].LongName;

      if OptionDefs[idx].FlagSet(ofValuePossible) or
         OptionDefs[idx].FlagSet(ofValueRequired) then
      begin
        if OptionDefs[idx].FlagSet(ofValuePossible) then Result := Result + '[';
        Result := Result + ValueDelim + 'value';
        if OptionDefs[idx].FlagSet(ofValuePossible) then Result := Result + '[';
      end;
    end
    else
      HasLongName := false;

    if OptionDefs[idx].ShortName <> #0 then
    begin
      if HasLongName then Result := Result + '|';
      Result := Result + ShortOptStart + OptionDefs[idx].ShortName;

      if OptionDefs[idx].FlagSet(ofValuePossible) or
         OptionDefs[idx].FlagSet(ofValueRequired) then
      begin
        if OptionDefs[idx].FlagSet(ofValuePossible) then Result := Result + '[';
        Result := Result + ' value';
        if OptionDefs[idx].FlagSet(ofValuePossible) then Result := Result + '[';
      end;
    end;

    if not OptionDefs[idx].FlagSet(ofRequired) then Result := Result + ']';

    Result := Result + '  ' + OptionDefs[idx].Description;
  end;
end;

function TOptionParser.GetParams: TStringList;
begin
  NeedParsed;
  Result := FParams;
end;

function TOptionParser.GetOptions: TStringList;
begin
  NeedParsed;
  Result := FOptions;
end;

function TOptionParser.HasOption(AShortName: Char): Boolean;
var
  def : TOptionDef;
begin
  Result := FindOptionDef(AShortName, def) and HasOption(def);
end;

function TOptionParser.HasOption(ALongName: String): Boolean;
var
  def : TOptionDef;
begin
  Result := FindOptionDef(ALongName, def) and HasOption(def);
end;

function TOptionParser.GetOptionValue(AOptionDef: TOptionDef; ADefault : String): String;
var
  idx : Integer;
begin
  Result := ADefault;
  idx := FOptions.IndexOfObject(AOptionDef);
  if idx > -1 then
    Result := FOptions.ValueFromIndex[idx];
end;

function TOptionParser.GetOptionValue(AShortName: Char; ADefault : String): String;
var
  def : TOptionDef;
begin
  Result := ADefault;
  if FindOptionDef(AShortName, def) then
    Result := GetOptionValue(def, ADefault);
end;

function TOptionParser.GetOptionValue(ALongName: String; ADefault : String): String;
var
  def : TOptionDef;
begin
  Result := ADefault;
  if FindOptionDef(ALongName, def) then
    Result := GetOptionValue(def, ADefault);
end;

function TOptionParser.HasOption(AOptionDef: TOptionDef): Boolean;
begin
  Result := Options.IndexOfObject(AOptionDef) > -1;
end;

procedure TOptionParser.NeedParsed;
begin
  if not FParsed then
    Parse;
end;

function TOptionParser.Parse : Boolean;
var
  idxDef : Integer;
  Env : TParserEnvironment;
begin
  Result := true;
  if FParsing or (csDesigning in ComponentState) then exit;

  FExceptions.Clear;
  FOptions.Clear;
  FParams.Clear;

  FParsing := true;
  try
    Env.ParamIndex := 1;
    while Env.ParamIndex <= ParamCount do
    begin
      Env.ParamValue := ParamStr(Env.ParamIndex);
      try
        ParseParam(Env);
      except
        on E : EOptionParserException do
        begin
          case FExceptionMode of
            emIgnore : Result := false;
            emCollect:
            begin
              Result := false;
              FExceptions.Add(e.Message);
            end;
            emRaise : raise;
          end;
        end;
      end;

      Inc(Env.ParamIndex);
    end;

    for idxDef := 0 to OptionDefs.Count - 1 do
    begin
      if OptionDefs[idxDef].FlagSet(ofRequired) and
         (not HasOption(OptionDefs[idxDef])) then
      begin
        case FExceptionMode of
          emIgnore: Result := false;
          emCollect:
          begin
            Result := false;
            FExceptions.Add(Format(SErrRequiredOption, [OptionDefs[idxDef].DisplayName]));
          end;
          emRaise: raise EOptionParserException.CreateFmt(SErrRequiredOption, [OptionDefs[idxDef].DisplayName]);
        end;
      end;
    end;
  finally
    FParsing := false;
  end;

  FParsed := true;
end;

procedure TOptionParser.ParseLongOption(var AEnv: TParserEnvironment);
var
  def : TOptionDef;
  idxValueChar,
  idxOption : Integer;
begin
  idxOption := 0;
  for idxValueChar := 1 to Length(FValueChars) do
  begin
    idxOption := Pos(FValueChars[idxValueChar], AEnv.LongOption);
    if idxOption > 0 then break;
  end;

  if idxOption > 0 then
  begin
    AEnv.OptionValue := Copy(AEnv.LongOption, idxOption + 1, Length(AEnv.LongOption) - idxOption);
    AEnv.LongOption := Copy(AEnv.LongOption, 1, idxOption - 1);
  end
  else
    AEnv.OptionValue := EmptyStr;

  if FindOptionDef(AEnv.LongOption, def) then
  begin
    if (not def.FlagSet(ofAllowMultiple)) and HasOption(def) then
      raise EOptionParserException.CreateFmt(SErrDuplicateOption, [AEnv.ParamValue]);

    if def.FlagSet(ofValuePossible) or def.FlagSet(ofValueRequired) then
    begin
      if def.FlagSet(ofValueRequired) and (AEnv.OptionValue = EmptyStr) then
        raise EOptionParserException.CreateFmt(SErrRequiredValue, [AEnv.LongOption]);

      AddOption(AEnv.LongOption, AEnv.OptionValue, def);
    end
    else
    if AEnv.OptionValue <> EmptyStr then
      raise EOptionParserException.CreateFmt(SErrNoValueNeeded, [AEnv.LongOption])
    else
      AddOption(AEnv.LongOption, def);
  end
  else
    raise EOptionParserException.CreateFmt(SErrUnknownOption, [AEnv.ParamValue]);
end;

procedure TOptionParser.ParseParam(var AEnv : TParserEnvironment);
var
  idxOptionChar : Integer;
  IsShortOpt,
  IsLongOpt : Boolean;
begin
  IsShortOpt := false;
  IsLongOpt := false;

  //check if the param is an option
  if Length(AEnv.ParamValue) > 1 then
  begin
    for idxOptionChar := 1 to Length(FOptionChars) do
    begin
      if AEnv.ParamValue[1] = FOptionChars[idxOptionChar] then
      begin
        if (Length(AEnv.ParamValue) > 2) and
           (AEnv.ParamValue[2] = FOptionChars[idxOptionChar]) then
          IsLongOpt := true
        else
          IsShortOpt := true;
      end;
    end;
  end;

  if IsLongOpt then
  begin
    AEnv.LongOption := Copy(AEnv.ParamValue, 3, Length(AEnv.ParamValue) - 2);
    ParseLongOption(AEnv);
  end
  else
  if IsShortOpt then
  begin
    AEnv.ShortOption := AEnv.ParamValue[2];
    ParseShortOption(AEnv);
  end
  else
    AddParam(AEnv.ParamValue);
end;

function TOptionParser.ParserSameStr(AStr1, AStr2: String): Boolean;
begin
  if pfIgnoreCase in FParsingFlags then
    Result := LowerCase(AStr1) = LowerCase(AStr2)
  else
    Result := AStr1 = AStr2;
end;

procedure TOptionParser.ParseShortOption(var AEnv: TParserEnvironment);
var
  def : TOptionDef;
  NextParam : String;
  idxOptionChar : Integer;
  NextParamIsOption : Boolean;
begin
  if FindOptionDef(AEnv.ShortOption, def) then
  begin
    if (not def.FlagSet(ofAllowMultiple)) and HasOption(def) then
      raise EOptionParserException.CreateFmt(SErrDuplicateOption, [AEnv.ParamValue]);

    if def.FlagSet(ofValuePossible) or def.FlagSet(ofValueRequired) then
    begin
      AEnv.OptionValue := EmptyStr;

      if AEnv.ParamIndex < ParamCount then
      begin
        NextParam := ParamStr(AEnv.ParamIndex + 1);
        NextParamIsOption := false;

        for idxOptionChar := 0 to Length(FOptionChars) do
        begin
          if NextParam[1] = FOptionChars[idxOptionChar] then
          begin
            NextParamIsOption := true;
            break;
          end;
        end;

        if not NextParamIsOption then
        begin
          AEnv.OptionValue := NextParam;
          Inc(AEnv.ParamIndex);
        end;
      end;

      if def.FlagSet(ofValueRequired) and (AEnv.OptionValue = EmptyStr) then
        raise EOptionParserException.CreateFmt(SErrRequiredValue, [AEnv.ShortOption]);

      AddOption(AEnv.ShortOption, AEnv.OptionValue, def);
    end
    else
      AddOption(AEnv.ShortOption, def);
  end
  else
    raise EOptionParserException.CreateFmt(SErrUnknownOption, [AEnv.ParamValue]);
end;

procedure TOptionParser.SetDefs(const Value: TOptionDefs);
begin
  FOptionDefs.Assign(Value);
end;

procedure TOptionParser.SetOptionChars(const Value: String);
begin
  FOptionChars := Value;
  FParsed := false;
end;

procedure TOptionParser.SetParsingFlags(const Value: TParsingFlags);
begin
  FParsingFlags := Value;
  FParsed := false;
end;

procedure TOptionParser.SetValueChars(const Value: String);
begin
  FValueChars := Value;
  FParsed := false;
end;

{ TOptionDef }

procedure TOptionDef.Assign(Source: TPersistent);
begin
  if Source is TOptionDef then
  begin
    Flags := TOptionDef(Source).Flags;
    LongName := TOptionDef(Source).LongName;
    ShortName := TOptionDef(Source).ShortName;
  end
  else
    inherited Assign(Source);
end;

constructor TOptionDef.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);

  FLongName := EmptyStr;
  FShortName := #0;
  FFlags := [];
  FDescription := EmptyStr;
end;

function TOptionDef.FlagSet(AFlag: TOptionFlag): Boolean;
begin
  Result := AFlag in FFlags;
end;

function TOptionDef.GetDisplayName: string;
begin
  Result := FLongName;

  if FShortName <> #0 then
    Result := Result + '|' + FShortName;
end;

procedure TOptionDef.SetDescription(const Value: String);
begin
  FDescription := Value;
  Changed(false);
end;

procedure TOptionDef.SetFlags(const Value: TOptionFlags);
begin
  FFlags := Value;
  Changed(false);
end;

procedure TOptionDef.SetLongName(const Value: String);
begin
  FLongName := Value;
  Changed(false);
end;

procedure TOptionDef.SetShortName(const Value: Char);
begin
  FShortName := Value;
  Changed(false);
end;

end.
