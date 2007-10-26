unit TUOScript;
{
  TUOScript-Implementation
  ========================

  Syntax:
    - not casesensitive
    - one command per line
    - one command: ProcedureName(ParamCommaText)
      example:  DoSomething("Hello World","I am a second param")
                DoAnotherAction(|pointname1|,|pointname2|)
    - at least one line like: Version=1.0
      first occurence will be used
}
interface

uses
  Classes, SysUtils, StrUtils, ToolsAPI, CodeTemplateAPI;

type
  TTUOScriptParams = TStrings;
  TTUOScriptFunctionResult = Integer;
  TTUOScriptFunction = function (Params : TTUOScriptParams): TTUOScriptFunctionResult  of object;
  TTUOScriptFunctionAddr = Integer;


  TTUOScriptInterpreter_1_0 = class(TObject)
  private
  public
    constructor Create();
    destructor Destroy(); override;

    function Run( const ATemplate: IOTACodeTemplate;
                  const APoint: IOTACodeTemplatePoint;
                  const ASyncPoints: IOTASyncEditPoints;
                  const AScript: IOTACodeTemplateScript;
                  RegisteredFunctions : TStrings) : Integer;
  end;

  TTUOScript = class(TNotifierObject, IOTACodeTemplateScriptEngine)
  private
    FRegisteredFunctions : TStrings;
  protected
  public
    constructor Create();
    destructor Destroy(); override;
    
    procedure Execute(const ATemplate: IOTACodeTemplate;
                      const APoint: IOTACodeTemplatePoint;
                      const ASyncPoints: IOTASyncEditPoints;
                      const AScript: IOTACodeTemplateScript;
                      var Cancel: Boolean);
    function GetIDString: WideString;
    function GetLanguage: WideString;

    function RegisterFunction(AName : String;
                              AFunctionAddr : TTUOScriptFunctionAddr) : Boolean;
    function UnregisterFunction(AName : String) : Boolean; overload;
    function UnregisterFunction(AFunction : TTUOScriptFunctionAddr) : Boolean; overload;
  end;

var
  TUOScriptEngine : TTUOScript;

implementation
{$REGION 'HelperFunctions'}

function MsgServices: IOTAMessageServices;
begin
  Result := (BorlandIDEServices as IOTAMessageServices);
  Assert(Result <> nil, 'IOTAMessageServices not available');
end;

{$ENDREGION}

{$REGION 'TTUOScript'}
constructor TTUOScript.Create;
begin
  FRegisteredFunctions:=TStringList.Create;
end;

destructor TTUOScript.Destroy;
begin
  FRegisteredFunctions.Free;
end;

procedure TTUOScript.Execute(const ATemplate: IOTACodeTemplate;
  const APoint: IOTACodeTemplatePoint; const ASyncPoints: IOTASyncEditPoints;
  const AScript: IOTACodeTemplateScript; var Cancel: Boolean);
var
  Script : TStrings;
  idxLine : Integer;
  Version : Single;
  Script_1_0 : TTUOScriptInterpreter_1_0;
begin
  Script:=TStringList.Create;
  try
    Script.Text:=AScript.Script;
    Version:=0;
    if (not TryStrToFloat(Script.Values['Version'],Version)) or (Version=0) then
      exit;
    if (Version=1) then
    begin
      try
        Script_1_0:=TTUOScriptInterpreter_1_0.Create;
        Script_1_0.Run(ATemplate,APoint,ASyncPoints,AScript,FRegisteredFunctions);
      finally
        Script_1_0.Free;
      end;
    end;

  finally
    Script.Free;
  end;
end;

function TTUOScript.GetIDString: WideString;
begin
  Result:='{8F3A7C21-F071-4E46-A1E3-5AF3F93C932B}';
end;

function TTUOScript.GetLanguage: WideString;
begin
  Result:='TUOScript';
end;


function TTUOScript.RegisterFunction(AName: String;
  AFunctionAddr: TTUOScriptFunctionAddr): Boolean;
var
  idx : Integer;
begin
  Result:=false;
  
  //Functionname already registered?
  idx:=FRegisteredFunctions.IndexOf(AName);
  if (idx>-1) then
  begin
    MsgServices.AddTitleMessage('[TUOScript] Can not register scriptfunction "'+AName+'". Name already exists.');
    exit;
  end;

  //Function already registered?
  idx:=FRegisteredFunctions.IndexOfObject(TObject(AFunctionAddr));
  if (idx>-1) then
  begin
    MsgServices.AddTitleMessage('[TUOScript] Can not register scriptfunction "'+AName+'". Function already exists.');
    exit;
  end;

  Result:=FRegisteredFunctions.AddObject(AName,TObject(AFunctionAddr))>-1;
  MsgServices.AddTitleMessage('[TUOScript] Registered successfully scriptfunction "'+AName+'".');
end;

function TTUOScript.UnregisterFunction(AName: String): Boolean;
var
  idx : Integer;
begin
  Result:=true;

  idx:=FRegisteredFunctions.IndexOf(AName);
  if (idx>-1) then
  begin
    FRegisteredFunctions.Delete(idx);
    Result:=true;
    MsgServices.AddTitleMessage('[TUOScript] Unregistered scriptfunction "'+AName+'" successfully.');
  end
  else
    MsgServices.AddTitleMessage('[TUOScript] Can not unregister scriptfunction "'+AName+'". Name does not exist.');
end;

function TTUOScript.UnregisterFunction(AFunction: TTUOScriptFunctionAddr): Boolean;
var
  idx : Integer;
begin
  Result:=true;

  idx:=FRegisteredFunctions.IndexOfObject(TObject(@AFunction));
  if (idx>-1) then
  begin
    FRegisteredFunctions.Delete(idx);
    Result:=true;
    MsgServices.AddTitleMessage('[TUOScript] Unregistered scriptfunction "'+FRegisteredFunctions[idx]+'" successfully.');
  end
  else
    MsgServices.AddTitleMessage('[TUOScript] Can not unregister scriptfunction.');
end;

{$ENDREGION}


{$REGION 'TTUOScriptInterpreter_1_0'}

constructor TTUOScriptInterpreter_1_0.Create;
begin

end;

destructor TTUOScriptInterpreter_1_0.Destroy;
begin

  inherited;
end;

function TTUOScriptInterpreter_1_0.Run( const ATemplate: IOTACodeTemplate;
                                        const APoint: IOTACodeTemplatePoint;
                                        const ASyncPoints: IOTASyncEditPoints;
                                        const AScript: IOTACodeTemplateScript;
                                        RegisteredFunctions : TStrings): Integer;
var
  Script            : TStrings;
  idxPoint          : Integer;
  idxLine           : Integer;
  posFirstBracket,
  posLastBracket    : Integer;
  curCommand        : String;
  ProcedureName     : String;
  idxFunction       : Integer;
  Func              : TTUOScriptFunction;
  Params            : TStrings;
begin
  //By default, we want to cancel the script-execution
  Result:=0;
  
  Script:=TStringList.Create;
  Params:=TStringList.Create;
  try
    //First, get the script.
    Script.Text:=AScript.Script;

    //Anything to do?
    if (Script.Count=0) then
      exit;

    //Now, replace all |pointname| with the values of the points
    for idxPoint := 0 to ASyncPoints.Count - 1 do
      Script.Text:=StringReplace(Script.Text,
                      ATemplate.Delimiter+ASyncPoints.Points[idxPoint].Name+ATemplate.Delimiter,
                      '"'+ASyncPoints.Points[idxPoint].Text+'"',
                      [rfReplaceAll]);

    //Lets parse each line
    for idxLine := 0 to Script.Count - 1 do
    begin
      //Get the current command
      curCommand:=Trim(Script[idxLine]);

      //Find the first and the last bracket in the command
      posFirstBracket:=Pos('(',curCommand);
      posLastBracket:=Length(curCommand)-Pos(')',ReverseString(curCommand));

      //Is this line valid?
      if (posFirstBracket>0) and (posLastBracket>0) then
      begin
        //Get the name of the procedure
        ProcedureName:=LeftStr(curCommand,posFirstBracket-1);

        //parse all registered functions
        for idxFunction := 0 to RegisteredFunctions.Count - 1 do
        begin
          //Is this the function we want to call?
          if (AnsiSameText(ProcedureName,RegisteredFunctions[idxFunction])) then
          begin
            Params.CommaText:=Copy(curCommand,posFirstBracket+1,posLastBracket-posFirstBracket);
            @Func:=Pointer(RegisteredFunctions.Objects[idxFunction]);
            Result:=Func(Params);
          end;
        end;
      end;
    end;
  finally
    Script.Free;
    Params.Free;
  end;
end;

{$ENDREGION}


initialization
  TUOScriptEngine:=TTUOScript.Create;
  (BorlandIDEServices as IOTACodeTemplateServices).RegisterScriptEngine(TUOScriptEngine);

finalization
  
end.
