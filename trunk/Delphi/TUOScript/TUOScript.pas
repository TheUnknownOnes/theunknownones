//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
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
  Classes, SysUtils, StrUtils, ToolsAPI, CodeTemplateAPI,
  {$IFDEF VER230}VCL.Dialogs{$ELSE}Dialogs{$ENDIF};

type
  TTUOScriptParams = TStrings;
  TTUOScriptFunctionResult = Integer;
  TTUOScriptFunction = function (Params : TTUOScriptParams): TTUOScriptFunctionResult  of object;
  TTUOScriptFunctionAddr = Integer;

  TTUOSctiptFunction2 = function(AParams : TTUOScriptParams; const AEditor : IOTAEditor) : TTUOScriptFunctionResult of object;

  ITUOScriptFunctionHandler = interface
  ['{1F75D9E2-1E1E-4166-BA09-4DAB68E519BD}']
    function GetFunctionCount : Integer;
    function GetFunctionName(AIndex : Integer) : WideString;
    function GetFunctionItself(AIndex : Integer) : TTUOSctiptFunction2;

    property FunctionCount : Integer read GetFunctionCount;
    property FunctionName[AIndex : Integer] : WideString read GetFunctionName;
    property FunctionItself[AIndex : Integer] : TTUOSctiptFunction2 read GetFunctionItself;
  end;

  TTUOScriptInterpreter_1_0 = class(TObject)
  private
    FRegisteredFunctions : TStrings;
    FRegisteredHandlers : TInterfaceList;
    
    function Execute(const AFunctionName : WideString; const AParams : TTUOScriptParams) : TTUOScriptFunctionResult;
  public
    constructor Create(const ARegisteredFunctions : TStrings;
                       const ARegisteredFunctionHandlers : TInterfaceList);
    destructor Destroy(); override;

    function Run( const ATemplate: IOTACodeTemplate;
                  const APoint: IOTACodeTemplatePoint;
                  const ASyncPoints: IOTASyncEditPoints;
                  const AScript: IOTACodeTemplateScript) : Integer;
  end;

  TTUOScriptLogType = (ltInfo, ltError);

  TTUOScript = class(TNotifierObject, IOTACodeTemplateScriptEngine)
  private
    FRegisteredFunctions : TStrings;
    FRegisteredHandlers : TInterfaceList;

    FMessageGroup : IOTAMessageGroup;

    function GetFunctionNamesCommaList(const AHandler : ITUOScriptFunctionHandler) : WideString;

    procedure LogMessage(AMessage : WideString; AlogType : TTUOScriptLogType = ltInfo);
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
                              AFunctionAddr : TTUOScriptFunctionAddr) : Boolean; deprecated;
    function UnregisterFunction(AName : String) : Boolean; overload; deprecated;
    function UnregisterFunction(AFunction : TTUOScriptFunctionAddr) : Boolean; overload; deprecated;

    function RegisterFunctionHandler(const AHandler : ITUOScriptFunctionHandler) : Boolean;
    function UnregisterFunctionHandler(const AHandler : ITUOScriptFunctionHandler) : Boolean;
  end;

var
  TUOScriptEngine : TTUOScript;

implementation


{$REGION 'TTUOScript'}
constructor TTUOScript.Create;
begin
  FRegisteredFunctions:=TStringList.Create;
  FRegisteredHandlers:=TInterfaceList.Create;

  FMessageGroup:=(BorlandIDEServices as IOTAMessageServices).AddMessageGroup('TUOScript');

  if Assigned(FMessageGroup) then
    FMessageGroup.CanClose:=true;


  LogMessage('Alive! :)');
end;

destructor TTUOScript.Destroy;
begin
  FRegisteredFunctions.Free;
  FRegisteredHandlers.Free;

  if Assigned(FMessageGroup) then  
    (BorlandIDEServices as IOTAMessageServices).RemoveMessageGroup(FMessageGroup);
end;

procedure TTUOScript.Execute(const ATemplate: IOTACodeTemplate;
  const APoint: IOTACodeTemplatePoint; const ASyncPoints: IOTASyncEditPoints;
  const AScript: IOTACodeTemplateScript; var Cancel: Boolean);
var
  Script : TStrings;
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
      Script_1_0:=TTUOScriptInterpreter_1_0.Create(FRegisteredFunctions, FRegisteredHandlers);
      try
        Script_1_0.Run(ATemplate,APoint,ASyncPoints,AScript);
      finally
        Script_1_0.Free;
      end;
    end;

  finally
    Script.Free;
  end;
end;

function TTUOScript.GetFunctionNamesCommaList(
  const AHandler: ITUOScriptFunctionHandler): WideString;
var
  idx : Integer;
begin
  Result:='';
  for idx := 0 to AHandler.FunctionCount - 1 do
  begin
    if idx>0 then
      Result:=Result+',';

    Result:=Result+AHandler.FunctionName[idx]
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


procedure TTUOScript.LogMessage(AMessage: WideString; ALogType : TTUOScriptLogType);
var
  Prefix : WideString;
begin
  case AlogType of
    ltInfo: Prefix:='';
    ltError: Prefix:='[Error] ';
  end;

  AMessage:=Prefix + AMessage;

  if Assigned(FMessageGroup) then
    (BorlandIDEServices as IOTAMessageServices).AddWideTitleMessage(AMessage, FMessageGroup)
  else
    (BorlandIDEServices as IOTAMessageServices).AddWideTitleMessage(AMessage);
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
    LogMessage('Can not register scriptfunction "'+AName+'". Name already exists.');
    exit;
  end;

  //Function already registered?
  idx:=FRegisteredFunctions.IndexOfObject(TObject(AFunctionAddr));
  if (idx>-1) then
  begin
    LogMessage('Can not register scriptfunction "'+AName+'". Function already exists.');
    exit;
  end;

  Result:=FRegisteredFunctions.AddObject(AName,TObject(AFunctionAddr))>-1;
  LogMessage('Registered successfully scriptfunction "'+AName+'".');
end;

function TTUOScript.RegisterFunctionHandler(
  const AHandler: ITUOScriptFunctionHandler): Boolean;
begin
  Result:=false;
  try
    if FRegisteredHandlers.IndexOf(AHandler)=-1 then
      Result:=FRegisteredHandlers.Add(AHandler)>-1;

    if Result then
      LogMessage(Format('Registered function handler with %d functions (%s)',
                        [AHandler.FunctionCount, GetFunctionNamesCommaList(AHandler)]))
    else
      LogMessage('Failed to register function handler');
  except
    on E: Exception do
      LogMessage(e.Message, ltError);
  end;


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
    LogMessage('Unregistered scriptfunction "'+AName+'" successfully.');
  end
  else
    LogMessage('Can not unregister scriptfunction "'+AName+'". Name does not exist.');
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
    LogMessage('Unregistered scriptfunction "'+FRegisteredFunctions[idx]+'" successfully.');
  end
  else
    LogMessage('Can not unregister scriptfunction.');
end;

function TTUOScript.UnregisterFunctionHandler(
  const AHandler: ITUOScriptFunctionHandler): Boolean;
begin
  Result:=FRegisteredHandlers.IndexOf(AHandler)>-1;
  try
    if Result then
      FRegisteredHandlers.Remove(AHandler);

    LogMessage(Format('Unregistered function handler (%s)', [GetFunctionNamesCommaList(AHandler)]));
  except
    on E : Exception do
      LogMessage(e.Message, ltError);
  end;
end;

{$ENDREGION}


{$REGION 'TTUOScriptInterpreter_1_0'}

constructor TTUOScriptInterpreter_1_0.Create(const ARegisteredFunctions : TStrings;
                                             const ARegisteredFunctionHandlers : TInterfaceList);
begin
  FRegisteredFunctions:=ARegisteredFunctions;
  FRegisteredHandlers:=ARegisteredFunctionHandlers;
end;

destructor TTUOScriptInterpreter_1_0.Destroy;
begin

  inherited;
end;

function TTUOScriptInterpreter_1_0.Execute(const AFunctionName: WideString;
  const AParams: TTUOScriptParams): TTUOScriptFunctionResult;
var                                                            
  idxFunction       : Integer;
  Func              : TTUOScriptFunction;
  idxHandler        : Integer;
  Handler           : ITUOScriptFunctionHandler;
  Func2             : TTUOSctiptFunction2;
  Editor            : IOTAEditor;
begin
  Result:=0;

  Editor:=(BorlandIDEServices as IOTAModuleServices).CurrentModule.CurrentEditor;

  //First, try to locate the function inside the function list (for old apps)
  for idxFunction := 0 to FRegisteredFunctions.Count - 1 do
  begin
    //Is this the function we want to call?
    if (AnsiSameText(AFunctionName, FRegisteredFunctions[idxFunction])) then
    begin

      @Func:=Pointer(FRegisteredFunctions.Objects[idxFunction]);
      Result:=Func(AParams);
    end;
  end;

  //try to find the function in the list of function handlers
  for idxHandler := 0 to FRegisteredHandlers.Count - 1 do
  begin
    if Supports(FRegisteredHandlers[idxHandler], ITUOScriptFunctionHandler, Handler) then
    begin
      for idxFunction := 0 to Handler.FunctionCount - 1 do
      begin
        if AnsiSameText(AFunctionName, Handler.FunctionName[idxFunction]) then
        begin
          Func2:=Handler.FunctionItself[idxFunction];
          Result:=Func2(AParams, Editor);
        end;
      end;                                                               
    end;
  end;    
end;

function TTUOScriptInterpreter_1_0.Run( const ATemplate: IOTACodeTemplate;
                                        const APoint: IOTACodeTemplatePoint;
                                        const ASyncPoints: IOTASyncEditPoints;
                                        const AScript: IOTACodeTemplateScript): Integer;
var
  Script            : TStrings;
  idxPoint          : Integer;
  idxLine           : Integer;
  posFirstBracket,
  posLastBracket    : Integer;
  curCommand        : String;
  ProcedureName     : String;
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

        //Get the params into the buffer
        Params.CommaText:=Copy(curCommand,posFirstBracket+1,posLastBracket-posFirstBracket);                                          

        //Execute the function
        Execute(ProcedureName, Params)
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
