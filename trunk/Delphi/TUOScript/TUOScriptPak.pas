//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit TUOScriptPak;

interface

uses
  Classes,SysUtils, ToolsAPI, TUOScript, Dialogs;

type  
  TTUOScriptPak = class(TInterfacedObject, ITUOScriptFunctionHandler)
  private
    function InsertCompo(Params : TTUOScriptParams; const AEditor : IOTAEditor) : TTUOScriptFunctionResult;
    function TestParams(Params : TTUOScriptParams; const AEditor : IOTAEditor) : TTUOScriptFunctionResult;
  public
    function GetFunctionCount : Integer;
    function GetFunctionName(AIndex : Integer) : WideString;
    function GetFunctionItself(AIndex : Integer) : TTUOSctiptFunction2;
  end;



implementation

var
  ScriptPak : TTUOScriptPak;

function TTUOScriptPak.GetFunctionCount: Integer;
begin
  Result:=2;
end;

function TTUOScriptPak.GetFunctionItself(AIndex: Integer): TTUOSctiptFunction2;
begin
  case AIndex of
    0: Result:=InsertCompo;
    1: Result:=TestParams;
  end;
end;

function TTUOScriptPak.GetFunctionName(AIndex: Integer): WideString;
begin
    case AIndex of
    0: Result:='InsertComponent';
    1: Result:='TestParams';
  end;
end;

function TTUOScriptPak.InsertCompo(Params: TTUOScriptParams; const AEditor : IOTAEditor): TTUOScriptFunctionResult;
var
  FormEditor : IOTAFormEditor;
  NewCompo :  IOTAComponent;
  pas       : IOTASourceEditor;
  editview  : IOTAEditView;
  LastCode  : String;
  CName     : String;
begin
  Result:=-1;

  if (Params.Count=0) then
    exit;
  CName:=Trim(Params[0]);

  if not Assigned(AEditor) then
    MessageDlg('Got no module', mtWarning, [mbOK], 0)
  else if Supports(AEditor.Module.ModuleFileEditors[1], IOTAFormEditor, FormEditor) then
  begin
    NewCompo:=FormEditor.CreateComponent(FormEditor.GetRootComponent,CName,0,0,50,50);

    if (NewCompo<>nil) then
    begin
      Result:=1;
      //MessageDlg('All ok ... component inside!', mtWarning, [mbOK], 0);
    end                                       
    else
    begin
      Result:=0;
      //MessageDlg('Failed to insert new component.', mtError, [mbOK], 0);
    end;

    if Supports(AEditor.Module.CurrentEditor, IOTASourceEditor, pas) and (Result=1) then
    begin
      EditView := pas.EditViews[0];
      pas.Show;
      pas.SwitchToView(0);
      editview.Position.MoveRelative(0,Length(CName)*-1);
      LastCode:=editview.Position.Read(Length(CName));
      editview.Position.MoveRelative(0,Length(CName));
      if (AnsiSameStr(LastCode,CName)) then
        EditView.Position.BackspaceDelete(Length(CName));
    end;
  end
  else                                     
    MessageDlg('Got no FormEditor!', mtError, [mbOK], 0);
end;


function TTUOScriptPak.TestParams(Params: TTUOScriptParams; const AEditor : IOTAEditor): TTUOScriptFunctionResult;
begin
  MessageDlg(Params.Text, mtInformation, [mbOK], 0);

  Result:=1;
end;


initialization
  ScriptPak:=TTUOScriptPak.Create;
  TUOScriptEngine.RegisterFunctionHandler(ScriptPak);

finalization
  TUOScriptEngine.UnregisterFunctionHandler(ScriptPak);

end.
