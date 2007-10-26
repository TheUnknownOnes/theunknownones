unit TUOScriptPak;

interface
uses
  Classes,SysUtils, ToolsAPI, TUOScript, Dialogs;

type  
  TTUOScriptPak = class
    class function InsertCompo(Params : TTUOScriptParams):TTUOScriptFunctionResult;
    class function TestParams(Params : TTUOScriptParams):TTUOScriptFunctionResult;
  end;



implementation

class function TTUOScriptPak.InsertCompo(Params: TTUOScriptParams): TTUOScriptFunctionResult;
var
  services  : IOTAModuleServices;
  module    : IOTAModule;
  edit      : IOTAEditor;
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

  Services := BorlandIDEServices as IOTAModuleServices;

  Module:=services.CurrentModule;

  if module<>nil then
    edit:=Module.ModuleFileEditors[1]
  else
    MessageDlg('Got no module', mtWarning, [mbOK], 0);

  if (edit<>nil) then
  begin
    FormEditor:=Edit as IOTAFormEditor;
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

    if Supports(Module.CurrentEditor, IOTASourceEditor, pas) and (Result=1) then
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


class function TTUOScriptPak.TestParams(Params: TTUOScriptParams): TTUOScriptFunctionResult;
begin
  MessageDlg(Params.Text, mtInformation, [mbOK], 0);

  Result:=1;
end;


initialization                 
  TUOScriptEngine.RegisterFunction('InsertComponent',
                    Integer(@TTUOScriptPak.InsertCompo));

  TUOScriptEngine.RegisterFunction('TestParams',                 
                    Integer(@TTUOScriptPak.TestParams));

finalization
  TUOScriptEngine.UnregisterFunction('InsertComponent');

  TUOScriptEngine.UnregisterFunction('TestParams');
end.
