unit uCNA2IDENotifier;

interface

uses
  ToolsAPI,
  Classes,
  SysUtils,
  uCNA2FormEditorHooks;

type
  Tcna2IDENotifier = class(TInterfacedObject,
                           IOTAIDENotifier,
                           IOTANotifier)
  private

    //IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;

    //IOTAIDENotifier
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
  end;

implementation

{ Tcna2IDENotifier }

procedure Tcna2IDENotifier.AfterCompile(Succeeded: Boolean);
begin

end;

procedure Tcna2IDENotifier.AfterSave;
begin

end;

procedure Tcna2IDENotifier.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin

end;

procedure Tcna2IDENotifier.BeforeSave;
begin

end;

procedure Tcna2IDENotifier.Destroyed;
begin

end;

procedure Tcna2IDENotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
var
  ModServ : IOTAModuleServices;
  Module : IOTAModule;
  Editor : IOTAEditor;
  FormEditor : IOTAFormEditor;
  Hook : Tcna2FormEditorHook;
begin
  Cancel := false;

  if Supports(BorlandIDEServices, IOTAModuleServices, ModServ) then
  begin
    Module := ModServ.FindModule(FileName);
    if Assigned(Module) then
    begin
      Editor := Module.ModuleFileEditors[1];

      if Supports(Editor, IOTAFormEditor, FormEditor) then
      begin
        case NotifyCode of
          ofnFileOpened: Tcna2FormEditorHook.Create(FormEditor);
          ofnFileClosing:
          begin
            if cna2FormEditorHooks.FindByEditor(FormEditor, Hook) then
              Hook.Unregister;
          end;
        end;
      end;
    end;
  end;
end;

procedure Tcna2IDENotifier.Modified;
begin

end;

initialization
 

finalization

end.
