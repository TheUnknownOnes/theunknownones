//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uCNA2IDENotifier;

interface

uses
  ToolsAPI,
  Classes,
  SysUtils,
  uCNA2FormEditorHooks,
  Dialogs;

type
  Tcna2IDENotifier = class(TInterfacedObject,
                           IOTAIDENotifier,
                           IOTANotifier)
  private
    FNotifierIndex : Integer;

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
  public
    constructor Create();
    destructor Destroy(); override;

    procedure Unregister;
  end;

var
  cna2IDENotifier : Tcna2IDENotifier;

implementation

uses uCNA2Settings;

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

constructor Tcna2IDENotifier.Create;
var
  Serv : IOTAServices;
begin
  inherited;
  
  if Supports(BorlandIDEServices, IOTAServices, Serv) then
    FNotifierIndex := Serv.AddNotifier(Self);
end;

destructor Tcna2IDENotifier.Destroy;
begin
  Unregister;
  
  cna2IDENotifier := nil;

  inherited;
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

  if NotifyCode in [ofnFileOpened, ofnFileClosing] then
  begin
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


end;

procedure Tcna2IDENotifier.Modified;
begin

end;

procedure Tcna2IDENotifier.Unregister;
var
  Serv : IOTAServices;
begin
  if FNotifierIndex <> -1 then

  if Supports(BorlandIDEServices, IOTAServices, Serv) then
  begin
    Serv.RemoveNotifier(FNotifierIndex);
    FNotifierIndex := -1;
  end;
end;

initialization
  cna2IDENotifier := nil; 

finalization

end.
