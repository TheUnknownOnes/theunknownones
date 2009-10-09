{-----------------------------------------------------------------------------
 Purpose: Open the welcomepage if there is no active project 
 
 (c) by TheUnknownOnes
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uWPFMain;

interface

uses
 Classes,
 Dialogs,
 Windows,
 Controls,
 uWPFWelcomePageIntf,
 SysUtils,
 Forms,
 ToolsAPI;

type
  IWPF = interface
    ['{6A9BA2D1-86C6-449E-846F-A9E86597ED6C}']
    procedure Unregister;
  end;

  TWPF = class(TInterfacedObject, IOTAIDENotifier, IWPF)
  private
    FServices : IOTAServices;
    FModuleServices : IOTAModuleServices;
    FIDEServices : INTAServices;
    FNotifierIndex : Integer;

    {$REGION 'IOTANotifier'}
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    {$ENDREGION}

    {$REGION 'IOTAIDENotifier'}
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
    {$ENDREGION}

    {$REGION 'IWPF'}
    procedure Unregister;
    {$ENDREGION}

    procedure ForceWP;
    function WPAvailable : Boolean;
    procedure ShowWP;
    procedure NavToURL(AURL : String);
  public
    constructor Create;
    destructor Destroy; override;
  end;



procedure Register;

implementation

uses
  ActnList;

{$i jedi.inc}

const
  DefaultWPURL =
    {$IFDEF DELPHI2006}'bds:/default.htm'{$ENDIF}
    {$IFDEF DELPHI2007}'bds:/default.htm'{$ENDIF}
    {$IFDEF DELPHI2009}'bds:/default.htm'{$ENDIF}
    {$IFDEF DELPHI2010}'bds:/default.htm'{$ENDIF} //?
    ;


var
  FWPF : IWPF;

procedure Register;
begin
end;

{ TWPF }

procedure TWPF.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TWPF.AfterSave;
begin

end;

procedure TWPF.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin

end;

procedure TWPF.BeforeSave;
begin

end;

constructor TWPF.Create;
begin
  FServices := BorlandIDEServices as IOTAServices;
  FModuleServices := BorlandIDEServices as IOTAModuleServices;
  FIDEServices := BorlandIDEServices as INTAServices;

  FNotifierIndex := FServices.AddNotifier(Self);
end;

destructor TWPF.Destroy;
begin
  inherited;
end;

procedure TWPF.Destroyed;
begin

end;

procedure TWPF.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  if NotifyCode = ofnFileClosing then
  begin
    if not Assigned(FModuleServices.GetActiveProject) then
      ForceWP;
  end;
end;

procedure TWPF.Modified;
begin

end;              

procedure TWPF.NavToURL(AURL: String);
begin
  GoURL(DefaultWPURL);
end;

procedure TWPF.ShowWP;
//Thanks to Daniel
var
  actList : TCustomActionList;
  idx : integer;
  act : TContainedAction;
begin
  actList:= FIDEServices.ActionList;

  for idx:= 0 to actList.ActionCount-1 do
  begin
    act:= actList.Actions[idx];

    if act.Name = 'ViewWelcomePageCommand' then
      act.Execute;
  end;
end;

procedure TWPF.ForceWP;
begin
  if not WPAvailable then
    exit;

  ShowWP;

  Application.ProcessMessages;

  NavToURL(DefaultWPURL);
end;

procedure TWPF.Unregister;
begin
  FServices.RemoveNotifier(FNotifierIndex);
end;

function TWPF.WPAvailable: Boolean;
begin
  Result := (GetModuleHandle('startpageide100.bpl') +
             GetModuleHandle('startpageide120.bpl') +
             GetModuleHandle('startpageide140.bpl')) > 0;
end;

initialization
  FWPF := TWPF.Create;

finalization
  if Assigned(FWPF) then
    FWPF.Unregister;

end.
