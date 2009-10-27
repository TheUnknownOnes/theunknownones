unit uTUOCommonIntf;

interface

uses
  Classes, Menus;

type
  ITUOCommon = interface
    function GetRegistryRootKey: String;
    function GetToolsMenuItem: TMenuItem;

    property RegistryRootKey: String read GetRegistryRootKey;
    property ToolsMenuItem: TMenuItem read GetToolsMenuItem;
  end;

function GetTUOCommon: ITUOCommon;

implementation

uses
  uTUOCommonMain;

function GetTUOCommon: ITUOCommon;
begin
  Result:=FTUOCommon;
end;

end.
