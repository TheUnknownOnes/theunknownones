unit uTaskBarList_Reg;

{-----------------------------------------------------------------------------
 Project: uTaskBarList
 Purpose: Register components

 (c) by TheUnknownOnes under Apache License 2.0
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}


interface

uses
  Classes, uTaskBarListThumbButtons, uTaskBarListOverlayIcon, uTaskBarListTab,
  uTaskBarListProgress;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TUO', [TTaskBarListThumbButtons,
                             TTaskBarListOverlayIcon,
                             TTaskbarListFormTab,
                             TTaskbarListControlTab,
                             TTaskbarListProgress]);
end;


end.
