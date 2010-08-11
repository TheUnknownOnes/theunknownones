unit uTaskBarList_Reg;

{-----------------------------------------------------------------------------
 Project: uTaskBarList
 Purpose: Register components

 (c) by TheUnknownOnes under dwywbdbu license - see http://theunknownones.googlecode.com/svn/ for the license
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
                             TTaskbarListTab,
                             TTaskbarListProgress]);
end;


end.
