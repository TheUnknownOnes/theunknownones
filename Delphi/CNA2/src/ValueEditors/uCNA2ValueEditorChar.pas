//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uCNA2ValueEditorChar;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uCNA2ValueEditorString, StdCtrls,
  TypInfo;

type
  Tcna2ValueEditorChar = class(Tcna2ValueEditorString)
  private
    { Private-Deklarationen }
  public
    class function CanHandle(ATypeInfo : PTypeInfo) : Boolean; override;
  end;

implementation

{$R *.dfm}

{ Tcna2ValueEditorChar }

class function Tcna2ValueEditorChar.CanHandle(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := ATypeInfo.Kind in [tkChar, tkWChar];
end;

end.
