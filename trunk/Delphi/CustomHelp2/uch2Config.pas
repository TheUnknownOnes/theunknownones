unit uch2Config;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  Tch2FormConfig = class(TForm)
    PC: TPageControl;
    ts_General: TTabSheet;
  private
    { Private-Deklarationen }
  public
    class procedure Execute;
  end;

implementation

{$R *.dfm}

{ Tch2FormConfig }

class procedure Tch2FormConfig.Execute;
var
  form : Tch2FormConfig;
begin
  form := Tch2FormConfig.Create(nil);
  try
    form.ShowModal;
  finally
    form.Free;
  end;
end;

end.
