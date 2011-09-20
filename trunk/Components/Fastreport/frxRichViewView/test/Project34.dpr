program Project34;

uses
  Forms,
  Unit42 in 'Unit42.pas' {Form42},
  ufrxRichViewView in '..\ufrxRichViewView.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm42, Form42);
  Application.Run;
end.
