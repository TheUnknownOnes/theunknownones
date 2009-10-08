program DPShouterV2;



uses
  Forms,
  unit_FormMain in 'unit_FormMain.pas' {form_Login},
  unit_Data in 'unit_Data.pas' {Data: TDataModule},
  unit_FormSB in 'unit_FormSB.pas' {form_SB},
  Unit_FormSmileySelect in 'Unit_FormSmileySelect.pas' {Form_SmileySelect},
  unit_FormOptions in 'unit_FormOptions.pas' {form_Options};

{$R *.res}
{$R 'images.res'}

begin
  Application.Initialize;
  Application.CreateForm(TData, Data);
  Application.CreateForm(Tform_Login, form_Login);
  Application.Run;
end.
