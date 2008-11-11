unit uCNA2FrameValueEditorString;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uCNA2FrameValueEditorBase, StdCtrls;

type
  Tframe_ValueEditorString = class(Tframe_ValueEditorBase)
    ed_Value: TEdit;
  protected
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}


{ Tframe_ValueEditorString }

function Tframe_ValueEditorString.GetValue: Variant;
begin
  Result := ed_Value.Text;
end;

procedure Tframe_ValueEditorString.SetValue(const Value: Variant);
begin
  ed_Value.Text := Value;
end;

end.
