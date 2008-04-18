unit uEditFeed;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvExMask, JvToolEdit, ExtCtrls, uData, uSysTools,
  ShlObj;

type
  Tform_EditFeed = class(TForm)
    pan_Bottom: TPanel;
    btn_OK: TButton;
    btn_Cancel: TButton;
    lbl_Label1: TLabel;
    ed_Name: TEdit;
    ed_Source: TEdit;
    lbl_Label2: TLabel;
    ed_SaveTo: TJvDirectoryEdit;
    lbl_Label3: TLabel;
    gb_Options: TGroupBox;
    cb_RenameFile: TCheckBox;
  private
    FFeed: TFeed;
    procedure SetFeed(const Value: TFeed);
  public
    property Feed : TFeed read FFeed write SetFeed;
    
    class function NewFeed(var AFeed : TFeed) : Boolean;
  end;

implementation

{$R *.dfm}


{ Tform_EditFeed }

class function Tform_EditFeed.NewFeed(var AFeed: TFeed): Boolean;
var
  Form : Tform_EditFeed;
begin
  Application.CreateForm(Tform_EditFeed, Form);
  
  try
    Form.Feed := AFeed;
    Result := IsPositiveResult(Form.ShowModal);

    if Result then
    begin
      if not Assigned(AFeed) then
        AFeed := TFeed.Create;

      AFeed.Name := Form.ed_Name.Text;
      AFeed.Source := Form.ed_Source.Text;
      AFeed.SaveTo := Form.ed_SaveTo.Text;
    end;
  finally
    Form.Release;
  end;
end;

procedure Tform_EditFeed.SetFeed(const Value: TFeed);
begin
  FFeed := Value;

  if Assigned(FFeed) then
  begin
    ed_Name.Text := Feed.Name;
    ed_Source.Text := Feed.Source;
    ed_SaveTo.Text := Feed.SaveTo;
  end
  else
  begin
    ed_Name.Text := 'Give me a name please';
    ed_Source.Text := 'http://';
    ed_SaveTo.Text := GetShellFolder(CSIDL_PERSONAL);
  end;
end;

end.
